using System.Text.Json;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services
{
    /// <summary>
    /// Background job service for managing and executing scheduled batch jobs.
    /// Handles job scheduling, execution, retry logic, and recurrence patterns.
    /// </summary>
    public class BackgroundJobService : IBackgroundJobService
    {
        private readonly IBatchJobRepository _batchJobRepository;
        private readonly IReportGenerationService _reportGenerationService;
        private readonly ILogger<BackgroundJobService> _logger;

        public BackgroundJobService(
            IBatchJobRepository batchJobRepository,
            IReportGenerationService reportGenerationService,
            ILogger<BackgroundJobService> logger)
        {
            _batchJobRepository = batchJobRepository ?? throw new ArgumentNullException(nameof(batchJobRepository));
            _reportGenerationService = reportGenerationService ?? throw new ArgumentNullException(nameof(reportGenerationService));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <inheritdoc />
        public async Task<BatchJob> ScheduleJobAsync(
            BatchJob job,
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Scheduling new batch job: {JobName}", job.JobName);

            // Calculate first execution time
            job.NextExecutionTime = CalculateNextExecutionTime(job, DateTime.UtcNow);
            job.CreatedDate = DateTime.UtcNow;
            job.Status = "ACTIVE";
            job.RetryCount = 0;

            await _batchJobRepository.AddAsync(job, cancellationToken);
            await _batchJobRepository.SaveChangesAsync(cancellationToken);

            _logger.LogInformation("Job {JobId} scheduled for next execution at {NextExecution}",
                job.JobId, job.NextExecutionTime);

            return job;
        }

        /// <inheritdoc />
        public async Task<BatchJobExecution> ExecuteJobAsync(
            int jobId,
            string executedBy = "SYSTEM",
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Executing job {JobId} by {ExecutedBy}", jobId, executedBy);

            BatchJob? job = await _batchJobRepository.GetByIdAsync(jobId, cancellationToken);
            if (job == null)
            {
                throw new InvalidOperationException($"Job {jobId} not found");
            }

            // Create execution record
            var execution = new BatchJobExecution
            {
                JobId = jobId,
                StartTime = DateTime.UtcNow,
                Status = "RUNNING",
                ExecutedBy = executedBy,
                ExecutionLog = $"Job started at {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss}\n"
            };

            await _batchJobRepository.AddExecutionAsync(execution, cancellationToken);
            await _batchJobRepository.SaveChangesAsync(cancellationToken);

            // Execute job asynchronously
            _ = Task.Run(async () => await ProcessJobExecutionAsync(job, execution, cancellationToken), cancellationToken);

            return execution;
        }

        /// <summary>
        /// Internal method to process job execution.
        /// </summary>
        private async Task ProcessJobExecutionAsync(
            BatchJob job,
            BatchJobExecution execution,
            CancellationToken cancellationToken)
        {
            try
            {
                _logger.LogInformation("Processing job execution {ExecutionId} for job {JobId}",
                    execution.ExecutionId, job.JobId);

                // Deserialize report parameters
                ReportGenerationRequestDto? reportRequest = JsonSerializer.Deserialize<ReportGenerationRequestDto>(job.ReportParameters);
                if (reportRequest == null)
                {
                    throw new InvalidOperationException("Invalid report parameters");
                }

                // Generate report
                execution.ExecutionLog += $"Generating report with parameters: {job.ReportParameters}\n";
                Guid reportId = await _reportGenerationService.GenerateReportAsync(reportRequest, cancellationToken);

                // Poll for report completion
                ReportStatusDto reportStatus = await WaitForReportCompletionAsync(reportId, cancellationToken);

                execution.EndTime = DateTime.UtcNow;
                execution.RecordsProcessed = reportStatus.RecordsProcessed;
                execution.OutputFilePath = reportStatus.Files.Count > 0
                    ? string.Join(", ", reportStatus.Files.Select(f => f.FileName))
                    : string.Empty;

                if (reportStatus.Status == "Completed")
                {
                    execution.Status = "SUCCESS";
                    execution.ExecutionLog += $"Report generated successfully: {execution.OutputFilePath}\n";
                    _logger.LogInformation("Job {JobId} execution {ExecutionId} completed successfully",
                        job.JobId, execution.ExecutionId);

                    // Reset retry count on success
                    job.RetryCount = 0;
                }
                else
                {
                    execution.Status = "FAILED";
                    execution.ErrorMessage = reportStatus.ErrorMessage ?? "Report generation failed";
                    execution.ExecutionLog += $"ERROR: {execution.ErrorMessage}\n";
                    _logger.LogError("Job {JobId} execution {ExecutionId} failed: {Error}",
                        job.JobId, execution.ExecutionId, execution.ErrorMessage);

                    // Increment retry count
                    job.RetryCount++;
                }

                // Update execution record
                _batchJobRepository.UpdateExecution(execution);

                // Calculate next execution time for recurring jobs
                if (job.RecurrencePattern != "ONCE")
                {
                    job.LastExecutionTime = DateTime.UtcNow;
                    job.NextExecutionTime = CalculateNextExecutionTime(job, DateTime.UtcNow);
                    job.UpdatedDate = DateTime.UtcNow;
                    _logger.LogInformation("Next execution for job {JobId} scheduled at {NextExecution}",
                        job.JobId, job.NextExecutionTime);
                }
                else
                {
                    // One-time job, mark as completed
                    job.Status = "COMPLETED";
                    job.LastExecutionTime = DateTime.UtcNow;
                    job.NextExecutionTime = null;
                }

                _batchJobRepository.Update(job);
                await _batchJobRepository.SaveChangesAsync(cancellationToken);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error processing job execution {ExecutionId}", execution.ExecutionId);

                execution.EndTime = DateTime.UtcNow;
                execution.Status = "FAILED";
                execution.ErrorMessage = ex.Message;
                execution.ExecutionLog += $"ERROR: {ex.Message}\n{ex.StackTrace}\n";

                _batchJobRepository.UpdateExecution(execution);

                // Increment retry count
                job.RetryCount++;
                if (job.RetryCount >= job.MaxRetries)
                {
                    job.Status = "FAILED";
                    job.IsEnabled = false;
                    _logger.LogError("Job {JobId} exceeded max retries ({MaxRetries}), disabling job",
                        job.JobId, job.MaxRetries);
                }

                _batchJobRepository.Update(job);
                await _batchJobRepository.SaveChangesAsync(cancellationToken);
            }
        }

        /// <summary>
        /// Waits for report generation to complete.
        /// </summary>
        private async Task<ReportStatusDto> WaitForReportCompletionAsync(
            Guid reportId,
            CancellationToken cancellationToken)
        {
            var maxWaitTime = TimeSpan.FromMinutes(30);
            var pollInterval = TimeSpan.FromSeconds(5);
            DateTime startTime = DateTime.UtcNow;

            while (DateTime.UtcNow - startTime < maxWaitTime)
            {
                ReportStatusDto status = await _reportGenerationService.GetReportStatusAsync(reportId, cancellationToken);
                if (status == null)
                {
                    throw new InvalidOperationException($"Report {reportId} not found");
                }

                if (status.Status == "Completed" || status.Status == "Failed")
                {
                    return status;
                }

                await Task.Delay(pollInterval, cancellationToken);
            }

            throw new TimeoutException($"Report {reportId} generation timed out after {maxWaitTime.TotalMinutes} minutes");
        }

        /// <inheritdoc />
        public async Task<bool> CancelJobAsync(
            int executionId,
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Cancelling job execution {ExecutionId}", executionId);

            IReadOnlyList<BatchJobExecution> executions = await _batchJobRepository.GetRunningExecutionsAsync(cancellationToken);
            BatchJobExecution? execution = executions.FirstOrDefault(e => e.ExecutionId == executionId);

            if (execution == null)
            {
                _logger.LogWarning("Execution {ExecutionId} not found or not running", executionId);
                return false;
            }

            execution.Status = "CANCELLED";
            execution.EndTime = DateTime.UtcNow;
            execution.ExecutionLog += $"Cancelled at {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss}\n";

            _batchJobRepository.UpdateExecution(execution);
            await _batchJobRepository.SaveChangesAsync(cancellationToken);

            _logger.LogInformation("Execution {ExecutionId} cancelled successfully", executionId);
            return true;
        }

        /// <inheritdoc />
        public async Task<BatchJobExecution?> GetJobStatusAsync(
            int executionId,
            CancellationToken cancellationToken = default)
        {
            IReadOnlyList<BatchJobExecution> executions = await _batchJobRepository.GetRunningExecutionsAsync(cancellationToken);
            return executions.FirstOrDefault(e => e.ExecutionId == executionId);
        }

        /// <inheritdoc />
        public async Task<int> ProcessScheduledJobsAsync(
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Processing scheduled jobs at {Time}", DateTime.UtcNow);

            IReadOnlyList<BatchJob> scheduledJobs = await _batchJobRepository.GetScheduledJobsAsync(DateTime.UtcNow, cancellationToken);

            _logger.LogInformation("Found {Count} jobs ready for execution", scheduledJobs.Count);

            var executedCount = 0;
            foreach (BatchJob job in scheduledJobs)
            {
                try
                {
                    await ExecuteJobAsync(job.JobId, "SYSTEM", cancellationToken);
                    executedCount++;
                }
                catch (Exception ex)
                {
                    _logger.LogError(ex, "Failed to execute scheduled job {JobId}", job.JobId);
                }
            }

            return executedCount;
        }

        /// <inheritdoc />
        public async Task<BatchJob> UpdateJobScheduleAsync(
            int jobId,
            BatchJob job,
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Updating job schedule for {JobId}", jobId);

            BatchJob? existingJob = await _batchJobRepository.GetByIdAsync(jobId, cancellationToken);
            if (existingJob == null)
            {
                throw new InvalidOperationException($"Job {jobId} not found");
            }

            existingJob.JobName = job.JobName;
            existingJob.Description = job.Description;
            existingJob.RecurrencePattern = job.RecurrencePattern;
            existingJob.ReportParameters = job.ReportParameters;
            existingJob.ExecutionHour = job.ExecutionHour;
            existingJob.ExecutionMinute = job.ExecutionMinute;
            existingJob.DayOfWeek = job.DayOfWeek;
            existingJob.DayOfMonth = job.DayOfMonth;
            existingJob.NotificationRecipients = job.NotificationRecipients;
            existingJob.UpdatedDate = DateTime.UtcNow;

            // Recalculate next execution time
            existingJob.NextExecutionTime = CalculateNextExecutionTime(existingJob, DateTime.UtcNow);

            _batchJobRepository.Update(existingJob);
            await _batchJobRepository.SaveChangesAsync(cancellationToken);

            return existingJob;
        }

        /// <inheritdoc />
        public async Task PauseJobAsync(int jobId, CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Pausing job {JobId}", jobId);

            BatchJob? job = await _batchJobRepository.GetByIdAsync(jobId, cancellationToken);
            if (job == null)
            {
                throw new InvalidOperationException($"Job {jobId} not found");
            }

            job.IsEnabled = false;
            job.Status = "PAUSED";
            job.UpdatedDate = DateTime.UtcNow;

            _batchJobRepository.Update(job);
            await _batchJobRepository.SaveChangesAsync(cancellationToken);
        }

        /// <inheritdoc />
        public async Task ResumeJobAsync(int jobId, CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Resuming job {JobId}", jobId);

            BatchJob? job = await _batchJobRepository.GetByIdAsync(jobId, cancellationToken);
            if (job == null)
            {
                throw new InvalidOperationException($"Job {jobId} not found");
            }

            job.IsEnabled = true;
            job.Status = "ACTIVE";
            job.UpdatedDate = DateTime.UtcNow;
            job.NextExecutionTime = CalculateNextExecutionTime(job, DateTime.UtcNow);

            _batchJobRepository.Update(job);
            await _batchJobRepository.SaveChangesAsync(cancellationToken);
        }

        /// <inheritdoc />
        public async Task DeleteJobAsync(int jobId, CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Deleting job {JobId}", jobId);

            BatchJob? job = await _batchJobRepository.GetByIdAsync(jobId, cancellationToken);
            if (job == null)
            {
                throw new InvalidOperationException($"Job {jobId} not found");
            }

            _batchJobRepository.Remove(job);
            await _batchJobRepository.SaveChangesAsync(cancellationToken);
        }

        /// <inheritdoc />
        public DateTime CalculateNextExecutionTime(BatchJob job, DateTime? fromTime = null)
        {
            DateTime baseTime = fromTime ?? DateTime.UtcNow;
            var executionHour = job.ExecutionHour ?? 0;
            var executionMinute = job.ExecutionMinute ?? 0;

            DateTime nextTime;

            switch (job.RecurrencePattern.ToUpper())
            {
                case "DAILY":
                    nextTime = baseTime.Date.AddHours(executionHour).AddMinutes(executionMinute);
                    if (nextTime <= baseTime)
                    {
                        nextTime = nextTime.AddDays(1);
                    }
                    break;

                case "WEEKLY":
                    var targetDayOfWeek = (DayOfWeek)(job.DayOfWeek ?? 0);
                    nextTime = baseTime.Date.AddHours(executionHour).AddMinutes(executionMinute);

                    // Find next occurrence of target day of week
                    while (nextTime.DayOfWeek != targetDayOfWeek || nextTime <= baseTime)
                    {
                        nextTime = nextTime.AddDays(1);
                    }
                    break;

                case "MONTHLY":
                    var targetDay = job.DayOfMonth ?? 1;
                    nextTime = new DateTime(baseTime.Year, baseTime.Month, 1)
                        .AddHours(executionHour)
                        .AddMinutes(executionMinute);

                    // Handle day of month
                    var daysInMonth = DateTime.DaysInMonth(nextTime.Year, nextTime.Month);
                    var actualDay = Math.Min(targetDay, daysInMonth);
                    nextTime = nextTime.AddDays(actualDay - 1);

                    if (nextTime <= baseTime)
                    {
                        // Move to next month
                        nextTime = nextTime.AddMonths(1);
                        nextTime = new DateTime(nextTime.Year, nextTime.Month, 1)
                            .AddHours(executionHour)
                            .AddMinutes(executionMinute);
                        daysInMonth = DateTime.DaysInMonth(nextTime.Year, nextTime.Month);
                        actualDay = Math.Min(targetDay, daysInMonth);
                        nextTime = nextTime.AddDays(actualDay - 1);
                    }
                    break;

                case "ONCE":
                    // For one-time jobs, use the specified time or immediate execution
                    nextTime = job.NextExecutionTime ?? baseTime;
                    break;

                default:
                    throw new InvalidOperationException($"Unknown recurrence pattern: {job.RecurrencePattern}");
            }

            return nextTime;
        }

        /// <inheritdoc />
        public async Task<BatchJobExecution> RetryJobAsync(
            int executionId,
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation("Retrying failed job execution {ExecutionId}", executionId);

            IReadOnlyList<BatchJobExecution> executions = await _batchJobRepository.GetRunningExecutionsAsync(cancellationToken);
            BatchJobExecution? failedExecution = executions.FirstOrDefault(e => e.ExecutionId == executionId);

            if (failedExecution == null || failedExecution.Status != "FAILED")
            {
                throw new InvalidOperationException($"Execution {executionId} not found or not in failed state");
            }

            return await ExecuteJobAsync(failedExecution.JobId, "RETRY", cancellationToken);
        }
    }
}
