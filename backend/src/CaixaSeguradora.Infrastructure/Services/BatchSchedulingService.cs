using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using CaixaSeguradora.Core.Interfaces;
using Hangfire;
using Hangfire.Storage;
using Hangfire.Storage.Monitoring;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Implementation of batch job scheduling service using Hangfire.
/// Supports recurring jobs with cron expressions and one-time scheduled jobs.
/// </summary>
public class BatchSchedulingService : IBatchSchedulingService
{
    private readonly IBackgroundJobClient _backgroundJobClient;
    private readonly IRecurringJobManager _recurringJobManager;
    private readonly ILogger<BatchSchedulingService> _logger;

    public BatchSchedulingService(
        IBackgroundJobClient backgroundJobClient,
        IRecurringJobManager recurringJobManager,
        ILogger<BatchSchedulingService> logger)
    {
        _backgroundJobClient = backgroundJobClient;
        _recurringJobManager = recurringJobManager;
        _logger = logger;
    }

    public Task<string> ScheduleRecurringJobAsync(
        string jobName,
        string cronExpression,
        CancellationToken cancellationToken = default)
    {
        try
        {
            _logger.LogInformation("Scheduling recurring job '{JobName}' with cron: {CronExpression}",
                jobName, cronExpression);

            // Create or update recurring job
            // The job itself will be implemented in the BackgroundJobService
            _recurringJobManager.AddOrUpdate(
                jobName,
                () => Console.WriteLine($"Executing recurring job: {jobName}"),
                cronExpression,
                new RecurringJobOptions
                {
                    TimeZone = TimeZoneInfo.Local
                });

            _logger.LogInformation("Recurring job '{JobName}' scheduled successfully", jobName);
            return Task.FromResult(jobName);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao agendar job recorrente '{JobName}'", jobName);
            throw new InvalidOperationException(
                $"Falha ao agendar job recorrente '{jobName}': {ex.Message}", ex);
        }
    }

    public Task<string> ScheduleDelayedJobAsync(
        string jobName,
        DateTimeOffset scheduledTime,
        CancellationToken cancellationToken = default)
    {
        try
        {
            TimeSpan delay = scheduledTime - DateTimeOffset.UtcNow;
            if (delay < TimeSpan.Zero)
            {
                throw new ArgumentException(
                    "O horário agendado deve ser no futuro",
                    nameof(scheduledTime));
            }

            _logger.LogInformation(
                "Scheduling delayed job '{JobName}' to run at {ScheduledTime}",
                jobName, scheduledTime);

            var jobId = _backgroundJobClient.Schedule(
                () => Console.WriteLine($"Executing delayed job: {jobName}"),
                delay);

            _logger.LogInformation(
                "Delayed job '{JobName}' scheduled successfully with ID: {JobId}",
                jobName, jobId);

            return Task.FromResult(jobId);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao agendar job com atraso '{JobName}'", jobName);
            throw new InvalidOperationException(
                $"Falha ao agendar job com atraso '{jobName}': {ex.Message}", ex);
        }
    }

    public Task<bool> CancelJobAsync(
        string jobId,
        CancellationToken cancellationToken = default)
    {
        try
        {
            _logger.LogInformation("Cancelling job with ID: {JobId}", jobId);

            // Try to delete as background job
            var deleted = _backgroundJobClient.Delete(jobId);

            if (!deleted)
            {
                // Try to remove as recurring job
                _recurringJobManager.RemoveIfExists(jobId);
                _logger.LogInformation("Recurring job '{JobId}' removed", jobId);
                return Task.FromResult(true);
            }

            _logger.LogInformation("Job '{JobId}' cancelled successfully", jobId);
            return Task.FromResult(true);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao cancelar job '{JobId}'", jobId);
            return Task.FromResult(false);
        }
    }

    public Task<IEnumerable<ScheduledJobInfo>> GetScheduledJobsAsync(
        CancellationToken cancellationToken = default)
    {
        try
        {
            var jobs = new List<ScheduledJobInfo>();

            using (IStorageConnection connection = JobStorage.Current.GetConnection())
            {
                IMonitoringApi monitoringApi = JobStorage.Current.GetMonitoringApi();

                // Get recurring jobs
                List<RecurringJobDto> recurringJobs = connection.GetRecurringJobs();
                foreach (RecurringJobDto? job in recurringJobs)
                {
                    jobs.Add(new ScheduledJobInfo
                    {
                        JobId = job.Id,
                        JobName = job.Id,
                        Type = JobType.Recurring,
                        CronExpression = job.Cron,
                        NextExecutionTime = job.NextExecution,
                        LastExecutionTime = job.LastExecution,
                        State = MapJobState(job.LastJobState),
                        CreatedAt = job.CreatedAt ?? DateTimeOffset.UtcNow
                    });
                }

                // Get scheduled jobs
                JobList<ScheduledJobDto> scheduledJobs = monitoringApi.ScheduledJobs(0, int.MaxValue);
                foreach (KeyValuePair<string, ScheduledJobDto> job in scheduledJobs)
                {
                    jobs.Add(new ScheduledJobInfo
                    {
                        JobId = job.Key,
                        JobName = job.Value.Job?.Method?.Name ?? "Unknown",
                        Type = JobType.Delayed,
                        ScheduledTime = job.Value.EnqueueAt,
                        State = JobState.Scheduled,
                        CreatedAt = job.Value.EnqueueAt
                    });
                }
            }

            _logger.LogDebug("Retrieved {Count} scheduled jobs", jobs.Count);
            return Task.FromResult<IEnumerable<ScheduledJobInfo>>(jobs);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao obter lista de jobs agendados");
            return Task.FromResult<IEnumerable<ScheduledJobInfo>>(Enumerable.Empty<ScheduledJobInfo>());
        }
    }

    public Task<ScheduledJobInfo?> GetJobInfoAsync(
        string jobId,
        CancellationToken cancellationToken = default)
    {
        try
        {
            using (IStorageConnection connection = JobStorage.Current.GetConnection())
            {
                // Check recurring jobs first
                List<RecurringJobDto> recurringJobs = connection.GetRecurringJobs();
                RecurringJobDto? recurringJob = recurringJobs.FirstOrDefault(j => j.Id == jobId);

                if (recurringJob != null)
                {
                    return Task.FromResult<ScheduledJobInfo?>(new ScheduledJobInfo
                    {
                        JobId = recurringJob.Id,
                        JobName = recurringJob.Id,
                        Type = JobType.Recurring,
                        CronExpression = recurringJob.Cron,
                        NextExecutionTime = recurringJob.NextExecution,
                        LastExecutionTime = recurringJob.LastExecution,
                        State = MapJobState(recurringJob.LastJobState),
                        CreatedAt = recurringJob.CreatedAt ?? DateTimeOffset.UtcNow
                    });
                }

                // Check scheduled/background jobs
                IMonitoringApi monitoringApi = JobStorage.Current.GetMonitoringApi();
                JobDetailsDto jobDetails = monitoringApi.JobDetails(jobId);

                if (jobDetails != null)
                {
                    return Task.FromResult<ScheduledJobInfo?>(new ScheduledJobInfo
                    {
                        JobId = jobId,
                        JobName = jobDetails.Job?.Method?.Name ?? "Unknown",
                        Type = JobType.Delayed,
                        State = MapHangfireState(jobDetails.History?.FirstOrDefault()?.StateName),
                        CreatedAt = jobDetails.CreatedAt ?? DateTimeOffset.UtcNow
                    });
                }
            }

            _logger.LogWarning("Job with ID '{JobId}' not found", jobId);
            return Task.FromResult<ScheduledJobInfo?>(null);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao obter informações do job '{JobId}'", jobId);
            return Task.FromResult<ScheduledJobInfo?>(null);
        }
    }

    public Task<bool> TriggerJobNowAsync(
        string jobId,
        CancellationToken cancellationToken = default)
    {
        try
        {
            _logger.LogInformation("Triggering job '{JobId}' to run immediately", jobId);

            using (IStorageConnection connection = JobStorage.Current.GetConnection())
            {
                // Check if it's a recurring job
                List<RecurringJobDto> recurringJobs = connection.GetRecurringJobs();
                RecurringJobDto? recurringJob = recurringJobs.FirstOrDefault(j => j.Id == jobId);

                if (recurringJob != null)
                {
                    // Trigger recurring job immediately
                    _recurringJobManager.Trigger(jobId);
                    _logger.LogInformation("Recurring job '{JobId}' triggered successfully", jobId);
                    return Task.FromResult(true);
                }
            }

            // If not a recurring job, try to requeue it
            var requeued = _backgroundJobClient.Requeue(jobId);
            if (requeued)
            {
                _logger.LogInformation("Job '{JobId}' requeued successfully", jobId);
                return Task.FromResult(true);
            }

            _logger.LogWarning("Job '{JobId}' could not be triggered", jobId);
            return Task.FromResult(false);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao disparar job '{JobId}'", jobId);
            return Task.FromResult(false);
        }
    }

    private static JobState MapJobState(string? hangfireState)
    {
        return hangfireState?.ToLowerInvariant() switch
        {
            "scheduled" => JobState.Scheduled,
            "enqueued" => JobState.Scheduled,
            "processing" => JobState.Running,
            "succeeded" => JobState.Completed,
            "failed" => JobState.Failed,
            "deleted" => JobState.Cancelled,
            _ => JobState.Scheduled
        };
    }

    private static JobState MapHangfireState(string? hangfireState)
    {
        return hangfireState?.ToLowerInvariant() switch
        {
            "scheduled" => JobState.Scheduled,
            "enqueued" => JobState.Scheduled,
            "processing" => JobState.Running,
            "succeeded" => JobState.Completed,
            "failed" => JobState.Failed,
            "deleted" => JobState.Cancelled,
            "awaiting" => JobState.Scheduled,
            _ => JobState.Scheduled
        };
    }
}
