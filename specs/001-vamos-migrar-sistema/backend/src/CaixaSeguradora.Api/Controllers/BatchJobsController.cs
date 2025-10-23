using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers
{
    /// <summary>
    /// Controller for managing batch jobs and scheduled report generation.
    /// Supports job creation, scheduling, execution, and history tracking.
    /// </summary>
    [ApiController]
    [Route("api/batch-jobs")]
    [Produces("application/json")]
    public class BatchJobsController : ControllerBase
    {
        private readonly IBatchJobRepository _batchJobRepository;
        private readonly IBackgroundJobService _backgroundJobService;
        private readonly ILogger<BatchJobsController> _logger;

        public BatchJobsController(
            IBatchJobRepository batchJobRepository,
            IBackgroundJobService backgroundJobService,
            ILogger<BatchJobsController> logger)
        {
            _batchJobRepository = batchJobRepository ?? throw new ArgumentNullException(nameof(batchJobRepository));
            _backgroundJobService = backgroundJobService ?? throw new ArgumentNullException(nameof(backgroundJobService));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Creates a new batch job with specified schedule.
        /// </summary>
        /// <param name="request">Job configuration including recurrence pattern and report parameters</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Created batch job</returns>
        /// <response code="201">Job created successfully</response>
        /// <response code="400">Invalid request parameters</response>
        [HttpPost]
        [ProducesResponseType(typeof(BatchJobResponseDto), StatusCodes.Status201Created)]
        [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
        public async Task<ActionResult<BatchJobResponseDto>> CreateBatchJob(
            [FromBody] BatchJobRequestDto request,
            CancellationToken cancellationToken)
        {
            try
            {
                _logger.LogInformation("Creating batch job: {JobName}", request.JobName);

                var job = new BatchJob
                {
                    JobName = request.JobName,
                    Description = request.Description,
                    RecurrencePattern = request.RecurrencePattern,
                    ReportParameters = request.ReportParameters,
                    ExecutionHour = request.ExecutionHour,
                    ExecutionMinute = request.ExecutionMinute,
                    DayOfWeek = request.DayOfWeek,
                    DayOfMonth = request.DayOfMonth,
                    NotificationRecipients = request.NotificationRecipients,
                    MaxRetries = request.MaxRetries,
                    CreatedBy = request.CreatedBy
                };

                var createdJob = await _backgroundJobService.ScheduleJobAsync(job, cancellationToken);

                var response = MapToResponseDto(createdJob);

                return CreatedAtAction(nameof(GetBatchJob), new { jobId = createdJob.JobId }, response);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error creating batch job");
                return BadRequest(new ErrorResponse
                {
                    Error = "Failed to create batch job",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Gets all batch jobs.
        /// </summary>
        /// <param name="status">Optional filter by status (ACTIVE, PAUSED, COMPLETED, FAILED)</param>
        /// <param name="createdBy">Optional filter by creator username</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>List of batch jobs</returns>
        [HttpGet]
        [ProducesResponseType(typeof(List<BatchJobResponseDto>), StatusCodes.Status200OK)]
        public async Task<ActionResult<List<BatchJobResponseDto>>> GetBatchJobs(
            [FromQuery] string? status = null,
            [FromQuery] string? createdBy = null,
            CancellationToken cancellationToken = default)
        {
            try
            {
                IReadOnlyList<BatchJob> jobs;

                if (!string.IsNullOrEmpty(status))
                {
                    jobs = await _batchJobRepository.GetJobsByStatusAsync(status, cancellationToken);
                }
                else if (!string.IsNullOrEmpty(createdBy))
                {
                    jobs = await _batchJobRepository.GetJobsByUserAsync(createdBy, cancellationToken);
                }
                else
                {
                    jobs = await _batchJobRepository.GetAllAsync(cancellationToken);
                }

                var response = jobs.Select(MapToResponseDto).ToList();

                return Ok(response);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error retrieving batch jobs");
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to retrieve batch jobs",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Gets a specific batch job by ID.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Batch job details</returns>
        [HttpGet("{jobId}")]
        [ProducesResponseType(typeof(BatchJobResponseDto), StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public async Task<ActionResult<BatchJobResponseDto>> GetBatchJob(
            int jobId,
            CancellationToken cancellationToken)
        {
            try
            {
                var job = await _batchJobRepository.GetByIdAsync(jobId, cancellationToken);
                if (job == null)
                {
                    return NotFound(new ErrorResponse
                    {
                        Error = "Job not found",
                        Message = $"Batch job {jobId} does not exist"
                    });
                }

                var response = MapToResponseDto(job);

                // Include latest execution
                var latestExecution = await _batchJobRepository.GetLatestExecutionAsync(jobId, cancellationToken);
                if (latestExecution != null)
                {
                    response.LatestExecution = new BatchJobExecutionSummaryDto
                    {
                        ExecutionId = latestExecution.ExecutionId,
                        StartTime = latestExecution.StartTime,
                        EndTime = latestExecution.EndTime,
                        Status = latestExecution.Status,
                        RecordsProcessed = latestExecution.RecordsProcessed,
                        ErrorMessage = latestExecution.ErrorMessage
                    };
                }

                return Ok(response);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error retrieving batch job {JobId}", jobId);
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to retrieve batch job",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Updates a batch job schedule.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="request">Updated job configuration</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Updated batch job</returns>
        [HttpPut("{jobId}")]
        [ProducesResponseType(typeof(BatchJobResponseDto), StatusCodes.Status200OK)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public async Task<ActionResult<BatchJobResponseDto>> UpdateBatchJob(
            int jobId,
            [FromBody] UpdateJobScheduleDto request,
            CancellationToken cancellationToken)
        {
            try
            {
                var existingJob = await _batchJobRepository.GetByIdAsync(jobId, cancellationToken);
                if (existingJob == null)
                {
                    return NotFound(new ErrorResponse
                    {
                        Error = "Job not found",
                        Message = $"Batch job {jobId} does not exist"
                    });
                }

                // Apply updates
                if (request.JobName != null) existingJob.JobName = request.JobName;
                if (request.Description != null) existingJob.Description = request.Description;
                if (request.RecurrencePattern != null) existingJob.RecurrencePattern = request.RecurrencePattern;
                if (request.ReportParameters != null) existingJob.ReportParameters = request.ReportParameters;
                if (request.ExecutionHour.HasValue) existingJob.ExecutionHour = request.ExecutionHour;
                if (request.ExecutionMinute.HasValue) existingJob.ExecutionMinute = request.ExecutionMinute;
                if (request.DayOfWeek.HasValue) existingJob.DayOfWeek = request.DayOfWeek;
                if (request.DayOfMonth.HasValue) existingJob.DayOfMonth = request.DayOfMonth;
                if (request.NotificationRecipients != null) existingJob.NotificationRecipients = request.NotificationRecipients;
                if (request.MaxRetries.HasValue) existingJob.MaxRetries = request.MaxRetries.Value;

                var updatedJob = await _backgroundJobService.UpdateJobScheduleAsync(jobId, existingJob, cancellationToken);

                return Ok(MapToResponseDto(updatedJob));
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error updating batch job {JobId}", jobId);
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to update batch job",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Deletes a batch job and all its execution history.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        [HttpDelete("{jobId}")]
        [ProducesResponseType(StatusCodes.Status204NoContent)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public async Task<IActionResult> DeleteBatchJob(
            int jobId,
            CancellationToken cancellationToken)
        {
            try
            {
                await _backgroundJobService.DeleteJobAsync(jobId, cancellationToken);
                return NoContent();
            }
            catch (InvalidOperationException)
            {
                return NotFound(new ErrorResponse
                {
                    Error = "Job not found",
                    Message = $"Batch job {jobId} does not exist"
                });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error deleting batch job {JobId}", jobId);
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to delete batch job",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Manually triggers immediate execution of a batch job.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="request">Execution request with user info</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Execution record</returns>
        [HttpPost("{jobId}/execute")]
        [ProducesResponseType(typeof(BatchJobExecutionDto), StatusCodes.Status202Accepted)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public async Task<ActionResult<BatchJobExecutionDto>> ExecuteBatchJob(
            int jobId,
            [FromBody] ExecuteJobRequestDto request,
            CancellationToken cancellationToken)
        {
            try
            {
                var execution = await _backgroundJobService.ExecuteJobAsync(
                    jobId,
                    request.ExecutedBy,
                    cancellationToken);

                var response = MapToExecutionDto(execution);

                return Accepted($"/api/batch-jobs/{jobId}/executions/{execution.ExecutionId}", response);
            }
            catch (InvalidOperationException ex)
            {
                return NotFound(new ErrorResponse
                {
                    Error = "Job not found",
                    Message = ex.Message
                });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error executing batch job {JobId}", jobId);
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to execute batch job",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Gets execution history for a specific batch job.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="limit">Maximum number of executions to return (default 50)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>List of executions</returns>
        [HttpGet("{jobId}/executions")]
        [ProducesResponseType(typeof(List<BatchJobExecutionDto>), StatusCodes.Status200OK)]
        public async Task<ActionResult<List<BatchJobExecutionDto>>> GetJobExecutions(
            int jobId,
            [FromQuery] int limit = 50,
            CancellationToken cancellationToken = default)
        {
            try
            {
                var executions = await _batchJobRepository.GetJobHistoryAsync(jobId, limit, cancellationToken);
                var response = executions.Select(MapToExecutionDto).ToList();

                return Ok(response);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error retrieving executions for job {JobId}", jobId);
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to retrieve job executions",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Pauses a batch job (stops scheduling future executions).
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        [HttpPost("{jobId}/pause")]
        [ProducesResponseType(StatusCodes.Status204NoContent)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public async Task<IActionResult> PauseBatchJob(
            int jobId,
            CancellationToken cancellationToken)
        {
            try
            {
                await _backgroundJobService.PauseJobAsync(jobId, cancellationToken);
                return NoContent();
            }
            catch (InvalidOperationException)
            {
                return NotFound(new ErrorResponse
                {
                    Error = "Job not found",
                    Message = $"Batch job {jobId} does not exist"
                });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error pausing batch job {JobId}", jobId);
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to pause batch job",
                    Message = ex.Message
                });
            }
        }

        /// <summary>
        /// Resumes a paused batch job.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        [HttpPost("{jobId}/resume")]
        [ProducesResponseType(StatusCodes.Status204NoContent)]
        [ProducesResponseType(StatusCodes.Status404NotFound)]
        public async Task<IActionResult> ResumeBatchJob(
            int jobId,
            CancellationToken cancellationToken)
        {
            try
            {
                await _backgroundJobService.ResumeJobAsync(jobId, cancellationToken);
                return NoContent();
            }
            catch (InvalidOperationException)
            {
                return NotFound(new ErrorResponse
                {
                    Error = "Job not found",
                    Message = $"Batch job {jobId} does not exist"
                });
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error resuming batch job {JobId}", jobId);
                return StatusCode(500, new ErrorResponse
                {
                    Error = "Failed to resume batch job",
                    Message = ex.Message
                });
            }
        }

        // Helper methods for mapping entities to DTOs
        private static BatchJobResponseDto MapToResponseDto(BatchJob job)
        {
            return new BatchJobResponseDto
            {
                JobId = job.JobId,
                JobName = job.JobName,
                Description = job.Description,
                RecurrencePattern = job.RecurrencePattern,
                Status = job.Status,
                NextExecutionTime = job.NextExecutionTime,
                LastExecutionTime = job.LastExecutionTime,
                CreatedBy = job.CreatedBy,
                CreatedDate = job.CreatedDate,
                IsEnabled = job.IsEnabled,
                RetryCount = job.RetryCount,
                MaxRetries = job.MaxRetries,
                NotificationRecipients = job.NotificationRecipients
            };
        }

        private static BatchJobExecutionDto MapToExecutionDto(BatchJobExecution execution)
        {
            return new BatchJobExecutionDto
            {
                ExecutionId = execution.ExecutionId,
                JobId = execution.JobId,
                StartTime = execution.StartTime,
                EndTime = execution.EndTime,
                Status = execution.Status,
                ErrorMessage = execution.ErrorMessage,
                OutputFilePath = execution.OutputFilePath,
                RecordsProcessed = execution.RecordsProcessed,
                ExecutedBy = execution.ExecutedBy,
                ExecutionLog = execution.ExecutionLog
            };
        }
    }
}
