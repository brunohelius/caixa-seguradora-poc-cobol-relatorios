using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces
{
    /// <summary>
    /// Service interface for managing and executing background batch jobs.
    /// Handles job scheduling, execution, retry logic, and recurrence patterns.
    /// </summary>
    public interface IBackgroundJobService
    {
        /// <summary>
        /// Schedules a new batch job with specified recurrence pattern.
        /// </summary>
        /// <param name="job">Job configuration to schedule</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Created job with calculated NextExecutionTime</returns>
        Task<BatchJob> ScheduleJobAsync(
            BatchJob job,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Executes a batch job immediately.
        /// Creates execution record and triggers report generation.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="executedBy">Username triggering execution (default: SYSTEM)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Execution record with status and results</returns>
        Task<BatchJobExecution> ExecuteJobAsync(
            int jobId,
            string executedBy = "SYSTEM",
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Cancels a running job execution.
        /// </summary>
        /// <param name="executionId">Execution identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>True if successfully cancelled, false otherwise</returns>
        Task<bool> CancelJobAsync(
            int executionId,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets the current status of a job execution.
        /// </summary>
        /// <param name="executionId">Execution identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Execution record with current status</returns>
        Task<BatchJobExecution?> GetJobStatusAsync(
            int executionId,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Processes all scheduled jobs that are due for execution.
        /// Called periodically by background scheduler.
        /// </summary>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Number of jobs executed</returns>
        Task<int> ProcessScheduledJobsAsync(
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Updates a job's schedule and recurrence pattern.
        /// Recalculates NextExecutionTime based on new pattern.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="job">Updated job configuration</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Updated job</returns>
        Task<BatchJob> UpdateJobScheduleAsync(
            int jobId,
            BatchJob job,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Pauses a scheduled job (sets IsEnabled = false).
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task PauseJobAsync(
            int jobId,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Resumes a paused job (sets IsEnabled = true).
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task ResumeJobAsync(
            int jobId,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Deletes a batch job and all its execution history.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task DeleteJobAsync(
            int jobId,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Calculates the next execution time based on recurrence pattern.
        /// </summary>
        /// <param name="job">Job with recurrence configuration</param>
        /// <param name="fromTime">Calculate from this time (default: current time)</param>
        /// <returns>Next execution time</returns>
        DateTime CalculateNextExecutionTime(BatchJob job, DateTime? fromTime = null);

        /// <summary>
        /// Retries a failed job execution.
        /// </summary>
        /// <param name="executionId">Failed execution identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>New execution record</returns>
        Task<BatchJobExecution> RetryJobAsync(
            int executionId,
            CancellationToken cancellationToken = default);
    }
}
