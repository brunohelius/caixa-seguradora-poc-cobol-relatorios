using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces
{
    /// <summary>
    /// Repository interface for batch job management.
    /// Provides operations for scheduling, executing, and tracking batch jobs.
    /// </summary>
    public interface IBatchJobRepository : IRepository<BatchJob>
    {
        /// <summary>
        /// Gets all scheduled jobs that are due for execution.
        /// </summary>
        /// <param name="currentTime">Current time to check against NextExecutionTime</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>List of jobs ready for execution</returns>
        Task<IReadOnlyList<BatchJob>> GetScheduledJobsAsync(
            DateTime currentTime,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets execution history for a specific job.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="limit">Maximum number of executions to return (default 50)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>List of executions ordered by start time descending</returns>
        Task<IReadOnlyList<BatchJobExecution>> GetJobHistoryAsync(
            int jobId,
            int limit = 50,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets all active jobs for a specific user.
        /// </summary>
        /// <param name="createdBy">Username of job creator</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>List of active jobs created by the user</returns>
        Task<IReadOnlyList<BatchJob>> GetJobsByUserAsync(
            string createdBy,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Adds a new execution record for a job.
        /// </summary>
        /// <param name="execution">Execution record to add</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task AddExecutionAsync(
            BatchJobExecution execution,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Updates an existing execution record.
        /// </summary>
        /// <param name="execution">Execution record to update</param>
        void UpdateExecution(BatchJobExecution execution);

        /// <summary>
        /// Gets the latest execution for a job.
        /// </summary>
        /// <param name="jobId">Job identifier</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Most recent execution or null if none exists</returns>
        Task<BatchJobExecution?> GetLatestExecutionAsync(
            int jobId,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets jobs by status.
        /// </summary>
        /// <param name="status">Job status (ACTIVE, PAUSED, COMPLETED, FAILED)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>List of jobs with specified status</returns>
        Task<IReadOnlyList<BatchJob>> GetJobsByStatusAsync(
            string status,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets currently running executions.
        /// </summary>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>List of executions with RUNNING status</returns>
        Task<IReadOnlyList<BatchJobExecution>> GetRunningExecutionsAsync(
            CancellationToken cancellationToken = default);
    }
}
