using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces
{
    /// <summary>
    /// Repository interface for ReportExecution entity operations.
    /// Manages report generation execution tracking and history.
    /// </summary>
    public interface IReportExecutionRepository : IRepository<ReportExecution>
    {
        /// <summary>
        /// Gets a report execution by its ID with related logs and file outputs.
        /// </summary>
        Task<ReportExecution?> GetByIdWithDetailsAsync(Guid executionId, CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets all report executions with pagination and optional filters.
        /// </summary>
        Task<(List<ReportExecution> Executions, int TotalCount)> GetAllWithPaginationAsync(
            int pageNumber,
            int pageSize,
            string? status = null,
            string? referenceMonth = null,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Checks if there is an active (Pending or Running) execution for the specified month.
        /// </summary>
        Task<bool> HasActiveExecutionForMonthAsync(string referenceMonth, CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets the most recent execution for a specific month.
        /// </summary>
        Task<ReportExecution?> GetLatestExecutionForMonthAsync(string referenceMonth, CancellationToken cancellationToken = default);

        /// <summary>
        /// Updates the progress of a report execution.
        /// </summary>
        Task UpdateProgressAsync(Guid executionId, int recordsProcessed, CancellationToken cancellationToken = default);

        /// <summary>
        /// Completes a report execution with final statistics.
        /// </summary>
        Task CompleteExecutionAsync(
            Guid executionId,
            string returnCode,
            int premitRecords,
            int premcedRecords,
            int warnings,
            int errors,
            CancellationToken cancellationToken = default);
    }
}
