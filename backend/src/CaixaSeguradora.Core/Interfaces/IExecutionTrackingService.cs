using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces
{
    /// <summary>
    /// Service interface for tracking report execution progress and logging.
    /// Manages execution lifecycle from creation to completion.
    /// </summary>
    public interface IExecutionTrackingService
    {
        /// <summary>
        /// Creates a new report execution record.
        /// </summary>
        /// <param name="referenceMonth">Month in YYYYMM format</param>
        /// <param name="reportType">Report type (PREMIT, PREMCED, Both)</param>
        /// <param name="executionMode">Execution mode (Weekly, Monthly)</param>
        /// <param name="triggeringUser">User who initiated the report</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Created execution ID</returns>
        Task<Guid> CreateExecutionAsync(
            string referenceMonth,
            string reportType,
            string executionMode,
            string triggeringUser,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Updates the status of a report execution.
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <param name="status">New status (Pending, Running, Completed, Failed)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task UpdateStatusAsync(Guid executionId, string status, CancellationToken cancellationToken = default);

        /// <summary>
        /// Updates the progress of a report execution.
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <param name="recordsProcessed">Number of records processed so far</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task UpdateProgressAsync(Guid executionId, int recordsProcessed, CancellationToken cancellationToken = default);

        /// <summary>
        /// Completes a report execution with final statistics.
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <param name="returnCode">COBOL return code (0000, 0004, 0008, 0012)</param>
        /// <param name="premitRecords">Number of PREMIT records generated</param>
        /// <param name="premcedRecords">Number of PREMCED records generated</param>
        /// <param name="warnings">Warning count</param>
        /// <param name="errors">Error count</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task CompleteExecutionAsync(
            Guid executionId,
            string returnCode,
            int premitRecords,
            int premcedRecords,
            int warnings,
            int errors,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Logs a processing event for an execution.
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <param name="severity">Log severity (INFO, WARNING, ERROR, DEBUG)</param>
        /// <param name="cobolSection">COBOL section reference (e.g., R0700-00)</param>
        /// <param name="message">Log message</param>
        /// <param name="policyNumber">Policy number context (optional)</param>
        /// <param name="stackTrace">Stack trace for errors (optional)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        Task LogEventAsync(
            Guid executionId,
            string severity,
            string cobolSection,
            string message,
            long? policyNumber = null,
            string? stackTrace = null,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets execution details by ID.
        /// </summary>
        /// <param name="executionId">Execution ID</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Report execution details or null if not found</returns>
        Task<ReportExecution?> GetExecutionAsync(Guid executionId, CancellationToken cancellationToken = default);

        /// <summary>
        /// Gets execution history with pagination.
        /// </summary>
        /// <param name="pageNumber">Page number (1-based)</param>
        /// <param name="pageSize">Page size</param>
        /// <param name="status">Filter by status (optional)</param>
        /// <param name="referenceMonth">Filter by month (optional)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Paginated execution history</returns>
        Task<(List<ReportExecution> Executions, int TotalCount)> GetExecutionHistoryAsync(
            int pageNumber,
            int pageSize,
            string? status = null,
            string? referenceMonth = null,
            CancellationToken cancellationToken = default);
    }
}
