using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for premium report generation.
/// Handles on-demand generation of SUSEP Circular 360 premium reports (PREMIT.TXT and PREMCED.TXT).
/// Implements async processing with progress tracking and output file management.
/// </summary>
/// <remarks>
/// Report Types:
/// - PREMIT: Premium emission report (direct insurance)
/// - PREMCED: Premium ceded to reinsurers (cossurance)
/// - Both: Generates both reports in a single execution
///
/// Processing Modes:
/// - Weekly Cumulative: Accumulates data for the week
/// - Monthly: Full month processing
///
/// CRITICAL: Output files must match legacy COBOL format byte-for-byte for regulatory compliance.
/// </remarks>
public interface IReportGenerationService
{
    /// <summary>
    /// Initiates asynchronous report generation with the specified parameters.
    /// Returns immediately with a report ID for status tracking.
    /// </summary>
    /// <param name="request">Report generation request with date range, system ID, and report type</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Report ID (GUID) for tracking generation progress</returns>
    /// <remarks>
    /// Processing Steps:
    /// 1. Validate request parameters (date range, system ID validity)
    /// 2. Create report tracking record in database
    /// 3. Queue background job for report generation
    /// 4. Return report ID immediately (HTTP 202 Accepted pattern)
    ///
    /// The actual report generation runs asynchronously to prevent HTTP timeout
    /// for large datasets (10,000+ records).
    /// </remarks>
    Task<Guid> GenerateReportAsync(ReportGenerationRequestDto request, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the current status of a report generation job.
    /// Used for polling progress during async processing.
    /// </summary>
    /// <param name="reportId">Report ID returned from GenerateReportAsync</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Report status including progress percentage, execution time, and file availability</returns>
    /// <remarks>
    /// Status Values:
    /// - Queued: Job queued for processing
    /// - Processing: Currently generating report
    /// - Completed: Generation successful, files ready for download
    /// - Failed: Generation failed, error details available
    /// - Cancelled: User cancelled the generation
    ///
    /// Progress Tracking:
    /// - Records processed vs total records
    /// - Current processing phase (data fetch, calculation, file generation)
    /// - Estimated time remaining
    /// </remarks>
    Task<ReportStatusDto> GetReportStatusAsync(Guid reportId, CancellationToken cancellationToken = default);

    /// <summary>
    /// Downloads a generated report file.
    /// </summary>
    /// <param name="reportId">Report ID</param>
    /// <param name="fileType">File type to download (PREMIT or PREMCED)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>File stream and metadata for download</returns>
    /// <remarks>
    /// File Types:
    /// - PREMIT: Premium emission report (PREMIT.TXT)
    /// - PREMCED: Premium ceded to reinsurers (PREMCED.TXT)
    ///
    /// Files are stored in temporary storage for 7 days after generation.
    /// After 7 days, files are automatically purged.
    ///
    /// Content-Type: text/plain
    /// Character Encoding: ISO-8859-1 (matching COBOL output)
    /// Line Terminator: CRLF (Windows style, matching legacy system)
    /// </remarks>
    Task<(Stream FileStream, string FileName, string ContentType)> DownloadReportAsync(
        Guid reportId,
        string fileType,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Compares generated report output with legacy COBOL output for validation.
    /// Critical for migration testing and regulatory compliance verification.
    /// </summary>
    /// <param name="reportId">Report ID of generated .NET report</param>
    /// <param name="cobolFilePath">File path to legacy COBOL output for comparison</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Comparison result with byte-level differences (if any)</returns>
    /// <remarks>
    /// Comparison Levels:
    /// 1. Byte-Level: Exact byte-for-byte comparison (required for regulatory compliance)
    /// 2. Line-Level: Line-by-line comparison showing differences
    /// 3. Semantic: Business logic comparison (premium totals, record counts)
    ///
    /// Success Criteria:
    /// - 100% byte match for regulatory compliance (Constitution requirement III)
    /// - If byte mismatch: detailed diff report showing line numbers and differences
    /// - Semantic totals must always match even if formatting differs
    ///
    /// Common Differences (non-critical):
    /// - Trailing whitespace variations
    /// - Line ending differences (CRLF vs LF)
    /// - Timestamp formatting differences
    ///
    /// Critical Differences (must resolve):
    /// - Premium amount mismatches
    /// - Record count differences
    /// - IOF calculation differences
    /// - Cossurance percentage discrepancies
    /// </remarks>
    Task<ReportComparisonDto> CompareWithCobolOutputAsync(
        Guid reportId,
        string cobolFilePath,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets report generation history for audit trail and monitoring.
    /// </summary>
    /// <param name="startDate">Filter: generation date start range</param>
    /// <param name="endDate">Filter: generation date end range</param>
    /// <param name="systemId">Filter: specific system ID (optional)</param>
    /// <param name="status">Filter: specific status (optional)</param>
    /// <param name="pageNumber">Pagination: page number (1-based)</param>
    /// <param name="pageSize">Pagination: records per page (default 20)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Paginated list of report generation history</returns>
    /// <remarks>
    /// History includes:
    /// - Report ID and generation timestamp
    /// - Request parameters (date range, system, report type)
    /// - Execution status and duration
    /// - Records processed
    /// - File sizes
    /// - User who initiated generation
    ///
    /// Used for:
    /// - Audit trail for regulatory compliance
    /// - Performance monitoring
    /// - Troubleshooting failed generations
    /// - Re-downloading historical reports
    /// </remarks>
    Task<(List<ReportStatusDto> Reports, int TotalCount)> GetReportHistoryAsync(
        DateTime? startDate = null,
        DateTime? endDate = null,
        string? systemId = null,
        string? status = null,
        int pageNumber = 1,
        int pageSize = 20,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Cancels an in-progress report generation job.
    /// </summary>
    /// <param name="reportId">Report ID to cancel</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>True if cancellation succeeded, false if job already completed/failed</returns>
    /// <remarks>
    /// Cancellation Behavior:
    /// - Queued jobs: Removed from queue immediately
    /// - Processing jobs: Graceful shutdown after current record batch completes
    /// - Completed/Failed jobs: Cannot be cancelled (returns false)
    ///
    /// Cleanup:
    /// - Partial output files are deleted
    /// - Database records marked as cancelled
    /// - Resources released
    /// </remarks>
    Task<bool> CancelReportGenerationAsync(Guid reportId, CancellationToken cancellationToken = default);
}
