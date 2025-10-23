namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Status DTO for tracking premium report generation progress.
/// Used for polling async report generation status.
/// </summary>
public class ReportStatusDto
{
    /// <summary>
    /// Unique report identifier (GUID).
    /// </summary>
    public Guid ReportId { get; set; }

    /// <summary>
    /// Current report status.
    /// Values: Queued, Processing, Completed, Failed, Cancelled
    /// </summary>
    public string Status { get; set; } = "Queued";

    /// <summary>
    /// Progress percentage (0-100).
    /// </summary>
    public int ProgressPercentage { get; set; }

    /// <summary>
    /// Current processing phase.
    /// Values: Validation, DataFetch, Calculation, FileGeneration, Complete
    /// </summary>
    public string? CurrentPhase { get; set; }

    /// <summary>
    /// Number of records processed so far.
    /// </summary>
    public int RecordsProcessed { get; set; }

    /// <summary>
    /// Total number of records to process.
    /// </summary>
    public int TotalRecords { get; set; }

    /// <summary>
    /// Timestamp when report generation was requested.
    /// </summary>
    public DateTime RequestedAt { get; set; }

    /// <summary>
    /// Timestamp when processing started.
    /// Null if still queued.
    /// </summary>
    public DateTime? StartedAt { get; set; }

    /// <summary>
    /// Timestamp when processing completed (success or failure).
    /// Null if still processing.
    /// </summary>
    public DateTime? CompletedAt { get; set; }

    /// <summary>
    /// Total execution time in seconds.
    /// Null if still processing.
    /// </summary>
    public double? ExecutionTimeSeconds { get; set; }

    /// <summary>
    /// Estimated time remaining in seconds.
    /// Null if cannot estimate or already complete.
    /// </summary>
    public double? EstimatedTimeRemainingSeconds { get; set; }

    /// <summary>
    /// List of generated report files.
    /// Empty until generation completes.
    /// </summary>
    public List<ReportFileDto> Files { get; set; } = new();

    /// <summary>
    /// Report generation summary (totals, counts, etc.).
    /// Populated after completion.
    /// </summary>
    public ReportSummaryDto? Summary { get; set; }

    /// <summary>
    /// Error message if status is Failed.
    /// Null for successful or in-progress reports.
    /// </summary>
    public string? ErrorMessage { get; set; }

    /// <summary>
    /// Detailed error stack trace (for debugging).
    /// Only populated in Development environment.
    /// </summary>
    public string? ErrorDetails { get; set; }

    /// <summary>
    /// Original request parameters.
    /// </summary>
    public ReportGenerationRequestDto? Request { get; set; }

    /// <summary>
    /// User who requested the report.
    /// </summary>
    public string? RequestedBy { get; set; }

    /// <summary>
    /// Optional notes/description.
    /// </summary>
    public string? Notes { get; set; }
}

/// <summary>
/// Summary of report generation results.
/// </summary>
public class ReportSummaryDto
{
    /// <summary>
    /// Total premium records processed.
    /// </summary>
    public int TotalRecordsProcessed { get; set; }

    /// <summary>
    /// Number of emission records (movement type 'E').
    /// </summary>
    public int EmissionCount { get; set; }

    /// <summary>
    /// Number of cancellation records (movement type 'C').
    /// </summary>
    public int CancellationCount { get; set; }

    /// <summary>
    /// Number of reversal records (movement type 'R').
    /// </summary>
    public int ReversalCount { get; set; }

    /// <summary>
    /// Total net premium amount (sum of all premiums).
    /// </summary>
    public decimal TotalNetPremium { get; set; }

    /// <summary>
    /// Total gross premium (net + taxes).
    /// </summary>
    public decimal TotalGrossPremium { get; set; }

    /// <summary>
    /// Total IOF tax collected.
    /// </summary>
    public decimal TotalIOF { get; set; }

    /// <summary>
    /// Total commissions paid.
    /// </summary>
    public decimal TotalCommissions { get; set; }

    /// <summary>
    /// Total cossurance premium ceded.
    /// </summary>
    public decimal TotalCossurancePremium { get; set; }

    /// <summary>
    /// Number of unique policies processed.
    /// </summary>
    public int UniquePolicyCount { get; set; }

    /// <summary>
    /// Number of unique products processed.
    /// </summary>
    public int UniqueProductCount { get; set; }

    /// <summary>
    /// Number of warnings encountered (non-fatal).
    /// </summary>
    public int WarningCount { get; set; }

    /// <summary>
    /// List of warning messages.
    /// </summary>
    public List<string> Warnings { get; set; } = new();

    /// <summary>
    /// Performance metrics.
    /// </summary>
    public PerformanceMetricsDto? Performance { get; set; }
}

/// <summary>
/// Performance metrics for report generation.
/// </summary>
public class PerformanceMetricsDto
{
    /// <summary>
    /// Records processed per second.
    /// </summary>
    public double RecordsPerSecond { get; set; }

    /// <summary>
    /// Database query time in milliseconds.
    /// </summary>
    public double DatabaseQueryTimeMs { get; set; }

    /// <summary>
    /// Calculation time in milliseconds.
    /// </summary>
    public double CalculationTimeMs { get; set; }

    /// <summary>
    /// File generation time in milliseconds.
    /// </summary>
    public double FileGenerationTimeMs { get; set; }

    /// <summary>
    /// Peak memory usage in megabytes.
    /// </summary>
    public double PeakMemoryUsageMB { get; set; }
}
