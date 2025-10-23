namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for report status polling.
/// Provides current status and progress information.
/// </summary>
public class ReportStatusResponse
{
    /// <summary>
    /// Job identifier.
    /// </summary>
    public Guid JobId { get; set; }

    /// <summary>
    /// Current status: Queued, Processing, Completed, Failed, Cancelled
    /// </summary>
    public string Status { get; set; } = "Queued";

    /// <summary>
    /// Progress percentage (0-100).
    /// </summary>
    public int Progress { get; set; }

    /// <summary>
    /// Current processing phase.
    /// </summary>
    public string? Phase { get; set; }

    /// <summary>
    /// Number of records processed.
    /// </summary>
    public int RecordsProcessed { get; set; }

    /// <summary>
    /// Total records to process.
    /// </summary>
    public int TotalRecords { get; set; }

    /// <summary>
    /// Path to the generated output file (available when Status = Completed).
    /// </summary>
    public string? OutputFilePath { get; set; }

    /// <summary>
    /// Size of the output file in bytes.
    /// </summary>
    public long? OutputFileSize { get; set; }

    /// <summary>
    /// Error message if Status = Failed.
    /// </summary>
    public string? ErrorMessage { get; set; }

    /// <summary>
    /// Started timestamp.
    /// </summary>
    public DateTime? StartedAt { get; set; }

    /// <summary>
    /// Completed timestamp.
    /// </summary>
    public DateTime? CompletedAt { get; set; }

    /// <summary>
    /// Duration in seconds.
    /// </summary>
    public double? DurationSeconds { get; set; }
}
