namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for report generation initiation.
/// Returns job ID for status polling.
/// </summary>
public class ReportGenerationResponse
{
    /// <summary>
    /// Unique job identifier for tracking report generation.
    /// </summary>
    public Guid JobId { get; set; }

    /// <summary>
    /// Initial status (typically "Queued" or "Processing").
    /// </summary>
    public string Status { get; set; } = "Queued";

    /// <summary>
    /// Timestamp when the job was queued.
    /// </summary>
    public DateTime QueuedAt { get; set; }

    /// <summary>
    /// Message describing the action taken.
    /// </summary>
    public string Message { get; set; } = "Report generation job has been queued successfully.";

    /// <summary>
    /// URL to poll for status updates.
    /// </summary>
    public string? StatusUrl { get; set; }

    /// <summary>
    /// Estimated processing time in seconds (if available).
    /// </summary>
    public int? EstimatedProcessingTimeSeconds { get; set; }
}
