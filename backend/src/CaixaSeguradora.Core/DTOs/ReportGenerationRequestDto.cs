namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Request DTO for initiating premium report generation.
/// Contains all parameters needed to generate SUSEP Circular 360 reports.
/// </summary>
public class ReportGenerationRequestDto
{
    /// <summary>
    /// Report start date (inclusive).
    /// Must be valid date in format YYYY-MM-DD.
    /// </summary>
    /// <example>2025-10-01</example>
    public DateTime StartDate { get; set; }

    /// <summary>
    /// Report end date (inclusive).
    /// Must be greater than or equal to StartDate.
    /// </summary>
    /// <example>2025-10-31</example>
    public DateTime EndDate { get; set; }

    /// <summary>
    /// System identifier code.
    /// Valid values: GL (Life), GE (General), GH (Health), etc.
    /// </summary>
    /// <example>GL</example>
    public string SystemId { get; set; } = string.Empty;

    /// <summary>
    /// Report type to generate.
    /// Valid values: PREMIT, PREMCED, Both
    /// </summary>
    /// <remarks>
    /// PREMIT: Premium emission report (direct insurance)
    /// PREMCED: Premium ceded to reinsurers (cossurance)
    /// Both: Generates both reports in single execution
    /// </remarks>
    /// <example>Both</example>
    public string ReportType { get; set; } = "Both";

    /// <summary>
    /// Processing mode for report generation.
    /// Valid values: WeeklyCumulative, Monthly
    /// </summary>
    /// <remarks>
    /// WeeklyCumulative: Accumulates data for the week
    /// Monthly: Full month processing
    /// </remarks>
    /// <example>Monthly</example>
    public string ProcessingMode { get; set; } = "Monthly";

    /// <summary>
    /// Optional filter by policy number.
    /// If specified, only processes specific policy.
    /// </summary>
    public string? PolicyNumber { get; set; }

    /// <summary>
    /// Optional filter by product code.
    /// If specified, only processes specific product.
    /// </summary>
    public int? ProductCode { get; set; }

    /// <summary>
    /// Optional filter by line of business.
    /// If specified, only processes specific line.
    /// </summary>
    public int? LineOfBusiness { get; set; }

    /// <summary>
    /// Whether to include cancelled policies in report.
    /// Default: true (matches COBOL behavior)
    /// </summary>
    public bool IncludeCancelled { get; set; } = true;

    /// <summary>
    /// Whether to include reversals in report.
    /// Default: true (matches COBOL behavior)
    /// </summary>
    public bool IncludeReversals { get; set; } = true;

    /// <summary>
    /// Optional user ID for audit trail.
    /// </summary>
    public string? RequestedBy { get; set; }

    /// <summary>
    /// Optional description/notes for the report generation.
    /// </summary>
    public string? Notes { get; set; }

    /// <summary>
    /// Whether to send email notification on completion.
    /// Default: false
    /// </summary>
    public bool SendEmailNotification { get; set; } = false;

    /// <summary>
    /// Email address for notification (required if SendEmailNotification = true).
    /// </summary>
    public string? NotificationEmail { get; set; }
}
