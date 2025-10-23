using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service for sending notifications via various channels (email, SMS, etc.).
/// Used for batch job completion notifications and system alerts.
/// </summary>
public interface INotificationService
{
    /// <summary>
    /// Sends an email notification.
    /// </summary>
    /// <param name="to">Recipient email address</param>
    /// <param name="subject">Email subject</param>
    /// <param name="body">Email body (supports HTML)</param>
    /// <param name="isHtml">Whether the body contains HTML (default: true)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    Task SendEmailAsync(
        string to,
        string subject,
        string body,
        bool isHtml = true,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Sends an email notification to multiple recipients.
    /// </summary>
    /// <param name="recipients">List of recipient email addresses</param>
    /// <param name="subject">Email subject</param>
    /// <param name="body">Email body (supports HTML)</param>
    /// <param name="isHtml">Whether the body contains HTML (default: true)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    Task SendEmailToMultipleAsync(
        IEnumerable<string> recipients,
        string subject,
        string body,
        bool isHtml = true,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Sends a notification about batch job completion.
    /// </summary>
    /// <param name="jobName">Name of the completed job</param>
    /// <param name="status">Job completion status (success/failure)</param>
    /// <param name="details">Additional details about the job execution</param>
    /// <param name="recipientEmail">Email address to notify</param>
    /// <param name="cancellationToken">Cancellation token</param>
    Task SendBatchJobNotificationAsync(
        string jobName,
        JobCompletionStatus status,
        string details,
        string recipientEmail,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Sends a notification about report generation completion.
    /// </summary>
    /// <param name="reportType">Type of report generated (PREMIT/PREMCED)</param>
    /// <param name="reportDate">Date range of the report</param>
    /// <param name="recordCount">Number of records in the report</param>
    /// <param name="downloadUrl">URL to download the report</param>
    /// <param name="recipientEmail">Email address to notify</param>
    /// <param name="cancellationToken">Cancellation token</param>
    Task SendReportGenerationNotificationAsync(
        string reportType,
        string reportDate,
        int recordCount,
        string downloadUrl,
        string recipientEmail,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Status of a completed job for notification purposes.
/// </summary>
public enum JobCompletionStatus
{
    /// <summary>
    /// Job completed successfully.
    /// </summary>
    Success,

    /// <summary>
    /// Job completed with warnings.
    /// </summary>
    Warning,

    /// <summary>
    /// Job failed with errors.
    /// </summary>
    Failure,

    /// <summary>
    /// Job was cancelled before completion.
    /// </summary>
    Cancelled
}
