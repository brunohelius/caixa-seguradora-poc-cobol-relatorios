using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using CaixaSeguradora.Core.Interfaces;
using MailKit.Net.Smtp;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using MimeKit;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Email notification service implementation using MailKit.
/// Supports batch job notifications and report generation alerts.
/// </summary>
public class EmailNotificationService : INotificationService
{
    private readonly IConfiguration _configuration;
    private readonly ILogger<EmailNotificationService> _logger;

    public EmailNotificationService(
        IConfiguration configuration,
        ILogger<EmailNotificationService> logger)
    {
        _configuration = configuration;
        _logger = logger;
    }

    public async Task SendEmailAsync(
        string to,
        string subject,
        string body,
        bool isHtml = true,
        CancellationToken cancellationToken = default)
    {
        await SendEmailToMultipleAsync(
            new[] { to },
            subject,
            body,
            isHtml,
            cancellationToken);
    }

    public async Task SendEmailToMultipleAsync(
        IEnumerable<string> recipients,
        string subject,
        string body,
        bool isHtml = true,
        CancellationToken cancellationToken = default)
    {
        try
        {
            var message = new MimeMessage();

            // Configure sender
            var fromName = _configuration["Email:FromName"] ?? "Caixa Seguradora";
            var fromAddress = _configuration["Email:From"] ?? "noreply@caixaseguradora.com.br";
            message.From.Add(new MailboxAddress(fromName, fromAddress));

            // Add recipients
            foreach (var recipient in recipients)
            {
                message.To.Add(MailboxAddress.Parse(recipient));
            }

            message.Subject = subject;

            // Build message body
            var builder = new BodyBuilder();
            if (isHtml)
            {
                builder.HtmlBody = body;
                builder.TextBody = StripHtml(body); // Fallback for email clients without HTML support
            }
            else
            {
                builder.TextBody = body;
            }

            message.Body = builder.ToMessageBody();

            // Send email
            await SendMessageAsync(message, cancellationToken);

            _logger.LogInformation(
                "Email enviado com sucesso para {RecipientCount} destinatário(s): {Subject}",
                recipients.Count(), subject);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao enviar email: {Subject}", subject);
            throw new InvalidOperationException($"Falha ao enviar email: {ex.Message}", ex);
        }
    }

    public async Task SendBatchJobNotificationAsync(
        string jobName,
        JobCompletionStatus status,
        string details,
        string recipientEmail,
        CancellationToken cancellationToken = default)
    {
        var statusText = status switch
        {
            JobCompletionStatus.Success => "concluído com sucesso",
            JobCompletionStatus.Warning => "concluído com avisos",
            JobCompletionStatus.Failure => "falhou",
            JobCompletionStatus.Cancelled => "foi cancelado",
            _ => "status desconhecido"
        };

        var subject = $"[Caixa Seguradora] Job {jobName} - {statusText}";

        var htmlBody = $@"
<!DOCTYPE html>
<html>
<head>
    <style>
        body {{ font-family: Arial, sans-serif; line-height: 1.6; color: #333; }}
        .container {{ max-width: 600px; margin: 0 auto; padding: 20px; }}
        .header {{ background-color: #0047BB; color: white; padding: 20px; text-align: center; }}
        .content {{ background-color: #f4f4f4; padding: 20px; margin-top: 20px; }}
        .status-{status.ToString().ToLower()} {{
            padding: 10px;
            margin: 15px 0;
            border-left: 4px solid {GetStatusColor(status)};
            background-color: white;
        }}
        .footer {{ text-align: center; margin-top: 20px; font-size: 12px; color: #666; }}
    </style>
</head>
<body>
    <div class=""container"">
        <div class=""header"">
            <h1>Caixa Seguradora - Notificação de Job</h1>
        </div>
        <div class=""content"">
            <h2>Job: {jobName}</h2>
            <div class=""status-{status.ToString().ToLower()}"">
                <strong>Status:</strong> {statusText.ToUpper()}
            </div>
            <p><strong>Detalhes:</strong></p>
            <p>{details}</p>
            <p><strong>Data/Hora:</strong> {DateTime.Now:dd/MM/yyyy HH:mm:ss}</p>
        </div>
        <div class=""footer"">
            <p>Esta é uma mensagem automática. Por favor, não responda a este email.</p>
            <p>&copy; 2025 Caixa Seguradora. Todos os direitos reservados.</p>
        </div>
    </div>
</body>
</html>";

        await SendEmailAsync(recipientEmail, subject, htmlBody, isHtml: true, cancellationToken);
    }

    public async Task SendReportGenerationNotificationAsync(
        string reportType,
        string reportDate,
        int recordCount,
        string downloadUrl,
        string recipientEmail,
        CancellationToken cancellationToken = default)
    {
        var subject = $"[Caixa Seguradora] Relatório {reportType} - {reportDate} disponível";

        var htmlBody = $@"
<!DOCTYPE html>
<html>
<head>
    <style>
        body {{ font-family: Arial, sans-serif; line-height: 1.6; color: #333; }}
        .container {{ max-width: 600px; margin: 0 auto; padding: 20px; }}
        .header {{ background-color: #0047BB; color: white; padding: 20px; text-align: center; }}
        .content {{ background-color: #f4f4f4; padding: 20px; margin-top: 20px; }}
        .download-button {{
            display: inline-block;
            padding: 12px 24px;
            background-color: #FFB81C;
            color: #000;
            text-decoration: none;
            border-radius: 4px;
            margin: 15px 0;
            font-weight: bold;
        }}
        .stats {{ background-color: white; padding: 15px; margin: 15px 0; border-radius: 4px; }}
        .footer {{ text-align: center; margin-top: 20px; font-size: 12px; color: #666; }}
    </style>
</head>
<body>
    <div class=""container"">
        <div class=""header"">
            <h1>Relatório Gerado com Sucesso</h1>
        </div>
        <div class=""content"">
            <h2>Relatório {reportType}</h2>
            <div class=""stats"">
                <p><strong>Período:</strong> {reportDate}</p>
                <p><strong>Total de Registros:</strong> {recordCount:N0}</p>
                <p><strong>Data de Geração:</strong> {DateTime.Now:dd/MM/yyyy HH:mm:ss}</p>
            </div>
            <p>Seu relatório está pronto para download:</p>
            <p style=""text-align: center;"">
                <a href=""{downloadUrl}"" class=""download-button"">BAIXAR RELATÓRIO</a>
            </p>
            <p><small>O link estará disponível por 7 dias.</small></p>
        </div>
        <div class=""footer"">
            <p>Esta é uma mensagem automática. Por favor, não responda a este email.</p>
            <p>&copy; 2025 Caixa Seguradora. Todos os direitos reservados.</p>
        </div>
    </div>
</body>
</html>";

        await SendEmailAsync(recipientEmail, subject, htmlBody, isHtml: true, cancellationToken);
    }

    private async Task SendMessageAsync(MimeMessage message, CancellationToken cancellationToken)
    {
        var host = _configuration["Email:Host"] ?? "localhost";
        var port = int.Parse(_configuration["Email:Port"] ?? "25");
        var useSsl = bool.Parse(_configuration["Email:UseSsl"] ?? "false");
        var username = _configuration["Email:Username"];
        var password = _configuration["Email:Password"];

        using var client = new SmtpClient();

        try
        {
            await client.ConnectAsync(host, port, useSsl, cancellationToken);

            // Authenticate if credentials are provided
            if (!string.IsNullOrEmpty(username) && !string.IsNullOrEmpty(password))
            {
                await client.AuthenticateAsync(username, password, cancellationToken);
            }

            await client.SendAsync(message, cancellationToken);
            await client.DisconnectAsync(true, cancellationToken);

            _logger.LogDebug("Email sent successfully via {Host}:{Port}", host, port);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao conectar ao servidor SMTP {Host}:{Port}", host, port);
            throw;
        }
    }

    private static string GetStatusColor(JobCompletionStatus status)
    {
        return status switch
        {
            JobCompletionStatus.Success => "#28a745",
            JobCompletionStatus.Warning => "#ffc107",
            JobCompletionStatus.Failure => "#dc3545",
            JobCompletionStatus.Cancelled => "#6c757d",
            _ => "#6c757d"
        };
    }

    private static string StripHtml(string html)
    {
        // Simple HTML stripping for plain text fallback
        return System.Text.RegularExpressions.Regex.Replace(html, "<.*?>", string.Empty)
            .Replace("&nbsp;", " ")
            .Replace("&lt;", "<")
            .Replace("&gt;", ">")
            .Replace("&amp;", "&");
    }
}
