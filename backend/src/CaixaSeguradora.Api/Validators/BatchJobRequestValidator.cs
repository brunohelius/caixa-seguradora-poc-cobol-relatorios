using CaixaSeguradora.Core.DTOs;
using FluentValidation;
using System.Text.Json;
using System.Text.RegularExpressions;

namespace CaixaSeguradora.Api.Validators;

/// <summary>
/// Validator for BatchJobRequestDto.
/// Validates batch job scheduling parameters including cron expressions and execution times.
/// </summary>
public class BatchJobRequestValidator : AbstractValidator<BatchJobRequestDto>
{
    private static readonly string[] ValidRecurrencePatterns = { "ONCE", "DAILY", "WEEKLY", "MONTHLY" };

    public BatchJobRequestValidator()
    {
        RuleFor(x => x.JobName)
            .NotEmpty()
            .WithMessage("Nome do job é obrigatório")
            .MaximumLength(200)
            .WithMessage("Nome do job não pode exceder 200 caracteres")
            .Matches("^[a-zA-Z0-9\\s\\-_]+$")
            .WithMessage("Nome do job deve conter apenas letras, números, espaços, hífens e underscores");

        RuleFor(x => x.Description)
            .MaximumLength(1000)
            .WithMessage("Descrição não pode exceder 1000 caracteres")
            .When(x => !string.IsNullOrEmpty(x.Description));

        RuleFor(x => x.RecurrencePattern)
            .NotEmpty()
            .WithMessage("Padrão de recorrência é obrigatório")
            .Must(pattern => ValidRecurrencePatterns.Contains(pattern, StringComparer.OrdinalIgnoreCase))
            .WithMessage($"Padrão de recorrência deve ser um dos seguintes: {string.Join(", ", ValidRecurrencePatterns)}");

        RuleFor(x => x.ReportParameters)
            .NotEmpty()
            .WithMessage("Parâmetros do relatório são obrigatórios")
            .Must(BeValidJson)
            .WithMessage("Parâmetros do relatório devem ser um JSON válido");

        // Execution hour validation
        RuleFor(x => x.ExecutionHour)
            .InclusiveBetween(0, 23)
            .WithMessage("Hora de execução deve estar entre 0 e 23")
            .When(x => x.ExecutionHour.HasValue);

        // Execution minute validation
        RuleFor(x => x.ExecutionMinute)
            .InclusiveBetween(0, 59)
            .WithMessage("Minuto de execução deve estar entre 0 e 59")
            .When(x => x.ExecutionMinute.HasValue);

        // Day of week validation (for WEEKLY jobs)
        RuleFor(x => x.DayOfWeek)
            .InclusiveBetween(0, 6)
            .WithMessage("Dia da semana deve estar entre 0 (Domingo) e 6 (Sábado)")
            .NotNull()
            .WithMessage("Dia da semana é obrigatório para jobs semanais")
            .When(x => x.RecurrencePattern.Equals("WEEKLY", StringComparison.OrdinalIgnoreCase));

        // Day of month validation (for MONTHLY jobs)
        RuleFor(x => x.DayOfMonth)
            .InclusiveBetween(1, 31)
            .WithMessage("Dia do mês deve estar entre 1 e 31")
            .NotNull()
            .WithMessage("Dia do mês é obrigatório para jobs mensais")
            .When(x => x.RecurrencePattern.Equals("MONTHLY", StringComparison.OrdinalIgnoreCase));

        // Notification recipients validation
        RuleFor(x => x.NotificationRecipients)
            .Must(BeValidEmailList)
            .WithMessage("Lista de e-mails deve conter endereços de e-mail válidos separados por vírgula")
            .When(x => !string.IsNullOrEmpty(x.NotificationRecipients));

        // Max retries validation
        RuleFor(x => x.MaxRetries)
            .InclusiveBetween(0, 10)
            .WithMessage("Número máximo de tentativas deve estar entre 0 e 10");

        RuleFor(x => x.CreatedBy)
            .NotEmpty()
            .WithMessage("Criador do job é obrigatório")
            .MaximumLength(100)
            .WithMessage("Criador do job não pode exceder 100 caracteres");
    }

    /// <summary>
    /// Validates if the string is a valid JSON.
    /// </summary>
    private static bool BeValidJson(string json)
    {
        if (string.IsNullOrWhiteSpace(json))
            return false;

        try
        {
            using var document = JsonDocument.Parse(json);
            return true;
        }
        catch (JsonException)
        {
            return false;
        }
    }

    /// <summary>
    /// Validates if the string contains a comma-separated list of valid email addresses.
    /// </summary>
    private static bool BeValidEmailList(string emailList)
    {
        if (string.IsNullOrWhiteSpace(emailList))
            return true; // Empty list is valid

        var emails = emailList.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        var emailRegex = new Regex(@"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$", RegexOptions.Compiled);

        return emails.All(email => emailRegex.IsMatch(email));
    }
}
