using CaixaSeguradora.Core.DTOs;
using FluentValidation;

namespace CaixaSeguradora.Api.Validators;

/// <summary>
/// Validator for ReportGenerationRequestDto.
/// Validates report generation parameters according to SUSEP Circular 360 requirements.
/// </summary>
public class ReportGenerationRequestValidator : AbstractValidator<ReportGenerationRequestDto>
{
    private static readonly string[] ValidReportTypes = { "PREMIT", "PREMCED", "Both" };
    private static readonly string[] ValidProcessingModes = { "WeeklyCumulative", "Monthly" };
    private static readonly string[] ValidSystemCodes = { "GL", "GE", "GH", "ST", "CA", "RE" };

    public ReportGenerationRequestValidator()
    {
        RuleFor(x => x.StartDate)
            .NotEmpty()
            .WithMessage("Data inicial é obrigatória")
            .LessThanOrEqualTo(DateTime.Now)
            .WithMessage("Data inicial não pode ser no futuro");

        RuleFor(x => x.EndDate)
            .NotEmpty()
            .WithMessage("Data final é obrigatória")
            .GreaterThanOrEqualTo(x => x.StartDate)
            .WithMessage("Data final deve ser maior ou igual à data inicial")
            .LessThanOrEqualTo(DateTime.Now)
            .WithMessage("Data final não pode ser no futuro");

        RuleFor(x => x.SystemId)
            .NotEmpty()
            .WithMessage("Código do sistema é obrigatório")
            .Length(2, 3)
            .WithMessage("Código do sistema deve ter entre 2 e 3 caracteres")
            .Matches("^[A-Z]{2,3}$")
            .WithMessage("Código do sistema deve conter apenas letras maiúsculas")
            .Must(code => ValidSystemCodes.Contains(code.ToUpper()))
            .WithMessage($"Código do sistema deve ser um dos seguintes: {string.Join(", ", ValidSystemCodes)}");

        RuleFor(x => x.ReportType)
            .NotEmpty()
            .WithMessage("Tipo de relatório é obrigatório")
            .Must(type => ValidReportTypes.Contains(type, StringComparer.OrdinalIgnoreCase))
            .WithMessage($"Tipo de relatório deve ser um dos seguintes: {string.Join(", ", ValidReportTypes)}");

        RuleFor(x => x.ProcessingMode)
            .NotEmpty()
            .WithMessage("Modo de processamento é obrigatório")
            .Must(mode => ValidProcessingModes.Contains(mode, StringComparer.OrdinalIgnoreCase))
            .WithMessage($"Modo de processamento deve ser um dos seguintes: {string.Join(", ", ValidProcessingModes)}");

        // Optional filters validation
        RuleFor(x => x.PolicyNumber)
            .MaximumLength(20)
            .WithMessage("Número da apólice não pode exceder 20 caracteres")
            .When(x => !string.IsNullOrEmpty(x.PolicyNumber));

        RuleFor(x => x.ProductCode)
            .GreaterThan(0)
            .WithMessage("Código do produto deve ser maior que zero")
            .When(x => x.ProductCode.HasValue);

        RuleFor(x => x.LineOfBusiness)
            .GreaterThan(0)
            .WithMessage("Ramo deve ser maior que zero")
            .When(x => x.LineOfBusiness.HasValue);

        RuleFor(x => x.RequestedBy)
            .MaximumLength(100)
            .WithMessage("Solicitante não pode exceder 100 caracteres")
            .When(x => !string.IsNullOrEmpty(x.RequestedBy));

        RuleFor(x => x.Notes)
            .MaximumLength(500)
            .WithMessage("Notas não podem exceder 500 caracteres")
            .When(x => !string.IsNullOrEmpty(x.Notes));

        // Email validation when notification is enabled
        RuleFor(x => x.NotificationEmail)
            .NotEmpty()
            .WithMessage("E-mail de notificação é obrigatório quando notificação está habilitada")
            .EmailAddress()
            .WithMessage("E-mail de notificação deve ser um endereço de e-mail válido")
            .MaximumLength(200)
            .WithMessage("E-mail de notificação não pode exceder 200 caracteres")
            .When(x => x.SendEmailNotification);
    }
}
