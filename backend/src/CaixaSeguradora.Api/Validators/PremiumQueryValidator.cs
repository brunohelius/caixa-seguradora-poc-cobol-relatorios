using CaixaSeguradora.Core.DTOs;
using FluentValidation;

namespace CaixaSeguradora.Api.Validators;

/// <summary>
/// Validator for PremiumQueryDto.
/// Validates premium query parameters including filters, pagination, and sorting.
/// </summary>
public class PremiumQueryValidator : AbstractValidator<PremiumQueryDto>
{
    private static readonly string[] ValidSortFields =
    {
        "PolicyNumber", "ReferenceDate", "ProductCode", "BasePremium",
        "TariffPremium", "NetPremium", "MovementType"
    };

    private static readonly string[] ValidSortOrders = { "asc", "desc" };
    private static readonly char[] ValidMovementTypes = { 'E', 'C', 'R', 'A', 'S' };

    public PremiumQueryValidator()
    {
        // Date range validation
        RuleFor(x => x.StartDate)
            .LessThanOrEqualTo(x => x.EndDate)
            .WithMessage("Data inicial deve ser menor ou igual à data final")
            .When(x => x.StartDate.HasValue && x.EndDate.HasValue);

        // Premium amount range validation
        RuleFor(x => x.MinPremiumAmount)
            .GreaterThanOrEqualTo(0)
            .WithMessage("Valor mínimo de prêmio deve ser maior ou igual a zero")
            .LessThanOrEqualTo(x => x.MaxPremiumAmount ?? decimal.MaxValue)
            .WithMessage("Valor mínimo de prêmio deve ser menor ou igual ao valor máximo")
            .When(x => x.MinPremiumAmount.HasValue);

        RuleFor(x => x.MaxPremiumAmount)
            .GreaterThanOrEqualTo(0)
            .WithMessage("Valor máximo de prêmio deve ser maior ou igual a zero")
            .GreaterThanOrEqualTo(x => x.MinPremiumAmount ?? 0)
            .WithMessage("Valor máximo de prêmio deve ser maior ou igual ao valor mínimo")
            .When(x => x.MaxPremiumAmount.HasValue);

        // Pagination validation
        RuleFor(x => x.Page)
            .GreaterThanOrEqualTo(1)
            .WithMessage("Número da página deve ser maior ou igual a 1");

        RuleFor(x => x.PageSize)
            .GreaterThanOrEqualTo(1)
            .WithMessage("Tamanho da página deve ser maior ou igual a 1")
            .LessThanOrEqualTo(1000)
            .WithMessage("Tamanho da página não pode exceder 1000 registros");

        // Sorting validation
        RuleFor(x => x.SortBy)
            .NotEmpty()
            .WithMessage("Campo de ordenação é obrigatório")
            .Must(field => ValidSortFields.Contains(field, StringComparer.OrdinalIgnoreCase))
            .WithMessage($"Campo de ordenação deve ser um dos seguintes: {string.Join(", ", ValidSortFields)}");

        RuleFor(x => x.SortOrder)
            .NotEmpty()
            .WithMessage("Ordem de classificação é obrigatória")
            .Must(order => ValidSortOrders.Contains(order, StringComparer.OrdinalIgnoreCase))
            .WithMessage("Ordem de classificação deve ser 'asc' ou 'desc'");

        // Movement type validation
        RuleFor(x => x.MovementType)
            .Length(1)
            .WithMessage("Tipo de movimento deve ter exatamente 1 caractere")
            .Must(type => string.IsNullOrEmpty(type) || ValidMovementTypes.Contains(char.ToUpper(type[0])))
            .WithMessage($"Tipo de movimento deve ser um dos seguintes: {string.Join(", ", ValidMovementTypes)}")
            .When(x => !string.IsNullOrEmpty(x.MovementType));

        // Policy number validation
        RuleFor(x => x.PolicyNumber)
            .GreaterThan(0)
            .WithMessage("Número da apólice deve ser maior que zero")
            .When(x => x.PolicyNumber.HasValue);

        // Product code validation
        RuleFor(x => x.ProductCode)
            .GreaterThan(0)
            .WithMessage("Código do produto deve ser maior que zero")
            .When(x => x.ProductCode.HasValue);

        // Line of business validation
        RuleFor(x => x.LineOfBusiness)
            .GreaterThan(0)
            .WithMessage("Ramo deve ser maior que zero")
            .When(x => x.LineOfBusiness.HasValue);

        // Company code validation
        RuleFor(x => x.CompanyCode)
            .GreaterThan(0)
            .WithMessage("Código da companhia deve ser maior que zero")
            .When(x => x.CompanyCode.HasValue);

        // Agency code validation
        RuleFor(x => x.AgencyCode)
            .GreaterThan(0)
            .WithMessage("Código da agência deve ser maior que zero")
            .When(x => x.AgencyCode.HasValue);

        // Producer code validation
        RuleFor(x => x.ProducerCode)
            .GreaterThan(0)
            .WithMessage("Código do produtor deve ser maior que zero")
            .When(x => x.ProducerCode.HasValue);
    }
}
