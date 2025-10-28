using System.Text.RegularExpressions;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Implementação do serviço de validação externa (equivalente ao módulo COBOL GE0010S).
/// Valida documentos, endereços e números de apólice conforme regras de negócio brasileiras.
/// </summary>
public class ExternalValidationService : IExternalValidationService
{
    private readonly ILogger<ExternalValidationService> _logger;
    private readonly IFormattingService _formattingService;

    // Estados brasileiros válidos (27 UFs)
    private static readonly HashSet<string> ValidStateCodes = new(StringComparer.OrdinalIgnoreCase)
    {
        "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
        "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
        "RS", "RO", "RR", "SC", "SP", "SE", "TO"
    };

    // Regex para CEP (8 dígitos)
    private static readonly Regex CepRegex = new(@"^\d{8}$", RegexOptions.Compiled);

    public ExternalValidationService(
        ILogger<ExternalValidationService> logger,
        IFormattingService formattingService)
    {
        _logger = logger;
        _formattingService = formattingService;
    }

    /// <inheritdoc/>
    public Task<bool> ValidateDocumentNumberAsync(string document, string documentType, CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrWhiteSpace(document))
        {
            _logger.LogWarning("Tentativa de validar documento vazio ou nulo");
            return Task.FromResult(false);
        }

        var cleaned = _formattingService.RemoveFormatting(document);
        bool isValid;

        switch (documentType?.ToUpperInvariant())
        {
            case "CPF":
                isValid = cleaned.Length == 11 && _formattingService.ValidateCpfCheckDigit(cleaned);
                _logger.LogDebug("Validação de CPF {Document}: {IsValid}", document, isValid);
                break;

            case "CNPJ":
                isValid = cleaned.Length == 14 && _formattingService.ValidateCnpjCheckDigit(cleaned);
                _logger.LogDebug("Validação de CNPJ {Document}: {IsValid}", document, isValid);
                break;

            default:
                _logger.LogWarning("Tipo de documento desconhecido: {DocumentType}", documentType);
                isValid = false;
                break;
        }

        return Task.FromResult(isValid);
    }

    /// <inheritdoc/>
    public Task<bool> ValidateAddressAsync(Address address, CancellationToken cancellationToken = default)
    {
        if (address == null)
        {
            _logger.LogWarning("Tentativa de validar endereço nulo");
            return Task.FromResult(false);
        }

        var validations = new List<(string Field, bool IsValid)>
        {
            ("CEP", ValidatePostalCode(address.PostalCode)),
            ("UF", ValidateStateCode(address.StateCode)),
            ("Logradouro", !string.IsNullOrWhiteSpace(address.Street)),
            ("Cidade", !string.IsNullOrWhiteSpace(address.City))
        };

        var allValid = validations.All(v => v.IsValid);

        if (!allValid)
        {
            var invalidFields = validations.Where(v => !v.IsValid).Select(v => v.Field);
            _logger.LogWarning("Endereço inválido. Campos com erro: {InvalidFields}", string.Join(", ", invalidFields));
        }

        return Task.FromResult(allValid);
    }

    /// <inheritdoc/>
    public Task<bool> ValidatePolicyNumberAsync(long policyNumber, CancellationToken cancellationToken = default)
    {
        // Validações de número de apólice conforme regras de negócio
        // Número deve ser positivo e ter entre 1 e 13 dígitos (PIC 9(13) no COBOL)
        var isValid = policyNumber > 0 && policyNumber <= 9999999999999;

        if (!isValid)
        {
            _logger.LogWarning("Número de apólice inválido: {PolicyNumber}. Deve estar entre 1 e 9999999999999", policyNumber);
        }

        return Task.FromResult(isValid);
    }

    /// <inheritdoc/>
    public bool ValidatePostalCode(string postalCode)
    {
        if (string.IsNullOrWhiteSpace(postalCode))
            return false;

        var cleaned = _formattingService.RemoveFormatting(postalCode);
        return CepRegex.IsMatch(cleaned);
    }

    /// <inheritdoc/>
    public bool ValidateStateCode(string stateCode)
    {
        if (string.IsNullOrWhiteSpace(stateCode))
            return false;

        return ValidStateCodes.Contains(stateCode.Trim());
    }
}
