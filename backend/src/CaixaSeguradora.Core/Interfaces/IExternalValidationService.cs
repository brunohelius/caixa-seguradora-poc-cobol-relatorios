using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Interface para serviço de validação externa (equivalente ao módulo COBOL GE0010S).
/// Valida documentos, endereços e números de apólice conforme regras de negócio.
/// </summary>
public interface IExternalValidationService
{
    /// <summary>
    /// Valida número de documento (CPF ou CNPJ) incluindo formato e dígitos verificadores.
    /// </summary>
    /// <param name="document">Número do documento (com ou sem formatação)</param>
    /// <param name="documentType">Tipo do documento ("CPF" ou "CNPJ")</param>
    /// <returns>True se o documento é válido</returns>
    Task<bool> ValidateDocumentNumberAsync(string document, string documentType, CancellationToken cancellationToken = default);

    /// <summary>
    /// Valida endereço incluindo CEP e UF.
    /// </summary>
    /// <param name="address">Entidade de endereço a ser validada</param>
    /// <returns>True se o endereço é válido</returns>
    Task<bool> ValidateAddressAsync(Address address, CancellationToken cancellationToken = default);

    /// <summary>
    /// Valida número de apólice conforme formato e regras de negócio.
    /// </summary>
    /// <param name="policyNumber">Número da apólice</param>
    /// <returns>True se o número da apólice é válido</returns>
    Task<bool> ValidatePolicyNumberAsync(long policyNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Valida código postal (CEP) brasileiro (8 dígitos).
    /// </summary>
    /// <param name="postalCode">CEP com ou sem formatação</param>
    /// <returns>True se o CEP está no formato válido</returns>
    bool ValidatePostalCode(string postalCode);

    /// <summary>
    /// Valida código de estado (UF) brasileiro (2 letras).
    /// </summary>
    /// <param name="stateCode">Código do estado (ex: "SP", "RJ")</param>
    /// <returns>True se a UF é válida</returns>
    bool ValidateStateCode(string stateCode);
}
