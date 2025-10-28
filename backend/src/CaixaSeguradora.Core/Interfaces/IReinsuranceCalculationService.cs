using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Interface para serviço de cálculo de resseguro (equivalente ao módulo COBOL RE0001S).
/// Calcula prêmios de resseguro e alocações de tratados conforme regras de negócio.
/// </summary>
public interface IReinsuranceCalculationService
{
    /// <summary>
    /// Calcula o resseguro para uma apólice específica.
    /// Equivalente ao CALL 'RE0001S' USING LKRE-PARM-RE0001S no COBOL.
    /// </summary>
    /// <param name="policyNumber">Número da apólice (LKRE-I-APOLICE)</param>
    /// <param name="premiumAmount">Valor do prêmio (LKRE-I-VALOR-PREMIO)</param>
    /// <param name="productCode">Código do produto (LKRE-I-CODIGO-PRODUTO)</param>
    /// <param name="effectiveDate">Data de vigência (LKRE-I-DATA-VIGENCIA)</param>
    /// <param name="susepBranchCode">Código do ramo SUSEP (LKRE-I-RAMO-SUSEP)</param>
    /// <param name="cancellationToken">Token de cancelamento</param>
    /// <returns>Resposta contendo valor ressegurado, percentual, código do tratado e código de retorno</returns>
    Task<ReinsuranceResponse> CalculateReinsuranceAsync(
        long policyNumber,
        decimal premiumAmount,
        int productCode,
        DateTime effectiveDate,
        int susepBranchCode,
        CancellationToken cancellationToken = default);
}
