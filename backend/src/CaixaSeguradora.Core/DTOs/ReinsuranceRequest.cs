namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Request DTO para cálculo de resseguro.
/// Corresponde aos parâmetros de entrada do módulo COBOL RE0001S (LKRE-I-*).
/// </summary>
public class ReinsuranceRequest
{
    /// <summary>
    /// Número da apólice (LKRE-I-APOLICE - PIC 9(10))
    /// </summary>
    public long PolicyNumber { get; set; }

    /// <summary>
    /// Valor do prêmio (LKRE-I-VALOR-PREMIO - PIC 9(13)V99 COMP-3)
    /// </summary>
    public decimal PremiumAmount { get; set; }

    /// <summary>
    /// Código do produto (LKRE-I-CODIGO-PRODUTO - PIC 9(4))
    /// </summary>
    public int ProductCode { get; set; }

    /// <summary>
    /// Data de vigência (LKRE-I-DATA-VIGENCIA - PIC 9(8) YYYYMMDD)
    /// </summary>
    public DateTime EffectiveDate { get; set; }

    /// <summary>
    /// Código do ramo SUSEP (LKRE-I-RAMO-SUSEP - PIC 9(4))
    /// </summary>
    public int SusepBranchCode { get; set; }
}
