namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO para cálculo de resseguro.
/// Corresponde aos parâmetros de saída do módulo COBOL RE0001S (LKRE-O-*).
/// </summary>
public class ReinsuranceResponse
{
    /// <summary>
    /// Valor do prêmio ressegurado (LKRE-O-VALOR-RESSEG - PIC 9(13)V99 COMP-3)
    /// </summary>
    public decimal ReinsuredAmount { get; set; }

    /// <summary>
    /// Percentual de resseguro (LKRE-O-PERC-RESSEG - PIC 9(3)V99 COMP-3)
    /// Exemplo: 30.00 significa 30% do prêmio vai para resseguro
    /// </summary>
    public decimal ReinsurancePercentage { get; set; }

    /// <summary>
    /// Código do tratado de resseguro (LKRE-O-COD-TRATADO - PIC X(10))
    /// </summary>
    public string TreatyCode { get; set; } = string.Empty;

    /// <summary>
    /// Data de corte (LKRE-O-CUTOFF-DATE - PIC 9(8) YYYYMMDD)
    /// Relacionado a JAZZ-192299
    /// </summary>
    public DateTime? CutoffDate { get; set; }

    /// <summary>
    /// Código do contrato de resseguro (LKRE-O-COD-CONTRATO - PIC X(15))
    /// Relacionado a JAZZ-221601
    /// </summary>
    public string ContractCode { get; set; } = string.Empty;

    /// <summary>
    /// Código de retorno (LKRE-O-RETURN-CODE - PIC 9(2))
    /// "00" = Sucesso
    /// "04" = Aviso (processado com observações)
    /// "08" = Erro de validação
    /// "12" = Erro crítico
    /// </summary>
    public string ReturnCode { get; set; } = "00";

    /// <summary>
    /// Mensagem de erro ou aviso (não existe no COBOL, adicionado para melhor diagnóstico)
    /// </summary>
    public string? ErrorMessage { get; set; }

    /// <summary>
    /// Indica se o processamento foi bem-sucedido (ReturnCode == "00")
    /// </summary>
    public bool IsSuccess => ReturnCode == "00";
}
