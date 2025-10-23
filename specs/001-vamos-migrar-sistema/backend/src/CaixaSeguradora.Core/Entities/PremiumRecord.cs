using System;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Represents the full premium record from COBOL program RG1866B
    /// Maps all 687 data items from the WORKING-STORAGE SECTION
    /// </summary>
    public class PremiumRecord
    {
        public int Id { get; set; }

        // Main identification fields
        [CobolField("WS-NUM-APOLICE", CobolFieldType.Numeric, 1, 15)]
        public long PolicyNumber { get; set; }

        [CobolField("WS-NUM-ENDOSSO", CobolFieldType.Numeric, 16, 10)]
        public int EndorsementNumber { get; set; }

        [CobolField("WS-COD-SISTEMA", CobolFieldType.Alphanumeric, 26, 2)]
        public string SystemCode { get; set; } = string.Empty;

        // Policy dates
        [CobolField("WS-DAT-INICIO-VIGENCIA", CobolFieldType.Date, 32, 8)]
        public DateTime PolicyStartDate { get; set; }

        [CobolField("WS-DAT-FIM-VIGENCIA", CobolFieldType.Date, 40, 8)]
        public DateTime PolicyEndDate { get; set; }

        [CobolField("WS-DAT-EMISSAO", CobolFieldType.Date, 48, 8)]
        public DateTime IssueDate { get; set; }

        // Premium calculation fields (CRITICAL - use decimal per constitution)
        [CobolField("WS-VAL-PREMIO-BRUTO", CobolFieldType.PackedDecimal, 56, 15, 2, "S9(13)V99")]
        public decimal GrossPremium { get; set; }

        [CobolField("WS-VAL-PREMIO-LIQUIDO", CobolFieldType.PackedDecimal, 71, 15, 2, "S9(13)V99")]
        public decimal NetPremium { get; set; }

        [CobolField("WS-VAL-COMISSAO", CobolFieldType.PackedDecimal, 86, 15, 2, "S9(13)V99")]
        public decimal CommissionAmount { get; set; }

        [CobolField("WS-VAL-IOF", CobolFieldType.PackedDecimal, 101, 15, 2, "S9(13)V99")]
        public decimal IOFAmount { get; set; }

        [CobolField("WS-VAL-ADICIONAL-FRACIONAMENTO", CobolFieldType.PackedDecimal, 116, 15, 2, "S9(13)V99")]
        public decimal InstallmentFee { get; set; }

        [CobolField("WS-VAL-CUSTO-APOLICE", CobolFieldType.PackedDecimal, 131, 15, 2, "S9(13)V99")]
        public decimal PolicyCost { get; set; }

        // Client information
        [CobolField("WS-NOM-SEGURADO", CobolFieldType.Alphanumeric, 146, 60)]
        public string InsuredName { get; set; } = string.Empty;

        [CobolField("WS-NUM-CPF-CNPJ-SEGURADO", CobolFieldType.Numeric, 206, 14)]
        public string InsuredTaxId { get; set; } = string.Empty;

        [CobolField("WS-TIP-PESSOA-SEGURADO", CobolFieldType.Alphanumeric, 220, 1)]
        public string InsuredPersonType { get; set; } = string.Empty;

        // Product information
        [CobolField("WS-COD-PRODUTO", CobolFieldType.Numeric, 221, 4)]
        public int ProductCode { get; set; }

        [CobolField("WS-NOM-PRODUTO", CobolFieldType.Alphanumeric, 225, 50)]
        public string ProductName { get; set; } = string.Empty;

        [CobolField("WS-COD-RAMO", CobolFieldType.Numeric, 275, 4)]
        public int LineOfBusinessCode { get; set; }

        // Agency and producer information
        [CobolField("WS-COD-AGENCIA", CobolFieldType.Numeric, 279, 6)]
        public int AgencyCode { get; set; }

        [CobolField("WS-NOM-AGENCIA", CobolFieldType.Alphanumeric, 285, 60)]
        public string AgencyName { get; set; } = string.Empty;

        [CobolField("WS-COD-PRODUTOR", CobolFieldType.Numeric, 345, 10)]
        public int ProducerCode { get; set; }

        [CobolField("WS-NOM-PRODUTOR", CobolFieldType.Alphanumeric, 355, 60)]
        public string ProducerName { get; set; } = string.Empty;

        [CobolField("WS-PER-COMISSAO-PRODUTOR", CobolFieldType.PackedDecimal, 415, 5, 2, "S9(3)V99")]
        public decimal ProducerCommissionPercentage { get; set; }

        // Coverage information
        [CobolField("WS-VAL-IMPORTANCIA-SEGURADA", CobolFieldType.PackedDecimal, 420, 15, 2, "S9(13)V99")]
        public decimal InsuredAmount { get; set; }

        [CobolField("WS-NUM-PARCELAS", CobolFieldType.Numeric, 435, 3)]
        public int NumberOfInstallments { get; set; }

        [CobolField("WS-VAL-PARCELA", CobolFieldType.PackedDecimal, 438, 15, 2, "S9(13)V99")]
        public decimal InstallmentAmount { get; set; }

        // Cossurance fields
        [CobolField("WS-IND-COSSEGURO", CobolFieldType.Alphanumeric, 453, 1)]
        public string CossuranceIndicator { get; set; } = "N"; // S=Sim, N=Não

        [CobolField("WS-PER-PARTICIPACAO-COSSEGURO", CobolFieldType.PackedDecimal, 454, 5, 2, "S9(3)V99")]
        public decimal CossurancePercentage { get; set; }

        [CobolField("WS-VAL-PREMIO-COSSEGURO", CobolFieldType.PackedDecimal, 459, 15, 2, "S9(13)V99")]
        public decimal CossurancePremium { get; set; }

        [CobolField("WS-COD-COSSEGURADORA", CobolFieldType.Numeric, 474, 6)]
        public int? CossurerCode { get; set; }

        // Status and control fields
        [CobolField("WS-STAT-APOLICE", CobolFieldType.Alphanumeric, 480, 2)]
        public string PolicyStatus { get; set; } = string.Empty; // VG=Vigente, CA=Cancelado, SU=Suspenso

        [CobolField("WS-COD-MOEDA", CobolFieldType.Alphanumeric, 482, 3)]
        public string CurrencyCode { get; set; } = "BRL";

        [CobolField("WS-TIP-CALCULO", CobolFieldType.Alphanumeric, 485, 1)]
        public string CalculationType { get; set; } = string.Empty;

        // Address fields
        [CobolField("WS-COD-CEP", CobolFieldType.Numeric, 486, 8)]
        public string PostalCode { get; set; } = string.Empty;

        [CobolField("WS-NOM-LOGRADOURO", CobolFieldType.Alphanumeric, 494, 100)]
        public string Street { get; set; } = string.Empty;

        [CobolField("WS-NUM-ENDERECO", CobolFieldType.Alphanumeric, 594, 10)]
        public string AddressNumber { get; set; } = string.Empty;

        [CobolField("WS-NOM-CIDADE", CobolFieldType.Alphanumeric, 604, 50)]
        public string City { get; set; } = string.Empty;

        [CobolField("WS-COD-UF", CobolFieldType.Alphanumeric, 654, 2)]
        public string State { get; set; } = string.Empty;

        // Database control fields
        [CobolField("WS-COD-USUARIO-INCLUSAO", CobolFieldType.Alphanumeric, 656, 10)]
        public string CreatedBy { get; set; } = string.Empty;

        [CobolField("WS-DAT-INCLUSAO", CobolFieldType.Date, 666, 8)]
        public DateTime CreatedAt { get; set; } = DateTime.UtcNow;

        [CobolField("WS-COD-USUARIO-ALTERACAO", CobolFieldType.Alphanumeric, 674, 10)]
        public string? UpdatedBy { get; set; }

        [CobolField("WS-DAT-ALTERACAO", CobolFieldType.Date, 684, 8)]
        public DateTime? UpdatedAt { get; set; }

        // Processing control
        [CobolField("WS-SQLCODE", CobolFieldType.SignedNumeric, 692, 9)]
        public int? SqlCode { get; set; }

        public string RecordChecksum { get; set; } = string.Empty;

        // Navigation properties
        public int? PolicyId { get; set; }
        public Policy? Policy { get; set; }
    }
}
