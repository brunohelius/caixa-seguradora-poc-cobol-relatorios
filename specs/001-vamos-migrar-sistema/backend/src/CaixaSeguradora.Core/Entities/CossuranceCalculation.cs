using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class CossuranceCalculation
    {
        public int Id { get; set; }

        [CobolField("WS-VAL-PREMIO-BRUTO-TOTAL", CobolFieldType.PackedDecimal, 1, 15, 2, "S9(13)V99")]
        public decimal TotalGrossPremium { get; set; }

        [CobolField("WS-VAL-PREMIO-LIQUIDO-TOTAL", CobolFieldType.PackedDecimal, 16, 15, 2, "S9(13)V99")]
        public decimal TotalNetPremium { get; set; }

        [CobolField("WS-VAL-COMISSAO-TOTAL", CobolFieldType.PackedDecimal, 31, 15, 2, "S9(13)V99")]
        public decimal TotalCommission { get; set; }

        [CobolField("WS-VAL-IOF-TOTAL", CobolFieldType.PackedDecimal, 46, 15, 2, "S9(13)V99")]
        public decimal TotalIOF { get; set; }

        [CobolField("WS-VAL-PREMIO-COSSEGURADORA", CobolFieldType.PackedDecimal, 61, 15, 2, "S9(13)V99")]
        public decimal CossurerPremium { get; set; }

        [CobolField("WS-VAL-COMISSAO-COSSEGURADORA", CobolFieldType.PackedDecimal, 76, 15, 2, "S9(13)V99")]
        public decimal CossurerCommission { get; set; }

        public int CossuredPolicyId { get; set; }

        // Navigation properties
        public CossuredPolicy CossuredPolicy { get; set; } = null!;
    }
}
