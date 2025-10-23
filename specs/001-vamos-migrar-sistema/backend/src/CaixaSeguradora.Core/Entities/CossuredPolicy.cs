using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class CossuredPolicy
    {
        public int Id { get; set; }

        [CobolField("WS-NUM-APOLICE-COSSEGURO", CobolFieldType.Numeric, 1, 15)]
        public long PolicyNumber { get; set; }

        [CobolField("WS-COD-COSSEGURADORA", CobolFieldType.Numeric, 16, 6)]
        public int CossurerCode { get; set; }

        [CobolField("WS-NOM-COSSEGURADORA", CobolFieldType.Alphanumeric, 22, 60)]
        public string CossurerName { get; set; } = string.Empty;

        [CobolField("WS-PER-PARTICIPACAO", CobolFieldType.PackedDecimal, 82, 5, 2, "S9(3)V99")]
        public decimal ParticipationPercentage { get; set; }

        [CobolField("WS-TIP-COSSEGURO", CobolFieldType.Alphanumeric, 87, 1)]
        public string CossuranceType { get; set; } = string.Empty; // A=Aceito, C=Cedido

        [CobolField("WS-STAT-COSSEGURO", CobolFieldType.Alphanumeric, 88, 1)]
        public string Status { get; set; } = "A"; // A=Ativo, C=Cancelado

        public int PolicyId { get; set; }

        // Navigation properties
        public Policy Policy { get; set; } = null!;
    }
}
