using System;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Endorsement
    {
        public int Id { get; set; }

        [CobolField("WS-NUM-ENDOSSO", CobolFieldType.Numeric, 1, 10)]
        public int EndorsementNumber { get; set; }

        [CobolField("WS-TIP-ENDOSSO", CobolFieldType.Alphanumeric, 11, 2)]
        public string EndorsementType { get; set; } = string.Empty; // IN=Inclusão, EX=Exclusão, AL=Alteração

        [CobolField("WS-DAT-EMISSAO-ENDOSSO", CobolFieldType.Date, 13, 8)]
        public DateTime IssueDate { get; set; }

        [CobolField("WS-DAT-INICIO-VIGENCIA-END", CobolFieldType.Date, 21, 8)]
        public DateTime EffectiveDate { get; set; }

        [CobolField("WS-VAL-PREMIO-ENDOSSO", CobolFieldType.PackedDecimal, 29, 15, 2, "S9(13)V99")]
        public decimal PremiumAmount { get; set; }

        [CobolField("WS-DES-MOTIVO-ENDOSSO", CobolFieldType.Alphanumeric, 44, 200)]
        public string Reason { get; set; } = string.Empty;

        [CobolField("WS-STAT-ENDOSSO", CobolFieldType.Alphanumeric, 244, 1)]
        public string Status { get; set; } = "A"; // A=Ativo, C=Cancelado

        public int PolicyId { get; set; }

        // Navigation properties
        public Policy Policy { get; set; } = null!;
    }
}
