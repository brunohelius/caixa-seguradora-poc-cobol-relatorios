using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Coverage
    {
        public int Id { get; set; }

        [CobolField("WS-COD-COBERTURA", CobolFieldType.Numeric, 1, 6)]
        public int CoverageCode { get; set; }

        [CobolField("WS-NOM-COBERTURA", CobolFieldType.Alphanumeric, 7, 50)]
        public string CoverageName { get; set; } = string.Empty;

        [CobolField("WS-VAL-IMPORTANCIA-SEGURADA", CobolFieldType.PackedDecimal, 57, 15, 2, "S9(13)V99")]
        public decimal InsuredAmount { get; set; }

        [CobolField("WS-VAL-PREMIO-COBERTURA", CobolFieldType.PackedDecimal, 72, 15, 2, "S9(13)V99")]
        public decimal PremiumAmount { get; set; }

        [CobolField("WS-PER-FRANQUIA", CobolFieldType.PackedDecimal, 87, 5, 2, "S9(3)V99")]
        public decimal DeductiblePercentage { get; set; }

        [CobolField("WS-STAT-COBERTURA", CobolFieldType.Alphanumeric, 92, 1)]
        public string Status { get; set; } = "A"; // A=Ativo, I=Inativo

        public int PolicyId { get; set; }
        public int ProductId { get; set; }

        // Additional properties for repository compatibility
        [System.ComponentModel.DataAnnotations.Schema.NotMapped]
        public long CoverageId => Id;  // Alias

        [CobolField("WS-TIP-COBERTURA", CobolFieldType.Alphanumeric, 100, 2)]
        public string CoverageType { get; set; } = string.Empty;  // BA=Básica, AD=Adicional

        [CobolField("WS-NUM-APOLICE-COB", CobolFieldType.Numeric, 102, 15)]
        public long PolicyNumber { get; set; }

        [System.ComponentModel.DataAnnotations.Schema.NotMapped]
        public System.Collections.Generic.ICollection<Coverage> Coverages { get; set; } = new System.Collections.Generic.List<Coverage>();

        // Navigation properties
        public Policy Policy { get; set; } = null!;
        public Product Product { get; set; } = null!;
    }
}
