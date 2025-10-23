using System.Collections.Generic;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Product
    {
        public int Id { get; set; }

        [CobolField("WS-COD-PRODUTO", CobolFieldType.Numeric, 1, 4)]
        public int ProductCode { get; set; }

        [CobolField("WS-NOM-PRODUTO", CobolFieldType.Alphanumeric, 5, 50)]
        public string ProductName { get; set; } = string.Empty;

        [CobolField("WS-DES-PRODUTO", CobolFieldType.Alphanumeric, 55, 200)]
        public string Description { get; set; } = string.Empty;

        [CobolField("WS-TIP-PRODUTO", CobolFieldType.Alphanumeric, 255, 20)]
        public string ProductType { get; set; } = string.Empty;

        [CobolField("WS-STAT-PRODUTO", CobolFieldType.Alphanumeric, 275, 1)]
        public string Status { get; set; } = "A"; // A=Ativo, I=Inativo

        [CobolField("WS-PER-COMISSAO", CobolFieldType.PackedDecimal, 276, 5, 2, "S9(3)V99")]
        public decimal CommissionPercentage { get; set; }

        // Navigation properties
        public ICollection<Policy> Policies { get; set; } = new List<Policy>();
        public ICollection<Coverage> Coverages { get; set; } = new List<Coverage>();
    }
}
