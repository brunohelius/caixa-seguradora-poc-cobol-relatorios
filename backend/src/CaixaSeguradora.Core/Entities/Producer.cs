using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Producer
    {
        [Key]
        [CobolField("WS-COD-PRODUTOR", CobolFieldType.Numeric, 1, 10)]
        public int ProducerCode { get; set; }

        [CobolField("WS-NOM-PRODUTOR", CobolFieldType.Alphanumeric, 11, 60)]
        public string ProducerName { get; set; } = string.Empty;

        [CobolField("WS-NUM-CPF-PRODUTOR", CobolFieldType.Numeric, 71, 11)]
        public string TaxId { get; set; } = string.Empty;

        [CobolField("WS-PER-COMISSAO-PADRAO", CobolFieldType.PackedDecimal, 82, 5, 2, "S9(3)V99")]
        public decimal DefaultCommissionPercentage { get; set; }

        [CobolField("WS-STAT-PRODUTOR", CobolFieldType.Alphanumeric, 87, 1)]
        public string Status { get; set; } = "A"; // A=Ativo, I=Inativo

        public int? AgencyId { get; set; }

        // Navigation properties
        public Agency? Agency { get; set; }
        public ICollection<Policy> Policies { get; set; } = new List<Policy>();
    }
}
