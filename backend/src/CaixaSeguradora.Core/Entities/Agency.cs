using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Agency
    {
        [Key]
        [CobolField("WS-COD-AGENCIA", CobolFieldType.Numeric, 1, 6)]
        public int AgencyCode { get; set; }

        [CobolField("WS-NOM-AGENCIA", CobolFieldType.Alphanumeric, 7, 60)]
        public string AgencyName { get; set; } = string.Empty;

        [CobolField("WS-COD-REGIONAL", CobolFieldType.Numeric, 67, 4)]
        public int RegionalCode { get; set; }

        [CobolField("WS-NOM-REGIONAL", CobolFieldType.Alphanumeric, 71, 50)]
        public string RegionalName { get; set; } = string.Empty;

        [CobolField("WS-STAT-AGENCIA", CobolFieldType.Alphanumeric, 121, 1)]
        public string Status { get; set; } = "A"; // A=Ativo, I=Inativo

        // Navigation properties
        public ICollection<Policy> Policies { get; set; } = new List<Policy>();
        public ICollection<Producer> Producers { get; set; } = new List<Producer>();
    }
}
