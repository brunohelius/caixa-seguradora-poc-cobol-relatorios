using System;
using System.Collections.Generic;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Policy
    {
        public int Id { get; set; }

        [CobolField("WS-NUM-APOLICE", CobolFieldType.Numeric, 1, 15)]
        public long PolicyNumber { get; set; }

        [CobolField("WS-NUM-ENDOSSO", CobolFieldType.Numeric, 16, 10)]
        public int EndorsementNumber { get; set; }

        [CobolField("WS-COD-SISTEMA", CobolFieldType.Alphanumeric, 26, 2)]
        public string SystemCode { get; set; } = string.Empty;

        [CobolField("WS-COD-PRODUTO", CobolFieldType.Numeric, 28, 4)]
        public int ProductCode { get; set; }

        [CobolField("WS-DAT-INICIO-VIGENCIA", CobolFieldType.Date, 32, 8)]
        public DateTime EffectiveDate { get; set; }

        [CobolField("WS-DAT-FIM-VIGENCIA", CobolFieldType.Date, 40, 8)]
        public DateTime ExpirationDate { get; set; }

        [CobolField("WS-VAL-PREMIO-TOTAL", CobolFieldType.PackedDecimal, 48, 15, 2, "S9(13)V99")]
        public decimal TotalPremium { get; set; }

        [CobolField("WS-VAL-PREMIO-LIQUIDO", CobolFieldType.PackedDecimal, 63, 15, 2, "S9(13)V99")]
        public decimal NetPremium { get; set; }

        [CobolField("WS-STAT-APOLICE", CobolFieldType.Alphanumeric, 78, 1)]
        public string PolicyStatus { get; set; } = string.Empty;

        public int ClientId { get; set; }
        public int AgencyId { get; set; }
        public int ProducerId { get; set; }

        // Navigation properties
        public Client Client { get; set; } = null!;
        public Agency Agency { get; set; } = null!;
        public Producer Producer { get; set; } = null!;
        public Product Product { get; set; } = null!;
        public ICollection<Endorsement> Endorsements { get; set; } = new List<Endorsement>();
        public ICollection<Coverage> Coverages { get; set; } = new List<Coverage>();
        public ICollection<Invoice> Invoices { get; set; } = new List<Invoice>();
    }
}
