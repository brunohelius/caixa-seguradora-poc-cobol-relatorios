using System;
using System.Collections.Generic;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Invoice
    {
        public int Id { get; set; }

        [CobolField("WS-NUM-FATURA", CobolFieldType.Numeric, 1, 15)]
        public long InvoiceNumber { get; set; }

        [CobolField("WS-DAT-EMISSAO-FATURA", CobolFieldType.Date, 16, 8)]
        public DateTime IssueDate { get; set; }

        [CobolField("WS-DAT-VENCIMENTO", CobolFieldType.Date, 24, 8)]
        public DateTime DueDate { get; set; }

        [CobolField("WS-VAL-TOTAL-FATURA", CobolFieldType.PackedDecimal, 32, 15, 2, "S9(13)V99")]
        public decimal TotalAmount { get; set; }

        [CobolField("WS-VAL-PAGO", CobolFieldType.PackedDecimal, 47, 15, 2, "S9(13)V99")]
        public decimal PaidAmount { get; set; }

        [CobolField("WS-STAT-FATURA", CobolFieldType.Alphanumeric, 62, 2)]
        public string Status { get; set; } = "PE"; // PE=Pendente, PG=Pago, CA=Cancelado, VE=Vencido

        [CobolField("WS-NUM-PARCELAS", CobolFieldType.Numeric, 64, 3)]
        public int NumberOfInstallments { get; set; }

        public long PolicyNumber { get; set; }

        // Navigation properties
        public Policy Policy { get; set; } = null!;
        public ICollection<Installment> Installments { get; set; } = new List<Installment>();
    }
}
