using System;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Installment
    {
        public int Id { get; set; }

        [CobolField("WS-NUM-PARCELA", CobolFieldType.Numeric, 1, 3)]
        public int InstallmentNumber { get; set; }

        [CobolField("WS-DAT-VENCIMENTO-PARCELA", CobolFieldType.Date, 4, 8)]
        public DateTime DueDate { get; set; }

        [CobolField("WS-VAL-PARCELA", CobolFieldType.PackedDecimal, 12, 15, 2, "S9(13)V99")]
        public decimal InstallmentAmount { get; set; }

        [CobolField("WS-VAL-PAGO-PARCELA", CobolFieldType.PackedDecimal, 27, 15, 2, "S9(13)V99")]
        public decimal PaidAmount { get; set; }

        [CobolField("WS-DAT-PAGAMENTO", CobolFieldType.Date, 42, 8)]
        public DateTime? PaymentDate { get; set; }

        [CobolField("WS-STAT-PARCELA", CobolFieldType.Alphanumeric, 50, 2)]
        public string Status { get; set; } = "PE"; // PE=Pendente, PG=Pago, CA=Cancelado, VE=Vencido

        [CobolField("WS-COD-BOLETO", CobolFieldType.Alphanumeric, 52, 47)]
        public string BarcodeNumber { get; set; } = string.Empty;

        public int InvoiceId { get; set; }

        // Navigation properties
        public Invoice Invoice { get; set; } = null!;
    }
}
