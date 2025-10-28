namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// Commission breakdown with all commission types.
    /// Maps to COBOL commission calculations from sections R1000-R1200.
    /// </summary>
    public class CommissionBreakdownDto
    {
        /// <summary>
        /// Policy number for reference.
        /// </summary>
        public long PolicyNumber { get; set; }

        /// <summary>
        /// Net premium amount used as basis for commission.
        /// </summary>
        public decimal NetPremiumBasis { get; set; }

        /// <summary>
        /// Broker commission (corretagem).
        /// COBOL: VLCORRET
        /// </summary>
        public decimal BrokerCommission { get; set; }

        /// <summary>
        /// Broker commission rate applied.
        /// </summary>
        public decimal BrokerCommissionRate { get; set; }

        /// <summary>
        /// Agency commission (agenciamento).
        /// COBOL: VLAGENC
        /// </summary>
        public decimal AgencyCommission { get; set; }

        /// <summary>
        /// Agency commission rate applied.
        /// </summary>
        public decimal AgencyCommissionRate { get; set; }

        /// <summary>
        /// Administration fee.
        /// COBOL: VLADMN
        /// </summary>
        public decimal AdministrationFee { get; set; }

        /// <summary>
        /// Administration fee rate applied.
        /// </summary>
        public decimal AdministrationFeeRate { get; set; }

        /// <summary>
        /// Total commission (sum of all types).
        /// </summary>
        public decimal TotalCommission { get; set; }

        /// <summary>
        /// Producer code for this commission.
        /// </summary>
        public int? ProducerCode { get; set; }

        /// <summary>
        /// Agency code for this commission.
        /// </summary>
        public int? AgencyCode { get; set; }

        /// <summary>
        /// Whether this is a direct sale (no agency commission).
        /// </summary>
        public bool IsDirectSale { get; set; }

        /// <summary>
        /// Calculation timestamp.
        /// </summary>
        public DateTime CalculatedAt { get; set; } = DateTime.UtcNow;
    }
}
