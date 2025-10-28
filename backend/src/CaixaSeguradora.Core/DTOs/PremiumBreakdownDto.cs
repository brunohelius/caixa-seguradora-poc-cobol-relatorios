namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// Premium calculation breakdown with all financial components.
    /// Maps to COBOL premium calculation output from sections R0700-R1300.
    /// </summary>
    public class PremiumBreakdownDto
    {
        /// <summary>
        /// Policy number for reference.
        /// </summary>
        public long PolicyNumber { get; set; }

        /// <summary>
        /// Base premium amount before any calculations.
        /// COBOL: VLPRMBAS
        /// </summary>
        public decimal BasePremium { get; set; }

        /// <summary>
        /// Discount applied to base premium.
        /// COBOL: VLDESCON
        /// </summary>
        public decimal Discount { get; set; }

        /// <summary>
        /// Net premium after discount (BasePremium - Discount).
        /// COBOL: VLPRMLIQ
        /// </summary>
        public decimal NetPremium { get; set; }

        /// <summary>
        /// IOF (Imposto sobre Operações Financeiras) tax.
        /// COBOL: VLIOCC
        /// </summary>
        public decimal Iof { get; set; }

        /// <summary>
        /// Installment surcharge for payment in installments.
        /// COBOL: VLADIFRA
        /// </summary>
        public decimal InstallmentSurcharge { get; set; }

        /// <summary>
        /// Policy issuance cost.
        /// COBOL: VLCUSEMI
        /// </summary>
        public decimal IssuanceCost { get; set; }

        /// <summary>
        /// Total premium including all components.
        /// COBOL: VLPRMTOT
        /// Formula: NetPremium + IOF + InstallmentSurcharge + IssuanceCost
        /// </summary>
        public decimal TotalPremium { get; set; }

        /// <summary>
        /// Total commission amount (sum of all commission types).
        /// COBOL: VLCOMIS
        /// </summary>
        public decimal TotalCommission { get; set; }

        /// <summary>
        /// Broker commission (corretagem).
        /// </summary>
        public decimal BrokerCommission { get; set; }

        /// <summary>
        /// Agency commission (agenciamento).
        /// </summary>
        public decimal AgencyCommission { get; set; }

        /// <summary>
        /// Administration fee.
        /// </summary>
        public decimal AdministrationFee { get; set; }

        /// <summary>
        /// Movement type applied (101-106).
        /// </summary>
        public string MovementType { get; set; }

        /// <summary>
        /// Number of installments for payment.
        /// </summary>
        public int NumberOfInstallments { get; set; }

        /// <summary>
        /// Calculation timestamp.
        /// </summary>
        public DateTime CalculatedAt { get; set; } = DateTime.UtcNow;

        /// <summary>
        /// Whether the premium was adjusted for a specific ramo (branch).
        /// </summary>
        public bool RamoSpecificAdjustmentApplied { get; set; }

        /// <summary>
        /// Details of any adjustments made during calculation.
        /// </summary>
        public string AdjustmentNotes { get; set; }
    }
}
