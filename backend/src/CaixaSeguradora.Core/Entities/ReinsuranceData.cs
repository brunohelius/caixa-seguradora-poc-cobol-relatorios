using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Reinsurance calculation result from RE0001S external module.
    /// Stores reinsurance treaty allocations for policies requiring risk distribution.
    /// COBOL Source: RE0001S external module output (section R1700)
    /// </summary>
    [Table("REINSURANCE_DATA")]
    public class ReinsuranceData : AuditableEntity
    {
        /// <summary>
        /// Primary key for reinsurance record.
        /// </summary>
        [Key]
        public long ReinsuranceId { get; set; }

        /// <summary>
        /// Policy number that requires reinsurance.
        /// Foreign key to Policy entity.
        /// COBOL: LKRE-I-NUM-APOLICE
        /// </summary>
        [Column(TypeName = "BIGINT")]
        [Required]
        public long PolicyNumber { get; set; }

        /// <summary>
        /// Effective date for reinsurance calculation.
        /// Typically matches policy effective date.
        /// COBOL: LKRE-I-DAT-EFETIV
        /// </summary>
        [Required]
        public DateTime EffectiveDate { get; set; }

        /// <summary>
        /// Total premium amount subject to reinsurance.
        /// COBOL: LKRE-I-VALOR-PREMIO (COMP-3 S9(13)V99)
        /// </summary>
        [Column(TypeName = "DECIMAL(15,2)")]
        [Required]
        public decimal PremiumAmount { get; set; }

        /// <summary>
        /// Amount of premium ceded to reinsurers.
        /// COBOL: LKRE-O-VALOR-RESSEG (COMP-3 S9(13)V99)
        /// </summary>
        [Column(TypeName = "DECIMAL(15,2)")]
        public decimal ReinsuredAmount { get; set; }

        /// <summary>
        /// Percentage of premium allocated to reinsurance.
        /// Range: 0.00 to 100.00
        /// COBOL: LKRE-O-PERC-RESSEG (COMP-3 S9(3)V99)
        /// </summary>
        [Column(TypeName = "DECIMAL(5,2)")]
        public decimal ReinsurancePercentage { get; set; }

        /// <summary>
        /// Treaty code identifying the reinsurance agreement.
        /// COBOL: LKRE-O-COD-TRATADO (X(10))
        /// </summary>
        [MaxLength(10)]
        public string TreatyCode { get; set; }

        /// <summary>
        /// Return code from RE0001S module.
        /// "00" = Success
        /// "04" = Warning (partial reinsurance)
        /// "08" = Error (reinsurance calculation failed)
        /// COBOL: LKRE-O-RETURN-CODE (X(2))
        /// </summary>
        [Required]
        [MaxLength(2)]
        public string ReturnCode { get; set; }

        /// <summary>
        /// Error message from reinsurance calculation (if any).
        /// COBOL: LKRE-O-MENSAGEM (X(100))
        /// </summary>
        [MaxLength(100)]
        public string ErrorMessage { get; set; }

        /// <summary>
        /// Product code for reinsurance treaty lookup.
        /// COBOL: LKRE-I-COD-PRODUTO (COMP S9(4))
        /// </summary>
        public short ProductCode { get; set; }

        /// <summary>
        /// Ramo SUSEP for treaty selection logic.
        /// COBOL: LKRE-I-RAMO-SUSEP (9(4))
        /// </summary>
        public int RamoSusep { get; set; }

        /// <summary>
        /// Company code for JV reinsurance allocation.
        /// COBOL: LKRE-I-COD-EMP (COMP S9(4))
        /// </summary>
        public short CompanyCode { get; set; }

        /// <summary>
        /// Calculated retained amount (not ceded to reinsurance).
        /// RetainedAmount = PremiumAmount - ReinsuredAmount
        /// </summary>
        [Column(TypeName = "DECIMAL(15,2)")]
        public decimal? RetainedAmount => PremiumAmount - ReinsuredAmount;

        /// <summary>
        /// Indicates if reinsurance calculation was successful.
        /// </summary>
        public bool IsSuccessful => ReturnCode == "00";

        /// <summary>
        /// Indicates if the policy has reinsurance coverage.
        /// </summary>
        public bool HasReinsurance => ReinsuredAmount > 0;

        // Navigation Properties

        /// <summary>
        /// Related policy entity.
        /// </summary>
        [ForeignKey("PolicyNumber")]
        public virtual Policy Policy { get; set; }
    }
}
