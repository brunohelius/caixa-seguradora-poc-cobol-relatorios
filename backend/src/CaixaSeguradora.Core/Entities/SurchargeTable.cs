using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Represents installment surcharge configuration table.
    /// COBOL Source: Table TB_ACRESCIMO_PARCELAMENTO referenced in premium calculation sections
    /// Used to apply fractional payment surcharges based on number of installments.
    /// Maps to COBOL paragraph R0900-CALCULAR-ACRESCIMO-FRACIONAMENTO.
    /// </summary>
    public class SurchargeTable : AuditableEntity
    {
        /// <summary>
        /// Primary key for surcharge configuration record.
        /// </summary>
        [Key]
        [CobolField(PicClause = "9(9)", Length = 9, FieldType = CobolFieldType.Numeric)]
        public int Id { get; set; }

        /// <summary>
        /// Number of installments for this surcharge rate.
        /// COBOL equivalent: TB-NUM-PARCELAS
        /// Valid range: 1-12 (monthly installments)
        /// </summary>
        [Required]
        [Range(1, 12, ErrorMessage = "Número de parcelas deve estar entre 1 e 12")]
        [CobolField(PicClause = "9(2)", Length = 2, FieldType = CobolFieldType.Numeric)]
        public int NumberOfInstallments { get; set; }

        /// <summary>
        /// Surcharge percentage applied for this installment count.
        /// COBOL equivalent: TB-PERC-ACRESCIMO (PIC 9(2)V9(4))
        /// Example: 5.25% stored as 5.2500
        /// Applied as: premium * (1 + SurchargePercentage/100)
        /// </summary>
        [Required]
        [Range(0, 99.9999, ErrorMessage = "Percentual de acréscimo deve estar entre 0 e 99.9999")]
        [CobolField(PicClause = "9(2)V9(4)", Length = 7, DecimalPlaces = 4, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(6,4)")]
        public decimal SurchargePercentage { get; set; }

        /// <summary>
        /// Date when this surcharge rate becomes effective.
        /// COBOL equivalent: TB-DATA-INICIO-VIGENCIA
        /// Used to support historical rate changes.
        /// </summary>
        [Required]
        [CobolField(PicClause = "X(8)", Length = 8)]
        public DateTime EffectiveDate { get; set; }

        /// <summary>
        /// Date when this surcharge rate expires (null = no expiration).
        /// COBOL equivalent: TB-DATA-FIM-VIGENCIA
        /// Allows for temporal rate configurations.
        /// </summary>
        [CobolField(PicClause = "X(8)", Length = 8)]
        public DateTime? ExpirationDate { get; set; }

        /// <summary>
        /// Company code that this surcharge applies to.
        /// COBOL equivalent: TB-COD-EMP
        /// Allows company-specific surcharge configurations.
        /// </summary>
        [CobolField(PicClause = "9(9)", Length = 9, FieldType = CobolFieldType.Numeric)]
        public int CompanyCode { get; set; }

        /// <summary>
        /// Line of business (Ramo SUSEP) this surcharge applies to.
        /// COBOL equivalent: TB-RAMO-SUSEP
        /// Null = applies to all lines of business.
        /// </summary>
        [CobolField(PicClause = "9(4)", Length = 4, FieldType = CobolFieldType.Numeric)]
        public int? LineOfBusiness { get; set; }

        /// <summary>
        /// Indicates if this surcharge configuration is currently active.
        /// COBOL equivalent: TB-IND-ATIVO ('S'/'N')
        /// </summary>
        [Required]
        [MaxLength(1)]
        [CobolField(PicClause = "X(1)", Length = 1)]
        public string IsActive { get; set; } = "S";

        /// <summary>
        /// Checks if this surcharge is valid for a given date.
        /// </summary>
        /// <param name="checkDate">Date to validate against</param>
        /// <returns>True if surcharge is valid on the given date</returns>
        public bool IsValidOnDate(DateTime checkDate)
        {
            return checkDate >= EffectiveDate &&
                   (ExpirationDate == null || checkDate <= ExpirationDate) &&
                   IsActive == "S";
        }

        /// <summary>
        /// Calculates the surcharge amount for a given premium value.
        /// COBOL equivalent: COMPUTE WS-VALOR-ACRESCIMO = WS-PREMIO * TB-PERC-ACRESCIMO / 100
        /// </summary>
        /// <param name="premiumAmount">Base premium amount to apply surcharge to</param>
        /// <returns>Surcharge amount to add to premium</returns>
        public decimal CalculateSurchargeAmount(decimal premiumAmount)
        {
            return premiumAmount * (SurchargePercentage / 100m);
        }

        /// <summary>
        /// Calculates total premium including surcharge.
        /// COBOL equivalent: COMPUTE WS-PREMIO-TOTAL = WS-PREMIO + WS-VALOR-ACRESCIMO
        /// </summary>
        /// <param name="premiumAmount">Base premium amount</param>
        /// <returns>Total premium with surcharge applied</returns>
        public decimal CalculateTotalWithSurcharge(decimal premiumAmount)
        {
            return premiumAmount + CalculateSurchargeAmount(premiumAmount);
        }
    }
}
