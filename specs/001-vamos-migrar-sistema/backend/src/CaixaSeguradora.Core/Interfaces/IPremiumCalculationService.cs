using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for premium calculation business logic.
/// Implements financial calculations from COBOL sections R0700-R1300.
/// CRITICAL: All decimal calculations must use exact precision matching COBOL arithmetic.
/// </summary>
/// <remarks>
/// COBOL Section Mapping:
/// - R0700-00-PROCESSA-REGISTRO: Main record processing logic
/// - R0800-00-SELECT-V0HISTOPARC: Installment history calculations
/// - R0900-00-SELECT-V0AUTOPROP: Auto insurance premium processing
/// - R1000-00-SELECT-EF-PRM-EMIT: Premium emission calculations
/// - R1100-00-SELECT-QTDE-VIDAS: Life insurance volume calculations
/// - R1200-00-SELECT-V0PRODUTOR: Producer commission calculations
/// - R1300-00-ACUMULA-VALORES: Value accumulation and totalization
/// </remarks>
public interface IPremiumCalculationService
{
    /// <summary>
    /// Calculates the net premium amount after applying business rules.
    /// Maps to COBOL section R0700-00-PROCESSA-REGISTRO.
    /// </summary>
    /// <param name="premium">Premium record with base values</param>
    /// <param name="policy">Related policy information</param>
    /// <param name="product">Product definition for calculation rules</param>
    /// <returns>Calculated net premium amount with exact decimal precision</returns>
    /// <remarks>
    /// Business Rules:
    /// - Apply product-specific multipliers
    /// - Handle movement type adjustments (emission, cancellation, reversal)
    /// - Process endorsement impacts on premium
    /// - Use COBOL rounding (MidpointRounding.AwayFromZero)
    /// </remarks>
    decimal CalculateNetPremium(PremiumRecord premium, Policy policy, Product product);

    /// <summary>
    /// Calculates gross premium including taxes and fees.
    /// Maps to COBOL section R1000-00-SELECT-EF-PRM-EMIT.
    /// </summary>
    /// <param name="netPremium">Net premium amount</param>
    /// <param name="taxRates">Applicable tax rates (IOF, etc.)</param>
    /// <returns>Gross premium with taxes applied</returns>
    /// <remarks>
    /// Formula: GrossPremium = NetPremium + IOF + AdditionalFees
    /// All calculations use decimal arithmetic with COBOL-compatible rounding
    /// </remarks>
    decimal CalculateGrossPremium(decimal netPremium, TaxRates taxRates);

    /// <summary>
    /// Calculates IOF (Imposto sobre Operações Financeiras) tax.
    /// Brazilian financial operations tax applied to insurance premiums.
    /// </summary>
    /// <param name="netPremium">Net premium amount</param>
    /// <param name="iofRate">IOF rate (typically as decimal, e.g., 0.0738 for 7.38%)</param>
    /// <param name="lineOfBusiness">Insurance line of business (affects IOF calculation)</param>
    /// <returns>Calculated IOF amount</returns>
    /// <remarks>
    /// IOF varies by product line:
    /// - Auto insurance: 7.38%
    /// - Life insurance: May be exempt or different rate
    /// - Other lines: Specific rates per regulation
    /// Uses COBOL ROUND mode for regulatory compliance
    /// </remarks>
    decimal CalculateIOF(decimal netPremium, decimal iofRate, int lineOfBusiness);

    /// <summary>
    /// Calculates producer/broker commission.
    /// Maps to COBOL section R1200-00-SELECT-V0PRODUTOR.
    /// </summary>
    /// <param name="premium">Premium amount for commission basis</param>
    /// <param name="commissionRate">Commission percentage (e.g., 0.15 for 15%)</param>
    /// <param name="producerCode">Producer/broker identifier</param>
    /// <returns>Commission amount</returns>
    /// <remarks>
    /// Commission may vary by:
    /// - Product type
    /// - Producer tier/level
    /// - Sales volume
    /// - Special agreements
    /// </remarks>
    decimal CalculateCommission(decimal premium, decimal commissionRate, int producerCode);

    /// <summary>
    /// Calculates installment values for premium payment plans.
    /// Maps to COBOL section R0800-00-SELECT-V0HISTOPARC.
    /// </summary>
    /// <param name="totalPremium">Total premium to be divided</param>
    /// <param name="numberOfInstallments">Number of installments</param>
    /// <param name="interestRate">Interest rate for installments (if applicable)</param>
    /// <returns>Array of installment amounts (may have adjusted first/last installment for rounding)</returns>
    /// <remarks>
    /// Handles rounding so sum of installments exactly equals total premium.
    /// First installment may be adjusted to absorb rounding differences.
    /// Uses COBOL truncation rules for intermediate calculations.
    /// </remarks>
    decimal[] CalculateInstallments(decimal totalPremium, int numberOfInstallments, decimal interestRate);

    /// <summary>
    /// Accumulates premium values by category for reporting.
    /// Maps to COBOL section R1300-00-ACUMULA-VALORES.
    /// </summary>
    /// <param name="premium">Premium record to accumulate</param>
    /// <param name="accumulator">Current accumulator totals</param>
    /// <remarks>
    /// Accumulates:
    /// - Total net premium
    /// - Total gross premium
    /// - Total IOF
    /// - Total commissions
    /// - Count of records by movement type
    /// - Totals by line of business
    /// - Totals by product modality
    /// CRITICAL: Must use same accumulator structure as COBOL to ensure totals match
    /// </remarks>
    void AccumulateValues(PremiumRecord premium, PremiumAccumulator accumulator);

    /// <summary>
    /// Calculates premium for life insurance based on number of insured lives.
    /// Maps to COBOL section R1100-00-SELECT-QTDE-VIDAS.
    /// </summary>
    /// <param name="baseRate">Base rate per insured life</param>
    /// <param name="numberOfLives">Number of insured individuals</param>
    /// <param name="coverageAmount">Coverage amount per life</param>
    /// <returns>Total life insurance premium</returns>
    /// <remarks>
    /// Life insurance premium calculation considers:
    /// - Base rate per thousand of coverage
    /// - Number of lives covered
    /// - Age and risk factors (handled in baseRate)
    /// </remarks>
    decimal CalculateLifeInsurancePremium(decimal baseRate, int numberOfLives, decimal coverageAmount);

    /// <summary>
    /// Applies movement type adjustments to premium.
    /// Handles emission (E), cancellation (C), reversal (R), etc.
    /// </summary>
    /// <param name="premium">Premium amount</param>
    /// <param name="movementType">Movement type code (E, C, R, etc.)</param>
    /// <returns>Adjusted premium (negative for cancellations/reversals)</returns>
    /// <remarks>
    /// Movement types:
    /// - 'E' (Emissão/Emission): Positive premium
    /// - 'C' (Cancelamento/Cancellation): Negative premium
    /// - 'R' (Estorno/Reversal): Negative premium
    /// - Others as defined in business rules
    /// </remarks>
    decimal ApplyMovementTypeAdjustment(decimal premium, string movementType);

    /// <summary>
    /// Rounds decimal value using COBOL rounding rules.
    /// Ensures consistency with legacy system calculations.
    /// </summary>
    /// <param name="value">Value to round</param>
    /// <param name="decimalPlaces">Number of decimal places</param>
    /// <returns>Rounded value using AwayFromZero mode</returns>
    /// <remarks>
    /// COBOL ROUND mode equivalent to C# MidpointRounding.AwayFromZero
    /// This is CRITICAL for byte-for-byte output matching
    /// </remarks>
    decimal RoundCobol(decimal value, int decimalPlaces);

    /// <summary>
    /// Truncates decimal value using COBOL truncation rules.
    /// Used in intermediate calculations where rounding is not applied.
    /// </summary>
    /// <param name="value">Value to truncate</param>
    /// <param name="decimalPlaces">Number of decimal places to preserve</param>
    /// <returns>Truncated value</returns>
    decimal TruncateCobol(decimal value, int decimalPlaces);
}

/// <summary>
/// Tax rates for premium calculations.
/// </summary>
public class TaxRates
{
    /// <summary>
    /// IOF (Imposto sobre Operações Financeiras) rate.
    /// Typically 0.0738 (7.38%) for most insurance products.
    /// </summary>
    public decimal IOFRate { get; set; }

    /// <summary>
    /// Additional fees or taxes (if applicable).
    /// </summary>
    public decimal AdditionalFeesRate { get; set; }
}

/// <summary>
/// Accumulator for premium totals across all processed records.
/// Matches COBOL AREA-DE-WORK accumulator structure.
/// </summary>
public class PremiumAccumulator
{
    /// <summary>
    /// Total net premium across all records.
    /// </summary>
    public decimal TotalNetPremium { get; set; }

    /// <summary>
    /// Total gross premium (net + taxes).
    /// </summary>
    public decimal TotalGrossPremium { get; set; }

    /// <summary>
    /// Total IOF tax collected.
    /// </summary>
    public decimal TotalIOF { get; set; }

    /// <summary>
    /// Total commissions paid to producers.
    /// </summary>
    public decimal TotalCommissions { get; set; }

    /// <summary>
    /// Count of emission records (movement type 'E').
    /// </summary>
    public int EmissionCount { get; set; }

    /// <summary>
    /// Count of cancellation records (movement type 'C').
    /// </summary>
    public int CancellationCount { get; set; }

    /// <summary>
    /// Count of reversal records (movement type 'R').
    /// </summary>
    public int ReversalCount { get; set; }

    /// <summary>
    /// Total records processed.
    /// </summary>
    public int TotalRecordsProcessed { get; set; }

    /// <summary>
    /// Totals by line of business (indexed by line code).
    /// </summary>
    public Dictionary<int, decimal> TotalsByLineOfBusiness { get; set; } = new();

    /// <summary>
    /// Totals by product modality (indexed by modality code).
    /// </summary>
    public Dictionary<int, decimal> TotalsByModality { get; set; } = new();
}
