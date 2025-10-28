using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Models;

/// <summary>
/// Thread-safe accumulator for premium report totals.
/// COBOL equivalent: Working-storage section counters and accumulators from RG1866B
/// Used in report generation to maintain running sums across premium records.
/// Replicates COBOL PERFORM loops with running totals (sections R0700-R1300).
/// </summary>
public class PremiumAccumulators
{
    private readonly object _lock = new object();

    /// <summary>
    /// Running sum of total premium bruto (gross premium).
    /// COBOL equivalent: WS-TOTAL-PREMIO-BRUTO accumulator
    /// </summary>
    public decimal TotalPremiumBruto { get; private set; }

    /// <summary>
    /// Running sum of total premium liquido (net premium).
    /// COBOL equivalent: WS-TOTAL-PREMIO-LIQUIDO accumulator
    /// </summary>
    public decimal TotalPremiumLiquido { get; private set; }

    /// <summary>
    /// Running sum of IOF tax amounts.
    /// COBOL equivalent: WS-TOTAL-IOF accumulator
    /// </summary>
    public decimal TotalIof { get; private set; }

    /// <summary>
    /// Running sum of commission amounts.
    /// COBOL equivalent: WS-TOTAL-COMISSAO accumulator
    /// </summary>
    public decimal TotalCommission { get; private set; }

    /// <summary>
    /// Count of premium records processed.
    /// COBOL equivalent: WS-RECORD-COUNT counter
    /// </summary>
    public int RecordCount { get; private set; }

    /// <summary>
    /// Adds a premium record to the accumulators.
    /// Thread-safe operation using lock to prevent race conditions in concurrent processing.
    /// </summary>
    /// <param name="premium">The premium record to add to running totals</param>
    /// <exception cref="ArgumentNullException">Thrown when premium is null</exception>
    public void AddPremium(PremiumRecord premium)
    {
        if (premium == null)
        {
            throw new ArgumentNullException(nameof(premium), "Premium record cannot be null");
        }

        lock (_lock)
        {
            TotalPremiumBruto += premium.TotalPremiumTotal;
            TotalPremiumLiquido += premium.NetPremiumTotal;
            TotalIof += premium.IofTotal;
            TotalCommission += premium.CommissionTotal;
            RecordCount++;
        }
    }

    /// <summary>
    /// Returns a summary object with all accumulator values.
    /// Provides a snapshot of current totals for report generation.
    /// </summary>
    /// <returns>Immutable summary object with current accumulator values</returns>
    public PremiumSummary GetSummary()
    {
        lock (_lock)
        {
            return new PremiumSummary
            {
                TotalPremiumBruto = TotalPremiumBruto,
                TotalPremiumLiquido = TotalPremiumLiquido,
                TotalIof = TotalIof,
                TotalCommission = TotalCommission,
                RecordCount = RecordCount,
                AveragePremiumBruto = RecordCount > 0 ? TotalPremiumBruto / RecordCount : 0m,
                AveragePremiumLiquido = RecordCount > 0 ? TotalPremiumLiquido / RecordCount : 0m
            };
        }
    }

    /// <summary>
    /// Resets all accumulator values to zero.
    /// Used when starting a new report generation cycle.
    /// </summary>
    public void Reset()
    {
        lock (_lock)
        {
            TotalPremiumBruto = 0m;
            TotalPremiumLiquido = 0m;
            TotalIof = 0m;
            TotalCommission = 0m;
            RecordCount = 0;
        }
    }
}

/// <summary>
/// Immutable summary snapshot of premium accumulators.
/// Used for report footer totals and summary statistics.
/// </summary>
public class PremiumSummary
{
    /// <summary>
    /// Total gross premium across all records.
    /// </summary>
    public decimal TotalPremiumBruto { get; init; }

    /// <summary>
    /// Total net premium across all records.
    /// </summary>
    public decimal TotalPremiumLiquido { get; init; }

    /// <summary>
    /// Total IOF tax across all records.
    /// </summary>
    public decimal TotalIof { get; init; }

    /// <summary>
    /// Total commission across all records.
    /// </summary>
    public decimal TotalCommission { get; init; }

    /// <summary>
    /// Total number of records processed.
    /// </summary>
    public int RecordCount { get; init; }

    /// <summary>
    /// Average gross premium per record.
    /// </summary>
    public decimal AveragePremiumBruto { get; init; }

    /// <summary>
    /// Average net premium per record.
    /// </summary>
    public decimal AveragePremiumLiquido { get; init; }
}
