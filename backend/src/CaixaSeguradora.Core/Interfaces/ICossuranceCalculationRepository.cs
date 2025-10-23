using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for cossurance calculation data (GE397, GE399 tables).
/// Accessed at COBOL sections R4600 (GE397), R5200, R5300 (cursor), R5400 (fetch), R5500 (calculation).
/// </summary>
public interface ICossuranceCalculationRepository : IRepository<CossuranceCalculation>
{
    /// <summary>
    /// Streams cossurance calculation records using cursor pattern.
    /// Maps to COBOL cursor: CUR-GE399 declared at R5300-00-DECLARE-GE399.
    /// Fetched at R5400-00-FETCH-GE399.
    /// </summary>
    IAsyncEnumerable<CossuranceCalculation> GetCalculationRecordsAsync(
        int companyCode,
        string calculationCriteria,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets GE397 reference data for cossurance processing.
    /// Maps to COBOL section R4600-00-SELECT-GE397.
    /// </summary>
    Task<CossuranceCalculation?> GetReferenceDataAsync(int referenceCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets calculation parameters for a specific policy and company.
    /// Maps to COBOL section R5200-00-SELECT-GE399.
    /// </summary>
    Task<CossuranceCalculation?> GetCalculationParametersAsync(
        long policyNumber,
        int companyCode,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Calculates cossurance premium distribution.
    /// Implements COBOL logic from R5500-00-CALCULA-COSG-CED.
    /// </summary>
    Task<CossurancePremiumDistribution> CalculatePremiumDistributionAsync(
        long policyNumber,
        decimal totalPremium,
        decimal cossurancePercentage,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Cossurance premium distribution result.
/// </summary>
public class CossurancePremiumDistribution
{
    public long PolicyNumber { get; set; }
    public decimal TotalPremium { get; set; }
    public decimal CossurancePercentage { get; set; }
    public decimal CededPremium { get; set; }
    public decimal RetainedPremium { get; set; }
    public List<CompanyShare> CompanyShares { get; set; } = new();
}

/// <summary>
/// Individual company share in cossurance.
/// </summary>
public class CompanyShare
{
    public int CompanyCode { get; set; }
    public string CompanyName { get; set; } = string.Empty;
    public decimal SharePercentage { get; set; }
    public decimal SharePremium { get; set; }
}
