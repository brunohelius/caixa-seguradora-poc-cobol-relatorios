using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for cossured/ceded policy data (V0APOLCOSCED view).
/// Accessed at COBOL sections R4800, R4900 (cursor declaration), R5000 (cursor fetch).
/// Critical for cossurance/reinsurance processing (PREMCED report).
/// </summary>
public interface ICossuredPolicyRepository : IRepository<CossuredPolicy>
{
    /// <summary>
    /// Streams cossured/ceded policies for a given policy using cursor pattern.
    /// Maps to COBOL cursor: CUR-V0APOLCOSCED declared at R4900-00-DECLARE-V0APOLCOSCED.
    /// Fetched at R5000-00-FETCH-V0APOLCOSCED.
    /// </summary>
    IAsyncEnumerable<CossuredPolicy> GetCossuredPoliciesAsync(long policyNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets cossurance policies by type (Ceded vs. Acquired).
    /// </summary>
    IAsyncEnumerable<CossuredPolicy> GetByTypeAsync(string cossuranceType, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets cossured policy information for specific company.
    /// Maps to COBOL section R4800-00-SELECT-V0APOLCOSCED.
    /// </summary>
    Task<CossuredPolicy?> GetByPolicyAndCompanyAsync(long policyNumber, int cossuredCompanyCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets all cossured policies for a reference period.
    /// Used for PREMCED report generation.
    /// </summary>
    IAsyncEnumerable<CossuredPolicy> GetForPeriodAsync(int referenceYear, int referenceMonth, CancellationToken cancellationToken = default);
}
