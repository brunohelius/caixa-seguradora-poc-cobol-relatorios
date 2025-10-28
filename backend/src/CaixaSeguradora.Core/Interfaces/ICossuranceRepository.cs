using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for cossurance data (V0APOLCOSCED view).
/// Accessed at COBOL sections R5000-R5500 with cursor pattern for PREMCED file generation.
/// </summary>
public interface ICossuranceRepository : IRepository<CossuredPolicy>
{
    /// <summary>
    /// Streams cossurance records for a policy using cursor pattern.
    /// Maps to COBOL cursor: CUR-V0APOLCOSCED declared at R5000-00-DECLARE-V0APOLCOSCED.
    /// Used for generating PREMCED.TXT output file.
    /// </summary>
    /// <param name="policyNumber">Policy number to fetch cossurance for</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Async enumerable stream of cossurance records</returns>
    IAsyncEnumerable<CossuredPolicy> GetCossuranceByPolicyAsync(
        long policyNumber,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Streams all active cossurance records for a date range.
    /// Used for bulk report generation across multiple policies.
    /// </summary>
    /// <param name="startDate">Start date</param>
    /// <param name="endDate">End date</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Async enumerable stream of cossurance records</returns>
    IAsyncEnumerable<CossuredPolicy> GetCossuranceForPeriodAsync(
        DateTime startDate,
        DateTime endDate,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets cossurance records by policy number for validation/summary.
    /// </summary>
    /// <param name="policyNumber">Policy number</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of cossurance records</returns>
    Task<List<CossuredPolicy>> GetByPolicyNumberAsync(
        long policyNumber,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Checks if a policy has active cossurance arrangements.
    /// </summary>
    /// <param name="policyNumber">Policy number to check</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>True if policy has cossurance</returns>
    Task<bool> HasCossuranceAsync(
        long policyNumber,
        CancellationToken cancellationToken = default);
}
