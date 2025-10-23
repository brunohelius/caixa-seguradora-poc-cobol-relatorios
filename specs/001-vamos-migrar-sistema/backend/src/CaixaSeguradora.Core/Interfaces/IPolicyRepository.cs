using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for policy master data (V0APOLICE view).
/// Accessed at COBOL sections R0980-00-SELECT-V0APOLICE, R0990-00-SELECT-EF-APOLICE.
/// </summary>
public interface IPolicyRepository : IRepository<Policy>
{
    /// <summary>
    /// Gets policy by policy number.
    /// </summary>
    Task<Policy?> GetByPolicyNumberAsync(long policyNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets policies for a specific client.
    /// </summary>
    IAsyncEnumerable<Policy> GetByClientCodeAsync(int clientCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets policy effective dates and status information.
    /// Maps to COBOL section R0720-00-SELECT-DTINIVIG-AP.
    /// </summary>
    Task<PolicyEffectiveData?> GetEffectiveDatesAsync(long policyNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets policies by product code for reporting.
    /// </summary>
    IAsyncEnumerable<Policy> GetByProductCodeAsync(int productCode, CancellationToken cancellationToken = default);
}

/// <summary>
/// Policy effective date and status data.
/// </summary>
public class PolicyEffectiveData
{
    public long PolicyNumber { get; set; }
    public DateTime EffectiveDate { get; set; }
    public DateTime? ExpirationDate { get; set; }
    public string PolicyStatus { get; set; } = string.Empty;
    public DateTime? CancellationDate { get; set; }
}
