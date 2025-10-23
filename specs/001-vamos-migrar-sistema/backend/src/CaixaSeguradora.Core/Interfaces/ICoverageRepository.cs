using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for policy coverage data (V0COBERAPOL, HTCTPBVA, COBPRPVA, HSTCOBPROP views).
/// Accessed at COBOL sections R0850, R1040, R1080, R1090, R1100, R1250.
/// </summary>
public interface ICoverageRepository : IRepository<Coverage>
{
    /// <summary>
    /// Gets all coverages for a specific policy.
    /// Maps to COBOL sections R0850-00-SELECT-V0COBERAPOL, R1250-00-SELECT-V0COBERAPOL.
    /// </summary>
    IAsyncEnumerable<Coverage> GetByPolicyNumberAsync(long policyNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets historical coverage type data.
    /// Maps to COBOL section R1040-00-SELECT-HTCTPBVA.
    /// </summary>
    Task<Coverage?> GetHistoricalCoverageAsync(int coverageCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets proposal coverage information.
    /// Maps to COBOL section R1080-00-SELECT-COBPRPVA.
    /// </summary>
    IAsyncEnumerable<Coverage> GetProposalCoverageAsync(long proposalNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets number of insured lives for life insurance policies.
    /// Maps to COBOL section R1100-00-SELECT-QTDE-VIDAS.
    /// </summary>
    Task<int> GetInsuredLivesCountAsync(long policyNumber, CancellationToken cancellationToken = default);
}
