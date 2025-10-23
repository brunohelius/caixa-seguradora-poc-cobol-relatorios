using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for endorsement data (V0ENDOSSO view).
/// Accessed at COBOL sections R0760-00-SELECT-V0ENDOSSO, R0780-00-SELECT-ENDOS-CANCLM, R0840-00-SELECT-ENDS-CANCELM.
/// </summary>
public interface IEndorsementRepository : IRepository<Endorsement>
{
    /// <summary>
    /// Gets endorsement by policy and endorsement number.
    /// </summary>
    Task<Endorsement?> GetByPolicyAndNumberAsync(long policyNumber, int endorsementNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets all endorsements for a policy.
    /// </summary>
    IAsyncEnumerable<Endorsement> GetByPolicyNumberAsync(long policyNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets cancelled endorsements for a policy.
    /// Maps to COBOL sections R0780, R0840.
    /// </summary>
    IAsyncEnumerable<Endorsement> GetCancelledEndorsementsAsync(long policyNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets count of cancelled documents for a policy.
    /// Maps to COBOL section R0820-00-SELECT-QTD-DOCT-CANC.
    /// </summary>
    Task<int> GetCancelledDocumentCountAsync(long policyNumber, CancellationToken cancellationToken = default);
}
