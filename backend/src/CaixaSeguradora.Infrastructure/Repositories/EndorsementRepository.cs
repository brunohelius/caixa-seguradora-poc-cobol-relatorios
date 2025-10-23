using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for endorsement data (V0ENDOSSO view).
/// Maps to COBOL sections R0760-00-SELECT-V0ENDOSSO, R0780-00-SELECT-ENDOS-CANCLM, R0840-00-SELECT-ENDS-CANCELM, R0820-00-SELECT-QTD-DOCT-CANC.
/// </summary>
public class EndorsementRepository : Repository<Endorsement>, IEndorsementRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public EndorsementRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async Task<Endorsement?> GetByPolicyAndNumberAsync(
        long policyNumber,
        int endorsementNumber,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL: SELECT * FROM V0ENDOSSO WHERE NUM_APOLICE = :policyNumber AND NUM_ENDOS = :endorsementNumber
        return await _premiumContext.Endorsements
            .AsNoTracking()
            .FirstOrDefaultAsync(e => e.PolicyNumber == policyNumber && e.EndorsementNumber == endorsementNumber, cancellationToken);
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Endorsement> GetByPolicyNumberAsync(
        long policyNumber,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL: SELECT * FROM V0ENDOSSO WHERE NUM_APOLICE = :policyNumber ORDER BY NUM_ENDOS
        IOrderedQueryable<Endorsement> query = _premiumContext.Endorsements
            .AsNoTracking()
            .Where(e => e.PolicyNumber == policyNumber)
            .OrderBy(e => e.EndorsementNumber);

        await foreach (Endorsement? endorsement in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return endorsement;
        }
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Endorsement> GetCancelledEndorsementsAsync(
        long policyNumber,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL sections R0780, R0840:
        // SELECT * FROM V0ENDOSSO WHERE NUM_APOLICE = :policyNumber AND IND_CANCELM = 'S'
        IOrderedQueryable<Endorsement> query = _premiumContext.Endorsements
            .AsNoTracking()
            .Where(e => e.PolicyNumber == policyNumber && e.CancellationFlag == "S")
            .OrderBy(e => e.EndorsementNumber);

        await foreach (Endorsement? endorsement in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return endorsement;
        }
    }

    /// <inheritdoc />
    public async Task<int> GetCancelledDocumentCountAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R0820-00-SELECT-QTD-DOCT-CANC:
        // SELECT COUNT(*) FROM V0ENDOSSO WHERE NUM_APOLICE = :policyNumber AND IND_CANCELM = 'S'
        return await _premiumContext.Endorsements
            .AsNoTracking()
            .Where(e => e.PolicyNumber == policyNumber && e.CancellationFlag == "S")
            .CountAsync(cancellationToken);
    }
}
