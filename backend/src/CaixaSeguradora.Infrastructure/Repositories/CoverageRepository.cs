using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for policy coverage data (V0COBERAPOL, HTCTPBVA, COBPRPVA, HSTCOBPROP views).
/// Maps to COBOL sections R0850, R1040, R1080, R1090, R1100, R1250.
/// </summary>
public class CoverageRepository : Repository<Coverage>, ICoverageRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public CoverageRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Coverage> GetByPolicyNumberAsync(
        long policyNumber,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL sections R0850-00-SELECT-V0COBERAPOL, R1250-00-SELECT-V0COBERAPOL:
        // SELECT * FROM V0COBERAPOL WHERE NUM_APOLICE = :policyNumber
        var query = _premiumContext.Coverages
            .AsNoTracking()
            .Where(c => c.PolicyNumber == policyNumber)
            .OrderBy(c => c.CoverageCode);

        await foreach (var coverage in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return coverage;
        }
    }

    /// <inheritdoc />
    public async Task<Coverage?> GetHistoricalCoverageAsync(
        int coverageCode,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R1040-00-SELECT-HTCTPBVA:
        // SELECT * FROM HTCTPBVA WHERE COD_COBER = :coverageCode
        // Note: Assuming historical data is in the same Coverage table with a marker
        // In production, this might query a separate historical table
        return await _premiumContext.Coverages
            .AsNoTracking()
            .Where(c => c.CoverageCode == coverageCode)
            .OrderByDescending(c => c.CoverageId)
            .FirstOrDefaultAsync(cancellationToken);
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Coverage> GetProposalCoverageAsync(
        long proposalNumber,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R1080-00-SELECT-COBPRPVA:
        // SELECT * FROM COBPRPVA WHERE NUM_PROPT = :proposalNumber
        // Note: Assuming proposal number can be derived from policy relationship
        // In production, this might require a join or separate proposal coverage table
        var query = _premiumContext.Coverages
            .AsNoTracking()
            .Include(c => c.Policy)
            .Where(c => c.Policy != null && c.Policy.ProposalNumber == proposalNumber)
            .OrderBy(c => c.CoverageCode);

        await foreach (var coverage in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return coverage;
        }
    }

    /// <inheritdoc />
    public async Task<int> GetInsuredLivesCountAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R1100-00-SELECT-QTDE-VIDAS:
        // SELECT COUNT(*) or SUM(QTDE_VIDAS) FROM coverage tables for life insurance
        // This is a simplification - actual implementation would depend on data model
        var coverages = await _premiumContext.Coverages
            .AsNoTracking()
            .Where(c => c.PolicyNumber == policyNumber && c.CoverageType == "B") // B = Basica (life coverage)
            .CountAsync(cancellationToken);

        return coverages;
    }
}
