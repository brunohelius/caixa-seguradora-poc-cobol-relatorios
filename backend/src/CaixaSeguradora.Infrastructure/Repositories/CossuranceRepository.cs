using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for cossurance data (V0APOLCOSCED view).
/// Maps to COBOL sections R5000-R5500 with cursor pattern for PREMCED file generation.
/// </summary>
public class CossuranceRepository : Repository<CossuredPolicy>, ICossuranceRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public CossuranceRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<CossuredPolicy> GetCossuranceByPolicyAsync(
        long policyNumber,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL cursor: CUR-V0APOLCOSCED declared at R5000-00-DECLARE-V0APOLCOSCED
        // DECLARE CUR-V0APOLCOSCED CURSOR FOR
        //   SELECT * FROM V0APOLCOSCED
        //   WHERE NUM_APOLICE = :policyNumber
        //     AND IND_STATUS = 'A'
        //   ORDER BY COD_COSSG
        IOrderedQueryable<CossuredPolicy> query = _premiumContext.CossuredPolicies
            .AsNoTracking()
            .Where(c => c.PolicyNumber == policyNumber && c.Status == "A")
            .OrderBy(c => c.CossuranceCode);

        await foreach (CossuredPolicy? cossurance in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return cossurance;
        }
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<CossuredPolicy> GetCossuranceForPeriodAsync(
        DateTime startDate,
        DateTime endDate,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Stream all active cossurance records for policies in the period
        // Join with policies to filter by date range
        var query = from cossurance in _premiumContext.CossuredPolicies.AsNoTracking()
                    join policy in _premiumContext.Policies.AsNoTracking()
                        on cossurance.PolicyNumber equals policy.PolicyNumber
                    where cossurance.Status == "A"
                       && policy.PolicyStartDate != null
                       && DateTime.Parse(policy.PolicyStartDate) >= startDate
                       && DateTime.Parse(policy.PolicyStartDate) <= endDate
                    orderby cossurance.PolicyNumber, cossurance.CossuranceCode
                    select cossurance;

        await foreach (CossuredPolicy? cossurance in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return cossurance;
        }
    }

    /// <inheritdoc />
    public async Task<List<CossuredPolicy>> GetByPolicyNumberAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        // Fetch all cossurance records for a policy (for validation/summary)
        return await _premiumContext.CossuredPolicies
            .AsNoTracking()
            .Where(c => c.PolicyNumber == policyNumber && c.Status == "A")
            .OrderBy(c => c.CossuranceCode)
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public async Task<bool> HasCossuranceAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        // Check if policy has any active cossurance arrangements
        return await _premiumContext.CossuredPolicies
            .AsNoTracking()
            .AnyAsync(c => c.PolicyNumber == policyNumber && c.Status == "A", cancellationToken);
    }
}
