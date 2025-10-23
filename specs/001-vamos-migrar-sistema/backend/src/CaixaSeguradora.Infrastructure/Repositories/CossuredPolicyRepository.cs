using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for cossured/ceded policy data (V0APOLCOSCED view).
/// Maps to COBOL sections R4800, R4900 (cursor declaration), R5000 (cursor fetch).
/// Critical for cossurance/reinsurance processing (PREMCED report).
/// </summary>
public class CossuredPolicyRepository : Repository<CossuredPolicy>, ICossuredPolicyRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public CossuredPolicyRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<CossuredPolicy> GetCossuredPoliciesAsync(
        long policyNumber,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL cursor: CUR-V0APOLCOSCED declared at R4900-00-DECLARE-V0APOLCOSCED
        // Fetched at R5000-00-FETCH-V0APOLCOSCED
        // SELECT * FROM V0APOLCOSCED WHERE NUM_APOLICE = :policyNumber ORDER BY COD_COSSG
        var query = _premiumContext.CossuredPolicies
            .AsNoTracking()
            .Where(cp => cp.PolicyNumber == policyNumber)
            .OrderBy(cp => cp.CossuranceCode);

        await foreach (var cossuredPolicy in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return cossuredPolicy;
        }
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<CossuredPolicy> GetByTypeAsync(
        string cossuranceType,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // SELECT * FROM V0APOLCOSCED WHERE TIP_COSSG = :cossuranceType
        var query = _premiumContext.CossuredPolicies
            .AsNoTracking()
            .Where(cp => cp.CossuranceType == cossuranceType)
            .OrderBy(cp => cp.PolicyNumber)
            .ThenBy(cp => cp.CossuranceCode);

        await foreach (var cossuredPolicy in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return cossuredPolicy;
        }
    }

    /// <inheritdoc />
    public async Task<CossuredPolicy?> GetByPolicyAndCompanyAsync(
        long policyNumber,
        int cossuredCompanyCode,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R4800-00-SELECT-V0APOLCOSCED:
        // SELECT * FROM V0APOLCOSCED WHERE NUM_APOLICE = :policyNumber AND COD_CIA_CESSIONARIA = :companyCode
        return await _premiumContext.CossuredPolicies
            .AsNoTracking()
            .FirstOrDefaultAsync(cp => cp.PolicyNumber == policyNumber
                                    && cp.AcquiringCompanyCode == cossuredCompanyCode,
                                    cancellationToken);
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<CossuredPolicy> GetForPeriodAsync(
        int referenceYear,
        int referenceMonth,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // For PREMCED report generation - get all cossured policies for the period
        // This requires joining with premium records or policies for date filtering
        // SELECT cp.* FROM V0APOLCOSCED cp
        // INNER JOIN V0APOLICE ap ON cp.NUM_APOLICE = ap.NUM_APOLICE
        // WHERE YEAR(ap.DT_INIVIG) = :referenceYear AND MONTH(ap.DT_INIVIG) = :referenceMonth

        var query = _premiumContext.CossuredPolicies
            .AsNoTracking()
            .Include(cp => cp.Policy)
            .Where(cp => cp.Policy != null)
            .OrderBy(cp => cp.PolicyNumber)
            .ThenBy(cp => cp.CossuranceCode);

        await foreach (var cossuredPolicy in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            // Filter by parsing the policy effective date
            if (cossuredPolicy.Policy != null)
            {
                var effectiveDate = DateTime.ParseExact(cossuredPolicy.Policy.EffectiveDate, "yyyy-MM-dd", null);
                if (effectiveDate.Year == referenceYear && effectiveDate.Month == referenceMonth)
                {
                    yield return cossuredPolicy;
                }
            }
        }
    }
}
