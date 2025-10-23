using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for policy master data (V0APOLICE view).
/// Maps to COBOL sections R0980-00-SELECT-V0APOLICE, R0990-00-SELECT-EF-APOLICE, R0720-00-SELECT-DTINIVIG-AP.
/// </summary>
public class PolicyRepository : Repository<Policy>, IPolicyRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public PolicyRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async Task<Policy?> GetByPolicyNumberAsync(long policyNumber, CancellationToken cancellationToken = default)
    {
        // Maps to COBOL: SELECT * FROM V0APOLICE WHERE NUM_APOLICE = :policyNumber
        return await _premiumContext.Policies
            .AsNoTracking()
            .FirstOrDefaultAsync(p => p.PolicyNumber == policyNumber, cancellationToken);
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Policy> GetByClientCodeAsync(
        int clientCode,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL: SELECT * FROM V0APOLICE WHERE COD_CLIEN = :clientCode
        var query = _premiumContext.Policies
            .AsNoTracking()
            .Where(p => p.ClientCode == clientCode)
            .OrderBy(p => p.PolicyNumber);

        await foreach (var policy in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return policy;
        }
    }

    /// <inheritdoc />
    public async Task<PolicyEffectiveData?> GetEffectiveDatesAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R0720-00-SELECT-DTINIVIG-AP
        // SELECT DT_INIVIG, DT_FIMVIG, IND_SITUACAO FROM V0APOLICE
        // WHERE NUM_APOLICE = :policyNumber

        var policy = await _premiumContext.Policies
            .AsNoTracking()
            .Where(p => p.PolicyNumber == policyNumber)
            .Select(p => new
            {
                p.PolicyNumber,
                p.EffectiveDate,
                p.ExpirationDate,
                p.PolicyStatus
            })
            .FirstOrDefaultAsync(cancellationToken);

        if (policy == null)
        {
            return null;
        }

        return new PolicyEffectiveData
        {
            PolicyNumber = policy.PolicyNumber,
            EffectiveDate = policy.EffectiveDate,
            ExpirationDate = policy.ExpirationDate,
            PolicyStatus = policy.PolicyStatus,
            CancellationDate = policy.PolicyStatus == "C"
                ? policy.ExpirationDate
                : null
        };
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Policy> GetByProductCodeAsync(
        int productCode,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL: SELECT * FROM V0APOLICE WHERE COD_PROD = :productCode
        var query = _premiumContext.Policies
            .AsNoTracking()
            .Where(p => p.ProductCode == productCode)
            .OrderBy(p => p.PolicyNumber);

        await foreach (var policy in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return policy;
        }
    }
}
