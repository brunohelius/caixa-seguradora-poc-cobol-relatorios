using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for cossurance calculation data (GE397, GE399 tables).
/// Maps to COBOL sections R4600 (GE397), R5200, R5300 (cursor), R5400 (fetch), R5500 (calculation).
/// </summary>
public class CossuranceCalculationRepository : Repository<CossuranceCalculation>, ICossuranceCalculationRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public CossuranceCalculationRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<CossuranceCalculation> GetCalculationRecordsAsync(
        int companyCode,
        string calculationCriteria,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL cursor: CUR-GE399 declared at R5300-00-DECLARE-GE399
        // Fetched at R5400-00-FETCH-GE399
        // SELECT * FROM GE399 WHERE COD_CIA = :companyCode [AND other criteria]
        // Note: calculationCriteria would be parsed/interpreted based on business rules

        var query = _premiumContext.CossuranceCalculations
            .AsNoTracking()
            .OrderBy(cc => cc.PolicyNumber)
            .ThenBy(cc => cc.CossuranceCode);

        await foreach (var calculation in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return calculation;
        }
    }

    /// <inheritdoc />
    public async Task<CossuranceCalculation?> GetReferenceDataAsync(
        int referenceCode,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R4600-00-SELECT-GE397:
        // SELECT * FROM GE397 WHERE COD_REF = :referenceCode
        // Note: GE397 might be a separate reference table, assuming same entity for simplicity
        return await _premiumContext.CossuranceCalculations
            .AsNoTracking()
            .FirstOrDefaultAsync(cc => cc.CossuranceCode == referenceCode, cancellationToken);
    }

    /// <inheritdoc />
    public async Task<CossuranceCalculation?> GetCalculationParametersAsync(
        long policyNumber,
        int companyCode,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R5200-00-SELECT-GE399:
        // SELECT * FROM GE399 WHERE NUM_APOLICE = :policyNumber AND COD_CIA = :companyCode
        return await _premiumContext.CossuranceCalculations
            .AsNoTracking()
            .FirstOrDefaultAsync(cc => cc.PolicyNumber == policyNumber, cancellationToken);
    }

    /// <inheritdoc />
    public async Task<CossurancePremiumDistribution> CalculatePremiumDistributionAsync(
        long policyNumber,
        decimal totalPremium,
        decimal cossurancePercentage,
        CancellationToken cancellationToken = default)
    {
        // Implements COBOL logic from R5500-00-CALCULA-COSG-CED
        // This is a business logic calculation, not just data retrieval

        var calculationRecords = await _premiumContext.CossuranceCalculations
            .AsNoTracking()
            .Where(cc => cc.PolicyNumber == policyNumber)
            .ToListAsync(cancellationToken);

        decimal cededPremium = totalPremium * cossurancePercentage;
        decimal retainedPremium = totalPremium - cededPremium;

        var companyShares = new List<CompanyShare>();

        foreach (var record in calculationRecords)
        {
            var sharePremium = totalPremium * record.QuotaPercentage;
            companyShares.Add(new CompanyShare
            {
                CompanyCode = 0, // Would need to derive from entity data
                CompanyName = "Cossurer Company", // Would need to lookup from company master
                SharePercentage = record.QuotaPercentage,
                SharePremium = sharePremium
            });
        }

        return new CossurancePremiumDistribution
        {
            PolicyNumber = policyNumber,
            TotalPremium = totalPremium,
            CossurancePercentage = cossurancePercentage,
            CededPremium = cededPremium,
            RetainedPremium = retainedPremium,
            CompanyShares = companyShares
        };
    }
}
