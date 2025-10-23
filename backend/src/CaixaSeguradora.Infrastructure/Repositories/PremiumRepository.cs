using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for premium emission records (V0PREMIOS view).
/// Implements COBOL cursor-style processing with IAsyncEnumerable for memory-efficient streaming.
/// Maps to COBOL sections R0500-R0600 for cursor operations.
/// </summary>
public class PremiumRepository : Repository<PremiumRecord>, IPremiumRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public PremiumRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<PremiumRecord> GetPremiumsForPeriodAsync(
        int companyCode,
        int referenceYear,
        int referenceMonth,
        string systemCode,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL cursor: CUR-V0PREMIOS
        // DECLARE CUR-V0PREMIOS CURSOR FOR
        //   SELECT ... FROM V0PREMIOS
        //   WHERE COD_EMP = :companyCode
        //     AND ANO_REFER = :referenceYear
        //     AND MES_REFER = :referenceMonth
        //   ORDER BY NUM_APOLICE, NUM_ENDOS

        var query = _premiumContext.PremiumRecords
            .AsNoTracking()
            .Where(p => p.CompanyCode == companyCode
                     && p.ReferenceYear == referenceYear
                     && p.ReferenceMonth == referenceMonth)
            .OrderBy(p => p.PolicyNumber)
            .ThenBy(p => p.EndorsementNumber);

        // Stream results using COBOL FETCH pattern
        await foreach (var premium in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return premium;
        }
    }

    /// <inheritdoc />
    public async Task<List<PremiumRecord>> GetByPolicyAndEndorsementAsync(
        long policyNumber,
        int endorsementNumber,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL SELECT with WHERE clause on policy + endorsement
        return await _premiumContext.PremiumRecords
            .AsNoTracking()
            .Where(p => p.PolicyNumber == policyNumber && p.EndorsementNumber == endorsementNumber)
            .OrderBy(p => p.InstallmentNumber)
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public async Task<PremiumSummaryDto> GetPeriodSummaryAsync(
        int referenceYear,
        int referenceMonth,
        CancellationToken cancellationToken = default)
    {
        // Aggregate statistics for dashboard and validation
        var premiums = await _premiumContext.PremiumRecords
            .AsNoTracking()
            .Where(p => p.ReferenceYear == referenceYear && p.ReferenceMonth == referenceMonth)
            .ToListAsync(cancellationToken);

        if (!premiums.Any())
        {
            return new PremiumSummaryDto
            {
                TotalRecords = 0,
                TotalPremiumAmount = 0,
                TotalNetPremium = 0,
                UniquePolichyCount = 0,
                MovementTypeCounts = new Dictionary<string, int>()
            };
        }

        return new PremiumSummaryDto
        {
            TotalRecords = premiums.Count,
            TotalPremiumAmount = premiums.Sum(p => p.TotalPremiumTotal),
            TotalNetPremium = premiums.Sum(p => p.NetPremiumTotal),
            UniquePolichyCount = premiums.Select(p => p.PolicyNumber).Distinct().Count(),
            MovementTypeCounts = premiums
                .GroupBy(p => p.MovementType)
                .ToDictionary(g => g.Key, g => g.Count())
        };
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<PremiumRecord> GetPremiumsForReportAsync(
        DateTime startDate,
        DateTime endDate,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Stream premium records for a date range
        // This is used for report generation that spans multiple periods
        // PolicyStartDate is in format "yyyy-MM-dd" so we can use string comparison
        var startDateStr = startDate.ToString("yyyy-MM-dd");
        var endDateStr = endDate.ToString("yyyy-MM-dd");

        var query = _premiumContext.PremiumRecords
            .AsNoTracking()
            .Where(p => string.Compare(p.PolicyStartDate, startDateStr) >= 0
                     && string.Compare(p.PolicyStartDate, endDateStr) <= 0)
            .OrderBy(p => p.PolicyNumber)
            .ThenBy(p => p.PolicyStartDate);

        await foreach (var record in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return record;
        }
    }
}
