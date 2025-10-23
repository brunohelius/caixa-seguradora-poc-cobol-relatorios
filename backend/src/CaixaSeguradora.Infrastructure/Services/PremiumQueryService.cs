using System.Globalization;
using System.Text;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Implementation of premium query service for filtering, sorting, and analyzing premium data.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
public class PremiumQueryService : IPremiumQueryService
{
    private readonly IPremiumRepository _premiumRepository;
    private readonly IProductRepository _productRepository;
    private readonly IClientRepository _clientRepository;
    private readonly PremiumReportingDbContext _context;

    public PremiumQueryService(
        IPremiumRepository premiumRepository,
        IProductRepository productRepository,
        IClientRepository clientRepository,
        PremiumReportingDbContext context)
    {
        _premiumRepository = premiumRepository;
        _productRepository = productRepository;
        _clientRepository = clientRepository;
        _context = context;
    }

    /// <summary>
    /// Queries premium records with filtering, sorting, and pagination.
    /// </summary>
    public async Task<PremiumQueryResponseDto> QueryPremiumsAsync(
        PremiumQueryDto query,
        CancellationToken cancellationToken = default)
    {
        // Validate query parameters
        if (!query.IsValid(out List<string>? errors))
        {
            throw new ArgumentException($"Invalid query parameters: {string.Join(", ", errors)}");
        }

        // Get base queryable from repository
        IQueryable<PremiumRecord> queryable = GetFilteredQuery(query);

        // Get total count before pagination
        var totalRecords = await queryable.CountAsync(cancellationToken);

        // Apply sorting
        queryable = ApplySorting(queryable, query.SortBy, query.SortOrder);

        // Apply pagination
        var skip = (query.Page - 1) * query.PageSize;
        IQueryable<PremiumRecord> pagedQuery = queryable
            .Skip(skip)
            .Take(query.PageSize);

        // Execute query and map to DTOs
        List<PremiumRecordDto> records = await pagedQuery
            .Select(p => MapToPremiumRecordDto(p))
            .ToListAsync(cancellationToken);

        // Calculate pagination metadata
        var pagination = new PaginationMetadata
        {
            CurrentPage = query.Page,
            PageSize = query.PageSize,
            TotalRecords = totalRecords,
            TotalPages = (int)Math.Ceiling(totalRecords / (double)query.PageSize),
            HasPreviousPage = query.Page > 1,
            HasNextPage = query.Page < Math.Ceiling(totalRecords / (double)query.PageSize),
            FirstRecordNumber = totalRecords > 0 ? skip + 1 : 0,
            LastRecordNumber = Math.Min(skip + query.PageSize, totalRecords)
        };

        // Get statistics for the filtered data
        PremiumStatisticsDto statistics = await GetPremiumStatisticsAsync(query, cancellationToken);

        return new PremiumQueryResponseDto
        {
            Records = records,
            Pagination = pagination,
            Statistics = statistics
        };
    }

    /// <summary>
    /// Gets aggregated statistics for premium data based on filter criteria.
    /// </summary>
    public async Task<PremiumStatisticsDto> GetPremiumStatisticsAsync(
        PremiumQueryDto query,
        CancellationToken cancellationToken = default)
    {
        IQueryable<PremiumRecord> queryable = GetFilteredQuery(query);

        List<PremiumRecord> records = await queryable.ToListAsync(cancellationToken);

        if (records.Count == 0)
        {
            return new PremiumStatisticsDto();
        }

        // Calculate basic statistics
        var statistics = new PremiumStatisticsDto
        {
            TotalRecords = records.Count,
            TotalPolicies = records.Select(p => p.PolicyNumber).Distinct().Count(),
            TotalBasePremium = records.Sum(p => p.BasePremiumItem),
            TotalTariffPremium = records.Sum(p => p.TariffPremiumItem),
            TotalNetPremium = records.Sum(p => p.NetPremiumItem),
            TotalInsuredAmount = records.Sum(p => p.InsuredAmountItem),
            TotalDiscounts = records.Sum(p => p.DiscountItem),
            TotalIofTax = records.Sum(p => p.IofTaxValue),
            TotalCommission = records.Sum(p => p.CommissionValue),
            AverageBasePremium = records.Average(p => p.BasePremiumItem),
            AverageNetPremium = records.Average(p => p.NetPremiumItem)
        };

        // Breakdown by movement type
        IOrderedEnumerable<IGrouping<string, PremiumRecord>> movementGroups = records
            .GroupBy(p => p.MovementType)
            .OrderByDescending(g => g.Count());

        foreach (IGrouping<string, PremiumRecord>? group in movementGroups)
        {
            statistics.ByMovementType[group.Key] = new MovementTypeStatistics
            {
                MovementType = group.Key,
                RecordCount = group.Count(),
                TotalNetPremium = group.Sum(p => p.NetPremiumItem),
                PercentageOfTotal = Math.Round((group.Count() / (decimal)records.Count) * 100, 2)
            };
        }

        // Breakdown by product code
        IEnumerable<IGrouping<int, PremiumRecord>> productGroups = records
            .GroupBy(p => p.ProductCode)
            .OrderByDescending(g => g.Sum(p => p.NetPremiumItem))
            .Take(10); // Top 10 products

        foreach (IGrouping<int, PremiumRecord>? group in productGroups)
        {
            statistics.ByProduct[group.Key] = new ProductStatistics
            {
                ProductCode = group.Key,
                ProductDescription = null, // Will be populated by controller if needed
                RecordCount = group.Count(),
                TotalNetPremium = group.Sum(p => p.NetPremiumItem),
                PercentageOfTotal = Math.Round((group.Count() / (decimal)records.Count) * 100, 2)
            };
        }

        // Date range
        var dates = records
            .Select(p => new DateTime(p.ReferenceYear, p.ReferenceMonth, p.ReferenceDay))
            .ToList();

        if (dates.Any())
        {
            statistics.DateRange = new DateRangeDto
            {
                MinDate = dates.Min(),
                MaxDate = dates.Max(),
                DaysCovered = (int)(dates.Max() - dates.Min()).TotalDays + 1
            };
        }

        return statistics;
    }

    /// <summary>
    /// Gets a single premium record by ID with full details.
    /// </summary>
    public async Task<PremiumRecordDto?> GetPremiumByIdAsync(
        long premiumId,
        CancellationToken cancellationToken = default)
    {
        PremiumRecord? premium = await _premiumRepository.GetByIdAsync(premiumId, cancellationToken);

        if (premium == null)
        {
            return null;
        }

        return MapToPremiumRecordDto(premium);
    }

    /// <summary>
    /// Gets all premium records for a specific policy number.
    /// </summary>
    public async Task<List<PremiumRecordDto>> GetPremiumsByPolicyAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        IReadOnlyList<PremiumRecord> premiums = await _premiumRepository
            .FindAsync(p => p.PolicyNumber == policyNumber, cancellationToken);

        return premiums
            .Select(MapToPremiumRecordDto)
            .OrderByDescending(p => p.ReferenceDate)
            .ToList();
    }

    /// <summary>
    /// Exports premium query results to CSV format.
    /// </summary>
    public async Task<byte[]> ExportPremiumsToCsvAsync(
        PremiumQueryDto query,
        CancellationToken cancellationToken = default)
    {
        // Apply filters and sorting, but no pagination
        IQueryable<PremiumRecord> queryable = GetFilteredQuery(query);
        queryable = ApplySorting(queryable, query.SortBy, query.SortOrder);

        // Limit to reasonable export size (max 100,000 records)
        List<PremiumRecordDto> records = await queryable
            .Take(100000)
            .Select(p => MapToPremiumRecordDto(p))
            .ToListAsync(cancellationToken);

        // Generate CSV content
        var csv = new StringBuilder();

        // Header row
        csv.AppendLine("PremiumId,PolicyNumber,ReferenceDate,MovementType,ProductCode,ProductDescription," +
                      "LineOfBusinessCode,LineOfBusinessDescription,CompanyCode,AgencyCode,ProducerCode," +
                      "BasePremium,TariffPremium,NetPremium,InsuredAmount,Discount,IofTax,Commission," +
                      "EndorsementNumber,InstallmentNumber,ClientCode,ClientName");

        // Data rows
        foreach (PremiumRecordDto? record in records)
        {
            csv.AppendLine(string.Join(",",
                EscapeCsv(record.PremiumId.ToString()),
                EscapeCsv(record.PolicyNumber.ToString()),
                EscapeCsv(record.ReferenceDate.ToString("yyyy-MM-dd")),
                EscapeCsv(record.MovementType),
                EscapeCsv(record.ProductCode.ToString()),
                EscapeCsv(record.ProductDescription ?? ""),
                EscapeCsv(record.LineOfBusinessCode.ToString()),
                EscapeCsv(record.LineOfBusinessDescription ?? ""),
                EscapeCsv(record.CompanyCode.ToString()),
                EscapeCsv(record.AgencyCode.ToString()),
                EscapeCsv(record.ProducerCode.ToString()),
                EscapeCsv(record.BasePremium.ToString("F5", CultureInfo.InvariantCulture)),
                EscapeCsv(record.TariffPremium.ToString("F5", CultureInfo.InvariantCulture)),
                EscapeCsv(record.NetPremium.ToString("F5", CultureInfo.InvariantCulture)),
                EscapeCsv(record.InsuredAmount.ToString("F5", CultureInfo.InvariantCulture)),
                EscapeCsv(record.Discount.ToString("F5", CultureInfo.InvariantCulture)),
                EscapeCsv(record.IofTax.ToString("F5", CultureInfo.InvariantCulture)),
                EscapeCsv(record.Commission.ToString("F5", CultureInfo.InvariantCulture)),
                EscapeCsv(record.EndorsementNumber?.ToString() ?? ""),
                EscapeCsv(record.InstallmentNumber?.ToString() ?? ""),
                EscapeCsv(record.ClientCode?.ToString() ?? ""),
                EscapeCsv(record.ClientName ?? "")
            ));
        }

        return Encoding.UTF8.GetBytes(csv.ToString());
    }

    /// <summary>
    /// Gets available filter options for dropdowns and search inputs.
    /// </summary>
    public async Task<PremiumFilterOptionsDto> GetFilterOptionsAsync(
        CancellationToken cancellationToken = default)
    {
        // This would typically query the database for distinct values
        // For now, returning a basic structure - can be enhanced with actual data queries

        var options = new PremiumFilterOptionsDto
        {
            MovementTypes = new List<FilterOption>
            {
                new() { Value = "E", Label = "Emissão", Count = 0 },
                new() { Value = "C", Label = "Cancelamento", Count = 0 },
                new() { Value = "R", Label = "Reembolso", Count = 0 },
                new() { Value = "A", Label = "Alteração", Count = 0 },
                new() { Value = "S", Label = "Substituição", Count = 0 }
            },
            CompanyCodes = new List<FilterOption>(),
            LinesOfBusiness = new List<FilterOption>(),
            Products = new List<ProductFilterOption>()
        };

        return await Task.FromResult(options);
    }

    #region Private Helper Methods

    /// <summary>
    /// Applies query filters to the base queryable.
    /// </summary>
    private IQueryable<PremiumRecord> GetFilteredQuery(PremiumQueryDto query)
    {
        IQueryable<PremiumRecord> queryable = _context.PremiumRecords.AsNoTracking();

        // Apply filters
        if (query.PolicyNumber.HasValue)
        {
            queryable = queryable.Where(p => p.PolicyNumber == query.PolicyNumber.Value);
        }

        if (query.ProductCode.HasValue)
        {
            queryable = queryable.Where(p => p.ProductCode == query.ProductCode.Value);
        }

        if (query.LineOfBusiness.HasValue)
        {
            queryable = queryable.Where(p => p.LineOfBusinessCode == query.LineOfBusiness.Value);
        }

        if (!string.IsNullOrEmpty(query.MovementType))
        {
            queryable = queryable.Where(p => p.MovementType == query.MovementType);
        }

        if (query.CompanyCode.HasValue)
        {
            queryable = queryable.Where(p => p.CompanyCode == query.CompanyCode.Value);
        }

        if (query.AgencyCode.HasValue)
        {
            queryable = queryable.Where(p => p.AgencyCode == query.AgencyCode.Value);
        }

        if (query.ProducerCode.HasValue)
        {
            queryable = queryable.Where(p => p.ProducerCode == query.ProducerCode.Value);
        }

        // Date range filter
        if (query.StartDate.HasValue)
        {
            var startYear = query.StartDate.Value.Year;
            var startMonth = query.StartDate.Value.Month;
            var startDay = query.StartDate.Value.Day;

            queryable = queryable.Where(p =>
                p.ReferenceYear > startYear ||
                (p.ReferenceYear == startYear && p.ReferenceMonth > startMonth) ||
                (p.ReferenceYear == startYear && p.ReferenceMonth == startMonth && p.ReferenceDay >= startDay));
        }

        if (query.EndDate.HasValue)
        {
            var endYear = query.EndDate.Value.Year;
            var endMonth = query.EndDate.Value.Month;
            var endDay = query.EndDate.Value.Day;

            queryable = queryable.Where(p =>
                p.ReferenceYear < endYear ||
                (p.ReferenceYear == endYear && p.ReferenceMonth < endMonth) ||
                (p.ReferenceYear == endYear && p.ReferenceMonth == endMonth && p.ReferenceDay <= endDay));
        }

        // Premium amount range filter
        if (query.MinPremiumAmount.HasValue)
        {
            queryable = queryable.Where(p => p.BasePremiumItem >= query.MinPremiumAmount.Value);
        }

        if (query.MaxPremiumAmount.HasValue)
        {
            queryable = queryable.Where(p => p.BasePremiumItem <= query.MaxPremiumAmount.Value);
        }

        return queryable;
    }

    /// <summary>
    /// Applies sorting to the queryable based on sort field and order.
    /// </summary>
    private IQueryable<PremiumRecord> ApplySorting(
        IQueryable<PremiumRecord> queryable,
        string sortBy,
        string sortOrder)
    {
        var ascending = sortOrder.Equals("asc", StringComparison.OrdinalIgnoreCase);

        return sortBy.ToLowerInvariant() switch
        {
            "policynumber" => ascending
                ? queryable.OrderBy(p => p.PolicyNumber)
                : queryable.OrderByDescending(p => p.PolicyNumber),

            "referencedate" => ascending
                ? queryable.OrderBy(p => p.ReferenceYear).ThenBy(p => p.ReferenceMonth).ThenBy(p => p.ReferenceDay)
                : queryable.OrderByDescending(p => p.ReferenceYear).ThenByDescending(p => p.ReferenceMonth).ThenByDescending(p => p.ReferenceDay),

            "productcode" => ascending
                ? queryable.OrderBy(p => p.ProductCode)
                : queryable.OrderByDescending(p => p.ProductCode),

            "basepremium" => ascending
                ? queryable.OrderBy(p => p.BasePremiumItem)
                : queryable.OrderByDescending(p => p.BasePremiumItem),

            "tariffpremium" => ascending
                ? queryable.OrderBy(p => p.TariffPremiumItem)
                : queryable.OrderByDescending(p => p.TariffPremiumItem),

            "netpremium" => ascending
                ? queryable.OrderBy(p => p.NetPremiumItem)
                : queryable.OrderByDescending(p => p.NetPremiumItem),

            "movementtype" => ascending
                ? queryable.OrderBy(p => p.MovementType)
                : queryable.OrderByDescending(p => p.MovementType),

            _ => queryable.OrderByDescending(p => p.ReferenceYear)
                         .ThenByDescending(p => p.ReferenceMonth)
                         .ThenByDescending(p => p.ReferenceDay)
        };
    }

    /// <summary>
    /// Maps PremiumRecord entity to PremiumRecordDto.
    /// </summary>
    private static PremiumRecordDto MapToPremiumRecordDto(PremiumRecord premium)
    {
        return new PremiumRecordDto
        {
            PremiumId = premium.PremiumId,
            PolicyNumber = premium.PolicyNumber,
            ReferenceDate = new DateTime(premium.ReferenceYear, premium.ReferenceMonth, premium.ReferenceDay),
            MovementType = premium.MovementType,
            ProductCode = premium.ProductCode,
            ProductDescription = premium.Product?.ProductDescription, // Navigation property
            LineOfBusinessCode = premium.LineOfBusinessCode,
            LineOfBusinessDescription = null, // TODO: Add navigation property if needed
            CompanyCode = premium.CompanyCode,
            AgencyCode = premium.AgencyCode,
            ProducerCode = premium.ProducerCode,
            BasePremium = premium.BasePremiumItem,
            TariffPremium = premium.TariffPremiumItem,
            NetPremium = premium.NetPremiumItem,
            InsuredAmount = premium.InsuredAmountItem,
            Discount = premium.DiscountItem,
            IofTax = premium.IofTaxValue,
            Commission = premium.CommissionValue,
            EndorsementNumber = premium.EndorsementNumber == 0 ? null : premium.EndorsementNumber,
            InstallmentNumber = premium.InstallmentNumber == 0 ? null : premium.InstallmentNumber,
            ClientCode = premium.InsuredCode,
            ClientName = null // TODO: Add navigation property if needed
        };
    }

    /// <summary>
    /// Escapes a value for CSV format (handles commas, quotes, newlines).
    /// </summary>
    private static string EscapeCsv(string value)
    {
        if (string.IsNullOrEmpty(value))
        {
            return value;
        }

        if (value.Contains(',') || value.Contains('"') || value.Contains('\n') || value.Contains('\r'))
        {
            return $"\"{value.Replace("\"", "\"\"")}\"";
        }

        return value;
    }

    #endregion
}
