using System.Globalization;
using System.Text;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Implementation of policy query service for filtering, sorting, and analyzing policy data.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
public class PolicyQueryService : IPolicyQueryService
{
    private readonly IPolicyRepository _policyRepository;
    private readonly PremiumReportingDbContext _context;

    public PolicyQueryService(
        IPolicyRepository policyRepository,
        PremiumReportingDbContext context)
    {
        _policyRepository = policyRepository;
        _context = context;
    }

    /// <summary>
    /// Queries policy records with filtering, sorting, and pagination.
    /// </summary>
    public async Task<PolicyQueryResponseDto> QueryPoliciesAsync(
        PolicyQueryDto query,
        CancellationToken cancellationToken = default)
    {
        // Validate query parameters
        if (!query.IsValid(out List<string>? errors))
        {
            throw new ArgumentException($"Invalid query parameters: {string.Join(", ", errors)}");
        }

        // Get filtered queryable
        IQueryable<Policy> queryable = GetFilteredQuery(query);

        // Get total count
        var totalRecords = await queryable.CountAsync(cancellationToken);

        // Apply sorting
        queryable = ApplySorting(queryable, query.SortBy, query.SortOrder);

        // Apply pagination
        var skip = (query.Page - 1) * query.PageSize;
        IQueryable<Policy> pagedQuery = queryable
            .Skip(skip)
            .Take(query.PageSize);

        // Execute query and map to DTOs
        List<PolicyRecordDto> records = await pagedQuery
            .Select(p => MapToPolicyRecordDto(p))
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

        // Get statistics
        PolicyStatisticsDto statistics = await GetPolicyStatisticsAsync(query, cancellationToken);

        return new PolicyQueryResponseDto
        {
            Records = records,
            Pagination = pagination,
            Statistics = statistics
        };
    }

    /// <summary>
    /// Gets a single policy record by policy number.
    /// </summary>
    public async Task<PolicyRecordDto?> GetPolicyByNumberAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        Policy? policy = await _policyRepository.GetByPolicyNumberAsync(policyNumber, cancellationToken);

        return policy != null ? MapToPolicyRecordDto(policy) : null;
    }

    /// <summary>
    /// Gets aggregated statistics for policy data.
    /// </summary>
    public async Task<PolicyStatisticsDto> GetPolicyStatisticsAsync(
        PolicyQueryDto query,
        CancellationToken cancellationToken = default)
    {
        IQueryable<Policy> queryable = GetFilteredQuery(query);
        List<Policy> records = await queryable.ToListAsync(cancellationToken);

        if (records.Count == 0)
        {
            return new PolicyStatisticsDto();
        }

        // Calculate active vs inactive
        var activeCount = records.Count(p => p.ExpirationDate >= DateTime.Now);

        var statistics = new PolicyStatisticsDto
        {
            TotalRecords = records.Count,
            ActivePolicies = activeCount,
            InactivePolicies = records.Count - activeCount,
            TotalInsuredCapital = 0, // InsuredCapital not available in Policy entity
            TotalPremiumAmount = records.Sum(p => p.TotalPremium),
            AverageInsuredCapital = 0, // InsuredCapital not available in Policy entity
            AveragePremium = records.Average(p => p.TotalPremium)
        };

        // Breakdown by product
        IEnumerable<IGrouping<int, Policy>> productGroups = records
            .GroupBy(p => p.ProductCode)
            .OrderByDescending(g => g.Sum(p => p.TotalPremium))
            .Take(10);

        foreach (IGrouping<int, Policy>? group in productGroups)
        {
            statistics.ByProduct[group.Key] = new PolicyProductStatistics
            {
                ProductCode = group.Key,
                PolicyCount = group.Count(),
                TotalInsuredCapital = 0,
                TotalPremium = group.Sum(p => p.TotalPremium),
                PercentageOfTotal = Math.Round((group.Count() / (decimal)records.Count) * 100, 2)
            };
        }

        // Breakdown by status
        IEnumerable<IGrouping<string, Policy>> statusGroups = records
            .GroupBy(p => p.PolicyStatus);

        foreach (IGrouping<string, Policy> group in statusGroups)
        {
            statistics.ByStatus[group.Key] = new PolicyStatusStatistics
            {
                PolicyStatus = group.Key,
                PolicyCount = group.Count(),
                PercentageOfTotal = Math.Round((group.Count() / (decimal)records.Count) * 100, 2)
            };
        }

        // Date range
        if (records.Any())
        {
            statistics.IssueDateRange = new DateRangeDto
            {
                MinDate = records.Min(p => p.EffectiveDate),
                MaxDate = records.Max(p => p.EffectiveDate),
                DaysCovered = (int)(records.Max(p => p.EffectiveDate) - records.Min(p => p.EffectiveDate)).TotalDays + 1
            };
        }

        return statistics;
    }

    /// <summary>
    /// Searches policies by client name or code.
    /// </summary>
    public async Task<List<PolicyRecordDto>> SearchPoliciesByClientAsync(
        string searchTerm,
        CancellationToken cancellationToken = default)
    {
        // Try to parse as client code
        if (long.TryParse(searchTerm, out var clientCode))
        {
            IReadOnlyList<Policy> policiesByCode = await _policyRepository
                .FindAsync(p => p.InsuredCode == clientCode, cancellationToken);

            return policiesByCode
                .Select(MapToPolicyRecordDto)
                .OrderByDescending(p => p.IssueDate)
                .Take(50)
                .ToList();
        }

        // Search by client name (would require join with Client entity in real implementation)
        // For now, return empty list - can be enhanced when client navigation property is available
        return new List<PolicyRecordDto>();
    }

    /// <summary>
    /// Exports policy query results to CSV.
    /// </summary>
    public async Task<byte[]> ExportPoliciesToCsvAsync(
        PolicyQueryDto query,
        CancellationToken cancellationToken = default)
    {
        IQueryable<Policy> queryable = GetFilteredQuery(query);
        queryable = ApplySorting(queryable, query.SortBy, query.SortOrder);

        // Limit to 50,000 records for export
        List<PolicyRecordDto> records = await queryable
            .Take(50000)
            .Select(p => MapToPolicyRecordDto(p))
            .ToListAsync(cancellationToken);

        var csv = new StringBuilder();

        // Header
        csv.AppendLine("PolicyNumber,ProductCode,LineOfBusiness,CompanyCode,AgencyCode,ProducerCode," +
                      "InsuredCode,InsuredName,IssueDate,StartValidityDate,EndValidityDate,PolicyStatus," +
                      "InsuredCapital,TotalPremium,NumberOfInstallments,IsActive");

        // Data rows
        foreach (PolicyRecordDto? record in records)
        {
            csv.AppendLine(string.Join(",",
                EscapeCsv(record.PolicyNumber.ToString()),
                EscapeCsv(record.ProductCode.ToString()),
                EscapeCsv(record.LineOfBusinessCode.ToString()),
                EscapeCsv(record.CompanyCode.ToString()),
                EscapeCsv(record.AgencyCode.ToString()),
                EscapeCsv(record.ProducerCode.ToString()),
                EscapeCsv(record.InsuredCode.ToString()),
                EscapeCsv(record.InsuredName ?? ""),
                EscapeCsv(record.IssueDate.ToString("yyyy-MM-dd")),
                EscapeCsv(record.StartValidityDate.ToString("yyyy-MM-dd")),
                EscapeCsv(record.EndValidityDate.ToString("yyyy-MM-dd")),
                EscapeCsv(record.PolicyStatus),
                EscapeCsv(record.InsuredCapital.ToString("F2", CultureInfo.InvariantCulture)),
                EscapeCsv(record.TotalPremium.ToString("F2", CultureInfo.InvariantCulture)),
                EscapeCsv(record.NumberOfInstallments?.ToString() ?? ""),
                EscapeCsv(record.IsActive.ToString())
            ));
        }

        return Encoding.UTF8.GetBytes(csv.ToString());
    }

    #region Private Helper Methods

    private IQueryable<Policy> GetFilteredQuery(PolicyQueryDto query)
    {
        IQueryable<Policy> queryable = _context.Policies.AsNoTracking();

        if (query.PolicyNumber.HasValue)
        {
            queryable = queryable.Where(p => p.PolicyNumber == query.PolicyNumber.Value);
        }

        if (!string.IsNullOrEmpty(query.PolicyNumberPattern))
        {
            var pattern = query.PolicyNumberPattern;
            queryable = queryable.Where(p => p.PolicyNumber.ToString().Contains(pattern));
        }

        if (query.ProductCode.HasValue)
        {
            queryable = queryable.Where(p => p.ProductCode == query.ProductCode.Value);
        }

        if (!string.IsNullOrEmpty(query.PolicyStatus))
        {
            queryable = queryable.Where(p => p.PolicyStatus == query.PolicyStatus);
        }

        if (query.AgencyCode.HasValue)
        {
            queryable = queryable.Where(p => p.AgencyCode == query.AgencyCode.Value);
        }

        if (query.ProducerCode.HasValue)
        {
            queryable = queryable.Where(p => p.ProducerCode == query.ProducerCode.Value);
        }

        if (query.ClientCode.HasValue)
        {
            queryable = queryable.Where(p => p.ClientCode == query.ClientCode.Value);
        }

        // Date filters
        if (query.IssueDateStart.HasValue)
        {
            queryable = queryable.Where(p => p.EffectiveDate >= query.IssueDateStart.Value);
        }

        if (query.IssueDateEnd.HasValue)
        {
            queryable = queryable.Where(p => p.EffectiveDate <= query.IssueDateEnd.Value);
        }

        if (query.ValidityDateStart.HasValue)
        {
            queryable = queryable.Where(p => p.EffectiveDate >= query.ValidityDateStart.Value);
        }

        if (query.ValidityDateEnd.HasValue)
        {
            queryable = queryable.Where(p => p.EffectiveDate <= query.ValidityDateEnd.Value);
        }

        // Active only filter
        if (query.ActiveOnly.HasValue && query.ActiveOnly.Value)
        {
            DateTime now = DateTime.Now;
            queryable = queryable.Where(p => p.ExpirationDate >= now);
        }

        return queryable;
    }

    private IQueryable<Policy> ApplySorting(
        IQueryable<Policy> queryable,
        string sortBy,
        string sortOrder)
    {
        var ascending = sortOrder.Equals("asc", StringComparison.OrdinalIgnoreCase);

        return sortBy.ToLowerInvariant() switch
        {
            "policynumber" => ascending
                ? queryable.OrderBy(p => p.PolicyNumber)
                : queryable.OrderByDescending(p => p.PolicyNumber),

            "issuedate" => ascending
                ? queryable.OrderBy(p => p.EffectiveDate)
                : queryable.OrderByDescending(p => p.EffectiveDate),

            "productcode" => ascending
                ? queryable.OrderBy(p => p.ProductCode)
                : queryable.OrderByDescending(p => p.ProductCode),

            "validitydate" => ascending
                ? queryable.OrderBy(p => p.EffectiveDate)
                : queryable.OrderByDescending(p => p.EffectiveDate),

            _ => queryable.OrderByDescending(p => p.EffectiveDate)
        };
    }

    private static PolicyRecordDto MapToPolicyRecordDto(Policy policy)
    {
        return new PolicyRecordDto
        {
            PolicyId = policy.PolicyNumber,
            PolicyNumber = policy.PolicyNumber,
            ProductCode = policy.ProductCode,
            ProductDescription = policy.Product?.ProductDescription,
            LineOfBusinessCode = 0, // Not available in Policy entity - needs to be added or queried from Product
            CompanyCode = 0, // Not available in Policy entity
            AgencyCode = policy.Agency?.AgencyCode ?? 0,
            ProducerCode = policy.Producer?.ProducerCode ?? 0,
            InsuredCode = policy.ClientCode,
            PolicyHolderCode = null,
            IssueDate = policy.EffectiveDate, // Using EffectiveDate as IssueDate
            StartValidityDate = policy.EffectiveDate,
            EndValidityDate = policy.ExpirationDate,
            PolicyStatus = policy.PolicyStatus,
            PolicyType = policy.SystemCode, // Using SystemCode as PolicyType placeholder
            ModalityCode = 0, // Not available in Policy entity
            InsuredCapital = 0, // Not available in Policy entity
            TotalPremium = policy.TotalPremium,
            NumberOfInstallments = null,
            InstallmentFrequency = null,
            IsActive = policy.ExpirationDate >= DateTime.Now,
            EndorsementCount = policy.Endorsements?.Count ?? 0,
            PremiumRecordCount = 0,
            LastModifiedDate = null,
            InsuredName = policy.Client?.ClientName,
            AgencyName = policy.Agency?.AgencyName,
            ProducerName = policy.Producer?.ProducerName
        };
    }

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
