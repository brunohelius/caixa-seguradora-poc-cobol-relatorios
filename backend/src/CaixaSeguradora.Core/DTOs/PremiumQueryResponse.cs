namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for premium data queries.
/// Contains paginated results and metadata.
/// </summary>
public class PremiumQueryResponse
{
    /// <summary>
    /// List of premium records matching the query.
    /// </summary>
    public List<PremiumRecordDto> Records { get; set; } = new();

    /// <summary>
    /// Total number of records matching the query (across all pages).
    /// </summary>
    public int TotalCount { get; set; }

    /// <summary>
    /// Current page number (1-based).
    /// </summary>
    public int PageNumber { get; set; }

    /// <summary>
    /// Alias for PageNumber (for compatibility).
    /// </summary>
    public int CurrentPage => PageNumber;

    /// <summary>
    /// Number of records per page.
    /// </summary>
    public int PageSize { get; set; }

    /// <summary>
    /// Total number of pages available.
    /// </summary>
    public int TotalPages { get; set; }

    /// <summary>
    /// Whether there is a next page.
    /// </summary>
    public bool HasNextPage { get; set; }

    /// <summary>
    /// Whether there is a previous page.
    /// </summary>
    public bool HasPreviousPage { get; set; }

    /// <summary>
    /// Summary statistics for the query results.
    /// </summary>
    public QueryStatistics? Statistics { get; set; }
}

/// <summary>
/// Summary statistics for query results.
/// </summary>
public class QueryStatistics
{
    /// <summary>
    /// Sum of all premium amounts.
    /// </summary>
    public decimal TotalPremiumAmount { get; set; }

    /// <summary>
    /// Average premium amount.
    /// </summary>
    public decimal AveragePremiumAmount { get; set; }

    /// <summary>
    /// Minimum premium amount.
    /// </summary>
    public decimal MinPremiumAmount { get; set; }

    /// <summary>
    /// Maximum premium amount.
    /// </summary>
    public decimal MaxPremiumAmount { get; set; }

    /// <summary>
    /// Number of unique policies.
    /// </summary>
    public int UniquePolicyCount { get; set; }

    /// <summary>
    /// Number of unique products.
    /// </summary>
    public int UniqueProductCount { get; set; }
}

