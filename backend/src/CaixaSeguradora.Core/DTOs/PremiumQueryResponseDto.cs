namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for premium query results with pagination metadata.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
public class PremiumQueryResponseDto
{
    /// <summary>
    /// List of premium records matching the query filters.
    /// </summary>
    public List<PremiumRecordDto> Records { get; set; } = new();

    /// <summary>
    /// Pagination metadata.
    /// </summary>
    public PaginationMetadata Pagination { get; set; } = new();

    /// <summary>
    /// Aggregated statistics for the current result set.
    /// </summary>
    public PremiumStatisticsDto? Statistics { get; set; }
}

/// <summary>
/// Simplified premium record DTO for query results display.
/// Contains essential fields for table/chart visualization.
/// </summary>
public class PremiumRecordDto
{
    /// <summary>
    /// Premium record identifier.
    /// </summary>
    public long PremiumId { get; set; }

    /// <summary>
    /// Policy number.
    /// </summary>
    public long PolicyNumber { get; set; }

    /// <summary>
    /// Reference date (combined from year, month, day).
    /// </summary>
    public DateTime ReferenceDate { get; set; }

    /// <summary>
    /// Movement type code.
    /// E = Emissão, C = Cancelamento, R = Reembolso, etc.
    /// </summary>
    public string MovementType { get; set; } = string.Empty;

    /// <summary>
    /// Product code.
    /// </summary>
    public int ProductCode { get; set; }

    /// <summary>
    /// Product description (from navigation property).
    /// </summary>
    public string? ProductDescription { get; set; }

    /// <summary>
    /// Line of business code (ramo).
    /// </summary>
    public int LineOfBusinessCode { get; set; }

    /// <summary>
    /// Line of business description.
    /// </summary>
    public string? LineOfBusinessDescription { get; set; }

    /// <summary>
    /// Company code.
    /// </summary>
    public int CompanyCode { get; set; }

    /// <summary>
    /// Agency code.
    /// </summary>
    public int AgencyCode { get; set; }

    /// <summary>
    /// Producer (corretor) code.
    /// </summary>
    public int ProducerCode { get; set; }

    /// <summary>
    /// Base premium amount (item level).
    /// </summary>
    public decimal BasePremium { get; set; }

    /// <summary>
    /// Tariff premium amount (item level).
    /// </summary>
    public decimal TariffPremium { get; set; }

    /// <summary>
    /// Net premium amount (calculated or item level).
    /// </summary>
    public decimal NetPremium { get; set; }

    /// <summary>
    /// Insured amount (item level).
    /// </summary>
    public decimal InsuredAmount { get; set; }

    /// <summary>
    /// Discount amount (item level).
    /// </summary>
    public decimal Discount { get; set; }

    /// <summary>
    /// IOF tax amount.
    /// </summary>
    public decimal IofTax { get; set; }

    /// <summary>
    /// Commission amount.
    /// </summary>
    public decimal Commission { get; set; }

    /// <summary>
    /// Endorsement number (if applicable).
    /// </summary>
    public long? EndorsementNumber { get; set; }

    /// <summary>
    /// Installment number.
    /// </summary>
    public int? InstallmentNumber { get; set; }

    /// <summary>
    /// Client code.
    /// </summary>
    public long? ClientCode { get; set; }

    /// <summary>
    /// Client name (from navigation property).
    /// </summary>
    public string? ClientName { get; set; }
}

/// <summary>
/// Pagination metadata for query responses.
/// </summary>
public class PaginationMetadata
{
    /// <summary>
    /// Current page number (1-based).
    /// </summary>
    public int CurrentPage { get; set; }

    /// <summary>
    /// Number of records per page.
    /// </summary>
    public int PageSize { get; set; }

    /// <summary>
    /// Total number of records matching the query (across all pages).
    /// </summary>
    public long TotalRecords { get; set; }

    /// <summary>
    /// Total number of pages.
    /// </summary>
    public int TotalPages { get; set; }

    /// <summary>
    /// Whether there is a previous page.
    /// </summary>
    public bool HasPreviousPage { get; set; }

    /// <summary>
    /// Whether there is a next page.
    /// </summary>
    public bool HasNextPage { get; set; }

    /// <summary>
    /// First record number on current page (1-based).
    /// </summary>
    public long FirstRecordNumber { get; set; }

    /// <summary>
    /// Last record number on current page (1-based).
    /// </summary>
    public long LastRecordNumber { get; set; }
}

/// <summary>
/// Aggregated statistics for premium query results.
/// </summary>
public class PremiumStatisticsDto
{
    /// <summary>
    /// Total number of records in the result set.
    /// </summary>
    public long TotalRecords { get; set; }

    /// <summary>
    /// Total number of unique policies.
    /// </summary>
    public long TotalPolicies { get; set; }

    /// <summary>
    /// Sum of base premium amounts.
    /// </summary>
    public decimal TotalBasePremium { get; set; }

    /// <summary>
    /// Sum of tariff premium amounts.
    /// </summary>
    public decimal TotalTariffPremium { get; set; }

    /// <summary>
    /// Sum of net premium amounts.
    /// </summary>
    public decimal TotalNetPremium { get; set; }

    /// <summary>
    /// Sum of insured amounts.
    /// </summary>
    public decimal TotalInsuredAmount { get; set; }

    /// <summary>
    /// Sum of discount amounts.
    /// </summary>
    public decimal TotalDiscounts { get; set; }

    /// <summary>
    /// Sum of IOF tax amounts.
    /// </summary>
    public decimal TotalIofTax { get; set; }

    /// <summary>
    /// Sum of commission amounts.
    /// </summary>
    public decimal TotalCommission { get; set; }

    /// <summary>
    /// Average base premium per record.
    /// </summary>
    public decimal AverageBasePremium { get; set; }

    /// <summary>
    /// Average net premium per record.
    /// </summary>
    public decimal AverageNetPremium { get; set; }

    /// <summary>
    /// Breakdown by movement type.
    /// </summary>
    public Dictionary<string, MovementTypeStatistics> ByMovementType { get; set; } = new();

    /// <summary>
    /// Breakdown by product code.
    /// </summary>
    public Dictionary<int, ProductStatistics> ByProduct { get; set; } = new();

    /// <summary>
    /// Date range covered by the result set.
    /// </summary>
    public DateRangeDto DateRange { get; set; } = new();
}

/// <summary>
/// Date range information.
/// </summary>
public class DateRangeDto
{
    /// <summary>
    /// Earliest reference date in the result set.
    /// </summary>
    public DateTime? MinDate { get; set; }

    /// <summary>
    /// Latest reference date in the result set.
    /// </summary>
    public DateTime? MaxDate { get; set; }

    /// <summary>
    /// Number of days covered by the range.
    /// </summary>
    public int? DaysCovered { get; set; }
}
