namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Request DTO for querying premium records with filtering, sorting, and pagination.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
public class PremiumQueryDto
{
    /// <summary>
    /// Filter by policy number (exact match).
    /// Maps to PremiumRecord.PolicyNumber.
    /// </summary>
    public long? PolicyNumber { get; set; }

    /// <summary>
    /// Filter by date range - start date (inclusive).
    /// Applied to reference date (ReferenceYear, ReferenceMonth, ReferenceDay).
    /// Format: YYYY-MM-DD
    /// </summary>
    public DateTime? StartDate { get; set; }

    /// <summary>
    /// Filter by date range - end date (inclusive).
    /// Applied to reference date (ReferenceYear, ReferenceMonth, ReferenceDay).
    /// Format: YYYY-MM-DD
    /// </summary>
    public DateTime? EndDate { get; set; }

    /// <summary>
    /// Filter by product code (exact match).
    /// Maps to PremiumRecord.ProductCode.
    /// </summary>
    public int? ProductCode { get; set; }

    /// <summary>
    /// Filter by line of business (ramo) code.
    /// Maps to PremiumRecord.LineOfBusinessCode.
    /// </summary>
    public int? LineOfBusiness { get; set; }

    /// <summary>
    /// Filter by movement type (tipo de movimento).
    /// Valid values: 'E' (Emissão), 'C' (Cancelamento), 'R' (Reembolso), etc.
    /// Maps to PremiumRecord.MovementType.
    /// </summary>
    public string? MovementType { get; set; }

    /// <summary>
    /// Filter by company code.
    /// Maps to PremiumRecord.CompanyCode.
    /// </summary>
    public int? CompanyCode { get; set; }

    /// <summary>
    /// Filter by agency code.
    /// Maps to PremiumRecord.AgencyCode.
    /// </summary>
    public int? AgencyCode { get; set; }

    /// <summary>
    /// Filter by producer code (corretor).
    /// Maps to PremiumRecord.ProducerCode.
    /// </summary>
    public int? ProducerCode { get; set; }

    /// <summary>
    /// Minimum premium amount filter (base premium).
    /// Maps to PremiumRecord.BasePremiumItem.
    /// </summary>
    public decimal? MinPremiumAmount { get; set; }

    /// <summary>
    /// Maximum premium amount filter (base premium).
    /// Maps to PremiumRecord.BasePremiumItem.
    /// </summary>
    public decimal? MaxPremiumAmount { get; set; }

    /// <summary>
    /// Current page number for pagination (1-based).
    /// Default: 1
    /// </summary>
    public int Page { get; set; } = 1;

    /// <summary>
    /// Number of records per page.
    /// Default: 50, Max: 1000 (for performance)
    /// </summary>
    public int PageSize { get; set; } = 50;

    /// <summary>
    /// Field to sort results by.
    /// Valid values: "PolicyNumber", "ReferenceDate", "ProductCode", "BasePremium", "TariffPremium", "NetPremium"
    /// Default: "ReferenceDate"
    /// </summary>
    public string SortBy { get; set; } = "ReferenceDate";

    /// <summary>
    /// Sort order direction.
    /// Valid values: "asc" (ascending), "desc" (descending)
    /// Default: "desc" (most recent first)
    /// </summary>
    public string SortOrder { get; set; } = "desc";

    /// <summary>
    /// Validates the query parameters.
    /// </summary>
    public bool IsValid(out List<string> errors)
    {
        errors = new List<string>();

        // Validate page
        if (Page < 1)
        {
            errors.Add("Page must be greater than or equal to 1.");
        }

        // Validate page size
        if (PageSize < 1)
        {
            errors.Add("PageSize must be greater than or equal to 1.");
        }
        else if (PageSize > 1000)
        {
            errors.Add("PageSize must be less than or equal to 1000.");
        }

        // Validate date range
        if (StartDate.HasValue && EndDate.HasValue && StartDate > EndDate)
        {
            errors.Add("StartDate must be less than or equal to EndDate.");
        }

        // Validate premium amount range
        if (MinPremiumAmount.HasValue && MaxPremiumAmount.HasValue && MinPremiumAmount > MaxPremiumAmount)
        {
            errors.Add("MinPremiumAmount must be less than or equal to MaxPremiumAmount.");
        }

        // Validate sort by field
        var validSortFields = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "PolicyNumber", "ReferenceDate", "ProductCode", "BasePremium", "TariffPremium", "NetPremium", "MovementType"
        };

        if (!validSortFields.Contains(SortBy))
        {
            errors.Add($"SortBy must be one of: {string.Join(", ", validSortFields)}");
        }

        // Validate sort order
        var validSortOrders = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { "asc", "desc" };
        if (!validSortOrders.Contains(SortOrder))
        {
            errors.Add("SortOrder must be 'asc' or 'desc'.");
        }

        // Validate movement type
        if (!string.IsNullOrEmpty(MovementType) && MovementType.Length != 1)
        {
            errors.Add("MovementType must be a single character (E, C, R, etc.).");
        }

        return errors.Count == 0;
    }
}
