namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Request DTO for querying policy records with filtering, sorting, and pagination.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
public class PolicyQueryDto
{
    /// <summary>
    /// Filter by policy number (exact match).
    /// Maps to PolicyRecord.PolicyNumber.
    /// </summary>
    public long? PolicyNumber { get; set; }

    /// <summary>
    /// Filter by policy number pattern (partial match, contains).
    /// For search functionality.
    /// </summary>
    public string? PolicyNumberPattern { get; set; }

    /// <summary>
    /// Filter by product code.
    /// Maps to PolicyRecord.ProductCode.
    /// </summary>
    public int? ProductCode { get; set; }

    /// <summary>
    /// Filter by line of business (ramo) code.
    /// Maps to PolicyRecord.LineOfBusinessCode.
    /// </summary>
    public int? LineOfBusiness { get; set; }

    /// <summary>
    /// Filter by policy status.
    /// Maps to PolicyRecord.PolicyStatus.
    /// </summary>
    public string? PolicyStatus { get; set; }

    /// <summary>
    /// Filter by issue date range - start date (inclusive).
    /// Applied to PolicyRecord.IssueDate.
    /// Format: YYYY-MM-DD
    /// </summary>
    public DateTime? IssueDateStart { get; set; }

    /// <summary>
    /// Filter by issue date range - end date (inclusive).
    /// Applied to PolicyRecord.IssueDate.
    /// Format: YYYY-MM-DD
    /// </summary>
    public DateTime? IssueDateEnd { get; set; }

    /// <summary>
    /// Filter by start validity date range - start date.
    /// Applied to PolicyRecord.StartValidityDate.
    /// </summary>
    public DateTime? ValidityDateStart { get; set; }

    /// <summary>
    /// Filter by start validity date range - end date.
    /// Applied to PolicyRecord.StartValidityDate.
    /// </summary>
    public DateTime? ValidityDateEnd { get; set; }

    /// <summary>
    /// Filter by company code.
    /// Maps to PolicyRecord.CompanyCode.
    /// </summary>
    public int? CompanyCode { get; set; }

    /// <summary>
    /// Filter by agency code.
    /// Maps to PolicyRecord.AgencyCode.
    /// </summary>
    public int? AgencyCode { get; set; }

    /// <summary>
    /// Filter by producer (corretor) code.
    /// Maps to PolicyRecord.ProducerCode.
    /// </summary>
    public int? ProducerCode { get; set; }

    /// <summary>
    /// Filter by client code (insured).
    /// Maps to PolicyRecord.InsuredCode.
    /// </summary>
    public long? ClientCode { get; set; }

    /// <summary>
    /// Filter by client name pattern (partial match).
    /// For search functionality.
    /// </summary>
    public string? ClientNamePattern { get; set; }

    /// <summary>
    /// Filter by policy type.
    /// Maps to PolicyRecord.PolicyType.
    /// </summary>
    public string? PolicyType { get; set; }

    /// <summary>
    /// Filter by modality code.
    /// Maps to PolicyRecord.ModalityCode.
    /// </summary>
    public int? ModalityCode { get; set; }

    /// <summary>
    /// Include only active policies (not expired or cancelled).
    /// Default: null (all policies)
    /// </summary>
    public bool? ActiveOnly { get; set; }

    /// <summary>
    /// Current page number for pagination (1-based).
    /// Default: 1
    /// </summary>
    public int Page { get; set; } = 1;

    /// <summary>
    /// Number of records per page.
    /// Default: 50, Max: 1000
    /// </summary>
    public int PageSize { get; set; } = 50;

    /// <summary>
    /// Field to sort results by.
    /// Valid values: "PolicyNumber", "IssueDate", "ProductCode", "ClientName", "ValidityDate"
    /// Default: "IssueDate"
    /// </summary>
    public string SortBy { get; set; } = "IssueDate";

    /// <summary>
    /// Sort order direction.
    /// Valid values: "asc", "desc"
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

        // Validate issue date range
        if (IssueDateStart.HasValue && IssueDateEnd.HasValue && IssueDateStart > IssueDateEnd)
        {
            errors.Add("IssueDateStart must be less than or equal to IssueDateEnd.");
        }

        // Validate validity date range
        if (ValidityDateStart.HasValue && ValidityDateEnd.HasValue && ValidityDateStart > ValidityDateEnd)
        {
            errors.Add("ValidityDateStart must be less than or equal to ValidityDateEnd.");
        }

        // Validate sort by field
        var validSortFields = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "PolicyNumber", "IssueDate", "ProductCode", "ClientName", "ValidityDate", "LineOfBusiness"
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

        return errors.Count == 0;
    }
}
