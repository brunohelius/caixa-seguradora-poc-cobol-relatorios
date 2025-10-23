namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for policy query results with pagination metadata.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
public class PolicyQueryResponseDto
{
    /// <summary>
    /// List of policy records matching the query filters.
    /// </summary>
    public List<PolicyRecordDto> Records { get; set; } = new();

    /// <summary>
    /// Pagination metadata (reuses PaginationMetadata from PremiumQueryResponseDto).
    /// </summary>
    public PaginationMetadata Pagination { get; set; } = new();

    /// <summary>
    /// Aggregated statistics for the current result set.
    /// </summary>
    public PolicyStatisticsDto? Statistics { get; set; }
}

/// <summary>
/// Simplified policy record DTO for query results display.
/// Contains essential fields for table/chart visualization.
/// </summary>
public class PolicyRecordDto
{
    /// <summary>
    /// Policy record identifier.
    /// </summary>
    public long PolicyId { get; set; }

    /// <summary>
    /// Policy number (unique business key).
    /// </summary>
    public long PolicyNumber { get; set; }

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
    /// Agency description/name.
    /// </summary>
    public string? AgencyName { get; set; }

    /// <summary>
    /// Producer (corretor) code.
    /// </summary>
    public int ProducerCode { get; set; }

    /// <summary>
    /// Producer name.
    /// </summary>
    public string? ProducerName { get; set; }

    /// <summary>
    /// Insured (client) code.
    /// </summary>
    public long InsuredCode { get; set; }

    /// <summary>
    /// Insured name.
    /// </summary>
    public string? InsuredName { get; set; }

    /// <summary>
    /// Policy holder (tomador) code (may be same as insured).
    /// </summary>
    public long? PolicyHolderCode { get; set; }

    /// <summary>
    /// Policy holder name.
    /// </summary>
    public string? PolicyHolderName { get; set; }

    /// <summary>
    /// Policy issue date.
    /// </summary>
    public DateTime IssueDate { get; set; }

    /// <summary>
    /// Policy start validity date.
    /// </summary>
    public DateTime StartValidityDate { get; set; }

    /// <summary>
    /// Policy end validity date.
    /// </summary>
    public DateTime EndValidityDate { get; set; }

    /// <summary>
    /// Policy status code.
    /// </summary>
    public string PolicyStatus { get; set; } = string.Empty;

    /// <summary>
    /// Policy status description.
    /// </summary>
    public string? PolicyStatusDescription { get; set; }

    /// <summary>
    /// Policy type.
    /// </summary>
    public string? PolicyType { get; set; }

    /// <summary>
    /// Modality code.
    /// </summary>
    public int ModalityCode { get; set; }

    /// <summary>
    /// Modality description.
    /// </summary>
    public string? ModalityDescription { get; set; }

    /// <summary>
    /// Insured capital amount.
    /// </summary>
    public decimal InsuredCapital { get; set; }

    /// <summary>
    /// Total premium amount.
    /// </summary>
    public decimal TotalPremium { get; set; }

    /// <summary>
    /// Number of installments.
    /// </summary>
    public int? NumberOfInstallments { get; set; }

    /// <summary>
    /// Installment frequency code.
    /// </summary>
    public string? InstallmentFrequency { get; set; }

    /// <summary>
    /// Whether the policy is active (not expired or cancelled).
    /// Calculated field.
    /// </summary>
    public bool IsActive { get; set; }

    /// <summary>
    /// Number of endorsements associated with this policy.
    /// </summary>
    public int EndorsementCount { get; set; }

    /// <summary>
    /// Number of premium records associated with this policy.
    /// </summary>
    public int PremiumRecordCount { get; set; }

    /// <summary>
    /// Last modification date.
    /// </summary>
    public DateTime? LastModifiedDate { get; set; }
}

/// <summary>
/// Aggregated statistics for policy query results.
/// </summary>
public class PolicyStatisticsDto
{
    /// <summary>
    /// Total number of records in the result set.
    /// </summary>
    public long TotalRecords { get; set; }

    /// <summary>
    /// Number of active policies.
    /// </summary>
    public long ActivePolicies { get; set; }

    /// <summary>
    /// Number of inactive/expired policies.
    /// </summary>
    public long InactivePolicies { get; set; }

    /// <summary>
    /// Sum of insured capital amounts.
    /// </summary>
    public decimal TotalInsuredCapital { get; set; }

    /// <summary>
    /// Sum of total premium amounts.
    /// </summary>
    public decimal TotalPremiumAmount { get; set; }

    /// <summary>
    /// Average insured capital per policy.
    /// </summary>
    public decimal AverageInsuredCapital { get; set; }

    /// <summary>
    /// Average premium per policy.
    /// </summary>
    public decimal AveragePremium { get; set; }

    /// <summary>
    /// Breakdown by product code.
    /// </summary>
    public Dictionary<int, PolicyProductStatistics> ByProduct { get; set; } = new();

    /// <summary>
    /// Breakdown by line of business.
    /// </summary>
    public Dictionary<int, PolicyLineOfBusinessStatistics> ByLineOfBusiness { get; set; } = new();

    /// <summary>
    /// Breakdown by policy status.
    /// </summary>
    public Dictionary<string, PolicyStatusStatistics> ByStatus { get; set; } = new();

    /// <summary>
    /// Date range covered by the result set (issue dates).
    /// </summary>
    public DateRangeDto IssueDateRange { get; set; } = new();
}

/// <summary>
/// Statistics for a specific product in policy queries.
/// </summary>
public class PolicyProductStatistics
{
    /// <summary>
    /// Product code.
    /// </summary>
    public int ProductCode { get; set; }

    /// <summary>
    /// Product description.
    /// </summary>
    public string? ProductDescription { get; set; }

    /// <summary>
    /// Number of policies for this product.
    /// </summary>
    public long PolicyCount { get; set; }

    /// <summary>
    /// Sum of insured capital for this product.
    /// </summary>
    public decimal TotalInsuredCapital { get; set; }

    /// <summary>
    /// Sum of premiums for this product.
    /// </summary>
    public decimal TotalPremium { get; set; }

    /// <summary>
    /// Percentage of total policies.
    /// </summary>
    public decimal PercentageOfTotal { get; set; }
}

/// <summary>
/// Statistics for a specific line of business in policy queries.
/// </summary>
public class PolicyLineOfBusinessStatistics
{
    /// <summary>
    /// Line of business code.
    /// </summary>
    public int LineOfBusinessCode { get; set; }

    /// <summary>
    /// Line of business description.
    /// </summary>
    public string? LineOfBusinessDescription { get; set; }

    /// <summary>
    /// Number of policies for this line of business.
    /// </summary>
    public long PolicyCount { get; set; }

    /// <summary>
    /// Sum of insured capital for this line of business.
    /// </summary>
    public decimal TotalInsuredCapital { get; set; }

    /// <summary>
    /// Sum of premiums for this line of business.
    /// </summary>
    public decimal TotalPremium { get; set; }

    /// <summary>
    /// Percentage of total policies.
    /// </summary>
    public decimal PercentageOfTotal { get; set; }
}

/// <summary>
/// Statistics for a specific policy status.
/// </summary>
public class PolicyStatusStatistics
{
    /// <summary>
    /// Policy status code.
    /// </summary>
    public string PolicyStatus { get; set; } = string.Empty;

    /// <summary>
    /// Policy status description.
    /// </summary>
    public string? PolicyStatusDescription { get; set; }

    /// <summary>
    /// Number of policies with this status.
    /// </summary>
    public long PolicyCount { get; set; }

    /// <summary>
    /// Percentage of total policies.
    /// </summary>
    public decimal PercentageOfTotal { get; set; }
}
