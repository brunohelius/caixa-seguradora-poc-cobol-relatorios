using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for querying and analyzing premium data.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// Provides filtering, sorting, pagination, and statistical analysis capabilities.
/// </summary>
public interface IPremiumQueryService
{
    /// <summary>
    /// Queries premium records with filtering, sorting, and pagination.
    /// Returns paginated results with metadata for UI display.
    /// </summary>
    /// <param name="query">Query parameters including filters, sorting, and pagination</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Query response with premium records, pagination metadata, and statistics</returns>
    Task<PremiumQueryResponseDto> QueryPremiumsAsync(
        PremiumQueryDto query,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets aggregated statistics for premium data based on filter criteria.
    /// Includes totals, averages, and breakdowns by movement type and product.
    /// </summary>
    /// <param name="query">Query parameters for filtering (pagination ignored)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Aggregated statistics for filtered premium records</returns>
    Task<PremiumStatisticsDto> GetPremiumStatisticsAsync(
        PremiumQueryDto query,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets a single premium record by ID with full details.
    /// Includes navigation properties (policy, product, client, etc.).
    /// </summary>
    /// <param name="premiumId">Premium record identifier</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Premium record DTO or null if not found</returns>
    Task<PremiumRecordDto?> GetPremiumByIdAsync(
        long premiumId,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets all premium records for a specific policy number.
    /// Useful for policy detail views showing all premium movements.
    /// </summary>
    /// <param name="policyNumber">Policy number to filter by</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of premium records for the policy</returns>
    Task<List<PremiumRecordDto>> GetPremiumsByPolicyAsync(
        long policyNumber,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Exports premium query results to CSV format.
    /// Applies same filters as QueryPremiumsAsync but returns all results (no pagination).
    /// </summary>
    /// <param name="query">Query parameters for filtering</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV content as byte array</returns>
    Task<byte[]> ExportPremiumsToCsvAsync(
        PremiumQueryDto query,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets available filter options for dropdowns and search inputs.
    /// Returns distinct values for movement types, products, lines of business, etc.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Available filter options</returns>
    Task<PremiumFilterOptionsDto> GetFilterOptionsAsync(
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Available filter options for premium queries.
/// Used to populate dropdown lists and search filters in the UI.
/// </summary>
public class PremiumFilterOptionsDto
{
    /// <summary>
    /// Available movement types with descriptions.
    /// E = Emissão, C = Cancelamento, R = Reembolso, etc.
    /// </summary>
    public List<FilterOption> MovementTypes { get; set; } = new();

    /// <summary>
    /// Available products with codes and descriptions.
    /// </summary>
    public List<ProductFilterOption> Products { get; set; } = new();

    /// <summary>
    /// Available lines of business with codes and descriptions.
    /// </summary>
    public List<FilterOption> LinesOfBusiness { get; set; } = new();

    /// <summary>
    /// Available company codes.
    /// </summary>
    public List<FilterOption> CompanyCodes { get; set; } = new();

    /// <summary>
    /// Date range of available premium data.
    /// </summary>
    public DateRangeDto AvailableDateRange { get; set; } = new();
}

/// <summary>
/// Generic filter option for dropdown lists.
/// </summary>
public class FilterOption
{
    /// <summary>
    /// Option value (code).
    /// </summary>
    public string Value { get; set; } = string.Empty;

    /// <summary>
    /// Option display label (description).
    /// </summary>
    public string Label { get; set; } = string.Empty;

    /// <summary>
    /// Number of records with this option value.
    /// </summary>
    public long Count { get; set; }
}

/// <summary>
/// Product-specific filter option with additional metadata.
/// </summary>
public class ProductFilterOption
{
    /// <summary>
    /// Product code.
    /// </summary>
    public int ProductCode { get; set; }

    /// <summary>
    /// Product description.
    /// </summary>
    public string ProductDescription { get; set; } = string.Empty;

    /// <summary>
    /// Line of business code.
    /// </summary>
    public int LineOfBusinessCode { get; set; }

    /// <summary>
    /// Number of premium records for this product.
    /// </summary>
    public long RecordCount { get; set; }
}
