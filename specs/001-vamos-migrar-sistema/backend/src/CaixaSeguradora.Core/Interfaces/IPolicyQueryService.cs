using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for querying and analyzing policy data.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// Provides filtering, sorting, pagination, and statistical analysis for policies.
/// </summary>
public interface IPolicyQueryService
{
    /// <summary>
    /// Queries policy records with filtering, sorting, and pagination.
    /// Returns paginated results with metadata for UI display.
    /// </summary>
    /// <param name="query">Query parameters including filters, sorting, and pagination</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Query response with policy records, pagination metadata, and statistics</returns>
    Task<PolicyQueryResponseDto> QueryPoliciesAsync(
        PolicyQueryDto query,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets a single policy record by policy number with full details.
    /// Includes navigation properties and related data.
    /// </summary>
    /// <param name="policyNumber">Policy number (unique business key)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Policy record DTO or null if not found</returns>
    Task<PolicyRecordDto?> GetPolicyByNumberAsync(
        long policyNumber,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets aggregated statistics for policy data based on filter criteria.
    /// Includes totals, averages, and breakdowns by product, line of business, and status.
    /// </summary>
    /// <param name="query">Query parameters for filtering (pagination ignored)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Aggregated statistics for filtered policy records</returns>
    Task<PolicyStatisticsDto> GetPolicyStatisticsAsync(
        PolicyQueryDto query,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Searches policies by client name or code.
    /// Returns list of matching policies for client lookup.
    /// </summary>
    /// <param name="searchTerm">Client name or code to search for</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of policies matching the search criteria</returns>
    Task<List<PolicyRecordDto>> SearchPoliciesByClientAsync(
        string searchTerm,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Exports policy query results to CSV format.
    /// Applies same filters as QueryPoliciesAsync but returns all results (no pagination).
    /// </summary>
    /// <param name="query">Query parameters for filtering</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV content as byte array</returns>
    Task<byte[]> ExportPoliciesToCsvAsync(
        PolicyQueryDto query,
        CancellationToken cancellationToken = default);
}
