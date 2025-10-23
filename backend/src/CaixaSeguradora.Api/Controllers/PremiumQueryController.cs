using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Premium query controller for filtering, searching, and analyzing premium data.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// Provides endpoints for interactive querying, statistics, and CSV export.
/// </summary>
[ApiController]
[Route("api/premiums")]
[Produces("application/json")]
public class PremiumQueryController : ControllerBase
{
    private readonly IPremiumQueryService _queryService;
    private readonly ILogger<PremiumQueryController> _logger;

    public PremiumQueryController(
        IPremiumQueryService queryService,
        ILogger<PremiumQueryController> logger)
    {
        _queryService = queryService;
        _logger = logger;
    }

    /// <summary>
    /// Queries premium records with filtering, sorting, and pagination.
    /// Supports multiple filter criteria including policy number, date range, product, line of business, etc.
    /// </summary>
    /// <param name="query">Query parameters from query string</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Paginated premium records with metadata and statistics</returns>
    /// <response code="200">Returns query results successfully</response>
    /// <response code="400">Invalid query parameters</response>
    /// <response code="500">Internal server error</response>
    [HttpGet]
    [ProducesResponseType(typeof(PremiumQueryResponseDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<PremiumQueryResponseDto>> QueryPremiums(
        [FromQuery] PremiumQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Querying premiums with filters - Page: {Page}, PageSize: {PageSize}, SortBy: {SortBy}",
                query.Page, query.PageSize, query.SortBy);

            if (!query.IsValid(out var errors))
            {
                _logger.LogWarning("Invalid query parameters: {Errors}", string.Join(", ", errors));
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Invalid query parameters",
                    Details = string.Join("; ", errors),
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var result = await _queryService.QueryPremiumsAsync(query, cancellationToken);

            _logger.LogInformation("Query completed successfully - TotalRecords: {Total}, Page: {Page}/{TotalPages}",
                result.Pagination.TotalRecords,
                result.Pagination.CurrentPage,
                result.Pagination.TotalPages);

            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error querying premiums");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to query premium records",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets a single premium record by ID with full details.
    /// Includes navigation properties for related entities (policy, product, client, etc.).
    /// </summary>
    /// <param name="id">Premium record identifier</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Premium record details</returns>
    /// <response code="200">Returns premium record successfully</response>
    /// <response code="404">Premium record not found</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("{id}")]
    [ProducesResponseType(typeof(PremiumRecordDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<PremiumRecordDto>> GetPremiumById(
        long id,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching premium record with ID: {PremiumId}", id);

            var premium = await _queryService.GetPremiumByIdAsync(id, cancellationToken);

            if (premium == null)
            {
                _logger.LogWarning("Premium record not found: {PremiumId}", id);
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Premium record not found",
                    Details = $"No premium record found with ID: {id}",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            _logger.LogInformation("Premium record retrieved successfully: {PremiumId}, Policy: {PolicyNumber}",
                premium.PremiumId, premium.PolicyNumber);

            return Ok(premium);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching premium record {PremiumId}", id);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve premium record",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets aggregated statistics for premium data based on filter criteria.
    /// Includes totals, averages, and breakdowns by movement type and product.
    /// </summary>
    /// <param name="query">Query parameters for filtering (pagination ignored)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Aggregated statistics</returns>
    /// <response code="200">Returns statistics successfully</response>
    /// <response code="400">Invalid query parameters</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("statistics")]
    [ProducesResponseType(typeof(PremiumStatisticsDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<PremiumStatisticsDto>> GetStatistics(
        [FromQuery] PremiumQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching premium statistics with filters");

            if (!query.IsValid(out var errors))
            {
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Invalid query parameters",
                    Details = string.Join("; ", errors),
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var statistics = await _queryService.GetPremiumStatisticsAsync(query, cancellationToken);

            _logger.LogInformation("Statistics retrieved successfully - TotalRecords: {Total}, TotalNetPremium: {TotalNet}",
                statistics.TotalRecords, statistics.TotalNetPremium);

            return Ok(statistics);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching premium statistics");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve premium statistics",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets all premium records for a specific policy number.
    /// Useful for policy detail views showing all premium movements.
    /// </summary>
    /// <param name="policyNumber">Policy number</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of premium records for the policy</returns>
    /// <response code="200">Returns premium records successfully</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("by-policy/{policyNumber}")]
    [ProducesResponseType(typeof(List<PremiumRecordDto>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<List<PremiumRecordDto>>> GetPremiumsByPolicy(
        long policyNumber,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching premiums for policy: {PolicyNumber}", policyNumber);

            var premiums = await _queryService.GetPremiumsByPolicyAsync(policyNumber, cancellationToken);

            _logger.LogInformation("Retrieved {Count} premium records for policy {PolicyNumber}",
                premiums.Count, policyNumber);

            return Ok(premiums);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching premiums for policy {PolicyNumber}", policyNumber);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve premium records for policy",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Exports premium query results to CSV format.
    /// Applies same filters as query endpoint but returns all results (no pagination, max 100,000 records).
    /// </summary>
    /// <param name="query">Query parameters for filtering</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV file download</returns>
    /// <response code="200">Returns CSV file successfully</response>
    /// <response code="400">Invalid query parameters</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("export/csv")]
    [Produces("text/csv")]
    [ProducesResponseType(typeof(FileContentResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ExportToCsv(
        [FromQuery] PremiumQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Exporting premiums to CSV with filters");

            if (!query.IsValid(out var errors))
            {
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Invalid query parameters",
                    Details = string.Join("; ", errors),
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var csvData = await _queryService.ExportPremiumsToCsvAsync(query, cancellationToken);

            var fileName = $"premiums_export_{DateTime.UtcNow:yyyyMMddHHmmss}.csv";

            _logger.LogInformation("CSV export completed successfully - Size: {Size} bytes, File: {FileName}",
                csvData.Length, fileName);

            return File(csvData, "text/csv", fileName);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error exporting premiums to CSV");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to export premium records to CSV",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets available filter options for dropdowns and search inputs.
    /// Returns distinct values for movement types, products, lines of business, etc.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Available filter options</returns>
    /// <response code="200">Returns filter options successfully</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("filter-options")]
    [ProducesResponseType(typeof(PremiumFilterOptionsDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<PremiumFilterOptionsDto>> GetFilterOptions(
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching premium filter options");

            var options = await _queryService.GetFilterOptionsAsync(cancellationToken);

            _logger.LogInformation("Filter options retrieved successfully - Products: {ProductCount}, MovementTypes: {MovementTypeCount}",
                options.Products.Count, options.MovementTypes.Count);

            return Ok(options);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching filter options");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve filter options",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Health check endpoint for premium query service availability.
    /// </summary>
    /// <returns>Service health status</returns>
    /// <response code="200">Service is healthy</response>
    [HttpGet("health")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    public IActionResult HealthCheck()
    {
        return Ok(new
        {
            Status = "Healthy",
            Service = "PremiumQueryService",
            Timestamp = DateTime.UtcNow
        });
    }
}
