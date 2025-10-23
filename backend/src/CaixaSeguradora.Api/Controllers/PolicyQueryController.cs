using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Policy query controller for filtering, searching, and analyzing policy data.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
[ApiController]
[Route("api/policies")]
[Produces("application/json")]
public class PolicyQueryController : ControllerBase
{
    private readonly IPolicyQueryService _queryService;
    private readonly ILogger<PolicyQueryController> _logger;

    public PolicyQueryController(
        IPolicyQueryService queryService,
        ILogger<PolicyQueryController> logger)
    {
        _queryService = queryService;
        _logger = logger;
    }

    /// <summary>
    /// Queries policy records with filtering, sorting, and pagination.
    /// </summary>
    /// <param name="query">Query parameters from query string</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Paginated policy records with metadata and statistics</returns>
    [HttpGet]
    [ProducesResponseType(typeof(PolicyQueryResponseDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<PolicyQueryResponseDto>> QueryPolicies(
        [FromQuery] PolicyQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Querying policies - Page: {Page}, PageSize: {PageSize}",
                query.Page, query.PageSize);

            if (!query.IsValid(out List<string>? errors))
            {
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Invalid query parameters",
                    Details = string.Join("; ", errors),
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            PolicyQueryResponseDto result = await _queryService.QueryPoliciesAsync(query, cancellationToken);

            _logger.LogInformation("Query completed - TotalRecords: {Total}, Page: {Page}/{TotalPages}",
                result.Pagination.TotalRecords,
                result.Pagination.CurrentPage,
                result.Pagination.TotalPages);

            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error querying policies");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to query policy records",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets a single policy record by policy number.
    /// </summary>
    /// <param name="policyNumber">Policy number (unique business key)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Policy record details</returns>
    [HttpGet("{policyNumber}")]
    [ProducesResponseType(typeof(PolicyRecordDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<PolicyRecordDto>> GetPolicyByNumber(
        long policyNumber,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching policy: {PolicyNumber}", policyNumber);

            PolicyRecordDto? policy = await _queryService.GetPolicyByNumberAsync(policyNumber, cancellationToken);

            if (policy == null)
            {
                _logger.LogWarning("Policy not found: {PolicyNumber}", policyNumber);
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Policy not found",
                    Details = $"No policy found with number: {policyNumber}",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            return Ok(policy);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching policy {PolicyNumber}", policyNumber);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve policy record",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets aggregated statistics for policy data.
    /// </summary>
    /// <param name="query">Query parameters for filtering</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Aggregated statistics</returns>
    [HttpGet("statistics")]
    [ProducesResponseType(typeof(PolicyStatisticsDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<PolicyStatisticsDto>> GetStatistics(
        [FromQuery] PolicyQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            PolicyStatisticsDto statistics = await _queryService.GetPolicyStatisticsAsync(query, cancellationToken);
            return Ok(statistics);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching policy statistics");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve statistics",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Searches policies by client name or code.
    /// </summary>
    /// <param name="searchTerm">Client name or code</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of matching policies</returns>
    [HttpGet("search/client")]
    [ProducesResponseType(typeof(List<PolicyRecordDto>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<List<PolicyRecordDto>>> SearchByClient(
        [FromQuery] string searchTerm,
        CancellationToken cancellationToken)
    {
        try
        {
            List<PolicyRecordDto> policies = await _queryService.SearchPoliciesByClientAsync(searchTerm, cancellationToken);
            return Ok(policies);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error searching policies by client");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to search policies",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Exports policy query results to CSV.
    /// </summary>
    /// <param name="query">Query parameters for filtering</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV file download</returns>
    [HttpGet("export/csv")]
    [Produces("text/csv")]
    [ProducesResponseType(typeof(FileContentResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ExportToCsv(
        [FromQuery] PolicyQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            var csvData = await _queryService.ExportPoliciesToCsvAsync(query, cancellationToken);
            var fileName = $"policies_export_{DateTime.UtcNow:yyyyMMddHHmmss}.csv";
            return File(csvData, "text/csv", fileName);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error exporting policies to CSV");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to export policies",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }
}
