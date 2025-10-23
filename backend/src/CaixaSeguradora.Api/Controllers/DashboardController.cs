using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Dashboard controller providing COBOL program analysis metrics and migration progress.
/// Endpoints support the interactive dashboard UI for RG1866B migration project.
/// </summary>
[ApiController]
[Route("api/v1/[controller]")]
[Produces("application/json")]
public class DashboardController : ControllerBase
{
    private readonly IDashboardService _dashboardService;
    private readonly ILogger<DashboardController> _logger;

    public DashboardController(
        IDashboardService dashboardService,
        ILogger<DashboardController> logger)
    {
        _dashboardService = dashboardService;
        _logger = logger;
    }

    /// <summary>
    /// Gets comprehensive dashboard metrics including program info, data structure, complexity, and migration progress.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Dashboard metrics from COBOL analysis</returns>
    /// <response code="200">Returns dashboard metrics successfully</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("metrics")]
    [ProducesResponseType(typeof(DashboardMetricsDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<DashboardMetricsDto>> GetMetrics(CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching dashboard metrics");
            DashboardMetricsDto metrics = await _dashboardService.GetDashboardMetricsAsync(cancellationToken);
            _logger.LogInformation("Dashboard metrics retrieved successfully: {ProgramName}, {TasksCompleted}/{TotalTasks} tasks",
                metrics.ProgramInfo.ProgramName,
                metrics.MigrationProgress.TasksCompleted,
                metrics.MigrationProgress.TotalTasks);
            return Ok(metrics);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching dashboard metrics");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve dashboard metrics",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets function points analysis for project estimation and complexity assessment.
    /// Based on IFPUG (International Function Point Users Group) methodology.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Function points breakdown with complexity ratings</returns>
    /// <response code="200">Returns function points analysis successfully</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("function-points")]
    [ProducesResponseType(typeof(FunctionPointsDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<FunctionPointsDto>> GetFunctionPoints(CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching function points analysis");
            FunctionPointsDto functionPoints = await _dashboardService.GetFunctionPointsAsync(cancellationToken);
            _logger.LogInformation("Function points analysis retrieved: {TotalFP} adjusted FP, {EffortMonths} months estimated",
                functionPoints.TotalAdjustedFunctionPoints,
                functionPoints.EstimatedEffortMonths);
            return Ok(functionPoints);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching function points analysis");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve function points analysis",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets database dependencies showing all tables, views, cursors, and relationships.
    /// Critical for understanding data access patterns and migration complexity.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Database dependencies with table relationships and cursor information</returns>
    /// <response code="200">Returns database dependencies successfully</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("database-dependencies")]
    [ProducesResponseType(typeof(DatabaseDependenciesDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<DatabaseDependenciesDto>> GetDatabaseDependencies(CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching database dependencies analysis");
            DatabaseDependenciesDto dependencies = await _dashboardService.GetDatabaseDependenciesAsync(cancellationToken);
            _logger.LogInformation("Database dependencies retrieved: {TableCount} tables, {CursorCount} cursors, {SqlOps} SQL operations",
                dependencies.TotalTables,
                dependencies.TotalCursors,
                dependencies.SqlStats.TotalOperations);
            return Ok(dependencies);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching database dependencies");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve database dependencies",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Health check endpoint for dashboard service availability.
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
            Service = "DashboardService",
            Timestamp = DateTime.UtcNow
        });
    }
}
