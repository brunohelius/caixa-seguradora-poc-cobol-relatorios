using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Reports controller for generating and managing SUSEP Circular 360 premium reports.
/// Supports async report generation, progress tracking, file download, and COBOL comparison.
/// </summary>
[ApiController]
[Route("api/[controller]")]
[Produces("application/json")]
public class ReportsController : ControllerBase
{
    private readonly IReportGenerationService _reportService;
    private readonly ILogger<ReportsController> _logger;

    public ReportsController(
        IReportGenerationService reportService,
        ILogger<ReportsController> logger)
    {
        _reportService = reportService;
        _logger = logger;
    }

    /// <summary>
    /// Initiates async report generation for PREMIT and/or PREMCED files.
    /// Returns immediately with report ID for status polling (HTTP 202 Accepted pattern).
    /// </summary>
    /// <param name="request">Report generation parameters including date range, system ID, and report type</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Report ID for tracking generation progress</returns>
    /// <response code="202">Report generation accepted and queued</response>
    /// <response code="400">Invalid request parameters</response>
    /// <response code="500">Internal server error</response>
    /// <remarks>
    /// Sample request:
    ///
    ///     POST /api/reports/generate
    ///     {
    ///       "startDate": "2025-10-01",
    ///       "endDate": "2025-10-31",
    ///       "systemId": "GL",
    ///       "reportType": "Both",
    ///       "processingMode": "Monthly"
    ///     }
    ///
    /// After receiving report ID, poll GET /api/reports/{reportId} for status updates.
    /// </remarks>
    [HttpPost("generate")]
    [ProducesResponseType(typeof(ReportGenerationResponseDto), StatusCodes.Status202Accepted)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ReportGenerationResponseDto>> GenerateReport(
        [FromBody] ReportGenerationRequestDto request,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation(
                "Report generation requested: {SystemId}, {StartDate} to {EndDate}, Type: {ReportType}",
                request.SystemId,
                request.StartDate,
                request.EndDate,
                request.ReportType);

            var reportId = await _reportService.GenerateReportAsync(request, cancellationToken);

            var response = new ReportGenerationResponseDto
            {
                ReportId = reportId,
                Message = "Report generation started successfully",
                StatusUrl = $"/api/reports/{reportId}",
                EstimatedCompletionMinutes = 2
            };

            _logger.LogInformation("Report generation queued: {ReportId}", reportId);

            return Accepted(response.StatusUrl, response);
        }
        catch (ArgumentException ex)
        {
            _logger.LogWarning(ex, "Invalid report generation request");
            return BadRequest(new ErrorResponse
            {
                StatusCode = 400,
                Message = "Invalid request parameters",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error starting report generation");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to start report generation",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets the current status of a report generation job.
    /// Use this endpoint to poll progress during async processing.
    /// </summary>
    /// <param name="reportId">Report ID returned from generate endpoint</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Report status including progress, execution time, and file availability</returns>
    /// <response code="200">Report status retrieved successfully</response>
    /// <response code="404">Report not found</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("{reportId}")]
    [ProducesResponseType(typeof(ReportStatusDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ReportStatusDto>> GetReportStatus(
        Guid reportId,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching status for report: {ReportId}", reportId);
            var status = await _reportService.GetReportStatusAsync(reportId, cancellationToken);
            return Ok(status);
        }
        catch (KeyNotFoundException ex)
        {
            _logger.LogWarning(ex, "Report not found: {ReportId}", reportId);
            return NotFound(new ErrorResponse
            {
                StatusCode = 404,
                Message = "Report not found",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching report status: {ReportId}", reportId);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve report status",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Downloads a generated report file (PREMIT.TXT or PREMCED.TXT).
    /// File is returned as plain text with ISO-8859-1 encoding (COBOL-compatible).
    /// </summary>
    /// <param name="reportId">Report ID</param>
    /// <param name="fileType">File type to download (PREMIT or PREMCED)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Report file as downloadable stream</returns>
    /// <response code="200">File downloaded successfully</response>
    /// <response code="404">Report or file not found</response>
    /// <response code="410">File has expired and is no longer available</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("{reportId}/download/{fileType}")]
    [ProducesResponseType(typeof(FileStreamResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status410Gone)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> DownloadReport(
        Guid reportId,
        string fileType,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Download requested: Report {ReportId}, FileType {FileType}", reportId, fileType);

            var (fileStream, fileName, contentType) = await _reportService.DownloadReportAsync(
                reportId,
                fileType,
                cancellationToken);

            return File(fileStream, contentType, fileName);
        }
        catch (KeyNotFoundException ex)
        {
            _logger.LogWarning(ex, "Report not found for download: {ReportId}", reportId);
            return NotFound(new ErrorResponse
            {
                StatusCode = 404,
                Message = "Report not found",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (FileNotFoundException ex)
        {
            _logger.LogWarning(ex, "File not found or expired: {ReportId}/{FileType}", reportId, fileType);
            return StatusCode(410, new ErrorResponse
            {
                StatusCode = 410,
                Message = "File has expired or is no longer available",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (InvalidOperationException ex)
        {
            _logger.LogWarning(ex, "Report not ready for download: {ReportId}", reportId);
            return BadRequest(new ErrorResponse
            {
                StatusCode = 400,
                Message = "Report is not ready for download",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error downloading report: {ReportId}/{FileType}", reportId, fileType);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to download report",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Compares generated .NET report with legacy COBOL output for validation.
    /// Critical for migration testing and regulatory compliance verification.
    /// </summary>
    /// <param name="reportId">Report ID of generated .NET report</param>
    /// <param name="request">Comparison request with COBOL file path</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Detailed comparison result showing byte-level and semantic differences</returns>
    /// <response code="200">Comparison completed successfully</response>
    /// <response code="404">Report or COBOL file not found</response>
    /// <response code="400">Invalid request or report not ready</response>
    /// <response code="500">Internal server error</response>
    /// <remarks>
    /// Sample request:
    ///
    ///     POST /api/reports/{reportId}/compare
    ///     {
    ///       "cobolFilePath": "/path/to/PREMIT_COBOL_OUTPUT.TXT"
    ///     }
    ///
    /// The comparison performs:
    /// - Byte-level comparison (100% match required for regulatory compliance)
    /// - Line-by-line diff analysis
    /// - Semantic comparison of business totals (premium amounts, counts)
    /// </remarks>
    [HttpPost("{reportId}/compare")]
    [ProducesResponseType(typeof(ReportComparisonDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ReportComparisonDto>> CompareWithCobol(
        Guid reportId,
        [FromBody] ComparisonRequestDto request,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation(
                "COBOL comparison requested: Report {ReportId}, COBOL file {CobolFile}",
                reportId,
                request.CobolFilePath);

            var comparison = await _reportService.CompareWithCobolOutputAsync(
                reportId,
                request.CobolFilePath,
                cancellationToken);

            _logger.LogInformation(
                "Comparison completed: {Result} ({MatchPercentage}% match)",
                comparison.ComparisonResult,
                comparison.LineMatchPercentage);

            return Ok(comparison);
        }
        catch (FileNotFoundException ex)
        {
            _logger.LogWarning(ex, "COBOL file not found: {CobolFile}", request.CobolFilePath);
            return NotFound(new ErrorResponse
            {
                StatusCode = 404,
                Message = "COBOL output file not found",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (InvalidOperationException ex)
        {
            _logger.LogWarning(ex, "Cannot compare incomplete report: {ReportId}", reportId);
            return BadRequest(new ErrorResponse
            {
                StatusCode = 400,
                Message = "Report is not ready for comparison",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error comparing report with COBOL output: {ReportId}", reportId);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to compare report with COBOL output",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets report generation history with optional filtering and pagination.
    /// Useful for audit trail, monitoring, and accessing historical reports.
    /// </summary>
    /// <param name="startDate">Filter: generation date start (optional)</param>
    /// <param name="endDate">Filter: generation date end (optional)</param>
    /// <param name="systemId">Filter: system ID (optional)</param>
    /// <param name="status">Filter: report status (optional)</param>
    /// <param name="pageNumber">Page number (1-based, default: 1)</param>
    /// <param name="pageSize">Records per page (default: 20, max: 100)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Paginated list of report generation history</returns>
    /// <response code="200">History retrieved successfully</response>
    /// <response code="400">Invalid filter parameters</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("history")]
    [ProducesResponseType(typeof(ReportHistoryResponseDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ReportHistoryResponseDto>> GetReportHistory(
        [FromQuery] DateTime? startDate,
        [FromQuery] DateTime? endDate,
        [FromQuery] string? systemId,
        [FromQuery] string? status,
        [FromQuery] int pageNumber = 1,
        [FromQuery] int pageSize = 20,
        CancellationToken cancellationToken = default)
    {
        try
        {
            if (pageSize > 100)
                pageSize = 100;

            if (pageNumber < 1)
                pageNumber = 1;

            _logger.LogInformation(
                "Fetching report history: Page {PageNumber}, Size {PageSize}",
                pageNumber,
                pageSize);

            var (reports, totalCount) = await _reportService.GetReportHistoryAsync(
                startDate,
                endDate,
                systemId,
                status,
                pageNumber,
                pageSize,
                cancellationToken);

            var response = new ReportHistoryResponseDto
            {
                Reports = reports,
                TotalCount = totalCount,
                PageNumber = pageNumber,
                PageSize = pageSize,
                TotalPages = (int)Math.Ceiling((double)totalCount / pageSize)
            };

            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching report history");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve report history",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Cancels an in-progress report generation job.
    /// Only queued or processing jobs can be cancelled.
    /// </summary>
    /// <param name="reportId">Report ID to cancel</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Cancellation result</returns>
    /// <response code="200">Report cancelled successfully</response>
    /// <response code="400">Report cannot be cancelled (already completed or failed)</response>
    /// <response code="404">Report not found</response>
    /// <response code="500">Internal server error</response>
    [HttpDelete("{reportId}")]
    [ProducesResponseType(typeof(CancellationResultDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<CancellationResultDto>> CancelReportGeneration(
        Guid reportId,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Cancellation requested for report: {ReportId}", reportId);

            var cancelled = await _reportService.CancelReportGenerationAsync(reportId, cancellationToken);

            if (!cancelled)
            {
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Report cannot be cancelled (already completed or failed)",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var result = new CancellationResultDto
            {
                ReportId = reportId,
                Cancelled = true,
                Message = "Report generation cancelled successfully"
            };

            _logger.LogInformation("Report cancelled: {ReportId}", reportId);
            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error cancelling report: {ReportId}", reportId);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to cancel report generation",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Health check endpoint for report generation service availability.
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
            Service = "ReportGenerationService",
            Timestamp = DateTime.UtcNow
        });
    }
}

/// <summary>
/// Response DTO for report generation initiation.
/// </summary>
public class ReportGenerationResponseDto
{
    public Guid ReportId { get; set; }
    public string Message { get; set; } = string.Empty;
    public string StatusUrl { get; set; } = string.Empty;
    public int EstimatedCompletionMinutes { get; set; }
}

/// <summary>
/// Request DTO for COBOL comparison.
/// </summary>
public class ComparisonRequestDto
{
    public string CobolFilePath { get; set; } = string.Empty;
}

/// <summary>
/// Response DTO for report history with pagination.
/// </summary>
public class ReportHistoryResponseDto
{
    public List<ReportStatusDto> Reports { get; set; } = new();
    public int TotalCount { get; set; }
    public int PageNumber { get; set; }
    public int PageSize { get; set; }
    public int TotalPages { get; set; }
}

/// <summary>
/// Response DTO for cancellation operation.
/// </summary>
public class CancellationResultDto
{
    public Guid ReportId { get; set; }
    public bool Cancelled { get; set; }
    public string Message { get; set; } = string.Empty;
}
