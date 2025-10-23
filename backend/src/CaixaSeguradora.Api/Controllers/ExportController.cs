using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Services;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Export controller for generating downloadable files in various formats.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// Provides endpoints for CSV, Excel, and PDF export of query results.
/// </summary>
[ApiController]
[Route("api/export")]
[Produces("application/json")]
public class ExportController : ControllerBase
{
    private readonly IPremiumQueryService _queryService;
    private readonly IPremiumExportService _csvExportService;
    private readonly PremiumExcelExportService _excelExportService;
    private readonly PremiumPdfExportService _pdfExportService;
    private readonly ILogger<ExportController> _logger;

    public ExportController(
        IPremiumQueryService queryService,
        IPremiumExportService csvExportService,
        PremiumExcelExportService excelExportService,
        PremiumPdfExportService pdfExportService,
        ILogger<ExportController> logger)
    {
        _queryService = queryService ?? throw new ArgumentNullException(nameof(queryService));
        _csvExportService = csvExportService ?? throw new ArgumentNullException(nameof(csvExportService));
        _excelExportService = excelExportService ?? throw new ArgumentNullException(nameof(excelExportService));
        _pdfExportService = pdfExportService ?? throw new ArgumentNullException(nameof(pdfExportService));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// Exports premium query results to CSV format.
    /// </summary>
    /// <param name="query">Query parameters (same as /api/premiums endpoint)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV file download</returns>
    /// <response code="200">Returns CSV file</response>
    /// <response code="400">Invalid query parameters</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("premiums/csv")]
    [Produces("text/csv")]
    [ProducesResponseType(typeof(FileContentResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ExportPremiumsToCsv(
        [FromQuery] PremiumQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Exporting premiums to CSV with filters");

            // Execute query to get data
            PremiumQueryResponseDto queryResult = await _queryService.QueryPremiumsAsync(query, cancellationToken);

            if (queryResult.Records == null || !queryResult.Records.Any())
            {
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Nenhum registro encontrado para os filtros especificados.",
                    TraceId = HttpContext.TraceIdentifier
                });
            }

            // Export to CSV
            var csvBytes = await _csvExportService.ExportPremiumsToCsvAsync(
                queryResult.Records,
                includeHeaders: true,
                cancellationToken: cancellationToken);

            var fileName = $"premiums_{DateTime.Now:yyyyMMdd_HHmmss}.csv";

            _logger.LogInformation("CSV export completed. File: {FileName}, Size: {Size} bytes",
                fileName, csvBytes.Length);

            return File(csvBytes, "text/csv", fileName);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error exporting premiums to CSV");
            throw;
        }
    }

    /// <summary>
    /// Exports premium query results to Excel format (.xlsx).
    /// </summary>
    /// <param name="query">Query parameters</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Excel file download</returns>
    /// <response code="200">Returns Excel file</response>
    /// <response code="400">Invalid query parameters</response>
    /// <response code="501">Excel export not implemented (requires ClosedXML package)</response>
    [HttpGet("premiums/excel")]
    [Produces("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")]
    [ProducesResponseType(typeof(FileContentResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status501NotImplemented)]
    public async Task<IActionResult> ExportPremiumsToExcel(
        [FromQuery] PremiumQueryDto query,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Exporting premiums to Excel with filters");

            // Execute query to get data
            PremiumQueryResponseDto queryResult = await _queryService.QueryPremiumsAsync(query, cancellationToken);

            if (queryResult.Records == null || !queryResult.Records.Any())
            {
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Nenhum registro encontrado para os filtros especificados.",
                    TraceId = HttpContext.TraceIdentifier
                });
            }

            // Export to Excel
            var excelBytes = await _excelExportService.ExportPremiumsToExcelAsync(
                queryResult.Records,
                sheetName: "Prêmios",
                includeTotals: true,
                cancellationToken: cancellationToken);

            var fileName = $"premiums_{DateTime.Now:yyyyMMdd_HHmmss}.xlsx";

            _logger.LogInformation("Excel export completed. File: {FileName}, Size: {Size} bytes",
                fileName, excelBytes.Length);

            return File(
                excelBytes,
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                fileName);
        }
        catch (NotImplementedException ex)
        {
            _logger.LogWarning("Excel export not implemented: {Message}", ex.Message);
            return StatusCode(501, new ErrorResponse
            {
                StatusCode = 501,
                Message = "Exportação para Excel não implementada. Instale o pacote ClosedXML.",
                Details = ex.Message,
                TraceId = HttpContext.TraceIdentifier
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error exporting premiums to Excel");
            throw;
        }
    }

    /// <summary>
    /// Exports premium query results to PDF format.
    /// </summary>
    /// <param name="query">Query parameters</param>
    /// <param name="includeCharts">Include visualization charts in PDF</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>PDF file download</returns>
    /// <response code="200">Returns PDF file</response>
    /// <response code="400">Invalid query parameters</response>
    /// <response code="501">PDF export not implemented (requires QuestPDF package)</response>
    [HttpGet("premiums/pdf")]
    [Produces("application/pdf")]
    [ProducesResponseType(typeof(FileContentResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status501NotImplemented)]
    public async Task<IActionResult> ExportPremiumsToPdf(
        [FromQuery] PremiumQueryDto query,
        [FromQuery] bool includeCharts = false,
        CancellationToken cancellationToken = default)
    {
        try
        {
            _logger.LogInformation("Exporting premiums to PDF with filters. Include charts: {IncludeCharts}",
                includeCharts);

            // Execute query to get data
            PremiumQueryResponseDto queryResult = await _queryService.QueryPremiumsAsync(query, cancellationToken);

            if (queryResult.Records == null || !queryResult.Records.Any())
            {
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Nenhum registro encontrado para os filtros especificados.",
                    TraceId = HttpContext.TraceIdentifier
                });
            }

            // Export to PDF
            var pdfBytes = await _pdfExportService.ExportPremiumsToPdfAsync(
                queryResult.Records,
                title: "Relatório de Prêmios",
                includeCharts: includeCharts,
                cancellationToken: cancellationToken);

            var fileName = $"premiums_report_{DateTime.Now:yyyyMMdd_HHmmss}.pdf";

            _logger.LogInformation("PDF export completed. File: {FileName}, Size: {Size} bytes",
                fileName, pdfBytes.Length);

            return File(pdfBytes, "application/pdf", fileName);
        }
        catch (NotImplementedException ex)
        {
            _logger.LogWarning("PDF export not implemented: {Message}", ex.Message);
            return StatusCode(501, new ErrorResponse
            {
                StatusCode = 501,
                Message = "Exportação para PDF não implementada. Instale o pacote QuestPDF.",
                Details = ex.Message,
                TraceId = HttpContext.TraceIdentifier
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error exporting premiums to PDF");
            throw;
        }
    }

    /// <summary>
    /// Gets available export formats with their current implementation status.
    /// </summary>
    /// <returns>List of export format capabilities</returns>
    [HttpGet("formats")]
    [ProducesResponseType(typeof(object), StatusCodes.Status200OK)]
    public IActionResult GetExportFormats()
    {
        var formats = new[]
        {
            new
            {
                Format = "CSV",
                MimeType = "text/csv",
                Extension = ".csv",
                Implemented = true,
                Features = new[] { "UTF-8 with BOM", "Excel compatible", "Proper escaping" },
                RequiredPackage = (string?)null
            },
            new
            {
                Format = "Excel",
                MimeType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                Extension = ".xlsx",
                Implemented = true,
                Features = new[] { "Multiple sheets", "Formulas", "Formatting", "Charts", "SUM totals", "Alternating colors" },
                RequiredPackage = (string?)"Installed: ClosedXML"
            },
            new
            {
                Format = "PDF",
                MimeType = "application/pdf",
                Extension = ".pdf",
                Implemented = true,
                Features = new[] { "Professional layout", "Summary statistics", "Branding", "Page numbers" },
                RequiredPackage = (string?)"Installed: QuestPDF"
            }
        };

        return Ok(new
        {
            SupportedFormats = formats,
            Recommendation = "Use CSV export for immediate needs. Excel and PDF require additional NuGet packages."
        });
    }
}
