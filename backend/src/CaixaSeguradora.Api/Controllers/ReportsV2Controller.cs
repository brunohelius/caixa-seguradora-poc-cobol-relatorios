using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Reports controller for Phase 3 (US1) report generation.
/// Simplified version focusing on monthly report generation workflow.
/// </summary>
[ApiController]
[Route("api/v1/reports")]
[Produces("application/json")]
public class ReportsV2Controller : ControllerBase
{
    private readonly ReportOrchestrationService _orchestrationService;
    private readonly IExecutionTrackingService _executionTrackingService;
    private readonly ILogger<ReportsV2Controller> _logger;

    public ReportsV2Controller(
        ReportOrchestrationService orchestrationService,
        IExecutionTrackingService executionTrackingService,
        ILogger<ReportsV2Controller> logger)
    {
        _orchestrationService = orchestrationService;
        _executionTrackingService = executionTrackingService;
        _logger = logger;
    }

    /// <summary>
    /// Generates monthly premium report for specified month (YYYYMM format).
    /// Processes all premium records and returns execution ID for tracking.
    /// </summary>
    /// <param name="request">Report generation request with month</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>202 Accepted with execution details</returns>
    /// <response code="202">Report generation accepted and started</response>
    /// <response code="400">Invalid month format or future month</response>
    /// <response code="409">Already processing report for this month</response>
    /// <response code="500">Internal server error</response>
    /// <remarks>
    /// Sample request:
    ///
    ///     POST /api/v1/reports/generate
    ///     {
    ///       "month": "202510",
    ///       "reportType": "Both",
    ///       "executionMode": "Monthly",
    ///       "triggeringUser": "admin"
    ///     }
    ///
    /// Month format: YYYYMM (e.g., "202510" for October 2025)
    /// Valid reportType values: PREMIT, PREMCED, Both
    /// Valid executionMode values: Monthly, Weekly
    ///
    /// After receiving execution ID, poll GET /api/v1/reports/executions/{executionId} for status.
    /// </remarks>
    [HttpPost("generate")]
    [ProducesResponseType(typeof(ReportExecutionDto), StatusCodes.Status202Accepted)]
    [ProducesResponseType(typeof(ValidationErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status409Conflict)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ReportExecutionDto>> GenerateReport(
        [FromBody] GenerateReportRequest request,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation(
                "Solicitação de geração de relatório recebida: Mês={Month}, Tipo={ReportType}, Usuário={User}",
                request.Month,
                request.ReportType,
                request.TriggeringUser);

            // Validate month format
            if (!System.Text.RegularExpressions.Regex.IsMatch(request.Month, @"^\d{6}$"))
            {
                _logger.LogWarning("Formato de mês inválido: {Month}", request.Month);
                var validationResponse = new ValidationErrorResponse
                {
                    StatusCode = 400,
                    Message = "Formato de mês inválido",
                    Errors = new Dictionary<string, List<string>>
                    {
                        { "Month", new List<string> { "Mês deve estar no formato YYYYMM" } }
                    },
                    Timestamp = DateTime.UtcNow
                };
                return BadRequest(validationResponse);
            }

            // Check if already processing this month
            var hasActiveExecution = await CheckActiveExecutionAsync(request.Month, cancellationToken);
            if (hasActiveExecution)
            {
                _logger.LogWarning("Já existe execução ativa para o mês {Month}", request.Month);
                return Conflict(new ErrorResponse
                {
                    StatusCode = 409,
                    Message = "Já existe uma execução ativa para este mês",
                    Details = $"Aguarde a conclusão da execução atual para o mês {request.Month}",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            // Start report generation (async processing)
            var executionId = await _orchestrationService.GenerateReportAsync(request, cancellationToken);

            // Fetch execution details
            var execution = await _executionTrackingService.GetExecutionAsync(executionId, cancellationToken);

            if (execution == null)
            {
                throw new InvalidOperationException("Falha ao criar execução");
            }

            var response = new ReportExecutionDto
            {
                ExecutionId = execution.ExecutionId,
                ReferenceMonth = execution.ReferenceMonth,
                Status = execution.Status,
                StartTime = execution.StartTime,
                EndTime = execution.EndTime,
                RecordsProcessed = execution.RecordsProcessed,
                PremitRecordsGenerated = execution.PremitRecordsGenerated,
                PremcedRecordsGenerated = execution.PremcedRecordsGenerated,
                WarningsCount = execution.WarningsCount,
                ErrorsCount = execution.ErrorsCount,
                ReturnCode = execution.ReturnCode,
                TriggeringUser = execution.TriggeringUser,
                ReportType = execution.ReportType,
                ExecutionMode = execution.ExecutionMode
            };

            _logger.LogInformation("Relatório enfileirado: ExecutionId={ExecutionId}", executionId);

            return Accepted($"/api/v1/reports/executions/{executionId}", response);
        }
        catch (ArgumentException ex)
        {
            _logger.LogWarning(ex, "Parâmetros de requisição inválidos");
            return BadRequest(new ErrorResponse
            {
                StatusCode = 400,
                Message = "Parâmetros de requisição inválidos",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao iniciar geração de relatório");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Erro interno ao processar requisição",
                Details = "Consulte os logs do servidor para mais detalhes",
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets execution status and details by execution ID.
    /// Used for polling progress during async report generation.
    /// </summary>
    /// <param name="executionId">Execution ID (GUID)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Execution details with current status</returns>
    /// <response code="200">Execution found and details returned</response>
    /// <response code="404">Execution not found</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("executions/{executionId}")]
    [ProducesResponseType(typeof(ReportExecutionDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ReportExecutionDto>> GetExecutionStatus(
        Guid executionId,
        CancellationToken cancellationToken)
    {
        try
        {
            var execution = await _executionTrackingService.GetExecutionAsync(executionId, cancellationToken);

            if (execution == null)
            {
                _logger.LogWarning("Execução não encontrada: {ExecutionId}", executionId);
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Execução não encontrada",
                    Details = $"Nenhuma execução encontrada com ID {executionId}",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var response = new ReportExecutionDto
            {
                ExecutionId = execution.ExecutionId,
                ReferenceMonth = execution.ReferenceMonth,
                Status = execution.Status,
                StartTime = execution.StartTime,
                EndTime = execution.EndTime,
                RecordsProcessed = execution.RecordsProcessed,
                PremitRecordsGenerated = execution.PremitRecordsGenerated,
                PremcedRecordsGenerated = execution.PremcedRecordsGenerated,
                WarningsCount = execution.WarningsCount,
                ErrorsCount = execution.ErrorsCount,
                ReturnCode = execution.ReturnCode,
                TriggeringUser = execution.TriggeringUser,
                ReportType = execution.ReportType,
                ExecutionMode = execution.ExecutionMode
            };

            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao buscar status da execução {ExecutionId}", executionId);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Erro ao buscar status da execução",
                Details = "Consulte os logs do servidor para mais detalhes",
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets paginated execution history with optional filters.
    /// Returns list of all report generation executions.
    /// </summary>
    /// <param name="page">Page number (1-based, default: 1)</param>
    /// <param name="pageSize">Items per page (default: 20)</param>
    /// <param name="status">Filter by status (optional)</param>
    /// <param name="month">Filter by reference month (optional)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Paginated list of executions</returns>
    /// <response code="200">Execution history returned</response>
    /// <response code="400">Invalid pagination parameters</response>
    /// <response code="500">Internal server error</response>
    [HttpGet("executions")]
    [ProducesResponseType(typeof(ExecutionListResponseDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ExecutionListResponseDto>> GetExecutionHistory(
        [FromQuery] int page = 1,
        [FromQuery] int pageSize = 20,
        [FromQuery] string? status = null,
        [FromQuery] string? month = null,
        CancellationToken cancellationToken = default)
    {
        try
        {
            if (page < 1 || pageSize < 1 || pageSize > 100)
            {
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Parâmetros de paginação inválidos",
                    Details = "Page deve ser >= 1 e PageSize deve estar entre 1 e 100",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var (executions, totalCount) = await _executionTrackingService.GetExecutionHistoryAsync(
                page,
                pageSize,
                status,
                month,
                cancellationToken);

            var items = executions.Select(e => new ReportExecutionDto
            {
                ExecutionId = e.ExecutionId,
                ReferenceMonth = e.ReferenceMonth,
                Status = e.Status,
                StartTime = e.StartTime,
                EndTime = e.EndTime,
                RecordsProcessed = e.RecordsProcessed,
                PremitRecordsGenerated = e.PremitRecordsGenerated,
                PremcedRecordsGenerated = e.PremcedRecordsGenerated,
                WarningsCount = e.WarningsCount,
                ErrorsCount = e.ErrorsCount,
                ReturnCode = e.ReturnCode,
                TriggeringUser = e.TriggeringUser,
                ReportType = e.ReportType,
                ExecutionMode = e.ExecutionMode
            }).ToList();

            var response = new ExecutionListResponseDto
            {
                Items = items,
                TotalCount = totalCount,
                Page = page,
                PageSize = pageSize
            };

            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Erro ao buscar histórico de execuções");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Erro ao buscar histórico de execuções",
                Details = "Consulte os logs do servidor para mais detalhes",
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Checks if there is an active (Pending or Running) execution for the specified month.
    /// </summary>
    private async Task<bool> CheckActiveExecutionAsync(string month, CancellationToken cancellationToken)
    {
        var (executions, _) = await _executionTrackingService.GetExecutionHistoryAsync(
            1,
            10,
            status: null,
            referenceMonth: month,
            cancellationToken);

        return executions.Any(e => e.Status == "Pending" || e.Status == "Running");
    }
}
