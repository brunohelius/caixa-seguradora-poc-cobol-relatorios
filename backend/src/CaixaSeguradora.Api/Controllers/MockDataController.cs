using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Controller for managing mock database data.
/// Supports User Story 5 - Load, validate, and manage test data for migration testing.
/// </summary>
[ApiController]
[Route("api/mock-data")]
[Produces("application/json")]
public class MockDataController : ControllerBase
{
    private readonly IMockDataService _mockDataService;
    private readonly ILogger<MockDataController> _logger;

    public MockDataController(
        IMockDataService mockDataService,
        ILogger<MockDataController> logger)
    {
        _mockDataService = mockDataService ?? throw new ArgumentNullException(nameof(mockDataService));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// Loads mock data from uploaded file (CSV or JSON).
    /// </summary>
    [HttpPost("load")]
    [Authorize] // Require authentication for data loading
    [ProducesResponseType(typeof(MockDataLoadResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    public async Task<IActionResult> LoadData(
        IFormFile file,
        [FromForm] string entityType,
        [FromForm] string format = "csv",
        [FromForm] bool clearExisting = false,
        CancellationToken cancellationToken = default)
    {
        try
        {
            if (file == null || file.Length == 0)
            {
                return BadRequest(new ErrorResponse { StatusCode = 400, Message = "Nenhum arquivo foi enviado", TraceId = HttpContext.TraceIdentifier });
            }

            if (string.IsNullOrWhiteSpace(entityType))
            {
                return BadRequest(new ErrorResponse { StatusCode = 400, Message = "Tipo de entidade é obrigatório", TraceId = HttpContext.TraceIdentifier });
            }

            _logger.LogInformation("Loading {Format} data for {EntityType}", format, entityType);

            DataFormat dataFormat = format.ToLowerInvariant() == "json" ? DataFormat.Json : DataFormat.Csv;
            using Stream stream = file.OpenReadStream();
            MockDataLoadResponse response = await _mockDataService.LoadDataFromStreamAsync(stream, entityType, dataFormat, clearExisting, true, cancellationToken);

            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading mock data");
            return StatusCode(500, new ErrorResponse { StatusCode = 500, Message = "Erro ao carregar dados", Details = ex.Message, TraceId = HttpContext.TraceIdentifier });
        }
    }

    /// <summary>
    /// Validates all data in the database.
    /// </summary>
    [HttpGet("validate")]
    [ProducesResponseType(typeof(DataValidationResponse), StatusCodes.Status200OK)]
    public async Task<IActionResult> ValidateData([FromQuery] string? entityType = null, CancellationToken cancellationToken = default)
    {
        try
        {
            DataValidationResponse response = await _mockDataService.ValidateDataAsync(entityType, cancellationToken);
            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating data");
            return StatusCode(500, new ErrorResponse { StatusCode = 500, Message = "Erro ao validar dados", Details = ex.Message, TraceId = HttpContext.TraceIdentifier });
        }
    }

    /// <summary>
    /// Clears all data from a specific entity table.
    /// </summary>
    [HttpDelete("clear/{entityType}")]
    [Authorize] // Require authentication for data deletion
    [ProducesResponseType(typeof(object), StatusCodes.Status200OK)]
    public async Task<IActionResult> ClearEntityData(string entityType, CancellationToken cancellationToken = default)
    {
        try
        {
            var deletedCount = await _mockDataService.ClearEntityDataAsync(entityType, cancellationToken);
            return Ok(new { Success = true, Message = $"{deletedCount} registros excluídos", DeletedCount = deletedCount, EntityType = entityType });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error clearing data");
            return StatusCode(500, new ErrorResponse { StatusCode = 500, Message = "Erro ao limpar dados", Details = ex.Message, TraceId = HttpContext.TraceIdentifier });
        }
    }

    /// <summary>
    /// Resets the entire database.
    /// </summary>
    [HttpPost("reset")]
    [Authorize] // Require authentication for database reset
    [ProducesResponseType(typeof(object), StatusCodes.Status200OK)]
    public async Task<IActionResult> ResetDatabase(CancellationToken cancellationToken = default)
    {
        try
        {
            var totalDeleted = await _mockDataService.ResetDatabaseAsync(cancellationToken);
            return Ok(new { Success = true, Message = $"Banco resetado. {totalDeleted} registros excluídos", TotalDeleted = totalDeleted });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error resetting database");
            return StatusCode(500, new ErrorResponse { StatusCode = 500, Message = "Erro ao resetar banco", Details = ex.Message, TraceId = HttpContext.TraceIdentifier });
        }
    }

    /// <summary>
    /// Gets current record counts for all entities.
    /// </summary>
    [HttpGet("stats")]
    [ProducesResponseType(typeof(Dictionary<string, int>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetStats(CancellationToken cancellationToken = default)
    {
        try
        {
            Dictionary<string, int> counts = await _mockDataService.GetRecordCountsAsync(cancellationToken);
            return Ok(new { Success = true, Counts = counts, TotalRecords = counts.Values.Sum() });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error getting stats");
            return StatusCode(500, new ErrorResponse { StatusCode = 500, Message = "Erro ao obter estatísticas", Details = ex.Message, TraceId = HttpContext.TraceIdentifier });
        }
    }

    /// <summary>
    /// Gets database schema information.
    /// </summary>
    [HttpGet("schema")]
    [ProducesResponseType(typeof(Dictionary<string, object>), StatusCodes.Status200OK)]
    public async Task<IActionResult> GetSchema([FromQuery] string? entityType = null, CancellationToken cancellationToken = default)
    {
        try
        {
            Dictionary<string, object> schema = await _mockDataService.GetSchemaInfoAsync(entityType, cancellationToken);
            return Ok(schema);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error getting schema");
            return StatusCode(500, new ErrorResponse { StatusCode = 500, Message = "Erro ao obter esquema", Details = ex.Message, TraceId = HttpContext.TraceIdentifier });
        }
    }
}
