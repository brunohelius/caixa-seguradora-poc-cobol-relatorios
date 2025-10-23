using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Controller for managing mock data loading and validation.
/// </summary>
[ApiController]
[Route("api/mock-data")]
[Produces("application/json")]
public class MockDataController : ControllerBase
{
    private readonly ICsvParserService _csvParserService;
    private readonly IDataValidationService _validationService;
    private readonly PremiumReportingDbContext _context;
    private readonly ILogger<MockDataController> _logger;

    public MockDataController(
        ICsvParserService csvParserService,
        IDataValidationService validationService,
        PremiumReportingDbContext context,
        ILogger<MockDataController> logger)
    {
        _csvParserService = csvParserService;
        _validationService = validationService;
        _context = context;
        _logger = logger;
    }

    /// <summary>
    /// Upload and load CSV data into the database.
    /// </summary>
    /// <param name="file">CSV file to upload</param>
    /// <param name="entityType">Entity type (Premium, Policy, Client, etc.)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Parse result with record counts and errors</returns>
    [HttpPost("load")]
    [ProducesResponseType(typeof(CsvParseResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> LoadMockData(
        IFormFile file,
        [FromForm] string entityType,
        CancellationToken cancellationToken)
    {
        try
        {
            if (file == null || file.Length == 0)
            {
                return BadRequest(new ErrorResponse
                {
                    Error = "InvalidFile",
                    Message = "No file was uploaded or file is empty."
                });
            }

            if (string.IsNullOrWhiteSpace(entityType))
            {
                return BadRequest(new ErrorResponse
                {
                    Error = "InvalidEntityType",
                    Message = "Entity type must be specified."
                });
            }

            // Validate entity type
            var supportedTypes = _csvParserService.GetSupportedEntityTypes().ToList();
            if (!supportedTypes.Contains(entityType, StringComparer.OrdinalIgnoreCase))
            {
                return BadRequest(new ErrorResponse
                {
                    Error = "UnsupportedEntityType",
                    Message = $"Entity type '{entityType}' is not supported. Supported types: {string.Join(", ", supportedTypes)}"
                });
            }

            _logger.LogInformation("Loading CSV data for entity type: {EntityType}, file: {FileName}",
                entityType, file.FileName);

            // Parse CSV based on entity type
            CsvParseResult result;
            await using (var stream = file.OpenReadStream())
            {
                result = entityType.ToLower() switch
                {
                    "premium" => await _csvParserService.ParseCsvAsync<PremiumRecord>(stream, entityType, cancellationToken),
                    "policy" => await _csvParserService.ParseCsvAsync<Policy>(stream, entityType, cancellationToken),
                    "endorsement" => await _csvParserService.ParseCsvAsync<Endorsement>(stream, entityType, cancellationToken),
                    "product" => await _csvParserService.ParseCsvAsync<Product>(stream, entityType, cancellationToken),
                    "client" => await _csvParserService.ParseCsvAsync<Client>(stream, entityType, cancellationToken),
                    "address" => await _csvParserService.ParseCsvAsync<Address>(stream, entityType, cancellationToken),
                    "agency" => await _csvParserService.ParseCsvAsync<Agency>(stream, entityType, cancellationToken),
                    "producer" => await _csvParserService.ParseCsvAsync<Producer>(stream, entityType, cancellationToken),
                    "coverage" => await _csvParserService.ParseCsvAsync<Coverage>(stream, entityType, cancellationToken),
                    "invoice" => await _csvParserService.ParseCsvAsync<Invoice>(stream, entityType, cancellationToken),
                    "installment" => await _csvParserService.ParseCsvAsync<Installment>(stream, entityType, cancellationToken),
                    "cossuredpolicy" => await _csvParserService.ParseCsvAsync<CossuredPolicy>(stream, entityType, cancellationToken),
                    "cossurancecalculation" => await _csvParserService.ParseCsvAsync<CossuranceCalculation>(stream, entityType, cancellationToken),
                    "systemconfiguration" => await _csvParserService.ParseCsvAsync<SystemConfiguration>(stream, entityType, cancellationToken),
                    "reportdefinition" => await _csvParserService.ParseCsvAsync<ReportDefinition>(stream, entityType, cancellationToken),
                    _ => new CsvParseResult
                    {
                        Success = false,
                        Message = $"Entity type '{entityType}' is not implemented."
                    }
                };
            }

            if (!result.Success)
            {
                _logger.LogWarning("CSV load failed for {EntityType}: {Message}",
                    entityType, result.Message);
            }

            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading mock data for entity type: {EntityType}", entityType);
            return StatusCode(500, new ErrorResponse
            {
                Error = "LoadError",
                Message = $"Error loading mock data: {ex.Message}"
            });
        }
    }

    /// <summary>
    /// Validate data integrity and foreign key relationships.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Validation report with any issues found</returns>
    [HttpPost("validate")]
    [ProducesResponseType(typeof(DataValidationReport), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ValidateData(CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Running data validation");

            var report = await _validationService.GetValidationReportAsync(cancellationToken);

            return Ok(report);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating data");
            return StatusCode(500, new ErrorResponse
            {
                Error = "ValidationError",
                Message = $"Error validating data: {ex.Message}"
            });
        }
    }

    /// <summary>
    /// Reset database by clearing all mock data.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Result with number of records deleted</returns>
    [HttpPost("reset")]
    [ProducesResponseType(typeof(DatabaseResetResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> ResetDatabase(CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Resetting database - clearing all mock data");

            var result = new DatabaseResetResult
            {
                Success = false,
                StartTime = DateTime.UtcNow
            };

            // Delete all records in reverse dependency order
            var deleteCounts = new Dictionary<string, int>();

            // Delete dependent entities first
            deleteCounts["CossuranceCalculation"] = await DeleteAllAsync<CossuranceCalculation>(cancellationToken);
            deleteCounts["CossuredPolicy"] = await DeleteAllAsync<CossuredPolicy>(cancellationToken);
            deleteCounts["Installment"] = await DeleteAllAsync<Installment>(cancellationToken);
            deleteCounts["Invoice"] = await DeleteAllAsync<Invoice>(cancellationToken);
            deleteCounts["Coverage"] = await DeleteAllAsync<Coverage>(cancellationToken);
            deleteCounts["PremiumRecord"] = await DeleteAllAsync<PremiumRecord>(cancellationToken);
            deleteCounts["Endorsement"] = await DeleteAllAsync<Endorsement>(cancellationToken);
            deleteCounts["Policy"] = await DeleteAllAsync<Policy>(cancellationToken);
            deleteCounts["Address"] = await DeleteAllAsync<Address>(cancellationToken);
            deleteCounts["Client"] = await DeleteAllAsync<Client>(cancellationToken);
            deleteCounts["Producer"] = await DeleteAllAsync<Producer>(cancellationToken);
            deleteCounts["Agency"] = await DeleteAllAsync<Agency>(cancellationToken);
            deleteCounts["Product"] = await DeleteAllAsync<Product>(cancellationToken);
            deleteCounts["ReportDefinition"] = await DeleteAllAsync<ReportDefinition>(cancellationToken);
            deleteCounts["SystemConfiguration"] = await DeleteAllAsync<SystemConfiguration>(cancellationToken);

            // Also delete batch jobs if any
            deleteCounts["BatchJobExecution"] = await DeleteAllAsync<BatchJobExecution>(cancellationToken);
            deleteCounts["BatchJob"] = await DeleteAllAsync<BatchJob>(cancellationToken);

            result.RecordsDeleted = deleteCounts;
            result.TotalRecordsDeleted = deleteCounts.Values.Sum();
            result.EndTime = DateTime.UtcNow;
            result.ElapsedTime = result.EndTime - result.StartTime;
            result.Success = true;
            result.Message = $"Successfully deleted {result.TotalRecordsDeleted} records from {deleteCounts.Count} tables.";

            _logger.LogInformation("Database reset completed: {TotalRecords} records deleted",
                result.TotalRecordsDeleted);

            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error resetting database");
            return StatusCode(500, new ErrorResponse
            {
                Error = "ResetError",
                Message = $"Error resetting database: {ex.Message}"
            });
        }
    }

    /// <summary>
    /// Get database statistics (record counts per entity type).
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Statistics with record counts</returns>
    [HttpGet("stats")]
    [ProducesResponseType(typeof(DatabaseStats), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<IActionResult> GetStats(CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Retrieving database statistics");

            var stats = new DatabaseStats
            {
                Timestamp = DateTime.UtcNow,
                EntityCounts = new Dictionary<string, int>
                {
                    ["Premium"] = await _context.PremiumRecords.CountAsync(cancellationToken),
                    ["Policy"] = await _context.Policies.CountAsync(cancellationToken),
                    ["Endorsement"] = await _context.Endorsements.CountAsync(cancellationToken),
                    ["Product"] = await _context.Products.CountAsync(cancellationToken),
                    ["Client"] = await _context.Clients.CountAsync(cancellationToken),
                    ["Address"] = await _context.Addresses.CountAsync(cancellationToken),
                    ["Agency"] = await _context.Agencies.CountAsync(cancellationToken),
                    ["Producer"] = await _context.Producers.CountAsync(cancellationToken),
                    ["Coverage"] = await _context.Coverages.CountAsync(cancellationToken),
                    ["Invoice"] = await _context.Invoices.CountAsync(cancellationToken),
                    ["Installment"] = await _context.Installments.CountAsync(cancellationToken),
                    ["CossuredPolicy"] = await _context.CossuredPolicies.CountAsync(cancellationToken),
                    ["CossuranceCalculation"] = await _context.CossuranceCalculations.CountAsync(cancellationToken),
                    ["SystemConfiguration"] = await _context.SystemConfigurations.CountAsync(cancellationToken),
                    ["ReportDefinition"] = await _context.ReportDefinitions.CountAsync(cancellationToken)
                }
            };

            stats.TotalRecords = stats.EntityCounts.Values.Sum();

            return Ok(stats);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error retrieving database statistics");
            return StatusCode(500, new ErrorResponse
            {
                Error = "StatsError",
                Message = $"Error retrieving statistics: {ex.Message}"
            });
        }
    }

    // Helper method to delete all records of a given type
    private async Task<int> DeleteAllAsync<T>(CancellationToken cancellationToken) where T : class
    {
        var entities = await _context.Set<T>().ToListAsync(cancellationToken);
        var count = entities.Count;

        if (count > 0)
        {
            _context.Set<T>().RemoveRange(entities);
            await _context.SaveChangesAsync(cancellationToken);
            _logger.LogDebug("Deleted {Count} records from {EntityType}", count, typeof(T).Name);
        }

        return count;
    }
}

/// <summary>
/// Result of database reset operation.
/// </summary>
public class DatabaseResetResult
{
    public bool Success { get; set; }
    public string Message { get; set; } = string.Empty;
    public int TotalRecordsDeleted { get; set; }
    public Dictionary<string, int> RecordsDeleted { get; set; } = new();
    public DateTime StartTime { get; set; }
    public DateTime EndTime { get; set; }
    public TimeSpan ElapsedTime { get; set; }
}

/// <summary>
/// Database statistics.
/// </summary>
public class DatabaseStats
{
    public DateTime Timestamp { get; set; }
    public int TotalRecords { get; set; }
    public Dictionary<string, int> EntityCounts { get; set; } = new();
}
