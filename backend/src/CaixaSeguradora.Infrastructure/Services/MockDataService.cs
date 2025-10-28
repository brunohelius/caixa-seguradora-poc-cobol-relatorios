using System.Text;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using Microsoft.EntityFrameworkCore.Infrastructure;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Main service for managing mock database data.
/// Orchestrates CSV/JSON loading, validation, and database reset operations.
/// Supports User Story 5 - Mock Data Management.
/// </summary>
public class MockDataService : IMockDataService
{
    private readonly PremiumReportingDbContext _context;
    private readonly CsvDataLoader _csvLoader;
    private readonly JsonDataLoader _jsonLoader;
    private readonly IDataValidationService _validationService;
    private readonly ILogger<MockDataService> _logger;

    public MockDataService(
        PremiumReportingDbContext context,
        CsvDataLoader csvLoader,
        JsonDataLoader jsonLoader,
        IDataValidationService validationService,
        ILogger<MockDataService> logger)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _csvLoader = csvLoader ?? throw new ArgumentNullException(nameof(csvLoader));
        _jsonLoader = jsonLoader ?? throw new ArgumentNullException(nameof(jsonLoader));
        _validationService = validationService ?? throw new ArgumentNullException(nameof(validationService));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <inheritdoc />
    public async Task<MockDataLoadResponse> LoadDataAsync(
        MockDataLoadRequest request,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation(
            "Loading {Format} data for entity {EntityType}",
            request.Format, request.EntityType);

        // Decode base64 content if provided
        Stream? stream = null;
        if (!string.IsNullOrEmpty(request.FileContentBase64))
        {
            var bytes = Convert.FromBase64String(request.FileContentBase64);
            stream = new MemoryStream(bytes);
        }
        else if (!string.IsNullOrEmpty(request.RawDataContent))
        {
            var bytes = Encoding.UTF8.GetBytes(request.RawDataContent);
            stream = new MemoryStream(bytes);
        }

        if (stream == null)
        {
            return new MockDataLoadResponse
            {
                Success = false,
                Message = "Nenhum conteúdo de dados fornecido",
                EntityType = request.EntityType
            };
        }

        using (stream)
        {
            return await LoadDataFromStreamAsync(
                stream,
                request.EntityType,
                request.Format,
                request.ClearExistingData,
                request.ValidateForeignKeys,
                cancellationToken);
        }
    }

    /// <inheritdoc />
    public async Task<MockDataLoadResponse> LoadDataFromStreamAsync(
        Stream stream,
        string entityType,
        DataFormat format,
        bool clearExisting = false,
        bool validateForeignKeys = true,
        CancellationToken cancellationToken = default)
    {
        MockDataLoadResponse response;

        // Load data using appropriate loader
        if (format == DataFormat.Csv)
        {
            response = await _csvLoader.LoadCsvAsync(
                stream,
                entityType,
                clearExisting,
                delimiter: ',',
                hasHeaders: true,
                maxRecords: 0,
                cancellationToken);
        }
        else // JSON
        {
            response = await _jsonLoader.LoadJsonAsync(
                stream,
                entityType,
                clearExisting,
                maxRecords: 0,
                cancellationToken);
        }

        // Validate foreign keys if requested and load was successful
        if (validateForeignKeys && response.Success && response.RecordsInserted > 0)
        {
            _logger.LogInformation("Validating foreign keys for {EntityType}", entityType);
            try
            {
                ForeignKeyValidationSummary fkValidation = await _validationService.ValidateForeignKeysAsync(cancellationToken);
                response.ForeignKeyValidation = fkValidation;

                if (!fkValidation.AllValid)
                {
                    response.Warnings.Add($"Encontradas {fkValidation.ViolationsFound} violações de chave estrangeira");
                }
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Foreign key validation failed");
                response.Warnings.Add($"Validação de chaves estrangeiras falhou: {ex.Message}");
            }
        }

        return response;
    }

    /// <inheritdoc />
    public async Task<DataValidationResponse> ValidateDataAsync(
        string? entityType = null,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Validating data for entity: {EntityType}", entityType ?? "ALL");

        if (string.IsNullOrEmpty(entityType))
        {
            return await _validationService.ValidateAllDataAsync(cancellationToken);
        }

        EntityValidationResult entityResult = await _validationService.ValidateEntityAsync(entityType, cancellationToken);

        return new DataValidationResponse
        {
            IsValid = entityResult.Passed,
            Message = entityResult.Passed
                ? $"Validação bem-sucedida para {entityType}"
                : $"Validação falhou para {entityType}: {entityResult.InvalidRecords} registros inválidos",
            EntityResults = new List<EntityValidationResult> { entityResult },
            Statistics = new ValidationStatistics
            {
                TotalEntities = 1,
                TotalRecords = entityResult.TotalRecords,
                TotalErrors = entityResult.Errors.Count,
                PassRate = entityResult.TotalRecords > 0
                    ? (decimal)entityResult.ValidRecords / entityResult.TotalRecords * 100
                    : 100m
            }
        };
    }

    /// <inheritdoc />
    public async Task<int> ClearEntityDataAsync(
        string entityType,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Clearing data for entity: {EntityType}", entityType);

        var isRelational = _context.Database.IsRelational();

        var deletedCount = entityType switch
        {
            "PremiumRecord" or "premiums" => await DeleteAsync(
                "Premiums",
                () => _context.PremiumRecords,
                isRelational,
                cancellationToken),
            "Policy" or "policies" => await DeleteAsync(
                "Policies",
                () => _context.Policies,
                isRelational,
                cancellationToken),
            "Client" or "clients" => await DeleteAsync(
                "Clients",
                () => _context.Clients,
                isRelational,
                cancellationToken),
            "Product" or "products" => await DeleteAsync(
                "Products",
                () => _context.Products,
                isRelational,
                cancellationToken),
            "Endorsement" or "endorsements" => await DeleteAsync(
                "Endorsements",
                () => _context.Endorsements,
                isRelational,
                cancellationToken),
            "Coverage" or "coverages" => await DeleteAsync(
                "Coverages",
                () => _context.Coverages,
                isRelational,
                cancellationToken),
            "Address" or "addresses" => await DeleteAsync(
                "Addresses",
                () => _context.Addresses,
                isRelational,
                cancellationToken),
            "CossuranceCalculation" or "cossurance" => await DeleteAsync(
                "CossuranceCalculations",
                () => _context.CossuranceCalculations,
                isRelational,
                cancellationToken),
            "CossuredPolicy" or "cossured_policies" => await DeleteAsync(
                "CossuredPolicies",
                () => _context.CossuredPolicies,
                isRelational,
                cancellationToken),
            "Agency" or "agencies" => await DeleteAsync(
                "Agencies",
                () => _context.Agencies,
                isRelational,
                cancellationToken),
            "Producer" or "producers" => await DeleteAsync(
                "Producers",
                () => _context.Producers,
                isRelational,
                cancellationToken),
            "Installment" or "installments" => await DeleteAsync(
                "Installments",
                () => _context.Installments,
                isRelational,
                cancellationToken),
            "Invoice" or "invoices" => await DeleteAsync(
                "Invoices",
                () => _context.Invoices,
                isRelational,
                cancellationToken),
            "BatchJob" or "batchjobs" => await DeleteAsync(
                "BatchJobs",
                () => _context.BatchJobs,
                isRelational,
                cancellationToken),
            "BatchJobExecution" or "batchjobexecutions" => await DeleteAsync(
                "BatchJobExecutions",
                () => _context.BatchJobExecutions,
                isRelational,
                cancellationToken),
            "ReportDefinition" or "reportdefinitions" => await DeleteAsync(
                "ReportDefinitions",
                () => _context.ReportDefinitions,
                isRelational,
                cancellationToken),
            "SystemConfiguration" or "systemconfigurations" => await DeleteAsync(
                "SystemConfigurations",
                () => _context.SystemConfigurations,
                isRelational,
                cancellationToken),
            _ => throw new ArgumentException($"Tipo de entidade desconhecido: {entityType}", nameof(entityType))
        };

        _logger.LogInformation("Cleared {Count} records from {EntityType}", deletedCount, entityType);
        return deletedCount;
    }

    /// <inheritdoc />
    public async Task<int> ResetDatabaseAsync(CancellationToken cancellationToken = default)
    {
        _logger.LogWarning("Resetting entire database - ALL DATA WILL BE DELETED");

        var totalDeleted = 0;

        // Delete in correct order to respect foreign key constraints
        var entityTypes = new[]
        {
            "installments", "invoices", "cossured_policies", "cossurance",
            "coverages", "endorsements", "premiums", "policies",
            "addresses", "clients", "producers", "agencies", "products",
            "batchjobexecutions", "batchjobs", "reportdefinitions", "systemconfigurations"
        };

        foreach (var tableName in entityTypes)
        {
            try
            {
                var deleted = await ClearEntityDataAsync(tableName, cancellationToken);
                totalDeleted += deleted;
                _logger.LogInformation("Deleted {Count} records from {Table}", deleted, tableName);
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Failed to clear table {Table}", tableName);
            }
        }

        _logger.LogWarning("Database reset complete. Total records deleted: {Count}", totalDeleted);
        return totalDeleted;
    }

    /// <inheritdoc />
    public async Task<Dictionary<string, int>> GetRecordCountsAsync(
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Getting record counts for all entities");

        var counts = new Dictionary<string, int>
        {
            ["Premiums"] = await _context.PremiumRecords.CountAsync(cancellationToken),
            ["Policies"] = await _context.Policies.CountAsync(cancellationToken),
            ["Clients"] = await _context.Clients.CountAsync(cancellationToken),
            ["Products"] = await _context.Products.CountAsync(cancellationToken),
            ["Endorsements"] = await _context.Endorsements.CountAsync(cancellationToken),
            ["Coverages"] = await _context.Coverages.CountAsync(cancellationToken),
            ["Addresses"] = await _context.Addresses.CountAsync(cancellationToken),
            ["CossuranceCalculations"] = await _context.CossuranceCalculations.CountAsync(cancellationToken),
            ["CossuredPolicies"] = await _context.CossuredPolicies.CountAsync(cancellationToken),
            ["Agencies"] = await _context.Agencies.CountAsync(cancellationToken),
            ["Producers"] = await _context.Producers.CountAsync(cancellationToken),
            ["Installments"] = await _context.Installments.CountAsync(cancellationToken),
            ["Invoices"] = await _context.Invoices.CountAsync(cancellationToken),
            ["BatchJobs"] = await _context.BatchJobs.CountAsync(cancellationToken),
            ["BatchJobExecutions"] = await _context.BatchJobExecutions.CountAsync(cancellationToken)
        };

        return counts;
    }

    /// <inheritdoc />
    public async Task<Dictionary<string, object>> GetSchemaInfoAsync(
        string? entityType = null,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Getting schema info for: {EntityType}", entityType ?? "ALL");

        // This is a simplified implementation
        // SchemaInspectionService will provide more detailed schema information
        var schemaInfo = new Dictionary<string, object>();

        if (string.IsNullOrEmpty(entityType))
        {
            // Return all entity types
            schemaInfo["EntityTypes"] = new List<string>
            {
                "Premiums", "Policies", "Clients", "Products", "Endorsements",
                "Coverages", "Addresses", "CossuranceCalculations", "CossuredPolicies",
                "Agencies", "Producers", "Installments", "Invoices", "BatchJobs", "BatchJobExecutions"
            };
        }
        else
        {
            schemaInfo["EntityType"] = entityType;
            schemaInfo["Message"] = "Use SchemaInspectionService for detailed schema information";
        }

        return await Task.FromResult(schemaInfo);
    }

    /// <inheritdoc />
    public async Task<byte[]> ExportEntityDataAsync(
        string entityType,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Exporting data for entity: {EntityType}", entityType);

        // This would use the CsvExportService to export entity data
        // For now, return a simple implementation
        throw new NotImplementedException(
            $"Exportação de dados para {entityType} não implementada. Use o ExportController para exportação de dados.");
    }

    private async Task<int> DeleteAsync<TEntity>(
        string tableName,
        Func<DbSet<TEntity>> setAccessor,
        bool isRelationalProvider,
        CancellationToken cancellationToken) where TEntity : class
    {
        if (isRelationalProvider)
        {
            return await _context.Database.ExecuteSqlRawAsync($"DELETE FROM {tableName}", cancellationToken);
        }

        var set = setAccessor();
        var entities = await set.ToListAsync(cancellationToken);
        if (entities.Count == 0)
        {
            return 0;
        }

        set.RemoveRange(entities);
        await _context.SaveChangesAsync(cancellationToken);
        return entities.Count;
    }
}
