using System.Text.Json;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Service for loading JSON data into the database.
/// Supports User Story 5 - Mock Data Management.
/// Handles JSON arrays and single objects with flexible schema mapping.
/// </summary>
public class JsonDataLoader
{
    private readonly PremiumReportingDbContext _context;
    private readonly ILogger<JsonDataLoader> _logger;

    public JsonDataLoader(
        PremiumReportingDbContext context,
        ILogger<JsonDataLoader> logger)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// Loads JSON data from a stream into the specified entity table.
    /// Supports both JSON arrays and single objects.
    /// </summary>
    /// <param name="stream">JSON file stream</param>
    /// <param name="entityType">Entity type name (e.g., "PremiumRecord", "Policy")</param>
    /// <param name="clearExisting">Whether to clear existing data before loading</param>
    /// <param name="maxRecords">Maximum records to import (0 = unlimited)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Load response with statistics and validation errors</returns>
    public async Task<MockDataLoadResponse> LoadJsonAsync(
        Stream stream,
        string entityType,
        bool clearExisting = false,
        int maxRecords = 0,
        CancellationToken cancellationToken = default)
    {
        DateTime startTime = DateTime.UtcNow;
        var response = new MockDataLoadResponse
        {
            EntityType = entityType,
            Success = false
        };

        try
        {
            _logger.LogInformation("Starting JSON load for entity type: {EntityType}", entityType);

            // Clear existing data if requested
            if (clearExisting)
            {
                response.RecordsDeleted = await ClearEntityDataAsync(entityType, cancellationToken);
                _logger.LogInformation("Cleared {Count} existing records", response.RecordsDeleted);
            }

            // Configure JSON options
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                AllowTrailingCommas = true,
                ReadCommentHandling = JsonCommentHandling.Skip
            };

            // Load data based on entity type
            response.RecordsInserted = entityType switch
            {
                "PremiumRecord" or "premiums" => await LoadJsonEntityAsync<PremiumRecord>(stream, options, maxRecords, response, cancellationToken),
                "Policy" or "policies" => await LoadJsonEntityAsync<Policy>(stream, options, maxRecords, response, cancellationToken),
                "Client" or "clients" => await LoadJsonEntityAsync<Client>(stream, options, maxRecords, response, cancellationToken),
                "Product" or "products" => await LoadJsonEntityAsync<Product>(stream, options, maxRecords, response, cancellationToken),
                "Endorsement" or "endorsements" => await LoadJsonEntityAsync<Endorsement>(stream, options, maxRecords, response, cancellationToken),
                "Coverage" or "coverages" => await LoadJsonEntityAsync<Coverage>(stream, options, maxRecords, response, cancellationToken),
                "Address" or "addresses" => await LoadJsonEntityAsync<Address>(stream, options, maxRecords, response, cancellationToken),
                "CossuranceCalculation" or "cossurance" => await LoadJsonEntityAsync<CossuranceCalculation>(stream, options, maxRecords, response, cancellationToken),
                "CossuredPolicy" or "cossured_policies" => await LoadJsonEntityAsync<CossuredPolicy>(stream, options, maxRecords, response, cancellationToken),
                "Agency" or "agencies" => await LoadJsonEntityAsync<Agency>(stream, options, maxRecords, response, cancellationToken),
                "Producer" or "producers" => await LoadJsonEntityAsync<Producer>(stream, options, maxRecords, response, cancellationToken),
                "Installment" or "installments" => await LoadJsonEntityAsync<Installment>(stream, options, maxRecords, response, cancellationToken),
                "Invoice" or "invoices" => await LoadJsonEntityAsync<Invoice>(stream, options, maxRecords, response, cancellationToken),
                _ => throw new ArgumentException($"Tipo de entidade desconhecido: {entityType}", nameof(entityType))
            };

            response.Success = response.RecordsInserted > 0;
            response.Message = response.Success
                ? $"Carregados {response.RecordsInserted} registros com sucesso"
                : "Nenhum registro foi carregado";

            if (response.ValidationErrors.Any())
            {
                response.Message += $". {response.ValidationErrors.Count} erros de validação encontrados";
            }
        }
        catch (JsonException jsonEx)
        {
            _logger.LogError(jsonEx, "JSON parse error for entity type: {EntityType}", entityType);
            response.Success = false;
            response.Message = $"Erro ao analisar JSON: {jsonEx.Message}";
            response.ValidationErrors.Add(new DataValidationError
            {
                ErrorMessage = $"JSON inválido: {jsonEx.Message}",
                ErrorType = "JsonParseError"
            });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading JSON for entity type: {EntityType}", entityType);
            response.Success = false;
            response.Message = $"Erro ao carregar JSON: {ex.Message}";
            response.ValidationErrors.Add(new DataValidationError
            {
                ErrorMessage = ex.Message,
                ErrorType = "LoadException"
            });
        }
        finally
        {
            response.ProcessingTimeMs = (long)(DateTime.UtcNow - startTime).TotalMilliseconds;
        }

        _logger.LogInformation(
            "JSON load completed. Success: {Success}, Inserted: {Inserted}, Failed: {Failed}, Time: {Time}ms",
            response.Success, response.RecordsInserted, response.RecordsFailed, response.ProcessingTimeMs);

        return response;
    }

    /// <summary>
    /// Loads entity records from JSON stream.
    /// Supports both JSON arrays and single objects.
    /// </summary>
    private async Task<int> LoadJsonEntityAsync<TEntity>(
        Stream stream,
        JsonSerializerOptions options,
        int maxRecords,
        MockDataLoadResponse response,
        CancellationToken cancellationToken) where TEntity : class
    {
        // Read entire stream to detect if it's array or single object
        using var reader = new StreamReader(stream);
        var jsonContent = await reader.ReadToEndAsync(cancellationToken);

        JsonElement jsonElement = JsonSerializer.Deserialize<JsonElement>(jsonContent, options);

        List<TEntity> records;

        if (jsonElement.ValueKind == JsonValueKind.Array)
        {
            // JSON array
            records = JsonSerializer.Deserialize<List<TEntity>>(jsonContent, options) ?? new List<TEntity>();
        }
        else if (jsonElement.ValueKind == JsonValueKind.Object)
        {
            // Single JSON object
            TEntity? singleRecord = JsonSerializer.Deserialize<TEntity>(jsonContent, options);
            records = singleRecord != null ? new List<TEntity> { singleRecord } : new List<TEntity>();
        }
        else
        {
            throw new JsonException($"JSON inválido: esperado array ou objeto, encontrado {jsonElement.ValueKind}");
        }

        if (!records.Any())
        {
            response.Warnings.Add("Nenhum registro encontrado no JSON");
            return 0;
        }

        // Apply max records limit
        if (maxRecords > 0 && records.Count > maxRecords)
        {
            response.Warnings.Add($"Limite de {maxRecords} registros atingido. {records.Count - maxRecords} registros ignorados.");
            records = records.Take(maxRecords).ToList();
        }

        var insertedCount = 0;
        var rowNumber = 1;

        // Insert records in batches of 1000
        foreach (TEntity[] batch in records.Chunk(1000))
        {
            try
            {
                await _context.Set<TEntity>().AddRangeAsync(batch, cancellationToken);
                await _context.SaveChangesAsync(cancellationToken);
                insertedCount += batch.Length;
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Failed to insert batch starting at row {Row}", rowNumber);
                response.RecordsFailed += batch.Length;
                response.ValidationErrors.Add(new DataValidationError
                {
                    RowNumber = rowNumber,
                    ErrorMessage = ex.Message,
                    ErrorType = "BatchInsertError"
                });
            }

            rowNumber += batch.Length;
        }

        return insertedCount;
    }

    /// <summary>
    /// Clears all data from a specific entity table.
    /// </summary>
    private async Task<int> ClearEntityDataAsync(string entityType, CancellationToken cancellationToken)
    {
        var deletedCount = entityType switch
        {
            "PremiumRecord" or "premiums" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Premiums", cancellationToken),
            "Policy" or "policies" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Policies", cancellationToken),
            "Client" or "clients" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Clients", cancellationToken),
            "Product" or "products" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Products", cancellationToken),
            "Endorsement" or "endorsements" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Endorsements", cancellationToken),
            "Coverage" or "coverages" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Coverages", cancellationToken),
            "Address" or "addresses" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Addresses", cancellationToken),
            "CossuranceCalculation" or "cossurance" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM CossuranceCalculations", cancellationToken),
            "CossuredPolicy" or "cossured_policies" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM CossuredPolicies", cancellationToken),
            "Agency" or "agencies" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Agencies", cancellationToken),
            "Producer" or "producers" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Producers", cancellationToken),
            "Installment" or "installments" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Installments", cancellationToken),
            "Invoice" or "invoices" => await _context.Database.ExecuteSqlRawAsync("DELETE FROM Invoices", cancellationToken),
            _ => 0
        };

        return deletedCount;
    }
}
