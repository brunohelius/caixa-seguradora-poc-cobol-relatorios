using System.Globalization;
using System.Text;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Infrastructure.Data;
using CsvHelper;
using CsvHelper.Configuration;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using System.Runtime.CompilerServices;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Service for loading CSV data into the database.
/// Supports User Story 5 - Mock Data Management.
/// Uses CsvHelper library for robust CSV parsing with type conversion and validation.
/// </summary>
public class CsvDataLoader
{
    private readonly PremiumReportingDbContext _context;
    private readonly ILogger<CsvDataLoader> _logger;

    public CsvDataLoader(
        PremiumReportingDbContext context,
        ILogger<CsvDataLoader> logger)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// Loads CSV data from a stream into the specified entity table.
    /// </summary>
    /// <param name="stream">CSV file stream</param>
    /// <param name="entityType">Entity type name (e.g., "PremiumRecord", "Policy")</param>
    /// <param name="clearExisting">Whether to clear existing data before loading</param>
    /// <param name="delimiter">CSV delimiter character (default: comma)</param>
    /// <param name="hasHeaders">Whether CSV has header row</param>
    /// <param name="maxRecords">Maximum records to import (0 = unlimited)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Load response with statistics and validation errors</returns>
    public async Task<MockDataLoadResponse> LoadCsvAsync(
        Stream stream,
        string entityType,
        bool clearExisting = false,
        char delimiter = ',',
        bool hasHeaders = true,
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
            _logger.LogInformation("Starting CSV load for entity type: {EntityType}", entityType);

            // Clear existing data if requested
            if (clearExisting)
            {
                response.RecordsDeleted = await ClearEntityDataAsync(entityType, cancellationToken);
                _logger.LogInformation("Cleared {Count} existing records", response.RecordsDeleted);
            }

            // Configure CSV reader
            var config = new CsvConfiguration(CultureInfo.InvariantCulture)
            {
                Delimiter = delimiter.ToString(),
                HasHeaderRecord = hasHeaders,
                MissingFieldFound = null,
                HeaderValidated = null,
                BadDataFound = context =>
                {
                    response.ValidationErrors.Add(new DataValidationError
                    {
                        RowNumber = context.Context.Parser.Row,
                        ErrorMessage = $"Malformed CSV data: {context.RawRecord}",
                        ErrorType = "CsvParseError"
                    });
                }
            };

            using var reader = new StreamReader(stream, Encoding.UTF8);
            using var csv = new CsvReader(reader, config);

            // Load data based on entity type
            response.RecordsInserted = entityType switch
            {
                "PremiumRecord" or "premiums" => await LoadEntityAsync<PremiumRecord>(csv, maxRecords, response, cancellationToken),
                "Policy" or "policies" => await LoadEntityAsync<Policy>(csv, maxRecords, response, cancellationToken),
                "Client" or "clients" => await LoadEntityAsync<Client>(csv, maxRecords, response, cancellationToken),
                "Product" or "products" => await LoadEntityAsync<Product>(csv, maxRecords, response, cancellationToken),
                "Endorsement" or "endorsements" => await LoadEntityAsync<Endorsement>(csv, maxRecords, response, cancellationToken),
                "Coverage" or "coverages" => await LoadEntityAsync<Coverage>(csv, maxRecords, response, cancellationToken),
                "Address" or "addresses" => await LoadEntityAsync<Address>(csv, maxRecords, response, cancellationToken),
                "CossuranceCalculation" or "cossurance" => await LoadEntityAsync<CossuranceCalculation>(csv, maxRecords, response, cancellationToken),
                "CossuredPolicy" or "cossured_policies" => await LoadEntityAsync<CossuredPolicy>(csv, maxRecords, response, cancellationToken),
                "Agency" or "agencies" => await LoadEntityAsync<Agency>(csv, maxRecords, response, cancellationToken),
                "Producer" or "producers" => await LoadEntityAsync<Producer>(csv, maxRecords, response, cancellationToken),
                "Installment" or "installments" => await LoadEntityAsync<Installment>(csv, maxRecords, response, cancellationToken),
                "Invoice" or "invoices" => await LoadEntityAsync<Invoice>(csv, maxRecords, response, cancellationToken),
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
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error loading CSV for entity type: {EntityType}", entityType);
            response.Success = false;
            response.Message = $"Erro ao carregar CSV: {ex.Message}";
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
            "CSV load completed. Success: {Success}, Inserted: {Inserted}, Failed: {Failed}, Time: {Time}ms",
            response.Success, response.RecordsInserted, response.RecordsFailed, response.ProcessingTimeMs);

        return response;
    }

    /// <summary>
    /// Loads entity records from CSV reader.
    /// </summary>
    private async Task<int> LoadEntityAsync<TEntity>(
        CsvReader csv,
        int maxRecords,
        MockDataLoadResponse response,
        CancellationToken cancellationToken) where TEntity : class
    {
        var records = new List<TEntity>();
        var rowNumber = 1;

        // Disable auto-detect changes and lazy loading to prevent navigation property tracking issues
        var autoDetectChangesEnabled = _context.ChangeTracker.AutoDetectChangesEnabled;
        var lazyLoadingEnabled = _context.ChangeTracker.LazyLoadingEnabled;
        _context.ChangeTracker.AutoDetectChangesEnabled = false;
        _context.ChangeTracker.LazyLoadingEnabled = false;

        try
        {
            await foreach (TEntity record in csv.GetRecordsAsync<TEntity>(cancellationToken))
            {
                try
                {
                    records.Add(record);
                    rowNumber++;

                    // Check max records limit
                    if (maxRecords > 0 && records.Count >= maxRecords)
                    {
                        response.Warnings.Add($"Limite de {maxRecords} registros atingido. Dados restantes ignorados.");
                        break;
                    }

                    // Batch insert every 1000 records for performance
                    if (records.Count >= 1000)
                    {
                        await _context.Set<TEntity>().AddRangeAsync(records, cancellationToken);
                        await _context.SaveChangesAsync(cancellationToken);
                        records.Clear();

                        // Clear change tracker to prevent entity tracking conflicts
                        _context.ChangeTracker.Clear();
                    }
                }
                catch (Exception ex)
                {
                    _logger.LogWarning(ex, "Failed to load row {Row}", rowNumber);
                    response.RecordsFailed++;
                    response.ValidationErrors.Add(new DataValidationError
                    {
                        RowNumber = rowNumber,
                        ErrorMessage = ex.Message,
                        ErrorType = "RecordLoadError"
                    });
                }
            }

            // Insert remaining records
            if (records.Any())
            {
                await _context.Set<TEntity>().AddRangeAsync(records, cancellationToken);
                await _context.SaveChangesAsync(cancellationToken);

                // Clear change tracker to prevent entity tracking conflicts
                _context.ChangeTracker.Clear();
            }

            var insertedCount = rowNumber - 1 - response.RecordsFailed;
            return insertedCount;
        }
        finally
        {
            // Restore original settings
            _context.ChangeTracker.AutoDetectChangesEnabled = autoDetectChangesEnabled;
            _context.ChangeTracker.LazyLoadingEnabled = lazyLoadingEnabled;
        }
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
