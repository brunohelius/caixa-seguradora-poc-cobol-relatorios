using System.Globalization;
using System.IO;
using System.Text;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Infrastructure.Data;
using CsvHelper;
using CsvHelper.Configuration;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;
using Microsoft.EntityFrameworkCore.Infrastructure;
using System.Linq;

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

    private static readonly string[] ProductRequiredColumns =
    {
        "ProductCode",
        "ProductName",
        "LineOfBusiness",
        "ProductType",
        "CompanyCode"
    };

    private static readonly Dictionary<string, IReadOnlyList<string>> RequiredCsvColumns = new(StringComparer.OrdinalIgnoreCase)
    {
        ["product"] = ProductRequiredColumns,
        ["products"] = ProductRequiredColumns
    };

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

            using var buffer = new MemoryStream();
            await stream.CopyToAsync(buffer, cancellationToken);

            if (stream.CanSeek)
            {
                stream.Seek(0, SeekOrigin.Begin);
            }

            if (buffer.Length == 0)
            {
                response.Success = false;
                response.Message = "Arquivo CSV vazio";
                return response;
            }

            buffer.Seek(0, SeekOrigin.Begin);

            if (hasHeaders)
            {
                if (!ValidateHeader(buffer, entityType, delimiter, response))
                {
                    return response;
                }

                buffer.Seek(0, SeekOrigin.Begin);
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
                    var parser = context.Context?.Parser;
                    response.ValidationErrors.Add(new DataValidationError
                    {
                        RowNumber = parser?.Row ?? 0,
                        ErrorMessage = $"Malformed CSV data: {context.RawRecord}",
                        ErrorType = "CsvParseError"
                    });
                }
            };

            using var reader = new StreamReader(buffer, Encoding.UTF8, detectEncodingFromByteOrderMarks: true, leaveOpen: true);
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

            response.Success = response.RecordsInserted > 0 && !response.ValidationErrors.Any();
            response.Message = response.Success
                ? $"Carregados {response.RecordsInserted} registros com sucesso"
                : response.ValidationErrors.Any()
                    ? $"Carga concluída com {response.ValidationErrors.Count} erros de validação"
                    : "Nenhum registro foi carregado";
        }
        catch (Exception ex) when (ex is not ArgumentException)
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
        catch (ArgumentException)
        {
            // Preserve original stack for callers that rely on ArgumentException
            throw;
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
            _ => 0
        };

        return deletedCount;
    }

    private static IReadOnlyList<string> GetRequiredColumns(string entityType)
    {
        if (string.IsNullOrWhiteSpace(entityType))
        {
            return Array.Empty<string>();
        }

        return RequiredCsvColumns.TryGetValue(entityType, out var columns)
            ? columns
            : Array.Empty<string>();
    }

    private bool ValidateHeader(
        Stream buffer,
        string entityType,
        char delimiter,
        MockDataLoadResponse response)
    {
        long originalPosition = buffer.Position;
        using var previewReader = new StreamReader(buffer, Encoding.UTF8, detectEncodingFromByteOrderMarks: true, leaveOpen: true);
        string? headerLine = previewReader.ReadLine();

        if (headerLine == null)
        {
            buffer.Seek(originalPosition, SeekOrigin.Begin);
            response.ValidationErrors.Add(new DataValidationError
            {
                RowNumber = 0,
                ErrorMessage = "Cabeçalho CSV não encontrado",
                ErrorType = "MissingHeader"
            });
            response.Success = false;
            response.Message = "CSV inválido: cabeçalho ausente";
            return false;
        }

        var requiredHeaders = GetRequiredColumns(entityType);
        if (requiredHeaders.Count > 0)
        {
            var headers = headerLine.Split(delimiter)
                .Select(h => h.Trim())
                .ToArray();

            var missingHeaders = requiredHeaders
                .Where(required => !headers.Any(h => string.Equals(h, required, StringComparison.OrdinalIgnoreCase)))
                .ToList();

            if (missingHeaders.Count > 0)
            {
                buffer.Seek(originalPosition, SeekOrigin.Begin);

                foreach (var column in missingHeaders)
                {
                    response.ValidationErrors.Add(new DataValidationError
                    {
                        RowNumber = 0,
                        ColumnName = column,
                        ErrorMessage = $"Coluna obrigatória ausente: {column}",
                        ErrorType = "MissingColumn"
                    });
                }

                response.Success = false;
                response.Message = $"CSV inválido. Colunas obrigatórias ausentes: {string.Join(", ", missingHeaders)}";
                return false;
            }
        }

        buffer.Seek(originalPosition, SeekOrigin.Begin);
        return true;
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
