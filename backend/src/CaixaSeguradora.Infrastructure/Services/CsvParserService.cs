using System.Diagnostics;
using System.Globalization;
using System.Text;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Service for parsing CSV files and loading entity data into the database.
/// Uses manual CSV parsing to avoid external dependencies.
/// </summary>
public class CsvParserService : ICsvParserService
{
    private readonly PremiumReportingDbContext _context;
    private readonly ILogger<CsvParserService> _logger;

    // Map of entity type names to their schema definitions
    private readonly Dictionary<string, Type> _entityTypeMap = new()
    {
        { "Premium", typeof(PremiumRecord) },
        { "Policy", typeof(Policy) },
        { "Endorsement", typeof(Endorsement) },
        { "Product", typeof(Product) },
        { "Client", typeof(Client) },
        { "Address", typeof(Address) },
        { "Agency", typeof(Agency) },
        { "Producer", typeof(Producer) },
        { "Coverage", typeof(Coverage) },
        { "Invoice", typeof(Invoice) },
        { "Installment", typeof(Installment) },
        { "CossuredPolicy", typeof(CossuredPolicy) },
        { "CossuranceCalculation", typeof(CossuranceCalculation) },
        { "SystemConfiguration", typeof(SystemConfiguration) },
        { "ReportDefinition", typeof(ReportDefinition) }
    };

    public CsvParserService(
        PremiumReportingDbContext context,
        ILogger<CsvParserService> logger)
    {
        _context = context;
        _logger = logger;
    }

    public async Task<CsvParseResult> ParseCsvAsync<T>(
        Stream csvStream,
        string entityType,
        CancellationToken cancellationToken = default
    ) where T : class, new()
    {
        var stopwatch = Stopwatch.StartNew();
        var result = new CsvParseResult
        {
            EntityType = entityType,
            Success = false
        };

        try
        {
            _logger.LogInformation("Starting CSV parse for entity type: {EntityType}", entityType);

            // Validate entity type
            if (!_entityTypeMap.ContainsKey(entityType))
            {
                result.Message = $"Entity type '{entityType}' is not supported.";
                return result;
            }

            // Reset stream position
            if (csvStream.CanSeek)
            {
                csvStream.Position = 0;
            }

            // Parse CSV with manual parser
            var records = new List<T>();
            using var reader = new StreamReader(csvStream, Encoding.UTF8, leaveOpen: true);

            // Read header
            var headerLine = await reader.ReadLineAsync(cancellationToken);
            if (string.IsNullOrWhiteSpace(headerLine))
            {
                result.Message = "CSV file is empty or has no header.";
                return result;
            }

            var headers = ParseCsvLine(headerLine);
            _logger.LogDebug("CSV headers: {Headers}", string.Join(", ", headers));

            // Parse data rows
            int rowNumber = 2; // Start at 2 (1 is header)
            string? line;
            while ((line = await reader.ReadLineAsync(cancellationToken)) != null)
            {
                if (string.IsNullOrWhiteSpace(line))
                {
                    continue;
                }

                result.TotalRows++;

                try
                {
                    var values = ParseCsvLine(line);
                    var rowData = headers.Zip(values, (h, v) => new { Header = h, Value = v })
                        .ToDictionary(x => x.Header, x => x.Value);

                    var mappingResult = await MapToEntityAsync<T>(rowData, rowNumber);

                    if (mappingResult.Success && mappingResult.Entity != null)
                    {
                        records.Add(mappingResult.Entity);
                    }
                    else
                    {
                        result.RecordsFailed++;
                        foreach (var error in mappingResult.Errors)
                        {
                            result.Errors.Add(new CsvParseError
                            {
                                RowNumber = rowNumber,
                                ErrorMessage = error,
                                Severity = "Error"
                            });
                        }
                    }
                }
                catch (Exception ex)
                {
                    result.RecordsFailed++;
                    result.Errors.Add(new CsvParseError
                    {
                        RowNumber = rowNumber,
                        ErrorMessage = $"Error parsing row: {ex.Message}",
                        Severity = "Error"
                    });
                    _logger.LogWarning(ex, "Error parsing CSV row {RowNumber}", rowNumber);
                }

                rowNumber++;
            }

            // Bulk insert records
            if (records.Any())
            {
                _logger.LogInformation("Inserting {Count} records into database", records.Count);
                await _context.Set<T>().AddRangeAsync(records, cancellationToken);
                await _context.SaveChangesAsync(cancellationToken);
                result.RecordsLoaded = records.Count;
            }

            result.Success = result.RecordsLoaded > 0;
            result.Message = result.Success
                ? $"Successfully loaded {result.RecordsLoaded} of {result.TotalRows} records."
                : "No records were loaded.";

            stopwatch.Stop();
            result.ElapsedTime = stopwatch.Elapsed;

            _logger.LogInformation(
                "CSV parse completed: {RecordsLoaded} loaded, {RecordsFailed} failed, elapsed: {Elapsed}ms",
                result.RecordsLoaded, result.RecordsFailed, result.ElapsedTime.TotalMilliseconds);

            return result;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error parsing CSV file for entity type: {EntityType}", entityType);
            result.Message = $"Error parsing CSV: {ex.Message}";
            result.Success = false;
            return result;
        }
    }

    public async Task<HeaderValidationResult> ValidateHeadersAsync(
        Stream csvStream,
        string entityType,
        CancellationToken cancellationToken = default
    )
    {
        var result = new HeaderValidationResult
        {
            EntityType = entityType,
            IsValid = false
        };

        try
        {
            // Validate entity type
            if (!_entityTypeMap.TryGetValue(entityType, out var type))
            {
                result.Message = $"Entity type '{entityType}' is not supported.";
                return result;
            }

            // Get expected headers from entity properties
            var expectedHeaders = GetExpectedHeaders(type);
            result.ExpectedHeaders = expectedHeaders;

            // Reset stream position
            if (csvStream.CanSeek)
            {
                csvStream.Position = 0;
            }

            // Read actual headers from CSV
            using var reader = new StreamReader(csvStream, Encoding.UTF8, leaveOpen: true);
            var headerLine = await reader.ReadLineAsync(cancellationToken);

            if (string.IsNullOrWhiteSpace(headerLine))
            {
                result.Message = "CSV file is empty or has no header.";
                return result;
            }

            var actualHeaders = ParseCsvLine(headerLine);
            result.ActualHeaders = actualHeaders;

            // Find missing and extra headers
            result.MissingHeaders = expectedHeaders.Except(actualHeaders, StringComparer.OrdinalIgnoreCase).ToList();
            result.ExtraHeaders = actualHeaders.Except(expectedHeaders, StringComparer.OrdinalIgnoreCase).ToList();

            result.IsValid = !result.MissingHeaders.Any();
            result.Message = result.IsValid
                ? "Headers are valid."
                : $"Missing required headers: {string.Join(", ", result.MissingHeaders)}";

            if (result.ExtraHeaders.Any())
            {
                result.Message += $" Extra headers will be ignored: {string.Join(", ", result.ExtraHeaders)}";
            }

            return result;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating CSV headers for entity type: {EntityType}", entityType);
            result.Message = $"Error validating headers: {ex.Message}";
            return result;
        }
    }

    public async Task<EntityMappingResult<T>> MapToEntityAsync<T>(
        IDictionary<string, string> rowData,
        int rowNumber
    ) where T : class, new()
    {
        var result = new EntityMappingResult<T>
        {
            RowNumber = rowNumber,
            Success = false
        };

        try
        {
            var entity = new T();
            var entityType = typeof(T);
            var properties = entityType.GetProperties()
                .Where(p => p.CanWrite)
                .ToList();

            foreach (var property in properties)
            {
                // Skip primary keys (auto-generated)
                if (property.Name.EndsWith("Id") && property.PropertyType == typeof(long))
                {
                    continue;
                }

                // Try to find matching column in row data
                var columnName = rowData.Keys.FirstOrDefault(
                    k => k.Equals(property.Name, StringComparison.OrdinalIgnoreCase));

                if (columnName == null || !rowData.TryGetValue(columnName, out var value))
                {
                    // Optional: Skip missing values or set defaults
                    continue;
                }

                try
                {
                    if (string.IsNullOrWhiteSpace(value))
                    {
                        // Set null for nullable types, skip for non-nullable
                        if (Nullable.GetUnderlyingType(property.PropertyType) != null ||
                            property.PropertyType == typeof(string))
                        {
                            property.SetValue(entity, null);
                        }
                        continue;
                    }

                    // Type conversion
                    object? convertedValue = ConvertValue(value, property.PropertyType);
                    property.SetValue(entity, convertedValue);
                }
                catch (Exception ex)
                {
                    result.Errors.Add(
                        $"Column '{property.Name}': Failed to convert value '{value}' to type {property.PropertyType.Name}. {ex.Message}");
                }
            }

            if (!result.Errors.Any())
            {
                result.Success = true;
                result.Entity = entity;
            }

            return await Task.FromResult(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error mapping row {RowNumber} to entity type {EntityType}",
                rowNumber, typeof(T).Name);
            result.Errors.Add($"Mapping error: {ex.Message}");
            return result;
        }
    }

    public IEnumerable<string> GetSupportedEntityTypes()
    {
        return _entityTypeMap.Keys;
    }

    // Helper: Parse CSV line handling quoted fields
    private List<string> ParseCsvLine(string line)
    {
        var result = new List<string>();
        var inQuotes = false;
        var currentField = new StringBuilder();

        for (int i = 0; i < line.Length; i++)
        {
            char c = line[i];

            if (c == '"')
            {
                if (inQuotes && i + 1 < line.Length && line[i + 1] == '"')
                {
                    // Escaped quote
                    currentField.Append('"');
                    i++; // Skip next quote
                }
                else
                {
                    // Toggle quote state
                    inQuotes = !inQuotes;
                }
            }
            else if (c == ',' && !inQuotes)
            {
                // Field separator
                result.Add(currentField.ToString().Trim());
                currentField.Clear();
            }
            else
            {
                currentField.Append(c);
            }
        }

        // Add last field
        result.Add(currentField.ToString().Trim());
        return result;
    }

    // Helper: Get expected headers from entity type
    private List<string> GetExpectedHeaders(Type entityType)
    {
        return entityType.GetProperties()
            .Where(p => p.CanWrite && !p.Name.EndsWith("Id") || p.Name == "Id")
            .Select(p => p.Name)
            .ToList();
    }

    // Helper: Convert string value to target type
    private object? ConvertValue(string value, Type targetType)
    {
        // Handle nullable types
        var underlyingType = Nullable.GetUnderlyingType(targetType) ?? targetType;

        if (underlyingType == typeof(string))
        {
            return value;
        }

        if (underlyingType == typeof(int))
        {
            return int.Parse(value, CultureInfo.InvariantCulture);
        }

        if (underlyingType == typeof(long))
        {
            return long.Parse(value, CultureInfo.InvariantCulture);
        }

        if (underlyingType == typeof(decimal))
        {
            return decimal.Parse(value, CultureInfo.InvariantCulture);
        }

        if (underlyingType == typeof(DateTime))
        {
            return DateTime.Parse(value, CultureInfo.InvariantCulture);
        }

        if (underlyingType == typeof(bool))
        {
            return bool.Parse(value);
        }

        if (underlyingType == typeof(double))
        {
            return double.Parse(value, CultureInfo.InvariantCulture);
        }

        if (underlyingType.IsEnum)
        {
            return Enum.Parse(underlyingType, value, ignoreCase: true);
        }

        // Try generic conversion
        return Convert.ChangeType(value, underlyingType, CultureInfo.InvariantCulture);
    }
}
