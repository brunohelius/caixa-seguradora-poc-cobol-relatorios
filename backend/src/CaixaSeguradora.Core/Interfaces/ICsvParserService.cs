using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service for parsing CSV files and loading entity data into the database.
/// Supports all 15 entity types with header validation and type mapping.
/// </summary>
public interface ICsvParserService
{
    /// <summary>
    /// Parses CSV data from a stream and loads records into the database.
    /// </summary>
    /// <typeparam name="T">Entity type to parse (PremiumRecord, Policy, Client, etc.)</typeparam>
    /// <param name="csvStream">Stream containing CSV data</param>
    /// <param name="entityType">String identifier for entity type (e.g., "Premium", "Policy")</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Result containing success status, records loaded count, and any errors</returns>
    Task<CsvParseResult> ParseCsvAsync<T>(
        Stream csvStream,
        string entityType,
        CancellationToken cancellationToken = default
    ) where T : class, new();

    /// <summary>
    /// Validates CSV headers against expected entity schema.
    /// </summary>
    /// <param name="csvStream">Stream containing CSV data</param>
    /// <param name="entityType">String identifier for entity type</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Validation result with missing/extra headers</returns>
    Task<HeaderValidationResult> ValidateHeadersAsync(
        Stream csvStream,
        string entityType,
        CancellationToken cancellationToken = default
    );

    /// <summary>
    /// Maps CSV row data to entity object with type conversion and validation.
    /// </summary>
    /// <typeparam name="T">Entity type</typeparam>
    /// <param name="rowData">Dictionary of column name to value</param>
    /// <param name="rowNumber">Row number for error reporting</param>
    /// <returns>Mapped entity instance or null if mapping fails</returns>
    Task<EntityMappingResult<T>> MapToEntityAsync<T>(
        IDictionary<string, string> rowData,
        int rowNumber
    ) where T : class, new();

    /// <summary>
    /// Gets supported entity types for CSV import.
    /// </summary>
    /// <returns>List of supported entity type names</returns>
    IEnumerable<string> GetSupportedEntityTypes();
}
