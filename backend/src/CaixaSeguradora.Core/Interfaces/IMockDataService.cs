using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for managing mock database data.
/// Supports User Story 5 - Load, validate, and manage test data for migration testing.
/// </summary>
public interface IMockDataService
{
    /// <summary>
    /// Loads mock data from a file (CSV or JSON) into the database.
    /// </summary>
    /// <param name="request">Load request parameters including entity type, format, and options</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Load response with statistics and validation results</returns>
    Task<MockDataLoadResponse> LoadDataAsync(
        MockDataLoadRequest request,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Loads mock data from a stream (for file uploads).
    /// </summary>
    /// <param name="stream">File stream</param>
    /// <param name="entityType">Entity type to load (e.g., "premiums", "policies")</param>
    /// <param name="format">Data format (CSV or JSON)</param>
    /// <param name="clearExisting">Whether to clear existing data first</param>
    /// <param name="validateForeignKeys">Whether to validate foreign keys after loading</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Load response with statistics</returns>
    Task<MockDataLoadResponse> LoadDataFromStreamAsync(
        Stream stream,
        string entityType,
        DataFormat format,
        bool clearExisting = false,
        bool validateForeignKeys = true,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates all data in the database for integrity and foreign key relationships.
    /// </summary>
    /// <param name="entityType">Specific entity type to validate, or null for all entities</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Comprehensive validation report</returns>
    Task<DataValidationResponse> ValidateDataAsync(
        string? entityType = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Clears all data from a specific entity table.
    /// </summary>
    /// <param name="entityType">Entity type to clear (e.g., "premiums")</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Number of records deleted</returns>
    Task<int> ClearEntityDataAsync(
        string entityType,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Resets the entire database to empty state (clears all tables).
    /// USE WITH CAUTION - This deletes all test data.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Total number of records deleted across all tables</returns>
    Task<int> ResetDatabaseAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets current record counts for all entity tables.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Dictionary mapping entity type to record count</returns>
    Task<Dictionary<string, int>> GetRecordCountsAsync(
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets database schema information for a specific entity or all entities.
    /// </summary>
    /// <param name="entityType">Entity type to inspect, or null for all entities</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Schema information including columns, types, and constraints</returns>
    Task<Dictionary<string, object>> GetSchemaInfoAsync(
        string? entityType = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Exports entity data to CSV format.
    /// </summary>
    /// <param name="entityType">Entity type to export</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV file content as byte array</returns>
    Task<byte[]> ExportEntityDataAsync(
        string entityType,
        CancellationToken cancellationToken = default);
}
