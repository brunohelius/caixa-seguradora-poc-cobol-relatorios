using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for validating database data integrity.
/// Performs comprehensive validation checks including foreign keys, data types, and business rules.
/// Supports User Story 5 - Mock Data Management.
/// </summary>
public interface IDataValidationService
{
    /// <summary>
    /// Validates all data in the database.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Comprehensive validation report</returns>
    Task<DataValidationResponse> ValidateAllDataAsync(
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates data for a specific entity type.
    /// </summary>
    /// <param name="entityType">Entity type to validate (e.g., "PremiumRecord", "Policy")</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Entity-specific validation results</returns>
    Task<EntityValidationResult> ValidateEntityAsync(
        string entityType,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates foreign key relationships across all entities.
    /// Checks that all foreign key values reference existing records in parent tables.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Foreign key validation summary</returns>
    Task<ForeignKeyValidationSummary> ValidateForeignKeysAsync(
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates database schema matches expected DB2 structure.
    /// Checks for missing tables, columns, and type mismatches.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Schema validation results</returns>
    Task<SchemaValidationResult> ValidateSchemaAsync(
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Performs data quality checks (null required fields, out-of-range values, etc.).
    /// </summary>
    /// <param name="entityType">Entity type to check, or null for all entities</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of data quality issues found</returns>
    Task<List<DataQualityIssue>> PerformDataQualityChecksAsync(
        string? entityType = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates business rules specific to premium calculations.
    /// Examples: TotalPremiumNet must equal sum of components, percentages must add to 100, etc.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of business rule violations</returns>
    Task<List<DataValidationError>> ValidateBusinessRulesAsync(
        CancellationToken cancellationToken = default);
}
