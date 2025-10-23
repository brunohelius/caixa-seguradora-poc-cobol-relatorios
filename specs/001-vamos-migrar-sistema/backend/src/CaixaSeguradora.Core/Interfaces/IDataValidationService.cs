using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service for validating data integrity and foreign key relationships in the database.
/// Ensures referential integrity across all entities.
/// </summary>
public interface IDataValidationService
{
    /// <summary>
    /// Validates foreign key relationships across all entities.
    /// Checks that Policy references exist for Premiums, Client references exist, etc.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Validation report with any foreign key violations</returns>
    Task<DataValidationReport> ValidateForeignKeysAsync(
        CancellationToken cancellationToken = default
    );

    /// <summary>
    /// Validates data integrity rules (required fields, data ranges, business rules).
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Validation report with any integrity violations</returns>
    Task<DataValidationReport> ValidateDataIntegrityAsync(
        CancellationToken cancellationToken = default
    );

    /// <summary>
    /// Generates comprehensive validation report covering all validation checks.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Complete validation report</returns>
    Task<DataValidationReport> GetValidationReportAsync(
        CancellationToken cancellationToken = default
    );

    /// <summary>
    /// Validates a specific entity type.
    /// </summary>
    /// <param name="entityType">Entity type name (e.g., "Premium", "Policy")</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Validation report for the specific entity type</returns>
    Task<DataValidationReport> ValidateEntityTypeAsync(
        string entityType,
        CancellationToken cancellationToken = default
    );
}
