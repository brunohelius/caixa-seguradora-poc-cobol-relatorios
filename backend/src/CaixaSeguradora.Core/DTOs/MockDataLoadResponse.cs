namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for mock data loading operations.
/// Returns detailed statistics and validation results.
/// </summary>
public class MockDataLoadResponse
{
    /// <summary>
    /// Whether the load operation was successful.
    /// </summary>
    public bool Success { get; set; }

    /// <summary>
    /// Human-readable message describing the operation result.
    /// </summary>
    public string Message { get; set; } = string.Empty;

    /// <summary>
    /// Entity type that was loaded.
    /// </summary>
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Number of records successfully inserted.
    /// </summary>
    public int RecordsInserted { get; set; }

    /// <summary>
    /// Number of records that failed validation.
    /// </summary>
    public int RecordsFailed { get; set; }

    /// <summary>
    /// Number of records that were updated (if applicable).
    /// </summary>
    public int RecordsUpdated { get; set; }

    /// <summary>
    /// Number of records that were deleted (if ClearExistingData was true).
    /// </summary>
    public int RecordsDeleted { get; set; }

    /// <summary>
    /// Total processing time in milliseconds.
    /// </summary>
    public long ProcessingTimeMs { get; set; }

    /// <summary>
    /// List of validation errors encountered during loading.
    /// Each error includes row number and error description.
    /// </summary>
    public List<DataValidationError> ValidationErrors { get; set; } = new();

    /// <summary>
    /// List of warnings (non-fatal issues) encountered.
    /// </summary>
    public List<string> Warnings { get; set; } = new();

    /// <summary>
    /// Foreign key validation results (if ValidateForeignKeys was true).
    /// </summary>
    public ForeignKeyValidationSummary? ForeignKeyValidation { get; set; }
}

/// <summary>
/// Represents a validation error for a specific data record.
/// </summary>
public class DataValidationError
{
    /// <summary>
    /// Row number in the source file (1-based).
    /// </summary>
    public int RowNumber { get; set; }

    /// <summary>
    /// Column name where the error occurred (if applicable).
    /// </summary>
    public string? ColumnName { get; set; }

    /// <summary>
    /// Value that caused the error.
    /// </summary>
    public string? InvalidValue { get; set; }

    /// <summary>
    /// Error message describing the validation failure.
    /// </summary>
    public string ErrorMessage { get; set; } = string.Empty;

    /// <summary>
    /// Error type (e.g., "TypeMismatch", "ForeignKeyViolation", "RequiredField").
    /// </summary>
    public string ErrorType { get; set; } = string.Empty;
}

/// <summary>
/// Summary of foreign key validation results.
/// </summary>
public class ForeignKeyValidationSummary
{
    /// <summary>
    /// Whether all foreign key validations passed.
    /// </summary>
    public bool AllValid { get; set; }

    /// <summary>
    /// Total number of foreign key relationships checked.
    /// </summary>
    public int TotalChecks { get; set; }

    /// <summary>
    /// Number of foreign key violations found.
    /// </summary>
    public int ViolationsFound { get; set; }

    /// <summary>
    /// List of specific foreign key violations.
    /// </summary>
    public List<ForeignKeyViolation> Violations { get; set; } = new();
}

/// <summary>
/// Represents a foreign key violation.
/// </summary>
public class ForeignKeyViolation
{
    /// <summary>
    /// Foreign key column name.
    /// </summary>
    public string ColumnName { get; set; } = string.Empty;

    /// <summary>
    /// Referenced table name.
    /// </summary>
    public string ReferencedTable { get; set; } = string.Empty;

    /// <summary>
    /// Foreign key value that doesn't exist in referenced table.
    /// </summary>
    public string InvalidValue { get; set; } = string.Empty;

    /// <summary>
    /// Number of records with this violation.
    /// </summary>
    public int AffectedRecords { get; set; }
}
