namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for data validation operations.
/// Provides comprehensive validation results for database integrity checks.
/// </summary>
public class DataValidationResponse
{
    /// <summary>
    /// Whether all validation checks passed.
    /// </summary>
    public bool IsValid { get; set; }

    /// <summary>
    /// Overall validation message summary.
    /// </summary>
    public string Message { get; set; } = string.Empty;

    /// <summary>
    /// Timestamp when validation was performed.
    /// </summary>
    public DateTime ValidationTimestamp { get; set; } = DateTime.UtcNow;

    /// <summary>
    /// Total validation time in milliseconds.
    /// </summary>
    public long ValidationTimeMs { get; set; }

    /// <summary>
    /// Results for each entity type validated.
    /// </summary>
    public List<EntityValidationResult> EntityResults { get; set; } = new();

    /// <summary>
    /// Foreign key validation results across all entities.
    /// </summary>
    public ForeignKeyValidationSummary? ForeignKeyValidation { get; set; }

    /// <summary>
    /// Schema validation results (type mismatches, missing columns, etc.).
    /// </summary>
    public SchemaValidationResult? SchemaValidation { get; set; }

    /// <summary>
    /// Data quality checks (null required fields, out-of-range values, etc.).
    /// </summary>
    public List<DataQualityIssue> DataQualityIssues { get; set; } = new();

    /// <summary>
    /// Overall statistics.
    /// </summary>
    public ValidationStatistics Statistics { get; set; } = new();
}

/// <summary>
/// Validation results for a specific entity type.
/// </summary>
public class EntityValidationResult
{
    /// <summary>
    /// Entity type name (e.g., "PremiumRecord", "Policy").
    /// </summary>
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Table name in database.
    /// </summary>
    public string TableName { get; set; } = string.Empty;

    /// <summary>
    /// Total number of records in entity.
    /// </summary>
    public int TotalRecords { get; set; }

    /// <summary>
    /// Number of valid records.
    /// </summary>
    public int ValidRecords { get; set; }

    /// <summary>
    /// Number of records with validation errors.
    /// </summary>
    public int InvalidRecords { get; set; }

    /// <summary>
    /// Whether this entity passed validation.
    /// </summary>
    public bool Passed { get; set; }

    /// <summary>
    /// Specific validation errors for this entity.
    /// </summary>
    public List<DataValidationError> Errors { get; set; } = new();
}

/// <summary>
/// Schema validation result.
/// </summary>
public class SchemaValidationResult
{
    /// <summary>
    /// Whether schema matches expected DB2 structure.
    /// </summary>
    public bool SchemaMatches { get; set; }

    /// <summary>
    /// Expected number of tables (from DB2 specification).
    /// </summary>
    public int ExpectedTables { get; set; }

    /// <summary>
    /// Actual number of tables in SQLite.
    /// </summary>
    public int ActualTables { get; set; }

    /// <summary>
    /// List of missing tables (expected but not found).
    /// </summary>
    public List<string> MissingTables { get; set; } = new();

    /// <summary>
    /// List of extra tables (found but not expected).
    /// </summary>
    public List<string> ExtraTables { get; set; } = new();

    /// <summary>
    /// Column mismatches by table.
    /// </summary>
    public Dictionary<string, ColumnMismatch> ColumnMismatches { get; set; } = new();
}

/// <summary>
/// Represents column schema mismatches for a table.
/// </summary>
public class ColumnMismatch
{
    /// <summary>
    /// Table name.
    /// </summary>
    public string TableName { get; set; } = string.Empty;

    /// <summary>
    /// Columns that are missing from SQLite.
    /// </summary>
    public List<string> MissingColumns { get; set; } = new();

    /// <summary>
    /// Columns with type mismatches.
    /// </summary>
    public List<TypeMismatch> TypeMismatches { get; set; } = new();
}

/// <summary>
/// Represents a column type mismatch.
/// </summary>
public class TypeMismatch
{
    /// <summary>
    /// Column name.
    /// </summary>
    public string ColumnName { get; set; } = string.Empty;

    /// <summary>
    /// Expected type (from DB2 specification).
    /// </summary>
    public string ExpectedType { get; set; } = string.Empty;

    /// <summary>
    /// Actual type in SQLite.
    /// </summary>
    public string ActualType { get; set; } = string.Empty;
}

/// <summary>
/// Represents a data quality issue.
/// </summary>
public class DataQualityIssue
{
    /// <summary>
    /// Issue severity (Info, Warning, Error, Critical).
    /// </summary>
    public string Severity { get; set; } = "Warning";

    /// <summary>
    /// Entity type affected.
    /// </summary>
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Column name affected.
    /// </summary>
    public string ColumnName { get; set; } = string.Empty;

    /// <summary>
    /// Issue description.
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Number of records affected.
    /// </summary>
    public int AffectedRecords { get; set; }

    /// <summary>
    /// Example values causing the issue (up to 5).
    /// </summary>
    public List<string> ExampleValues { get; set; } = new();
}

/// <summary>
/// Overall validation statistics.
/// </summary>
public class ValidationStatistics
{
    /// <summary>
    /// Total number of entities validated.
    /// </summary>
    public int TotalEntities { get; set; }

    /// <summary>
    /// Total number of records validated across all entities.
    /// </summary>
    public int TotalRecords { get; set; }

    /// <summary>
    /// Total number of validation checks performed.
    /// </summary>
    public int TotalChecks { get; set; }

    /// <summary>
    /// Total number of errors found.
    /// </summary>
    public int TotalErrors { get; set; }

    /// <summary>
    /// Total number of warnings.
    /// </summary>
    public int TotalWarnings { get; set; }

    /// <summary>
    /// Pass rate percentage (0-100).
    /// </summary>
    public decimal PassRate { get; set; }
}
