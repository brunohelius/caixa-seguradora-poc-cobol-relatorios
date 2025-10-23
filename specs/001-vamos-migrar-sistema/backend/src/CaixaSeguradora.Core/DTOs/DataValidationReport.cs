namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Report of data validation results.
/// </summary>
public class DataValidationReport
{
    /// <summary>
    /// Whether all validations passed.
    /// </summary>
    public bool IsValid { get; set; }

    /// <summary>
    /// Timestamp when validation was performed.
    /// </summary>
    public DateTime ValidationTimestamp { get; set; } = DateTime.UtcNow;

    /// <summary>
    /// Total number of validations performed.
    /// </summary>
    public int TotalValidations { get; set; }

    /// <summary>
    /// Number of validations that passed.
    /// </summary>
    public int PassedValidations { get; set; }

    /// <summary>
    /// Number of validations that failed.
    /// </summary>
    public int FailedValidations { get; set; }

    /// <summary>
    /// Number of warnings (non-critical issues).
    /// </summary>
    public int WarningCount { get; set; }

    /// <summary>
    /// Detailed validation issues.
    /// </summary>
    public List<ValidationIssue> Issues { get; set; } = new();

    /// <summary>
    /// Statistics about validated entities.
    /// </summary>
    public Dictionary<string, EntityValidationStats> EntityStats { get; set; } = new();

    /// <summary>
    /// Summary message.
    /// </summary>
    public string Summary { get; set; } = string.Empty;

    /// <summary>
    /// Elapsed time for validation.
    /// </summary>
    public TimeSpan ElapsedTime { get; set; }
}

/// <summary>
/// Represents a validation issue found in the data.
/// </summary>
public class ValidationIssue
{
    /// <summary>
    /// Entity type where issue was found.
    /// </summary>
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Entity ID (if applicable).
    /// </summary>
    public string? EntityId { get; set; }

    /// <summary>
    /// Field/property name where issue was found.
    /// </summary>
    public string? FieldName { get; set; }

    /// <summary>
    /// Validation rule that was violated.
    /// </summary>
    public string ValidationRule { get; set; } = string.Empty;

    /// <summary>
    /// Description of the issue.
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Severity level (Error, Warning, Info).
    /// </summary>
    public ValidationSeverity Severity { get; set; } = ValidationSeverity.Error;

    /// <summary>
    /// Actual value that caused the issue.
    /// </summary>
    public string? ActualValue { get; set; }

    /// <summary>
    /// Expected value or constraint.
    /// </summary>
    public string? ExpectedValue { get; set; }
}

/// <summary>
/// Statistics for entity validation.
/// </summary>
public class EntityValidationStats
{
    /// <summary>
    /// Entity type name.
    /// </summary>
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Total number of records.
    /// </summary>
    public int TotalRecords { get; set; }

    /// <summary>
    /// Number of records with issues.
    /// </summary>
    public int RecordsWithIssues { get; set; }

    /// <summary>
    /// Number of errors found.
    /// </summary>
    public int ErrorCount { get; set; }

    /// <summary>
    /// Number of warnings found.
    /// </summary>
    public int WarningCount { get; set; }
}

/// <summary>
/// Severity levels for validation issues.
/// </summary>
public enum ValidationSeverity
{
    /// <summary>
    /// Informational message.
    /// </summary>
    Info = 0,

    /// <summary>
    /// Warning - data may be questionable but not invalid.
    /// </summary>
    Warning = 1,

    /// <summary>
    /// Error - data violates validation rules.
    /// </summary>
    Error = 2
}
