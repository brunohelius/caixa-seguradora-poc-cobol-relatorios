namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Result of CSV parsing operation.
/// </summary>
public class CsvParseResult
{
    /// <summary>
    /// Whether the parsing operation succeeded.
    /// </summary>
    public bool Success { get; set; }

    /// <summary>
    /// Entity type that was parsed.
    /// </summary>
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Number of records successfully loaded.
    /// </summary>
    public int RecordsLoaded { get; set; }

    /// <summary>
    /// Number of records that failed validation.
    /// </summary>
    public int RecordsFailed { get; set; }

    /// <summary>
    /// Total rows processed (excluding header).
    /// </summary>
    public int TotalRows { get; set; }

    /// <summary>
    /// List of validation errors encountered during parsing.
    /// </summary>
    public List<CsvParseError> Errors { get; set; } = new();

    /// <summary>
    /// Elapsed time for parsing operation.
    /// </summary>
    public TimeSpan ElapsedTime { get; set; }

    /// <summary>
    /// Message describing the result.
    /// </summary>
    public string Message { get; set; } = string.Empty;
}

/// <summary>
/// Represents an error encountered during CSV parsing.
/// </summary>
public class CsvParseError
{
    /// <summary>
    /// Row number where error occurred.
    /// </summary>
    public int RowNumber { get; set; }

    /// <summary>
    /// Column name where error occurred (if applicable).
    /// </summary>
    public string? ColumnName { get; set; }

    /// <summary>
    /// Error message.
    /// </summary>
    public string ErrorMessage { get; set; } = string.Empty;

    /// <summary>
    /// Raw value that caused the error.
    /// </summary>
    public string? RawValue { get; set; }

    /// <summary>
    /// Error severity (Error, Warning).
    /// </summary>
    public string Severity { get; set; } = "Error";
}
