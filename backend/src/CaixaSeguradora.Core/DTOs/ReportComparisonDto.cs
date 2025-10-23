namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// DTO for comparing .NET generated reports with legacy COBOL output.
/// Critical for migration validation and regulatory compliance verification.
/// </summary>
public class ReportComparisonDto
{
    /// <summary>
    /// Unique comparison identifier.
    /// </summary>
    public Guid ComparisonId { get; set; }

    /// <summary>
    /// Report ID of the .NET generated report being compared.
    /// </summary>
    public Guid ReportId { get; set; }

    /// <summary>
    /// File path of the COBOL output used for comparison.
    /// </summary>
    public string CobolFilePath { get; set; } = string.Empty;

    /// <summary>
    /// Timestamp when comparison was performed.
    /// </summary>
    public DateTime ComparedAt { get; set; } = DateTime.UtcNow;

    /// <summary>
    /// Overall comparison result.
    /// Values: ExactMatch, SemanticMatch, Mismatch
    /// </summary>
    /// <remarks>
    /// ExactMatch: Byte-for-byte identical (ideal for regulatory compliance)
    /// SemanticMatch: Business logic matches but formatting differs
    /// Mismatch: Critical differences found
    /// </remarks>
    public string ComparisonResult { get; set; } = "Mismatch";

    /// <summary>
    /// Whether files match byte-for-byte.
    /// True = 100% identical (regulatory requirement).
    /// </summary>
    public bool ByteLevelMatch { get; set; }

    /// <summary>
    /// Whether semantic totals match (premium amounts, record counts, etc.).
    /// </summary>
    public bool SemanticMatch { get; set; }

    /// <summary>
    /// Percentage of matching lines (0-100).
    /// </summary>
    public decimal LineMatchPercentage { get; set; }

    /// <summary>
    /// Total execution time for comparison in seconds.
    /// </summary>
    public double ComparisonTimeSeconds { get; set; }

    /// <summary>
    /// List of differences found.
    /// </summary>
    public List<ReportDifferenceDto> Differences { get; set; } = new();

    /// <summary>
    /// Summary statistics for the comparison.
    /// </summary>
    public ComparisonSummaryDto Summary { get; set; } = new();

    /// <summary>
    /// Semantic comparison of business logic totals.
    /// </summary>
    public SemanticComparisonDto? SemanticComparison { get; set; }

    /// <summary>
    /// Overall pass/fail status for regulatory compliance.
    /// True if byte-level match OR all semantic values match.
    /// </summary>
    public bool PassedValidation { get; set; }

    /// <summary>
    /// Validation error messages (if any).
    /// </summary>
    public List<string> ValidationErrors { get; set; } = new();
}

/// <summary>
/// Individual difference found during comparison.
/// </summary>
public class ReportDifferenceDto
{
    /// <summary>
    /// Difference type.
    /// Values: ByteDifference, LineDifference, SemanticDifference, FormatDifference
    /// </summary>
    public string DifferenceType { get; set; } = string.Empty;

    /// <summary>
    /// Severity level.
    /// Values: Critical, Warning, Info
    /// </summary>
    /// <remarks>
    /// Critical: Affects business logic (premium amounts, counts)
    /// Warning: Format differences that may not affect compliance
    /// Info: Non-impactful differences (whitespace, timestamps)
    /// </remarks>
    public string Severity { get; set; } = "Warning";

    /// <summary>
    /// Line number where difference was found (1-based).
    /// </summary>
    public int? LineNumber { get; set; }

    /// <summary>
    /// Column/byte position where difference starts (0-based).
    /// </summary>
    public int? ColumnPosition { get; set; }

    /// <summary>
    /// Field name or description of the differing field.
    /// </summary>
    public string? FieldName { get; set; }

    /// <summary>
    /// Expected value (from COBOL output).
    /// </summary>
    public string? ExpectedValue { get; set; }

    /// <summary>
    /// Actual value (from .NET output).
    /// </summary>
    public string? ActualValue { get; set; }

    /// <summary>
    /// Detailed description of the difference.
    /// </summary>
    public string Description { get; set; } = string.Empty;

    /// <summary>
    /// Context lines around the difference (for better understanding).
    /// </summary>
    public List<string> ContextLines { get; set; } = new();
}

/// <summary>
/// Summary statistics for file comparison.
/// </summary>
public class ComparisonSummaryDto
{
    /// <summary>
    /// Total lines in .NET output.
    /// </summary>
    public int TotalLinesNet { get; set; }

    /// <summary>
    /// Total lines in COBOL output.
    /// </summary>
    public int TotalLinesCobol { get; set; }

    /// <summary>
    /// Number of matching lines.
    /// </summary>
    public int MatchingLines { get; set; }

    /// <summary>
    /// Number of differing lines.
    /// </summary>
    public int DifferingLines { get; set; }

    /// <summary>
    /// Total number of differences found.
    /// </summary>
    public int TotalDifferences { get; set; }

    /// <summary>
    /// Number of critical differences.
    /// </summary>
    public int CriticalDifferences { get; set; }

    /// <summary>
    /// Number of warning-level differences.
    /// </summary>
    public int WarningDifferences { get; set; }

    /// <summary>
    /// Number of info-level differences.
    /// </summary>
    public int InfoDifferences { get; set; }

    /// <summary>
    /// File size comparison.
    /// </summary>
    public long FileSizeDifferenceBytes { get; set; }
}

/// <summary>
/// Semantic comparison of business logic values.
/// Compares calculated totals and counts independent of formatting.
/// </summary>
public class SemanticComparisonDto
{
    /// <summary>
    /// Total records comparison.
    /// </summary>
    public ValueComparisonDto TotalRecords { get; set; } = new();

    /// <summary>
    /// Total net premium comparison.
    /// </summary>
    public ValueComparisonDto TotalNetPremium { get; set; } = new();

    /// <summary>
    /// Total gross premium comparison.
    /// </summary>
    public ValueComparisonDto TotalGrossPremium { get; set; } = new();

    /// <summary>
    /// Total IOF comparison.
    /// </summary>
    public ValueComparisonDto TotalIOF { get; set; } = new();

    /// <summary>
    /// Total commissions comparison.
    /// </summary>
    public ValueComparisonDto TotalCommissions { get; set; } = new();

    /// <summary>
    /// Total cossurance premium comparison.
    /// </summary>
    public ValueComparisonDto TotalCossurance { get; set; } = new();

    /// <summary>
    /// Emission count comparison.
    /// </summary>
    public ValueComparisonDto EmissionCount { get; set; } = new();

    /// <summary>
    /// Cancellation count comparison.
    /// </summary>
    public ValueComparisonDto CancellationCount { get; set; } = new();

    /// <summary>
    /// Whether all semantic values match.
    /// </summary>
    public bool AllValuesMatch { get; set; }

    /// <summary>
    /// List of semantic mismatches.
    /// </summary>
    public List<string> Mismatches { get; set; } = new();
}

/// <summary>
/// Comparison of a single value between COBOL and .NET output.
/// </summary>
public class ValueComparisonDto
{
    /// <summary>
    /// Field name being compared.
    /// </summary>
    public string FieldName { get; set; } = string.Empty;

    /// <summary>
    /// Expected value (from COBOL).
    /// </summary>
    public string ExpectedValue { get; set; } = string.Empty;

    /// <summary>
    /// Actual value (from .NET).
    /// </summary>
    public string ActualValue { get; set; } = string.Empty;

    /// <summary>
    /// Whether values match.
    /// </summary>
    public bool Match { get; set; }

    /// <summary>
    /// Absolute difference (for numeric values).
    /// </summary>
    public decimal? Difference { get; set; }

    /// <summary>
    /// Percentage difference (for numeric values).
    /// </summary>
    public decimal? PercentageDifference { get; set; }
}
