using System.Collections.Generic;
using System.Linq;

namespace CaixaSeguradora.Core.Models;

/// <summary>
/// Represents the result of business rule validation for a premium record.
/// Used during report processing to validate data quality and apply COBOL business rules.
/// FR-014 through FR-018: Data validation and business rules
/// </summary>
public class ValidationResult
{
    public bool IsValid => !Errors.Any();

    public List<ValidationError> Errors { get; set; } = new();

    public List<ValidationWarning> Warnings { get; set; } = new();

    public List<AutoCorrection> AutoCorrected { get; set; } = new();

    /// <summary>
    /// Adds an error to the validation result
    /// </summary>
    public void AddError(string errorCode, string message, string fieldName, long? policyNumber = null)
    {
        Errors.Add(new ValidationError
        {
            ErrorCode = errorCode,
            Message = message,
            FieldName = fieldName,
            PolicyNumber = policyNumber
        });
    }

    /// <summary>
    /// Adds a warning to the validation result
    /// </summary>
    public void AddWarning(string warningCode, string message, string fieldName, long? policyNumber = null)
    {
        Warnings.Add(new ValidationWarning
        {
            WarningCode = warningCode,
            Message = message,
            FieldName = fieldName,
            PolicyNumber = policyNumber
        });
    }

    /// <summary>
    /// Adds an auto-correction record
    /// </summary>
    public void AddAutoCorrection(string fieldName, object originalValue, object correctedValue, string reason, long? policyNumber = null)
    {
        AutoCorrected.Add(new AutoCorrection
        {
            FieldName = fieldName,
            OriginalValue = originalValue?.ToString() ?? "null",
            CorrectedValue = correctedValue?.ToString() ?? "null",
            Reason = reason,
            PolicyNumber = policyNumber
        });
    }
}

/// <summary>
/// Represents a validation error with context
/// </summary>
public class ValidationError
{
    /// <summary>
    /// Error code for categorization (e.g., ERR_INVALID_DATE, ERR_MISSING_BILHETE)
    /// </summary>
    public string ErrorCode { get; set; } = string.Empty;

    /// <summary>
    /// Portuguese error message
    /// </summary>
    public string Message { get; set; } = string.Empty;

    /// <summary>
    /// Field name that failed validation
    /// </summary>
    public string FieldName { get; set; } = string.Empty;

    /// <summary>
    /// Policy number for traceability (optional)
    /// </summary>
    public long? PolicyNumber { get; set; }
}

/// <summary>
/// Represents a validation warning (non-blocking issue)
/// </summary>
public class ValidationWarning
{
    /// <summary>
    /// Warning code for categorization
    /// </summary>
    public string WarningCode { get; set; } = string.Empty;

    /// <summary>
    /// Portuguese warning message
    /// </summary>
    public string Message { get; set; } = string.Empty;

    /// <summary>
    /// Field name that triggered warning
    /// </summary>
    public string FieldName { get; set; } = string.Empty;

    /// <summary>
    /// Policy number for traceability (optional)
    /// </summary>
    public long? PolicyNumber { get; set; }
}

/// <summary>
/// Represents an automatic correction applied during validation
/// COBOL equivalent: Auto-adjustments in paragraphs like R0800-20-VALIDA-DATAS
/// </summary>
public class AutoCorrection
{
    /// <summary>
    /// Field name that was corrected
    /// </summary>
    public string FieldName { get; set; } = string.Empty;

    /// <summary>
    /// Original value before correction
    /// </summary>
    public string OriginalValue { get; set; } = string.Empty;

    /// <summary>
    /// Corrected value applied
    /// </summary>
    public string CorrectedValue { get; set; } = string.Empty;

    /// <summary>
    /// Portuguese reason for correction
    /// </summary>
    public string Reason { get; set; } = string.Empty;

    /// <summary>
    /// Policy number for traceability (optional)
    /// </summary>
    public long? PolicyNumber { get; set; }
}
