namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for validation errors.
/// Returns structured validation error information in Portuguese.
/// </summary>
public class ValidationErrorResponse
{
    /// <summary>
    /// Dictionary of field names to their validation error messages.
    /// Each field can have multiple error messages.
    /// </summary>
    public Dictionary<string, List<string>> Errors { get; set; } = new();

    /// <summary>
    /// HTTP status code (typically 400 for validation errors).
    /// </summary>
    public int StatusCode { get; set; } = 400;

    /// <summary>
    /// General error message describing the validation failure.
    /// </summary>
    public string Message { get; set; } = "Erro de validação nos dados fornecidos";

    /// <summary>
    /// Timestamp when the error occurred.
    /// </summary>
    public DateTime Timestamp { get; set; } = DateTime.UtcNow;

    /// <summary>
    /// Adds a validation error for a specific field.
    /// </summary>
    public void AddError(string fieldName, string errorMessage)
    {
        if (!Errors.ContainsKey(fieldName))
        {
            Errors[fieldName] = new List<string>();
        }

        Errors[fieldName].Add(errorMessage);
    }

    /// <summary>
    /// Gets the total number of validation errors across all fields.
    /// </summary>
    public int ErrorCount => Errors.Values.Sum(messages => messages.Count);
}
