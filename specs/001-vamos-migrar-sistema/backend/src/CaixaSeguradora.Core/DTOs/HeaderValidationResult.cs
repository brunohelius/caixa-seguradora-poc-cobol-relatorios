namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Result of CSV header validation.
/// </summary>
public class HeaderValidationResult
{
    /// <summary>
    /// Whether headers are valid.
    /// </summary>
    public bool IsValid { get; set; }

    /// <summary>
    /// Entity type being validated.
    /// </summary>
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Expected header columns.
    /// </summary>
    public List<string> ExpectedHeaders { get; set; } = new();

    /// <summary>
    /// Actual header columns found in CSV.
    /// </summary>
    public List<string> ActualHeaders { get; set; } = new();

    /// <summary>
    /// Required headers that are missing.
    /// </summary>
    public List<string> MissingHeaders { get; set; } = new();

    /// <summary>
    /// Extra headers not in schema.
    /// </summary>
    public List<string> ExtraHeaders { get; set; } = new();

    /// <summary>
    /// Validation message.
    /// </summary>
    public string Message { get; set; } = string.Empty;
}
