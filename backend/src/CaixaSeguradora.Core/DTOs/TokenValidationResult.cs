namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Result DTO for token validation.
/// </summary>
public class TokenValidationResult
{
    /// <summary>
    /// Indicates if the token is valid.
    /// </summary>
    public bool IsValid { get; set; }

    /// <summary>
    /// Username extracted from the token (if valid).
    /// </summary>
    public string? Username { get; set; }

    /// <summary>
    /// Token expiration timestamp (if valid).
    /// </summary>
    public DateTime? ExpiresAt { get; set; }

    /// <summary>
    /// Time remaining until expiration in seconds (if valid).
    /// </summary>
    public int? SecondsRemaining { get; set; }

    /// <summary>
    /// Validation error message (if invalid).
    /// </summary>
    public string? ErrorMessage { get; set; }

    /// <summary>
    /// Validation timestamp.
    /// </summary>
    public DateTime ValidatedAt { get; set; } = DateTime.UtcNow;
}
