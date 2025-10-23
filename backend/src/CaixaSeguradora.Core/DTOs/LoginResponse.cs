namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for successful authentication.
/// </summary>
public class LoginResponse
{
    /// <summary>
    /// JWT token for authenticated requests.
    /// </summary>
    public string Token { get; set; } = string.Empty;

    /// <summary>
    /// Token expiration time in seconds.
    /// </summary>
    public int ExpiresIn { get; set; }

    /// <summary>
    /// Authenticated username.
    /// </summary>
    public string Username { get; set; } = string.Empty;

    /// <summary>
    /// Token type (always "Bearer").
    /// </summary>
    public string TokenType { get; set; } = "Bearer";

    /// <summary>
    /// Token issue timestamp.
    /// </summary>
    public DateTime IssuedAt { get; set; }

    /// <summary>
    /// Token expiration timestamp.
    /// </summary>
    public DateTime ExpiresAt { get; set; }
}
