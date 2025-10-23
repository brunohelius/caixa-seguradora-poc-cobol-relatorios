using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for authentication and authorization operations.
/// </summary>
public interface IAuthService
{
    /// <summary>
    /// Authenticates a user and generates a JWT token.
    /// </summary>
    /// <param name="username">Username</param>
    /// <param name="password">Password</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Login response with JWT token if authentication succeeds, null otherwise</returns>
    Task<LoginResponse?> AuthenticateAsync(
        string username,
        string password,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates a JWT token.
    /// </summary>
    /// <param name="token">JWT token to validate</param>
    /// <returns>Token validation result</returns>
    TokenValidationResult ValidateToken(string token);

    /// <summary>
    /// Extracts username from a valid JWT token.
    /// </summary>
    /// <param name="token">JWT token</param>
    /// <returns>Username if token is valid, null otherwise</returns>
    string? GetUsernameFromToken(string token);
}
