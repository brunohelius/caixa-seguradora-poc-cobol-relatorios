using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.IdentityModel.Tokens;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Service implementation for JWT-based authentication and authorization.
/// For MVP: Uses hardcoded test users (admin/admin123, user/user123).
/// Production: Should integrate with Caixa's Active Directory/LDAP.
/// </summary>
public class AuthService : IAuthService
{
    private readonly IConfiguration _configuration;
    private readonly ILogger<AuthService> _logger;
    private readonly string _secretKey;
    private readonly string _issuer;
    private readonly string _audience;
    private readonly int _expirationMinutes;

    // Hardcoded test users for MVP
    // TODO: Replace with AD/LDAP integration for production
    private readonly Dictionary<string, string> _testUsers = new()
    {
        { "admin", "admin123" },
        { "user", "user123" },
        { "teste.usuario", "senha123" } // For compatibility with existing tests
    };

    public AuthService(
        IConfiguration configuration,
        ILogger<AuthService> logger)
    {
        _configuration = configuration;
        _logger = logger;

        // Load JWT configuration
        _secretKey = _configuration["Jwt:SecretKey"]
            ?? throw new InvalidOperationException("JWT SecretKey não configurada");
        _issuer = _configuration["Jwt:Issuer"] ?? "CaixaSeguradora";
        _audience = _configuration["Jwt:Audience"] ?? "CaixaSeguradora.API";
        _expirationMinutes = _configuration.GetValue<int>("Jwt:ExpirationMinutes", 60);

        _logger.LogInformation(
            "AuthService initialized: Issuer={Issuer}, Audience={Audience}, ExpirationMinutes={ExpirationMinutes}",
            _issuer,
            _audience,
            _expirationMinutes);
    }

    /// <inheritdoc/>
    public async Task<LoginResponse?> AuthenticateAsync(
        string username,
        string password,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Authentication attempt for user: {Username}", username);

        // Validate credentials against test users
        // TODO: Replace with AD/LDAP authentication for production
        if (!_testUsers.TryGetValue(username, out string? expectedPassword) ||
            expectedPassword != password)
        {
            _logger.LogWarning("Authentication failed for user: {Username}", username);
            return null;
        }

        // Generate JWT token
        var tokenHandler = new JwtSecurityTokenHandler();
        var key = Encoding.ASCII.GetBytes(_secretKey);
        var issuedAt = DateTime.UtcNow;
        var expiresAt = issuedAt.AddMinutes(_expirationMinutes);

        var tokenDescriptor = new SecurityTokenDescriptor
        {
            Subject = new ClaimsIdentity(new[]
            {
                new Claim(ClaimTypes.Name, username),
                new Claim(ClaimTypes.NameIdentifier, username),
                new Claim(JwtRegisteredClaimNames.Sub, username),
                new Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()),
                new Claim(JwtRegisteredClaimNames.Iat,
                    new DateTimeOffset(issuedAt).ToUnixTimeSeconds().ToString(),
                    ClaimValueTypes.Integer64)
            }),
            Expires = expiresAt,
            IssuedAt = issuedAt,
            Issuer = _issuer,
            Audience = _audience,
            SigningCredentials = new SigningCredentials(
                new SymmetricSecurityKey(key),
                SecurityAlgorithms.HmacSha256Signature)
        };

        var token = tokenHandler.CreateToken(tokenDescriptor);
        var tokenString = tokenHandler.WriteToken(token);

        _logger.LogInformation(
            "JWT token generated for user: {Username}, ExpiresAt: {ExpiresAt}",
            username,
            expiresAt);

        return await Task.FromResult(new LoginResponse
        {
            Token = tokenString,
            ExpiresIn = _expirationMinutes * 60, // Convert to seconds
            Username = username,
            TokenType = "Bearer",
            IssuedAt = issuedAt,
            ExpiresAt = expiresAt
        });
    }

    /// <inheritdoc/>
    public Core.DTOs.TokenValidationResult ValidateToken(string token)
    {
        try
        {
            var tokenHandler = new JwtSecurityTokenHandler();
            var key = Encoding.ASCII.GetBytes(_secretKey);

            var validationParameters = new TokenValidationParameters
            {
                ValidateIssuerSigningKey = true,
                IssuerSigningKey = new SymmetricSecurityKey(key),
                ValidateIssuer = true,
                ValidIssuer = _issuer,
                ValidateAudience = true,
                ValidAudience = _audience,
                ValidateLifetime = true,
                ClockSkew = TimeSpan.Zero // No tolerance for expired tokens
            };

            var principal = tokenHandler.ValidateToken(token, validationParameters, out var validatedToken);
            var jwtToken = (JwtSecurityToken)validatedToken;

            var username = principal.FindFirst(ClaimTypes.Name)?.Value;
            var expiresAt = jwtToken.ValidTo;
            var secondsRemaining = (int)(expiresAt - DateTime.UtcNow).TotalSeconds;

            _logger.LogDebug(
                "Token validated successfully: Username={Username}, SecondsRemaining={SecondsRemaining}",
                username,
                secondsRemaining);

            return new Core.DTOs.TokenValidationResult
            {
                IsValid = true,
                Username = username,
                ExpiresAt = expiresAt,
                SecondsRemaining = Math.Max(0, secondsRemaining)
            };
        }
        catch (SecurityTokenExpiredException ex)
        {
            _logger.LogWarning("Token validation failed: Token expired - {Message}", ex.Message);
            return new Core.DTOs.TokenValidationResult
            {
                IsValid = false,
                ErrorMessage = "Token expirado"
            };
        }
        catch (SecurityTokenException ex)
        {
            _logger.LogWarning("Token validation failed: {Message}", ex.Message);
            return new Core.DTOs.TokenValidationResult
            {
                IsValid = false,
                ErrorMessage = "Token inválido"
            };
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Unexpected error during token validation");
            return new Core.DTOs.TokenValidationResult
            {
                IsValid = false,
                ErrorMessage = "Erro ao validar token"
            };
        }
    }

    /// <inheritdoc/>
    public string? GetUsernameFromToken(string token)
    {
        try
        {
            var validationResult = ValidateToken(token);
            return validationResult.IsValid ? validationResult.Username : null;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error extracting username from token");
            return null;
        }
    }
}
