using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Authentication controller for JWT token generation and validation.
/// Supports login and token validation endpoints.
/// </summary>
[ApiController]
[Route("api/v1/[controller]")]
[Produces("application/json")]
public class AuthController : ControllerBase
{
    private readonly IAuthService _authService;
    private readonly ILogger<AuthController> _logger;

    public AuthController(
        IAuthService authService,
        ILogger<AuthController> logger)
    {
        _authService = authService;
        _logger = logger;
    }

    /// <summary>
    /// Authenticates a user and generates a JWT token.
    /// </summary>
    /// <param name="request">Login credentials (username and password)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>JWT token and authentication details</returns>
    /// <response code="200">Authentication successful, token generated</response>
    /// <response code="400">Invalid request format</response>
    /// <response code="401">Invalid credentials</response>
    /// <response code="500">Internal server error</response>
    /// <remarks>
    /// Sample request:
    ///
    ///     POST /api/v1/auth/login
    ///     {
    ///       "username": "admin",
    ///       "password": "admin123"
    ///     }
    ///
    /// For MVP, use one of the following test credentials:
    /// - admin / admin123
    /// - user / user123
    /// - teste.usuario / senha123
    ///
    /// Production: Will integrate with Caixa's Active Directory/LDAP.
    /// </remarks>
    [HttpPost("login")]
    [AllowAnonymous]
    [ProducesResponseType(typeof(LoginResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status400BadRequest)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status401Unauthorized)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<LoginResponse>> Login(
        [FromBody] LoginRequest request,
        CancellationToken cancellationToken)
    {
        try
        {
            if (!ModelState.IsValid)
            {
                _logger.LogWarning("Invalid login request: {Errors}", ModelState.Values);
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Requisição inválida",
                    Details = string.Join("; ", ModelState.Values
                        .SelectMany(v => v.Errors)
                        .Select(e => e.ErrorMessage)),
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            _logger.LogInformation("Login attempt for user: {Username}", request.Username);

            var response = await _authService.AuthenticateAsync(
                request.Username,
                request.Password,
                cancellationToken);

            if (response == null)
            {
                _logger.LogWarning("Login failed: Invalid credentials for user {Username}", request.Username);
                return Unauthorized(new ErrorResponse
                {
                    StatusCode = 401,
                    Message = "Credenciais inválidas",
                    Details = "Nome de usuário ou senha incorretos",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            _logger.LogInformation("Login successful for user: {Username}", request.Username);
            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error during login for user: {Username}", request.Username);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Erro ao processar autenticação",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Validates the current JWT token.
    /// Requires a valid token in the Authorization header.
    /// </summary>
    /// <returns>Token validation result with expiration details</returns>
    /// <response code="200">Token is valid</response>
    /// <response code="401">Token is invalid or expired</response>
    /// <response code="500">Internal server error</response>
    /// <remarks>
    /// Sample request:
    ///
    ///     GET /api/v1/auth/validate
    ///     Authorization: Bearer {your-jwt-token}
    ///
    /// Returns validation details including:
    /// - Username
    /// - Token expiration time
    /// - Seconds remaining until expiration
    /// </remarks>
    [HttpGet("validate")]
    [Authorize]
    [ProducesResponseType(typeof(TokenValidationResult), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status401Unauthorized)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public IActionResult ValidateToken()
    {
        try
        {
            // Extract token from Authorization header
            var authHeader = Request.Headers.Authorization.ToString();
            if (string.IsNullOrEmpty(authHeader) || !authHeader.StartsWith("Bearer "))
            {
                _logger.LogWarning("Validation failed: Missing or invalid Authorization header");
                return Unauthorized(new ErrorResponse
                {
                    StatusCode = 401,
                    Message = "Token não fornecido",
                    Details = "Authorization header ausente ou inválido",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var token = authHeader["Bearer ".Length..].Trim();
            var validationResult = _authService.ValidateToken(token);

            if (!validationResult.IsValid)
            {
                _logger.LogWarning("Token validation failed: {ErrorMessage}", validationResult.ErrorMessage);
                return Unauthorized(new ErrorResponse
                {
                    StatusCode = 401,
                    Message = "Token inválido",
                    Details = validationResult.ErrorMessage ?? "Token inválido ou expirado",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            _logger.LogInformation(
                "Token validated successfully: Username={Username}, SecondsRemaining={SecondsRemaining}",
                validationResult.Username,
                validationResult.SecondsRemaining);

            return Ok(validationResult);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating token");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Erro ao validar token",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Health check endpoint for authentication service availability.
    /// </summary>
    /// <returns>Service health status</returns>
    /// <response code="200">Service is healthy</response>
    [HttpGet("health")]
    [AllowAnonymous]
    [ProducesResponseType(StatusCodes.Status200OK)]
    public IActionResult HealthCheck()
    {
        return Ok(new
        {
            Status = "Healthy",
            Service = "AuthService",
            Timestamp = DateTime.UtcNow
        });
    }
}
