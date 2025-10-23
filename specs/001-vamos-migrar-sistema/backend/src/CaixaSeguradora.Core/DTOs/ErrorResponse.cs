namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Standard error response DTO for API error handling.
/// Provides consistent error structure across all endpoints.
/// </summary>
public class ErrorResponse
{
    /// <summary>
    /// HTTP status code (e.g., 400, 404, 500)
    /// </summary>
    public int StatusCode { get; set; }

    /// <summary>
    /// Error message in Portuguese for client display
    /// </summary>
    public string Message { get; set; } = string.Empty;

    /// <summary>
    /// Detailed error information for debugging (optional, only in development)
    /// </summary>
    public string? Details { get; set; }

    /// <summary>
    /// Unique error tracking identifier for support/logging correlation
    /// </summary>
    public string? TraceId { get; set; }

    /// <summary>
    /// Timestamp when the error occurred (ISO 8601 format)
    /// </summary>
    public string Timestamp { get; set; } = DateTime.UtcNow.ToString("O");

    /// <summary>
    /// Request path that generated the error
    /// </summary>
    public string? Path { get; set; }

    /// <summary>
    /// Validation errors for field-level validation failures (optional)
    /// </summary>
    public Dictionary<string, string[]>? ValidationErrors { get; set; }

    /// <summary>
    /// Error code for programmatic error handling (optional)
    /// </summary>
    public string? ErrorCode { get; set; }

    /// <summary>
    /// Creates a basic error response with status code and message.
    /// </summary>
    public static ErrorResponse Create(int statusCode, string message, string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = statusCode,
            Message = message,
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates an error response with validation errors.
    /// </summary>
    public static ErrorResponse CreateValidationError(Dictionary<string, string[]> validationErrors, string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = 400,
            Message = "Erro de validação. Verifique os campos informados.",
            ValidationErrors = validationErrors,
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates a detailed error response with exception details (for development only).
    /// </summary>
    public static ErrorResponse CreateDetailed(int statusCode, string message, string details, string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = statusCode,
            Message = message,
            Details = details,
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates a not found error response.
    /// </summary>
    public static ErrorResponse CreateNotFound(string resourceName, string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = 404,
            Message = $"{resourceName} não encontrado(a).",
            ErrorCode = "NOT_FOUND",
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates an unauthorized error response.
    /// </summary>
    public static ErrorResponse CreateUnauthorized(string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = 401,
            Message = "Acesso não autorizado. Faça login para continuar.",
            ErrorCode = "UNAUTHORIZED",
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates a forbidden error response.
    /// </summary>
    public static ErrorResponse CreateForbidden(string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = 403,
            Message = "Acesso negado. Você não tem permissão para acessar este recurso.",
            ErrorCode = "FORBIDDEN",
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates a bad request error response.
    /// </summary>
    public static ErrorResponse CreateBadRequest(string message, string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = 400,
            Message = message,
            ErrorCode = "BAD_REQUEST",
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates an internal server error response.
    /// </summary>
    public static ErrorResponse CreateInternalServerError(string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = 500,
            Message = "Erro interno do servidor. Por favor, tente novamente mais tarde.",
            ErrorCode = "INTERNAL_SERVER_ERROR",
            TraceId = traceId
        };
    }

    /// <summary>
    /// Creates a database error response (COBOL SQLCODE equivalent).
    /// </summary>
    public static ErrorResponse CreateDatabaseError(int sqlCode, string? traceId = null)
    {
        return new ErrorResponse
        {
            StatusCode = 500,
            Message = "Erro ao acessar o banco de dados. Por favor, tente novamente.",
            ErrorCode = $"DB_ERROR_{sqlCode}",
            TraceId = traceId
        };
    }
}
