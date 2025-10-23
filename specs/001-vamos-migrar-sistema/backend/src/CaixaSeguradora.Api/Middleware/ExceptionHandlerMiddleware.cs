using System.Net;
using System.Text.Json;
using CaixaSeguradora.Core.DTOs;
using Microsoft.Data.Sqlite;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Api.Middleware;

/// <summary>
/// Global exception handler middleware for consistent error responses.
/// Provides Portuguese error messages and detailed logging for troubleshooting.
/// </summary>
public class ExceptionHandlerMiddleware
{
    private readonly RequestDelegate _next;
    private readonly ILogger<ExceptionHandlerMiddleware> _logger;
    private readonly IWebHostEnvironment _environment;

    public ExceptionHandlerMiddleware(
        RequestDelegate next,
        ILogger<ExceptionHandlerMiddleware> logger,
        IWebHostEnvironment environment)
    {
        _next = next ?? throw new ArgumentNullException(nameof(next));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        _environment = environment ?? throw new ArgumentNullException(nameof(environment));
    }

    public async Task InvokeAsync(HttpContext context)
    {
        try
        {
            await _next(context);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Unhandled exception occurred: {Message}", ex.Message);
            await HandleExceptionAsync(context, ex);
        }
    }

    private async Task HandleExceptionAsync(HttpContext context, Exception exception)
    {
        // Get trace ID for error correlation
        var traceId = context.TraceIdentifier;

        // Build error response based on exception type
        var errorResponse = exception switch
        {
            ArgumentNullException => ErrorResponse.CreateBadRequest(
                "Parâmetro obrigatório não informado.", traceId),

            ArgumentException argEx => ErrorResponse.CreateBadRequest(
                argEx.Message, traceId),

            InvalidOperationException invalidOpEx => ErrorResponse.Create(
                (int)HttpStatusCode.BadRequest,
                invalidOpEx.Message,
                traceId),

            KeyNotFoundException => ErrorResponse.CreateNotFound(
                "Recurso", traceId),

            UnauthorizedAccessException => ErrorResponse.CreateUnauthorized(traceId),

            DbUpdateException dbEx => HandleDatabaseException(dbEx, traceId),

            SqliteException sqliteEx => HandleSqliteException(sqliteEx, traceId),

            DivideByZeroException => ErrorResponse.Create(
                (int)HttpStatusCode.BadRequest,
                "Erro de cálculo: divisão por zero detectada.",
                traceId),

            TimeoutException => ErrorResponse.Create(
                (int)HttpStatusCode.RequestTimeout,
                "A operação excedeu o tempo limite. Por favor, tente novamente.",
                traceId),

            OperationCanceledException => ErrorResponse.Create(
                (int)HttpStatusCode.BadRequest,
                "A operação foi cancelada pelo usuário.",
                traceId),

            NotImplementedException => ErrorResponse.Create(
                (int)HttpStatusCode.NotImplemented,
                "Funcionalidade ainda não implementada.",
                traceId),

            _ => ErrorResponse.CreateInternalServerError(traceId)
        };

        // Add request path for debugging
        errorResponse.Path = context.Request.Path;

        // In development, include exception details
        if (_environment.IsDevelopment())
        {
            errorResponse.Details = exception.ToString();
        }

        // Set response properties
        context.Response.ContentType = "application/json";
        context.Response.StatusCode = errorResponse.StatusCode;

        // Serialize and write response
        var jsonOptions = new JsonSerializerOptions
        {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            WriteIndented = _environment.IsDevelopment()
        };

        var json = JsonSerializer.Serialize(errorResponse, jsonOptions);
        await context.Response.WriteAsync(json);
    }

    private ErrorResponse HandleDatabaseException(DbUpdateException exception, string traceId)
    {
        // Check for common database errors
        if (exception.InnerException is SqliteException sqliteEx)
        {
            return HandleSqliteException(sqliteEx, traceId);
        }

        // Log detailed database error for investigation
        _logger.LogError(exception, "Database update exception: {Message}", exception.Message);

        return ErrorResponse.Create(
            (int)HttpStatusCode.InternalServerError,
            "Erro ao salvar dados no banco de dados. Verifique as informações e tente novamente.",
            traceId);
    }

    private ErrorResponse HandleSqliteException(SqliteException exception, string traceId)
    {
        // Map SQLite error codes to user-friendly messages
        var errorMessage = exception.SqliteErrorCode switch
        {
            // Constraint violations
            19 => "Violação de restrição do banco de dados. Verifique se os dados informados são válidos.",

            // Unique constraint
            2067 => "Registro duplicado. Este valor já existe no sistema.",

            // Foreign key constraint
            787 => "Erro de integridade referencial. O registro está relacionado a outros dados.",

            // NOT NULL constraint
            1299 => "Campo obrigatório não preenchido.",

            // Database locked
            5 => "Banco de dados temporariamente indisponível. Tente novamente em alguns instantes.",

            // Disk I/O error
            10 => "Erro ao acessar o disco. Verifique o espaço disponível e as permissões.",

            // Corrupt database
            11 => "Banco de dados corrompido. Entre em contato com o suporte técnico.",

            // Not a database
            26 => "Arquivo de banco de dados inválido.",

            // General error
            _ => "Erro no banco de dados. Por favor, tente novamente."
        };

        _logger.LogError(exception,
            "SQLite exception (Code {ErrorCode}): {Message}",
            exception.SqliteErrorCode,
            exception.Message);

        var error = ErrorResponse.CreateDatabaseError((int)exception.SqliteErrorCode, traceId);
        error.Message = errorMessage;
        return error;
    }
}

/// <summary>
/// Extension methods for registering the exception handler middleware.
/// </summary>
public static class ExceptionHandlerMiddlewareExtensions
{
    /// <summary>
    /// Adds the global exception handler middleware to the application pipeline.
    /// Should be registered early in the pipeline to catch all exceptions.
    /// </summary>
    public static IApplicationBuilder UseGlobalExceptionHandler(this IApplicationBuilder app)
    {
        return app.UseMiddleware<ExceptionHandlerMiddleware>();
    }
}
