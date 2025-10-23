using System;
using System.Data.Common;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore.Diagnostics;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Data;

/// <summary>
/// EF Core interceptor that enforces read-only database access (SC-007).
/// Blocks all write operations (INSERT, UPDATE, DELETE, DROP, CREATE, ALTER, TRUNCATE).
/// Used to ensure the .NET application maintains read-only access to production DB2 databases.
/// </summary>
public class ReadOnlyDbCommandInterceptor : DbCommandInterceptor
{
    private readonly ILogger<ReadOnlyDbCommandInterceptor> _logger;
    private readonly bool _isEnabled;

    // SQL keywords that indicate write operations (MUST be blocked)
    private static readonly string[] WriteOperations = new[]
    {
        "INSERT",
        "UPDATE",
        "DELETE",
        "DROP",
        "CREATE",
        "ALTER",
        "TRUNCATE",
        "MERGE",
        "GRANT",
        "REVOKE"
    };

    // SQL keywords that modify transaction state (MUST be blocked)
    private static readonly string[] TransactionOperations = new[]
    {
        "COMMIT",
        "ROLLBACK",
        "SAVEPOINT"
    };

    // Safe operations that are always allowed
    private static readonly string[] SafeOperations = new[]
    {
        "SELECT",
        "WITH",     // Common Table Expressions
        "EXPLAIN",  // Query plans
        "SHOW",     // Database metadata
        "DESCRIBE", // Table structure
        "PRAGMA"    // SQLite metadata (for dev)
    };

    public ReadOnlyDbCommandInterceptor(
        ILogger<ReadOnlyDbCommandInterceptor> logger,
        bool isEnabled = true)
    {
        _logger = logger;
        _isEnabled = isEnabled;
    }

    /// <summary>
    /// Intercepts command execution before it's sent to the database (synchronous).
    /// </summary>
    public override InterceptionResult<DbDataReader> ReaderExecuting(
        DbCommand command,
        CommandEventData eventData,
        InterceptionResult<DbDataReader> result)
    {
        ValidateCommand(command, eventData);
        return base.ReaderExecuting(command, eventData, result);
    }

    /// <summary>
    /// Intercepts command execution before it's sent to the database (asynchronous).
    /// </summary>
    public override ValueTask<InterceptionResult<DbDataReader>> ReaderExecutingAsync(
        DbCommand command,
        CommandEventData eventData,
        InterceptionResult<DbDataReader> result,
        CancellationToken cancellationToken = default)
    {
        ValidateCommand(command, eventData);
        return base.ReaderExecutingAsync(command, eventData, result, cancellationToken);
    }

    /// <summary>
    /// Intercepts scalar command execution (e.g., COUNT queries).
    /// </summary>
    public override InterceptionResult<object> ScalarExecuting(
        DbCommand command,
        CommandEventData eventData,
        InterceptionResult<object> result)
    {
        ValidateCommand(command, eventData);
        return base.ScalarExecuting(command, eventData, result);
    }

    /// <summary>
    /// Intercepts scalar command execution (asynchronous).
    /// </summary>
    public override ValueTask<InterceptionResult<object>> ScalarExecutingAsync(
        DbCommand command,
        CommandEventData eventData,
        InterceptionResult<object> result,
        CancellationToken cancellationToken = default)
    {
        ValidateCommand(command, eventData);
        return base.ScalarExecutingAsync(command, eventData, result, cancellationToken);
    }

    /// <summary>
    /// Intercepts non-query command execution (e.g., INSERT, UPDATE, DELETE).
    /// </summary>
    public override InterceptionResult<int> NonQueryExecuting(
        DbCommand command,
        CommandEventData eventData,
        InterceptionResult<int> result)
    {
        ValidateCommand(command, eventData);
        return base.NonQueryExecuting(command, eventData, result);
    }

    /// <summary>
    /// Intercepts non-query command execution (asynchronous).
    /// </summary>
    public override ValueTask<InterceptionResult<int>> NonQueryExecutingAsync(
        DbCommand command,
        CommandEventData eventData,
        InterceptionResult<int> result,
        CancellationToken cancellationToken = default)
    {
        ValidateCommand(command, eventData);
        return base.NonQueryExecutingAsync(command, eventData, result, cancellationToken);
    }

    /// <summary>
    /// Validates that the command is read-only.
    /// Throws InvalidOperationException if a write operation is detected.
    /// </summary>
    private void ValidateCommand(DbCommand command, CommandEventData eventData)
    {
        // Skip validation if interceptor is disabled (for testing/seeding)
        if (!_isEnabled)
        {
            _logger.LogTrace("ReadOnly interceptor is disabled, allowing command: {CommandText}",
                TruncateForLog(command.CommandText));
            return;
        }

        var sql = command.CommandText?.Trim() ?? string.Empty;

        if (string.IsNullOrWhiteSpace(sql))
        {
            _logger.LogWarning("Empty SQL command detected");
            return;
        }

        // Normalize SQL for checking (uppercase, remove comments)
        var normalizedSql = NormalizeSql(sql);

        _logger.LogTrace("Validating command: {CommandText}", TruncateForLog(normalizedSql));

        // Check for write operations
        foreach (var operation in WriteOperations)
        {
            if (StartsWithKeyword(normalizedSql, operation))
            {
                var errorMessage = $"Operação de escrita '{operation}' não permitida. " +
                                   $"Sistema é somente leitura (SC-007). " +
                                   $"Comando bloqueado: {TruncateForLog(sql)}";

                _logger.LogError("BLOCKED: {ErrorMessage}", errorMessage);

                throw new InvalidOperationException(errorMessage);
            }
        }

        // Check for transaction operations
        foreach (var operation in TransactionOperations)
        {
            if (StartsWithKeyword(normalizedSql, operation))
            {
                var errorMessage = $"Operação de transação '{operation}' não permitida. " +
                                   $"Sistema é somente leitura (SC-007). " +
                                   $"Comando bloqueado: {TruncateForLog(sql)}";

                _logger.LogError("BLOCKED: {ErrorMessage}", errorMessage);

                throw new InvalidOperationException(errorMessage);
            }
        }

        // Log allowed operations
        var safeOperation = SafeOperations.FirstOrDefault(op => StartsWithKeyword(normalizedSql, op));
        if (safeOperation != null)
        {
            _logger.LogDebug("Allowed read-only operation: {Operation}", safeOperation);
        }
        else
        {
            // Not a known safe operation, but didn't match write operations either
            // This could be a stored procedure call or complex query
            _logger.LogInformation("Unknown operation type, allowing: {CommandText}",
                TruncateForLog(normalizedSql));
        }
    }

    /// <summary>
    /// Normalizes SQL by removing comments and converting to uppercase.
    /// </summary>
    private static string NormalizeSql(string sql)
    {
        // Remove single-line comments (-- ...)
        var lines = sql.Split('\n');
        var withoutComments = string.Join(' ',
            lines.Select(line =>
            {
                var commentIndex = line.IndexOf("--", StringComparison.Ordinal);
                return commentIndex >= 0 ? line.Substring(0, commentIndex) : line;
            }));

        // Remove multi-line comments (/* ... */)
        while (withoutComments.Contains("/*"))
        {
            var startIndex = withoutComments.IndexOf("/*", StringComparison.Ordinal);
            var endIndex = withoutComments.IndexOf("*/", startIndex, StringComparison.Ordinal);

            if (endIndex > startIndex)
            {
                withoutComments = withoutComments.Remove(startIndex, endIndex - startIndex + 2);
            }
            else
            {
                break; // Malformed comment, stop processing
            }
        }

        return withoutComments.Trim().ToUpperInvariant();
    }

    /// <summary>
    /// Checks if SQL starts with a specific keyword (ignoring leading whitespace and comments).
    /// </summary>
    private static bool StartsWithKeyword(string normalizedSql, string keyword)
    {
        // Check if SQL starts with the keyword followed by a space or special character
        if (normalizedSql.StartsWith(keyword, StringComparison.OrdinalIgnoreCase))
        {
            // Ensure it's a complete keyword, not part of a larger word
            if (normalizedSql.Length == keyword.Length)
            {
                return true;
            }

            var nextChar = normalizedSql[keyword.Length];
            return char.IsWhiteSpace(nextChar) ||
                   nextChar == '(' ||
                   nextChar == ';';
        }

        return false;
    }

    /// <summary>
    /// Truncates SQL for logging to avoid excessive log messages.
    /// </summary>
    private static string TruncateForLog(string sql, int maxLength = 200)
    {
        if (string.IsNullOrEmpty(sql) || sql.Length <= maxLength)
        {
            return sql;
        }

        return sql.Substring(0, maxLength) + "... [truncated]";
    }
}
