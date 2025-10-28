using System;
using System.Collections.Generic;
using System.Data.Common;
using Microsoft.Data.Sqlite;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Translates database-specific error codes to domain-friendly error messages in Portuguese.
/// Supports both DB2 SQLCODE values (production) and SQLite errors (development).
/// </summary>
public class SqlErrorTranslator
{
    private readonly ILogger<SqlErrorTranslator> _logger;

    // DB2 SQLCODE to Portuguese error message mappings
    private static readonly Dictionary<int, string> DB2ErrorMappings = new()
    {
        // Constraint violations
        { -803, "Violação de chave única: registro duplicado encontrado" },
        { -530, "Violação de chave estrangeira: referência inválida" },
        { -531, "Violação de chave estrangeira: atualização não permitida" },
        { -532, "Violação de chave estrangeira: exclusão não permitida" },
        { -545, "Violação de constraint: valor não permitido" },

        // Object not found errors
        { -204, "Tabela ou view não encontrada" },
        { -206, "Coluna não encontrada na tabela especificada" },
        { -601, "Objeto não pode ser criado: já existe" },

        // Transaction errors
        { -911, "Deadlock detectado: tente novamente após alguns segundos" },
        { -913, "Timeout na operação: recurso não disponível no momento" },
        { -964, "Falha ao bloquear tabela: recurso em uso" },

        // Data errors
        { -302, "Erro de conversão: valor não é compatível com o tipo esperado" },
        { -407, "Valor nulo não permitido em coluna obrigatória" },
        { -408, "Valor fornecido excede o tamanho máximo permitido" },

        // Connection errors
        { -1776, "Timeout na conexão com o banco de dados" },
        { -30081, "Falha na comunicação com o banco de dados" },

        // No data found (not an error, but important for queries)
        { 100, "Nenhum registro encontrado para os critérios especificados" }
    };

    // SQLite error code mappings (for development environment)
    private static readonly Dictionary<int, string> SqliteErrorMappings = new()
    {
        { 1, "Erro de SQL: sintaxe inválida" },
        { 5, "Operação não permitida: banco de dados está ocupado" },
        { 6, "Operação não permitida: banco de dados está bloqueado" },
        { 11, "Tabela ou coluna não encontrada" },
        { 13, "Operação não concluída: banco de dados está cheio" },
        { 19, "Violação de constraint: operação violou regra de integridade" }
    };

    // Transient errors that can be retried
    private static readonly HashSet<int> TransientDB2Errors = new()
    {
        -911,  // Deadlock
        -913,  // Timeout
        -964,  // Lock failure
        -1776, // Connection timeout
        -30081 // Communication failure
    };

    public SqlErrorTranslator(ILogger<SqlErrorTranslator> logger)
    {
        _logger = logger;
    }

    /// <summary>
    /// Translates a database exception to a user-friendly Portuguese error message.
    /// </summary>
    /// <param name="exception">The database exception to translate</param>
    /// <returns>A tuple containing the translated message and whether the error is transient</returns>
    public (string Message, bool IsTransient) TranslateException(DbException exception)
    {
        if (exception == null)
        {
            _logger.LogWarning("TranslateException called with null exception");
            return ("Erro desconhecido no banco de dados", false);
        }

        _logger.LogDebug("Translating database exception: {ExceptionType}, ErrorCode: {ErrorCode}, Message: {Message}",
            exception.GetType().Name, exception.ErrorCode, exception.Message);

        // Handle SQLite exceptions (development)
        if (exception is SqliteException sqliteEx)
        {
            return TranslateSqliteException(sqliteEx);
        }

        // Handle DB2 exceptions (production)
        // DB2Exception is not available in this project, so we check error code ranges
        return TranslateDB2Exception(exception);
    }

    /// <summary>
    /// Translates a DB2 SQLCODE to Portuguese error message.
    /// </summary>
    public (string Message, bool IsTransient) TranslateDB2SqlCode(int sqlCode)
    {
        _logger.LogDebug("Translating DB2 SQLCODE: {SqlCode}", sqlCode);

        if (DB2ErrorMappings.TryGetValue(sqlCode, out var message))
        {
            var isTransient = TransientDB2Errors.Contains(sqlCode);
            _logger.LogInformation("DB2 SQLCODE {SqlCode} translated to: {Message} (Transient: {IsTransient})",
                sqlCode, message, isTransient);
            return (message, isTransient);
        }

        // Categorize unknown errors by SQLCODE ranges
        var categoryMessage = sqlCode switch
        {
            < 0 when sqlCode >= -99 => "Erro de SQL: problema na sintaxe ou estrutura da consulta",
            < 0 when sqlCode >= -199 => "Erro de execução: problema ao executar a operação",
            < 0 when sqlCode >= -299 => "Erro de dados: problema com os valores fornecidos",
            < 0 when sqlCode >= -999 => "Erro de sistema: problema interno do banco de dados",
            > 0 => "Aviso: operação completada com condições especiais",
            _ => $"Erro desconhecido no banco de dados (SQLCODE: {sqlCode})"
        };

        _logger.LogWarning("Unknown DB2 SQLCODE {SqlCode}, using category message: {Message}",
            sqlCode, categoryMessage);

        return (categoryMessage, false);
    }

    /// <summary>
    /// Checks if a DB2 SQLCODE represents a transient error that can be retried.
    /// </summary>
    public bool IsTransientError(int sqlCode)
    {
        return TransientDB2Errors.Contains(sqlCode);
    }

    /// <summary>
    /// Checks if a database exception represents a transient error.
    /// </summary>
    public bool IsTransientError(DbException exception)
    {
        if (exception == null)
        {
            return false;
        }

        // SQLite transient errors
        if (exception is SqliteException sqliteEx)
        {
            var baseCode = (int)sqliteEx.SqliteErrorCode;
            var extendedCode = sqliteEx.SqliteExtendedErrorCode;

            return baseCode == 5 || baseCode == 6 ||
                   extendedCode == 5 || extendedCode == 6;
        }

        // DB2 transient errors (check error code)
        return TransientDB2Errors.Contains(exception.ErrorCode);
    }

    private (string Message, bool IsTransient) TranslateSqliteException(SqliteException exception)
    {
        var baseCode = (int)exception.SqliteErrorCode;
        var extendedCode = exception.SqliteExtendedErrorCode;

        _logger.LogDebug("SQLite exception details - BaseCode: {BaseCode}, ExtendedCode: {ExtendedCode}, Message: {ExceptionMessage}",
            baseCode, extendedCode, exception.Message);

        foreach (var code in GetSqliteLookupCodes(baseCode, extendedCode))
        {
            if (SqliteErrorMappings.TryGetValue(code, out var message))
            {
                var isTransient = code == 5 || code == 6;
                _logger.LogInformation("SQLite error {ErrorCode} translated to: {Message} (Transient: {IsTransient})",
                    code, message, isTransient);
                return (message, isTransient);
            }
        }

        var defaultMessage = $"Erro no banco de dados SQLite: {exception.Message}";
        _logger.LogWarning("Unknown SQLite error codes Base={BaseCode}, Extended={ExtendedCode}, using default message",
            baseCode, extendedCode);
        return (defaultMessage, false);
    }

    private static IEnumerable<int> GetSqliteLookupCodes(int baseCode, int extendedCode)
    {
        if (extendedCode != 0 && extendedCode != baseCode)
        {
            yield return extendedCode;
        }

        yield return baseCode;
    }

    private (string Message, bool IsTransient) TranslateDB2Exception(DbException exception)
    {
        // Use ErrorCode property which should contain SQLCODE for DB2
        var sqlCode = exception.ErrorCode;

        if (sqlCode != 0)
        {
            return TranslateDB2SqlCode(sqlCode);
        }

        // Fallback: try to extract SQLCODE from exception message
        var message = exception.Message;
        if (message.Contains("SQLCODE=") || message.Contains("SQL"))
        {
            _logger.LogDebug("Attempting to extract SQLCODE from exception message: {Message}", message);
            // Try to parse SQLCODE from message (format: "... SQLCODE=-803 ...")
            System.Text.RegularExpressions.Match sqlCodeMatch = System.Text.RegularExpressions.Regex.Match(
                message, @"SQLCODE[=:\s]+(-?\d+)");

            if (sqlCodeMatch.Success && int.TryParse(sqlCodeMatch.Groups[1].Value, out var extractedCode))
            {
                _logger.LogInformation("Extracted SQLCODE {SqlCode} from exception message", extractedCode);
                return TranslateDB2SqlCode(extractedCode);
            }
        }

        // No SQLCODE found, return generic message
        var genericMessage = "Erro ao acessar o banco de dados. Por favor, tente novamente.";
        _logger.LogWarning("Could not extract SQLCODE from DB exception, using generic message. Original: {Message}",
            exception.Message);
        return (genericMessage, false);
    }

    /// <summary>
    /// Gets a user-friendly error message for common database operations.
    /// </summary>
    public string GetOperationErrorMessage(string operation, DbException exception)
    {
        (string translatedMessage, bool _) = TranslateException(exception);

        return operation.ToLowerInvariant() switch
        {
            "insert" => $"Erro ao inserir registro: {translatedMessage}",
            "update" => $"Erro ao atualizar registro: {translatedMessage}",
            "delete" => $"Erro ao excluir registro: {translatedMessage}",
            "select" or "query" => $"Erro ao consultar dados: {translatedMessage}",
            _ => $"Erro na operação '{operation}': {translatedMessage}"
        };
    }
}
