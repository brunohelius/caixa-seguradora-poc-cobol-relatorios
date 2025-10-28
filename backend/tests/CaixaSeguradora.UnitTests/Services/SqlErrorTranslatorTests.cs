using Xunit;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using CaixaSeguradora.Infrastructure.Services;
using Microsoft.Data.Sqlite;
using System.Data.Common;

namespace CaixaSeguradora.UnitTests.Services;

/// <summary>
/// Unit tests for SqlErrorTranslator - User Story 2, Task T242
/// Tests DB2 SQLCODE and SQLite error code translation to Portuguese error messages
/// </summary>
public class SqlErrorTranslatorTests
{
    private readonly Mock<ILogger<SqlErrorTranslator>> _mockLogger;
    private readonly SqlErrorTranslator _translator;

    public SqlErrorTranslatorTests()
    {
        _mockLogger = new Mock<ILogger<SqlErrorTranslator>>();
        _translator = new SqlErrorTranslator(_mockLogger.Object);
    }

    #region DB2 SQLCODE Translation Tests

    [Theory]
    [InlineData(-803, "Violação de chave única: registro duplicado encontrado")]
    [InlineData(-530, "Violação de chave estrangeira: referência inválida")]
    [InlineData(-204, "Tabela ou view não encontrada")]
    [InlineData(-911, "Deadlock detectado: tente novamente após alguns segundos")]
    [InlineData(-913, "Timeout na operação: recurso não disponível no momento")]
    [InlineData(100, "Nenhum registro encontrado para os critérios especificados")]
    public void TranslateDB2SqlCode_KnownErrorCodes_ReturnsCorrectPortugueseMessage(int sqlCode, string expectedMessage)
    {
        // Act
        (string message, bool _) = _translator.TranslateDB2SqlCode(sqlCode);

        // Assert
        message.Should().Be(expectedMessage);
    }

    [Theory]
    [InlineData(-911, true)]   // Deadlock
    [InlineData(-913, true)]   // Timeout
    [InlineData(-964, true)]   // Lock failure
    [InlineData(-1776, true)]  // Connection timeout
    [InlineData(-30081, true)] // Communication failure
    [InlineData(-803, false)]  // Unique key violation (not transient)
    [InlineData(-530, false)]  // Foreign key violation (not transient)
    public void TranslateDB2SqlCode_ChecksTransientStatus_Correctly(int sqlCode, bool expectedIsTransient)
    {
        // Act
        (string _, bool isTransient) = _translator.TranslateDB2SqlCode(sqlCode);

        // Assert
        isTransient.Should().Be(expectedIsTransient);
    }

    [Fact]
    public void TranslateDB2SqlCode_UnknownNegativeCode_ReturnsCategoryMessage()
    {
        // Arrange
        var unknownSqlCode = -12345;

        // Act
        (string message, bool isTransient) = _translator.TranslateDB2SqlCode(unknownSqlCode);

        // Assert
        message.Should().Contain("Erro");
        message.Should().Contain("banco de dados");
        isTransient.Should().BeFalse();
    }

    [Theory]
    [InlineData(-50)]   // Syntax/structure errors (0 to -99)
    [InlineData(-150)]  // Execution errors (-100 to -199)
    [InlineData(-250)]  // Data errors (-200 to -299)
    [InlineData(-500)]  // System errors (-300 to -999)
    public void TranslateDB2SqlCode_UnknownCodeInRange_ReturnsAppropriateCategory(int sqlCode)
    {
        // Act
        (string message, bool _) = _translator.TranslateDB2SqlCode(sqlCode);

        // Assert
        message.Should().NotBeNullOrEmpty();
        message.Should().Contain("Erro");
    }

    [Fact]
    public void IsTransientError_SqlCode_CorrectlyIdentifiesTransientErrors()
    {
        // Arrange
        var transientCodes = new[] { -911, -913, -964, -1776, -30081 };
        var nonTransientCodes = new[] { -803, -530, -204, -407, 100 };

        // Act & Assert
        foreach (var code in transientCodes)
        {
            _translator.IsTransientError(code).Should().BeTrue($"SQLCODE {code} should be transient");
        }

        foreach (var code in nonTransientCodes)
        {
            _translator.IsTransientError(code).Should().BeFalse($"SQLCODE {code} should not be transient");
        }
    }

    #endregion

    #region SQLite Exception Translation Tests

    [Fact]
    public void TranslateException_SqliteConstraintViolation_ReturnsPortugueseMessage()
    {
        // Arrange
        SqliteException sqliteEx = CreateSqliteException(SqliteError.Constraint);

        // Act
        (string message, bool isTransient) = _translator.TranslateException(sqliteEx);

        // Assert
        message.Should().Contain("Violação de constraint");
        isTransient.Should().BeFalse();
    }

    [Fact]
    public void TranslateException_SqliteDatabaseLocked_ReturnsTransientError()
    {
        // Arrange
        SqliteException sqliteEx = CreateSqliteException(SqliteError.Locked);

        // Act
        (string message, bool isTransient) = _translator.TranslateException(sqliteEx);

        // Assert
        message.Should().Contain("banco de dados está bloqueado");
        isTransient.Should().BeTrue();
    }

    [Fact]
    public void TranslateException_NullException_ReturnsGenericMessage()
    {
        // Act
        (string message, bool isTransient) = _translator.TranslateException(null);

        // Assert
        message.Should().Be("Erro desconhecido no banco de dados");
        isTransient.Should().BeFalse();
    }

    [Fact]
    public void IsTransientError_SqliteException_CorrectlyIdentifiesTransientErrors()
    {
        // Arrange
        SqliteException busyException = CreateSqliteException(SqliteError.Busy);
        SqliteException lockedException = CreateSqliteException(SqliteError.Locked);
        SqliteException constraintException = CreateSqliteException(SqliteError.Constraint);

        // Act & Assert
        _translator.IsTransientError(busyException).Should().BeTrue();
        _translator.IsTransientError(lockedException).Should().BeTrue();
        _translator.IsTransientError(constraintException).Should().BeFalse();
    }

    #endregion

    #region Operation Error Message Tests

    [Theory]
    [InlineData("insert", "Erro ao inserir registro")]
    [InlineData("update", "Erro ao atualizar registro")]
    [InlineData("delete", "Erro ao excluir registro")]
    [InlineData("select", "Erro ao consultar dados")]
    [InlineData("query", "Erro ao consultar dados")]
    public void GetOperationErrorMessage_CommonOperations_ReturnsCorrectPrefix(string operation, string expectedPrefix)
    {
        // Arrange
        SqliteException sqliteEx = CreateSqliteException(SqliteError.Constraint);

        // Act
        var message = _translator.GetOperationErrorMessage(operation, sqliteEx);

        // Assert
        message.Should().StartWith(expectedPrefix);
        message.Should().Contain("Violação de constraint");
    }

    [Fact]
    public void GetOperationErrorMessage_UnknownOperation_IncludesOperationName()
    {
        // Arrange
        var operation = "customOperation";
        SqliteException sqliteEx = CreateSqliteException(SqliteError.Error);

        // Act
        var message = _translator.GetOperationErrorMessage(operation, sqliteEx);

        // Assert
        message.Should().Contain(operation);
        message.Should().StartWith("Erro na operação");
    }

    #endregion

    #region Helper Methods

    /// <summary>
    /// Creates a SqliteException with the specified error code.
    /// Uses reflection since SqliteException constructor is internal.
    /// </summary>
    private static SqliteException CreateSqliteException(SqliteError errorCode)
    {
        // Create a temporary connection to generate an exception
        using var connection = new SqliteConnection("Data Source=:memory:");
        connection.Open();

        try
        {
            // Trigger specific error types
            using SqliteCommand command = connection.CreateCommand();
            command.CommandText = errorCode switch
            {
                SqliteError.Constraint => "CREATE TABLE test (id INTEGER PRIMARY KEY); INSERT INTO test VALUES (1); INSERT INTO test VALUES (1);",
                SqliteError.Error => "SELECT * FROM nonexistent_table;",
                SqliteError.NotFound => "SELECT * FROM sqlite_master WHERE name = 'nonexistent';",
                _ => "INVALID SQL SYNTAX HERE;"
            };

            command.ExecuteNonQuery();
        }
        catch (SqliteException ex)
        {
            var requestedCode = (int)errorCode;
            if ((int)ex.SqliteErrorCode != requestedCode)
            {
                // Preserve original message but ensure desired error code for translation tests
                return new SqliteException(ex.Message, requestedCode);
            }

            return ex;
        }

        // Fallback: create a generic SqliteException
        // This should not happen in practice, but provides a safe fallback
        return new SqliteException("Test error", (int)errorCode);
    }

    private enum SqliteError
    {
        Error = 1,
        NotFound = 11,
        Constraint = 19,
        Busy = 5,
        Locked = 6,
        Full = 13
    }

    #endregion

    #region Edge Cases

    [Fact]
    public void TranslateException_GenericDbException_FallsBackToGenericMessage()
    {
        // Arrange
        var genericException = new CustomDbException("Generic database error", -999);

        // Act
        (string message, bool isTransient) = _translator.TranslateException(genericException);

        // Assert
        message.Should().Contain("Erro");
        isTransient.Should().BeFalse();
    }

    [Fact]
    public void TranslateException_ExceptionWithZeroErrorCode_HandlesGracefully()
    {
        // Arrange
        var exceptionWithZeroCode = new CustomDbException("Error with zero code", 0);

        // Act
        (string message, bool isTransient) = _translator.TranslateException(exceptionWithZeroCode);

        // Assert
        message.Should().NotBeNullOrEmpty();
        message.Should().Contain("Erro");
    }

    /// <summary>
    /// Custom DbException for testing generic database exceptions
    /// </summary>
    private class CustomDbException : DbException
    {
        public CustomDbException(string message, int errorCode) : base(message)
        {
            ErrorCodeValue = errorCode;
        }

        public override int ErrorCode => ErrorCodeValue;
        private int ErrorCodeValue { get; }
    }

    #endregion
}
