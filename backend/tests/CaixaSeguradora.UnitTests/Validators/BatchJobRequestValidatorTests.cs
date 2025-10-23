using CaixaSeguradora.Api.Validators;
using CaixaSeguradora.Core.DTOs;
using FluentValidation.TestHelper;
using Xunit;

namespace CaixaSeguradora.UnitTests.Validators;

public class BatchJobRequestValidatorTests
{
    private readonly BatchJobRequestValidator _validator;

    public BatchJobRequestValidatorTests()
    {
        _validator = new BatchJobRequestValidator();
    }

    [Fact]
    public void Validate_ValidBatchJobRequest_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Monthly Premium Report",
            Description = "Generates monthly premium report",
            RecurrencePattern = "MONTHLY",
            ReportParameters = "{\"startDate\":\"2025-10-01\",\"endDate\":\"2025-10-31\"}",
            ExecutionHour = 2,
            ExecutionMinute = 30,
            DayOfMonth = 1,
            MaxRetries = 3,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_EmptyJobName_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "",
            RecurrencePattern = "ONCE",
            ReportParameters = "{}",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.JobName)
            .WithErrorMessage("Nome do job é obrigatório");
    }

    [Fact]
    public void Validate_JobNameTooLong_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = new string('a', 201), // 201 characters
            RecurrencePattern = "ONCE",
            ReportParameters = "{}",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.JobName)
            .WithErrorMessage("Nome do job não pode exceder 200 caracteres");
    }

    [Fact]
    public void Validate_JobNameWithInvalidCharacters_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Job@#$%Name",
            RecurrencePattern = "ONCE",
            ReportParameters = "{}",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.JobName)
            .WithErrorMessage("Nome do job deve conter apenas letras, números, espaços, hífens e underscores");
    }

    [Theory]
    [InlineData("ONCE")]
    [InlineData("DAILY")]
    [InlineData("WEEKLY")]
    [InlineData("MONTHLY")]
    public void Validate_ValidRecurrencePatterns_ShouldNotHaveValidationErrors(string recurrencePattern)
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = recurrencePattern,
            ReportParameters = "{}",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.RecurrencePattern);
    }

    [Fact]
    public void Validate_InvalidRecurrencePattern_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "YEARLY",
            ReportParameters = "{}",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.RecurrencePattern);
    }

    [Fact]
    public void Validate_EmptyReportParameters_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "ONCE",
            ReportParameters = "",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ReportParameters)
            .WithErrorMessage("Parâmetros do relatório são obrigatórios");
    }

    [Fact]
    public void Validate_InvalidJsonReportParameters_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "ONCE",
            ReportParameters = "{invalid json}",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ReportParameters)
            .WithErrorMessage("Parâmetros do relatório devem ser um JSON válido");
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(24)]
    [InlineData(25)]
    public void Validate_InvalidExecutionHour_ShouldHaveValidationError(int hour)
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "DAILY",
            ReportParameters = "{}",
            ExecutionHour = hour,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ExecutionHour)
            .WithErrorMessage("Hora de execução deve estar entre 0 e 23");
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(60)]
    [InlineData(100)]
    public void Validate_InvalidExecutionMinute_ShouldHaveValidationError(int minute)
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "DAILY",
            ReportParameters = "{}",
            ExecutionMinute = minute,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ExecutionMinute)
            .WithErrorMessage("Minuto de execução deve estar entre 0 e 59");
    }

    [Fact]
    public void Validate_WeeklyJobWithoutDayOfWeek_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Weekly Job",
            RecurrencePattern = "WEEKLY",
            ReportParameters = "{}",
            DayOfWeek = null,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.DayOfWeek)
            .WithErrorMessage("Dia da semana é obrigatório para jobs semanais");
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(7)]
    public void Validate_InvalidDayOfWeek_ShouldHaveValidationError(int dayOfWeek)
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Weekly Job",
            RecurrencePattern = "WEEKLY",
            ReportParameters = "{}",
            DayOfWeek = dayOfWeek,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.DayOfWeek)
            .WithErrorMessage("Dia da semana deve estar entre 0 (Domingo) e 6 (Sábado)");
    }

    [Fact]
    public void Validate_MonthlyJobWithoutDayOfMonth_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Monthly Job",
            RecurrencePattern = "MONTHLY",
            ReportParameters = "{}",
            DayOfMonth = null,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.DayOfMonth)
            .WithErrorMessage("Dia do mês é obrigatório para jobs mensais");
    }

    [Theory]
    [InlineData(0)]
    [InlineData(32)]
    [InlineData(-1)]
    public void Validate_InvalidDayOfMonth_ShouldHaveValidationError(int dayOfMonth)
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Monthly Job",
            RecurrencePattern = "MONTHLY",
            ReportParameters = "{}",
            DayOfMonth = dayOfMonth,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.DayOfMonth)
            .WithErrorMessage("Dia do mês deve estar entre 1 e 31");
    }

    [Fact]
    public void Validate_InvalidEmailList_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "ONCE",
            ReportParameters = "{}",
            NotificationRecipients = "valid@email.com,invalid-email,another@valid.com",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.NotificationRecipients)
            .WithErrorMessage("Lista de e-mails deve conter endereços de e-mail válidos separados por vírgula");
    }

    [Fact]
    public void Validate_ValidEmailList_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "ONCE",
            ReportParameters = "{}",
            NotificationRecipients = "user1@example.com,user2@example.com,admin@caixaseguradora.com.br",
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.NotificationRecipients);
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(11)]
    public void Validate_InvalidMaxRetries_ShouldHaveValidationError(int maxRetries)
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "ONCE",
            ReportParameters = "{}",
            MaxRetries = maxRetries,
            CreatedBy = "admin"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.MaxRetries)
            .WithErrorMessage("Número máximo de tentativas deve estar entre 0 e 10");
    }

    [Fact]
    public void Validate_EmptyCreatedBy_ShouldHaveValidationError()
    {
        // Arrange
        var request = new BatchJobRequestDto
        {
            JobName = "Test Job",
            RecurrencePattern = "ONCE",
            ReportParameters = "{}",
            CreatedBy = ""
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.CreatedBy)
            .WithErrorMessage("Criador do job é obrigatório");
    }
}
