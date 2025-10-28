using CaixaSeguradora.Api.Validators;
using CaixaSeguradora.Core.DTOs;
using FluentValidation.TestHelper;
using Xunit;

namespace CaixaSeguradora.UnitTests.Validators;

public class ReportGenerationRequestValidatorTests
{
    private readonly ReportGenerationRequestValidator _validator;

    public ReportGenerationRequestValidatorTests()
    {
        _validator = new ReportGenerationRequestValidator();
    }

    [Fact]
    public void Validate_ValidReportRequest_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "Both",
            ProcessingMode = "Monthly"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_EndDateBeforeStartDate_ShouldHaveValidationError()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 31),
            EndDate = new DateTime(2025, 10, 1),
            SystemId = "GL",
            ReportType = "PREMIT"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.EndDate)
            .WithErrorMessage("Data final deve ser maior ou igual à data inicial");
    }

    [Fact]
    public void Validate_FutureDates_ShouldHaveValidationErrors()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = DateTime.Now.AddDays(10),
            EndDate = DateTime.Now.AddDays(20),
            SystemId = "GL",
            ReportType = "PREMIT"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.StartDate)
            .WithErrorMessage("Data inicial não pode ser no futuro");
        result.ShouldHaveValidationErrorFor(x => x.EndDate)
            .WithErrorMessage("Data final não pode ser no futuro");
    }

    [Theory]
    [InlineData("")]
    [InlineData("X")]
    [InlineData("ABCD")]
    [InlineData("g1")]
    public void Validate_InvalidSystemId_ShouldHaveValidationError(string systemId)
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = systemId,
            ReportType = "PREMIT"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.SystemId);
    }

    [Theory]
    [InlineData("GL")]
    [InlineData("GE")]
    [InlineData("GH")]
    [InlineData("ST")]
    [InlineData("CA")]
    [InlineData("RE")]
    public void Validate_ValidSystemIds_ShouldNotHaveValidationErrors(string systemId)
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = systemId,
            ReportType = "PREMIT"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.SystemId);
    }

    [Theory]
    [InlineData("INVALID")]
    [InlineData("")]
    [InlineData("Premium")]
    public void Validate_InvalidReportType_ShouldHaveValidationError(string reportType)
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = reportType
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ReportType);
    }

    [Theory]
    [InlineData("PREMIT")]
    [InlineData("PREMCED")]
    [InlineData("Both")]
    public void Validate_ValidReportTypes_ShouldNotHaveValidationErrors(string reportType)
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = reportType
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.ReportType);
    }

    [Theory]
    [InlineData("INVALID")]
    [InlineData("")]
    [InlineData("Yearly")]
    public void Validate_InvalidProcessingMode_ShouldHaveValidationError(string processingMode)
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            ProcessingMode = processingMode
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ProcessingMode);
    }

    [Fact]
    public void Validate_EmailNotificationEnabledWithoutEmail_ShouldHaveValidationError()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            SendEmailNotification = true,
            NotificationEmail = null
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.NotificationEmail)
            .WithErrorMessage("E-mail de notificação é obrigatório quando notificação está habilitada");
    }

    [Fact]
    public void Validate_EmailNotificationWithInvalidEmail_ShouldHaveValidationError()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            SendEmailNotification = true,
            NotificationEmail = "invalid-email"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.NotificationEmail)
            .WithErrorMessage("E-mail de notificação deve ser um endereço de e-mail válido");
    }

    [Fact]
    public void Validate_EmailNotificationWithValidEmail_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            SendEmailNotification = true,
            NotificationEmail = "user@example.com"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.NotificationEmail);
    }

    [Fact]
    public void Validate_OptionalFiltersWithValidValues_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            PolicyNumber = "POL123456",
            ProductCode = 100,
            LineOfBusiness = 50,
            RequestedBy = "admin@caixaseguradora.com.br",
            Notes = "Test report generation"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_PolicyNumberTooLong_ShouldHaveValidationError()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            PolicyNumber = new string('1', 21) // 21 characters
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.PolicyNumber)
            .WithErrorMessage("Número da apólice não pode exceder 20 caracteres");
    }

    [Fact]
    public void Validate_InvalidProductCode_ShouldHaveValidationError()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            ProductCode = -1
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ProductCode)
            .WithErrorMessage("Código do produto deve ser maior que zero");
    }

    [Fact]
    public void Validate_NotesTooLong_ShouldHaveValidationError()
    {
        // Arrange
        var request = new ReportGenerationRequestDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            SystemId = "GL",
            ReportType = "PREMIT",
            Notes = new string('a', 501) // 501 characters
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Notes)
            .WithErrorMessage("Notas não podem exceder 500 caracteres");
    }
}
