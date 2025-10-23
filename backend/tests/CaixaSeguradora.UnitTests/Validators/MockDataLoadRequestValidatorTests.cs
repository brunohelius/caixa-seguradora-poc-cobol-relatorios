using CaixaSeguradora.Api.Validators;
using CaixaSeguradora.Core.DTOs;
using FluentValidation.TestHelper;
using Xunit;

namespace CaixaSeguradora.UnitTests.Validators;

public class MockDataLoadRequestValidatorTests
{
    private readonly MockDataLoadRequestValidator _validator;

    public MockDataLoadRequestValidatorTests()
    {
        _validator = new MockDataLoadRequestValidator();
    }

    [Fact]
    public void Validate_ValidMockDataLoadRequest_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            RawDataContent = "PolicyNumber,Amount\n123456,1000.00",
            CsvDelimiter = ',',
            CsvHasHeaders = true,
            MaxRecords = 1000
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_EmptyEntityType_ShouldHaveValidationError()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "",
            Format = DataFormat.Csv,
            RawDataContent = "test data"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.EntityType)
            .WithErrorMessage("Tipo de entidade é obrigatório");
    }

    [Fact]
    public void Validate_EntityTypeTooLong_ShouldHaveValidationError()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = new string('a', 101), // 101 characters
            Format = DataFormat.Csv,
            RawDataContent = "test data"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.EntityType)
            .WithErrorMessage("Tipo de entidade não pode exceder 100 caracteres");
    }

    [Theory]
    [InlineData("invalid_type")]
    [InlineData("unknown")]
    [InlineData("PREMIUMS")] // Case-sensitive check
    public void Validate_InvalidEntityType_ShouldHaveValidationError(string entityType)
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = entityType,
            Format = DataFormat.Csv,
            RawDataContent = "test data"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.EntityType);
    }

    [Theory]
    [InlineData("premiums")]
    [InlineData("policies")]
    [InlineData("clients")]
    [InlineData("products")]
    [InlineData("endorsements")]
    [InlineData("coverages")]
    [InlineData("addresses")]
    [InlineData("agencies")]
    [InlineData("producers")]
    [InlineData("cossurance")]
    public void Validate_ValidEntityTypes_ShouldNotHaveValidationErrors(string entityType)
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = entityType,
            Format = DataFormat.Csv,
            RawDataContent = "test data"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.EntityType);
    }

    [Fact]
    public void Validate_NoDataSource_ShouldHaveValidationError()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            FileContentBase64 = null,
            RawDataContent = null
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x)
            .WithErrorMessage("Arquivo ou conteúdo de dados deve ser fornecido");
    }

    [Fact]
    public void Validate_ValidBase64FileContent_ShouldNotHaveValidationErrors()
    {
        // Arrange - Create a small base64 string
        var testData = "PolicyNumber,Amount\n123456,1000.00";
        var base64 = Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(testData));

        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            FileContentBase64 = base64
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.FileContentBase64);
    }

    [Theory]
    [InlineData(',')]
    [InlineData(';')]
    [InlineData('\t')]
    [InlineData('|')]
    public void Validate_ValidCsvDelimiters_ShouldNotHaveValidationErrors(char delimiter)
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            RawDataContent = "test data",
            CsvDelimiter = delimiter
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.CsvDelimiter);
    }

    [Fact]
    public void Validate_InvalidCsvDelimiter_ShouldHaveValidationError()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            RawDataContent = "test data",
            CsvDelimiter = '@'
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.CsvDelimiter);
    }

    [Fact]
    public void Validate_NegativeMaxRecords_ShouldHaveValidationError()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            RawDataContent = "test data",
            MaxRecords = -1
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.MaxRecords)
            .WithErrorMessage("Número máximo de registros deve ser maior ou igual a zero");
    }

    [Fact]
    public void Validate_MaxRecordsTooLarge_ShouldHaveValidationError()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            RawDataContent = "test data",
            MaxRecords = 100001
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.MaxRecords)
            .WithErrorMessage("Número máximo de registros não pode exceder 100.000");
    }

    [Fact]
    public void Validate_JsonFormat_ShouldNotValidateCsvDelimiter()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Json,
            RawDataContent = "{\"data\": []}",
            CsvDelimiter = '@' // Invalid for CSV, but should be ignored for JSON
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.CsvDelimiter);
    }

    [Fact]
    public void Validate_CompleteRequestWithAllOptions_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            RawDataContent = "PolicyNumber,Amount\n123456,1000.00\n123457,2000.00",
            ClearExistingData = true,
            ValidateForeignKeys = true,
            CsvDelimiter = ',',
            CsvHasHeaders = true,
            MaxRecords = 5000
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_JsonFormatWithValidData_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var jsonData = @"[
            {""policyNumber"": 123456, ""amount"": 1000.00},
            {""policyNumber"": 123457, ""amount"": 2000.00}
        ]";

        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Json,
            RawDataContent = jsonData,
            MaxRecords = 10
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_MaxRecordsZero_ShouldBeValid()
    {
        // Arrange - 0 means unlimited
        var request = new MockDataLoadRequest
        {
            EntityType = "premiums",
            Format = DataFormat.Csv,
            RawDataContent = "test data",
            MaxRecords = 0
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.MaxRecords);
    }
}
