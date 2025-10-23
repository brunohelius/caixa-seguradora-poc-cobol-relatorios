using CaixaSeguradora.Api.Validators;
using CaixaSeguradora.Core.DTOs;
using FluentValidation.TestHelper;
using Xunit;

namespace CaixaSeguradora.UnitTests.Validators;

public class PremiumQueryValidatorTests
{
    private readonly PremiumQueryValidator _validator;

    public PremiumQueryValidatorTests()
    {
        _validator = new PremiumQueryValidator();
    }

    [Fact]
    public void Validate_ValidQuery_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            Page = 1,
            PageSize = 50,
            SortBy = "ReferenceDate",
            SortOrder = "desc"
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_StartDateAfterEndDate_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            StartDate = new DateTime(2025, 10, 31),
            EndDate = new DateTime(2025, 10, 1),
            Page = 1,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.StartDate)
            .WithErrorMessage("Data inicial deve ser menor ou igual à data final");
    }

    [Fact]
    public void Validate_MinPremiumGreaterThanMax_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            MinPremiumAmount = 1000m,
            MaxPremiumAmount = 500m,
            Page = 1,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.MinPremiumAmount)
            .WithErrorMessage("Valor mínimo de prêmio deve ser menor ou igual ao valor máximo");
    }

    [Fact]
    public void Validate_NegativeMinPremiumAmount_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            MinPremiumAmount = -100m,
            Page = 1,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.MinPremiumAmount)
            .WithErrorMessage("Valor mínimo de prêmio deve ser maior ou igual a zero");
    }

    [Fact]
    public void Validate_PageZero_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            Page = 0,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Page)
            .WithErrorMessage("Número da página deve ser maior ou igual a 1");
    }

    [Fact]
    public void Validate_PageSizeTooLarge_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            Page = 1,
            PageSize = 1001
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.PageSize)
            .WithErrorMessage("Tamanho da página não pode exceder 1000 registros");
    }

    [Fact]
    public void Validate_PageSizeZero_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            Page = 1,
            PageSize = 0
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.PageSize)
            .WithErrorMessage("Tamanho da página deve ser maior ou igual a 1");
    }

    [Theory]
    [InlineData("InvalidField")]
    [InlineData("")]
    [InlineData("Amount")]
    public void Validate_InvalidSortByField_ShouldHaveValidationError(string sortBy)
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            Page = 1,
            PageSize = 50,
            SortBy = sortBy,
            SortOrder = "asc"
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.SortBy);
    }

    [Theory]
    [InlineData("PolicyNumber")]
    [InlineData("ReferenceDate")]
    [InlineData("ProductCode")]
    [InlineData("BasePremium")]
    [InlineData("TariffPremium")]
    [InlineData("NetPremium")]
    [InlineData("MovementType")]
    public void Validate_ValidSortByFields_ShouldNotHaveValidationErrors(string sortBy)
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            Page = 1,
            PageSize = 50,
            SortBy = sortBy,
            SortOrder = "asc"
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.SortBy);
    }

    [Theory]
    [InlineData("ascending")]
    [InlineData("")]
    [InlineData("up")]
    public void Validate_InvalidSortOrder_ShouldHaveValidationError(string sortOrder)
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            Page = 1,
            PageSize = 50,
            SortBy = "ReferenceDate",
            SortOrder = sortOrder
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.SortOrder);
    }

    [Theory]
    [InlineData("asc")]
    [InlineData("desc")]
    [InlineData("ASC")]
    [InlineData("DESC")]
    public void Validate_ValidSortOrders_ShouldNotHaveValidationErrors(string sortOrder)
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            Page = 1,
            PageSize = 50,
            SortBy = "ReferenceDate",
            SortOrder = sortOrder
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.SortOrder);
    }

    [Fact]
    public void Validate_InvalidMovementTypeLength_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            MovementType = "EM",
            Page = 1,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.MovementType)
            .WithErrorMessage("Tipo de movimento deve ter exatamente 1 caractere");
    }

    [Theory]
    [InlineData("E")]
    [InlineData("C")]
    [InlineData("R")]
    [InlineData("A")]
    [InlineData("S")]
    public void Validate_ValidMovementTypes_ShouldNotHaveValidationErrors(string movementType)
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            MovementType = movementType,
            Page = 1,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldNotHaveValidationErrorFor(x => x.MovementType);
    }

    [Fact]
    public void Validate_NegativePolicyNumber_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            PolicyNumber = -1,
            Page = 1,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.PolicyNumber)
            .WithErrorMessage("Número da apólice deve ser maior que zero");
    }

    [Fact]
    public void Validate_NegativeProductCode_ShouldHaveValidationError()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            ProductCode = -1,
            Page = 1,
            PageSize = 50
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.ProductCode)
            .WithErrorMessage("Código do produto deve ser maior que zero");
    }

    [Fact]
    public void Validate_CompleteQueryWithAllFilters_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var query = new PremiumQueryDto
        {
            PolicyNumber = 123456,
            StartDate = new DateTime(2025, 10, 1),
            EndDate = new DateTime(2025, 10, 31),
            ProductCode = 100,
            LineOfBusiness = 50,
            MovementType = "E",
            CompanyCode = 1,
            AgencyCode = 100,
            ProducerCode = 500,
            MinPremiumAmount = 100m,
            MaxPremiumAmount = 10000m,
            Page = 1,
            PageSize = 100,
            SortBy = "BasePremium",
            SortOrder = "desc"
        };

        // Act
        var result = _validator.TestValidate(query);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }
}
