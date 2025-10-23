using CaixaSeguradora.Api.Validators;
using CaixaSeguradora.Core.DTOs;
using FluentValidation.TestHelper;
using Xunit;

namespace CaixaSeguradora.UnitTests.Validators;

public class LoginRequestValidatorTests
{
    private readonly LoginRequestValidator _validator;

    public LoginRequestValidatorTests()
    {
        _validator = new LoginRequestValidator();
    }

    [Fact]
    public void Validate_ValidLoginRequest_ShouldNotHaveValidationErrors()
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = "testuser",
            Password = "password123"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }

    [Fact]
    public void Validate_EmptyUsername_ShouldHaveValidationError()
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = "",
            Password = "password123"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Username)
            .WithErrorMessage("Nome de usuário é obrigatório");
    }

    [Fact]
    public void Validate_UsernameTooShort_ShouldHaveValidationError()
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = "ab",
            Password = "password123"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Username)
            .WithErrorMessage("Nome de usuário deve ter no mínimo 3 caracteres");
    }

    [Fact]
    public void Validate_UsernameTooLong_ShouldHaveValidationError()
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = new string('a', 51), // 51 characters
            Password = "password123"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Username)
            .WithErrorMessage("Nome de usuário não pode exceder 50 caracteres");
    }

    [Fact]
    public void Validate_EmptyPassword_ShouldHaveValidationError()
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = "testuser",
            Password = ""
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Password)
            .WithErrorMessage("Senha é obrigatória");
    }

    [Fact]
    public void Validate_PasswordTooShort_ShouldHaveValidationError()
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = "testuser",
            Password = "12345"
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Password)
            .WithErrorMessage("Senha deve ter no mínimo 6 caracteres");
    }

    [Fact]
    public void Validate_PasswordTooLong_ShouldHaveValidationError()
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = "testuser",
            Password = new string('a', 101) // 101 characters
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldHaveValidationErrorFor(x => x.Password)
            .WithErrorMessage("Senha não pode exceder 100 caracteres");
    }

    [Theory]
    [InlineData("usr", "password123")]
    [InlineData("validuser", "pass12")]
    [InlineData("user.name@email", "mySecurePassword!123")]
    public void Validate_ValidCredentials_ShouldNotHaveValidationErrors(string username, string password)
    {
        // Arrange
        var request = new LoginRequest
        {
            Username = username,
            Password = password
        };

        // Act
        var result = _validator.TestValidate(request);

        // Assert
        result.ShouldNotHaveAnyValidationErrors();
    }
}
