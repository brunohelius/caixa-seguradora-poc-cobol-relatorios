using CaixaSeguradora.Infrastructure.Services;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace CaixaSeguradora.UnitTests.Services;

/// <summary>
/// Testes unitários para FormattingService (equivalente ao módulo COBOL GE0009S).
/// Valida formatação de CPF/CNPJ e validação de dígitos verificadores.
/// </summary>
public class FormattingServiceTests
{
    private readonly FormattingService _service;
    private readonly Mock<ILogger<FormattingService>> _loggerMock;

    public FormattingServiceTests()
    {
        _loggerMock = new Mock<ILogger<FormattingService>>();
        _service = new FormattingService(_loggerMock.Object);
    }

    #region CPF Formatting Tests

    [Fact]
    public void FormatCpf_ValidCpf_ReturnsFormattedString()
    {
        // Arrange
        const string cpf = "12345678901";
        const string expected = "123.456.789-01";

        // Act
        var result = _service.FormatCpf(cpf);

        // Assert
        result.Should().Be(expected);
    }

    [Fact]
    public void FormatCpf_AlreadyFormatted_ReturnsFormattedString()
    {
        // Arrange
        const string cpf = "123.456.789-01";
        const string expected = "123.456.789-01";

        // Act
        var result = _service.FormatCpf(cpf);

        // Assert
        result.Should().Be(expected);
    }

    [Fact]
    public void FormatCpf_EmptyString_ReturnsEmptyString()
    {
        // Arrange
        const string cpf = "";

        // Act
        var result = _service.FormatCpf(cpf);

        // Assert
        result.Should().BeEmpty();
    }

    [Fact]
    public void FormatCpf_InvalidLength_ReturnsOriginal()
    {
        // Arrange
        const string cpf = "123456789"; // Only 9 digits

        // Act
        var result = _service.FormatCpf(cpf);

        // Assert
        result.Should().Be(cpf); // Returns original if invalid
    }

    #endregion

    #region CNPJ Formatting Tests

    [Fact]
    public void FormatCnpj_ValidCnpj_ReturnsFormattedString()
    {
        // Arrange
        const string cnpj = "12345678000195";
        const string expected = "12.345.678/0001-95";

        // Act
        var result = _service.FormatCnpj(cnpj);

        // Assert
        result.Should().Be(expected);
    }

    [Fact]
    public void FormatCnpj_AlreadyFormatted_ReturnsFormattedString()
    {
        // Arrange
        const string cnpj = "12.345.678/0001-95";
        const string expected = "12.345.678/0001-95";

        // Act
        var result = _service.FormatCnpj(cnpj);

        // Assert
        result.Should().Be(expected);
    }

    [Fact]
    public void FormatCnpj_EmptyString_ReturnsEmptyString()
    {
        // Arrange
        const string cnpj = "";

        // Act
        var result = _service.FormatCnpj(cnpj);

        // Assert
        result.Should().BeEmpty();
    }

    [Fact]
    public void FormatCnpj_InvalidLength_ReturnsOriginal()
    {
        // Arrange
        const string cnpj = "12345678"; // Only 8 digits

        // Act
        var result = _service.FormatCnpj(cnpj);

        // Assert
        result.Should().Be(cnpj); // Returns original if invalid
    }

    #endregion

    #region CPF Validation Tests

    [Theory]
    [InlineData("11144477735")] // Valid CPF
    [InlineData("52998224725")] // Valid CPF
    [InlineData("04952610179")] // Valid CPF with leading zero
    public void ValidateCpfCheckDigit_ValidCpf_ReturnsTrue(string cpf)
    {
        // Act
        var result = _service.ValidateCpfCheckDigit(cpf);

        // Assert
        result.Should().BeTrue();
    }

    [Theory]
    [InlineData("111.444.777-35")] // Valid CPF with formatting
    [InlineData("529.982.247-25")] // Valid CPF with formatting
    public void ValidateCpfCheckDigit_ValidFormattedCpf_ReturnsTrue(string cpf)
    {
        // Act
        var result = _service.ValidateCpfCheckDigit(cpf);

        // Assert
        result.Should().BeTrue();
    }

    [Theory]
    [InlineData("11144477736")] // Invalid check digit
    [InlineData("52998224720")] // Invalid check digit
    public void ValidateCpfCheckDigit_InvalidCpf_ReturnsFalse(string cpf)
    {
        // Act
        var result = _service.ValidateCpfCheckDigit(cpf);

        // Assert
        result.Should().BeFalse();
    }

    [Theory]
    [InlineData("00000000000")] // All zeros
    [InlineData("11111111111")] // All ones
    [InlineData("99999999999")] // All nines
    public void ValidateCpfCheckDigit_AllSameDigits_ReturnsFalse(string cpf)
    {
        // Act
        var result = _service.ValidateCpfCheckDigit(cpf);

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCpfCheckDigit_EmptyString_ReturnsFalse()
    {
        // Act
        var result = _service.ValidateCpfCheckDigit("");

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCpfCheckDigit_WrongLength_ReturnsFalse()
    {
        // Act
        var result = _service.ValidateCpfCheckDigit("123456789");

        // Assert
        result.Should().BeFalse();
    }

    #endregion

    #region CNPJ Validation Tests

    [Theory]
    [InlineData("11222333000181")] // Valid CNPJ
    [InlineData("00000000000191")] // Valid CNPJ with leading zeros
    public void ValidateCnpjCheckDigit_ValidCnpj_ReturnsTrue(string cnpj)
    {
        // Act
        var result = _service.ValidateCnpjCheckDigit(cnpj);

        // Assert
        result.Should().BeTrue();
    }

    [Theory]
    [InlineData("11.222.333/0001-81")] // Valid CNPJ with formatting
    public void ValidateCnpjCheckDigit_ValidFormattedCnpj_ReturnsTrue(string cnpj)
    {
        // Act
        var result = _service.ValidateCnpjCheckDigit(cnpj);

        // Assert
        result.Should().BeTrue();
    }

    [Theory]
    [InlineData("11222333000182")] // Invalid check digit
    [InlineData("00000000000192")] // Invalid check digit
    public void ValidateCnpjCheckDigit_InvalidCnpj_ReturnsFalse(string cnpj)
    {
        // Act
        var result = _service.ValidateCnpjCheckDigit(cnpj);

        // Assert
        result.Should().BeFalse();
    }

    [Theory]
    [InlineData("00000000000000")] // All zeros
    [InlineData("11111111111111")] // All ones
    [InlineData("99999999999999")] // All nines
    public void ValidateCnpjCheckDigit_AllSameDigits_ReturnsFalse(string cnpj)
    {
        // Act
        var result = _service.ValidateCnpjCheckDigit(cnpj);

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCnpjCheckDigit_EmptyString_ReturnsFalse()
    {
        // Act
        var result = _service.ValidateCnpjCheckDigit("");

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCnpjCheckDigit_WrongLength_ReturnsFalse()
    {
        // Act
        var result = _service.ValidateCnpjCheckDigit("123456789012");

        // Assert
        result.Should().BeFalse();
    }

    #endregion

    #region Date Formatting Tests

    [Fact]
    public void FormatDate_DefaultFormat_ReturnsFormattedDate()
    {
        // Arrange
        var date = new DateTime(2025, 10, 27);
        const string expected = "27/10/2025";

        // Act
        var result = _service.FormatDate(date);

        // Assert
        result.Should().Be(expected);
    }

    [Fact]
    public void FormatDate_CustomFormat_ReturnsFormattedDate()
    {
        // Arrange
        var date = new DateTime(2025, 10, 27);
        const string format = "yyyy-MM-dd";
        const string expected = "2025-10-27";

        // Act
        var result = _service.FormatDate(date, format);

        // Assert
        result.Should().Be(expected);
    }

    [Fact]
    public void FormatDate_YearMonthFormat_ReturnsFormattedDate()
    {
        // Arrange
        var date = new DateTime(2025, 10, 27);
        const string format = "yyyyMM";
        const string expected = "202510";

        // Act
        var result = _service.FormatDate(date, format);

        // Assert
        result.Should().Be(expected);
    }

    #endregion

    #region RemoveFormatting Tests

    [Theory]
    [InlineData("123.456.789-01", "12345678901")]
    [InlineData("12.345.678/0001-95", "12345678000195")]
    [InlineData("12345678901", "12345678901")]
    [InlineData("(11) 98765-4321", "11987654321")]
    public void RemoveFormatting_VariousFormats_ReturnsOnlyDigits(string input, string expected)
    {
        // Act
        var result = _service.RemoveFormatting(input);

        // Assert
        result.Should().Be(expected);
    }

    [Fact]
    public void RemoveFormatting_EmptyString_ReturnsEmptyString()
    {
        // Act
        var result = _service.RemoveFormatting("");

        // Assert
        result.Should().BeEmpty();
    }

    [Fact]
    public void RemoveFormatting_OnlyNonDigits_ReturnsEmptyString()
    {
        // Act
        var result = _service.RemoveFormatting("abc-./");

        // Assert
        result.Should().BeEmpty();
    }

    #endregion
}
