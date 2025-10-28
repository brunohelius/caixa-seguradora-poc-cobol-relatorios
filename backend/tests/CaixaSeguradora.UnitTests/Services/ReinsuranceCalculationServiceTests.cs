using CaixaSeguradora.Infrastructure.Services;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace CaixaSeguradora.UnitTests.Services;

/// <summary>
/// Testes unitários para ReinsuranceCalculationService (equivalente ao módulo COBOL RE0001S).
/// Valida cálculos de resseguro usando implementação MOCK.
/// </summary>
public class ReinsuranceCalculationServiceTests
{
    private readonly ReinsuranceCalculationService _service;
    private readonly Mock<ILogger<ReinsuranceCalculationService>> _loggerMock;

    public ReinsuranceCalculationServiceTests()
    {
        _loggerMock = new Mock<ILogger<ReinsuranceCalculationService>>();
        _service = new ReinsuranceCalculationService(_loggerMock.Object);
    }

    [Fact]
    public async Task CalculateReinsurance_ValidPolicy_ReturnsMockAllocation()
    {
        // Arrange
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.Should().NotBeNull();
        result.ReturnCode.Should().Be("00", "o cálculo deve ser bem-sucedido");
        result.IsSuccess.Should().BeTrue();
        result.ReinsuredAmount.Should().BeGreaterThan(0);
        result.ReinsurancePercentage.Should().BeGreaterThan(0);
        result.TreatyCode.Should().NotBeNullOrEmpty();
        result.ContractCode.Should().NotBeNullOrEmpty();
        result.CutoffDate.Should().NotBeNull();
        result.ErrorMessage.Should().BeNull();
    }

    [Fact]
    public async Task CalculateReinsurance_HighPremium_Returns40PercentAllocation()
    {
        // Arrange - Prêmio alto (> 100.000) deve resultar em 40% de resseguro
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 150000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ReinsurancePercentage.Should().Be(40m, "prêmios altos devem ter 40% de resseguro");
        result.ReinsuredAmount.Should().Be(60000.00m, "40% de 150.000 = 60.000");
        result.ReturnCode.Should().Be("00");
    }

    [Fact]
    public async Task CalculateReinsurance_MediumPremium_Returns30PercentAllocation()
    {
        // Arrange - Prêmio médio (10.000 - 100.000) deve resultar em 30% de resseguro
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ReinsurancePercentage.Should().Be(30m, "prêmios médios devem ter 30% de resseguro");
        result.ReinsuredAmount.Should().Be(15000.00m, "30% de 50.000 = 15.000");
        result.ReturnCode.Should().Be("00");
    }

    [Fact]
    public async Task CalculateReinsurance_LowPremium_Returns20PercentAllocation()
    {
        // Arrange - Prêmio baixo (< 10.000) deve resultar em 20% de resseguro
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 5000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ReinsurancePercentage.Should().Be(20m, "prêmios baixos devem ter 20% de resseguro");
        result.ReinsuredAmount.Should().Be(1000.00m, "20% de 5.000 = 1.000");
        result.ReturnCode.Should().Be("00");
    }

    [Theory]
    [InlineData(40)] // Ramos GARANTIA
    [InlineData(45)]
    [InlineData(75)]
    [InlineData(76)]
    public async Task CalculateReinsurance_GarantiaBranch_AddsExtraPercentage(int susepBranchCode)
    {
        // Arrange - Ramos GARANTIA devem ter +5% adicional (CADMUS-154263)
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ReinsurancePercentage.Should().Be(35m, "ramos GARANTIA devem ter 30% + 5% = 35%");
        result.ReinsuredAmount.Should().Be(17500.00m, "35% de 50.000 = 17.500");
        result.ReturnCode.Should().Be("00");
    }

    [Fact]
    public async Task CalculateReinsurance_ZeroPremium_ReturnsValidationError()
    {
        // Arrange
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 0m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ReturnCode.Should().Be("08", "prêmio zero deve retornar erro de validação");
        result.IsSuccess.Should().BeFalse();
        result.ErrorMessage.Should().NotBeNullOrEmpty();
    }

    [Fact]
    public async Task CalculateReinsurance_NegativePremium_ReturnsValidationError()
    {
        // Arrange
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = -1000m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ReturnCode.Should().Be("08", "prêmio negativo deve retornar erro de validação");
        result.IsSuccess.Should().BeFalse();
        result.ErrorMessage.Should().Contain("maior que zero");
    }

    [Fact]
    public async Task CalculateReinsurance_InvalidPolicyNumber_ReturnsValidationError()
    {
        // Arrange
        const long policyNumber = 0;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ReturnCode.Should().Be("08", "número de apólice inválido deve retornar erro de validação");
        result.IsSuccess.Should().BeFalse();
        result.ErrorMessage.Should().Contain("apólice inválido");
    }

    [Fact]
    public async Task CalculateReinsurance_ValidRequest_GeneratesTreatyCode()
    {
        // Arrange
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.TreatyCode.Should().StartWith("TRT", "código de tratado deve começar com TRT");
        result.TreatyCode.Should().HaveLength(10, "código de tratado deve ter 10 caracteres");
        result.TreatyCode.Should().Contain(susepBranchCode.ToString("D4"), "código deve incluir código SUSEP");
    }

    [Fact]
    public async Task CalculateReinsurance_ValidRequest_GeneratesContractCode()
    {
        // Arrange
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 1);
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.ContractCode.Should().StartWith("CTR", "código de contrato deve começar com CTR");
        result.ContractCode.Should().HaveLength(15, "código de contrato deve ter 15 caracteres");
        result.ContractCode.Should().Contain(productCode.ToString("D4"), "código deve incluir código do produto");
        result.ContractCode.Should().Contain(effectiveDate.Year.ToString(), "código deve incluir ano");
    }

    [Fact]
    public async Task CalculateReinsurance_ValidRequest_GeneratesCutoffDate()
    {
        // Arrange
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 10, 15); // Mid-October
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.CutoffDate.Should().NotBeNull();
        result.CutoffDate.Value.Should().Be(new DateTime(2025, 11, 1), "cutoff date deve ser primeiro dia do mês seguinte");
    }

    [Fact]
    public async Task CalculateReinsurance_DecemberEffectiveDate_GeneratesJanuaryCutoff()
    {
        // Arrange
        const long policyNumber = 1234567890123;
        const decimal premiumAmount = 50000.00m;
        const int productCode = 1001;
        var effectiveDate = new DateTime(2025, 12, 15); // December
        const int susepBranchCode = 20;

        // Act
        var result = await _service.CalculateReinsuranceAsync(
            policyNumber,
            premiumAmount,
            productCode,
            effectiveDate,
            susepBranchCode);

        // Assert
        result.CutoffDate.Should().NotBeNull();
        result.CutoffDate.Value.Should().Be(new DateTime(2026, 1, 1), "cutoff date em dezembro deve gerar janeiro do ano seguinte");
    }
}
