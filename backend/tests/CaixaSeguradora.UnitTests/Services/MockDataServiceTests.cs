using Xunit;
using Moq;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using CaixaSeguradora.Infrastructure.Services;
using CaixaSeguradora.Infrastructure.Data;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.EntityFrameworkCore;
using System.Text;

namespace CaixaSeguradora.UnitTests.Services;

/// <summary>
/// Unit tests for MockDataService - User Story 5
/// Tests CSV/JSON data loading, validation, and database management functionality
/// </summary>
public class MockDataServiceTests : IDisposable
{
    private readonly Mock<ILogger<MockDataService>> _mockLogger;
    private readonly Mock<ILogger<CsvDataLoader>> _csvLogger;
    private readonly Mock<ILogger<JsonDataLoader>> _jsonLogger;
    private readonly Mock<IDataValidationService> _mockValidationService;
    private readonly PremiumReportingDbContext _context;
    private readonly MockDataService _service;

    public MockDataServiceTests()
    {
        // Create in-memory database for testing
        var options = new DbContextOptionsBuilder<PremiumReportingDbContext>()
            .UseInMemoryDatabase(databaseName: $"TestDb_{Guid.NewGuid()}")
            .Options;

        _context = new PremiumReportingDbContext(options);
        _mockLogger = new Mock<ILogger<MockDataService>>();
        _csvLogger = new Mock<ILogger<CsvDataLoader>>();
        _jsonLogger = new Mock<ILogger<JsonDataLoader>>();
        _mockValidationService = new Mock<IDataValidationService>();

        var csvLoader = new CsvDataLoader(_context, _csvLogger.Object);
        var jsonLoader = new JsonDataLoader(_context, _jsonLogger.Object);

        _service = new MockDataService(
            _context,
            csvLoader,
            jsonLoader,
            _mockValidationService.Object,
            _mockLogger.Object
        );
    }

    public void Dispose()
    {
        _context.Database.EnsureDeleted();
        _context.Dispose();
    }

    [Fact]
    public async Task GetRecordCountsAsync_EmptyDatabase_ReturnsZeroCounts()
    {
        // Act
        Dictionary<string, int> counts = await _service.GetRecordCountsAsync(CancellationToken.None);

        // Assert
        counts.Should().NotBeNull();
        counts.Should().ContainKey("Premiums");
        counts.Should().ContainKey("Policies");
        counts.Should().ContainKey("Clients");
        counts.Should().ContainKey("Products");
        counts["Premiums"].Should().Be(0);
        counts["Policies"].Should().Be(0);
        counts["Clients"].Should().Be(0);
    }

    [Fact]
    public async Task GetRecordCountsAsync_WithData_ReturnsCorrectCounts()
    {
        // Arrange - Add test data
        _context.Products.AddRange(
            new Product { ProductCode = 1001, ProductName = "Test Product 1", LineOfBusiness = 1001, ProductType = "Type1", CompanyCode = 1 },
            new Product { ProductCode = 1002, ProductName = "Test Product 2", LineOfBusiness = 1002, ProductType = "Type2", CompanyCode = 1 },
            new Product { ProductCode = 1003, ProductName = "Test Product 3", LineOfBusiness = 1003, ProductType = "Type3", CompanyCode = 1 }
        );
        await _context.SaveChangesAsync();

        // Act
        Dictionary<string, int> counts = await _service.GetRecordCountsAsync(CancellationToken.None);

        // Assert
        counts["Products"].Should().Be(3);
        counts["Clients"].Should().Be(0);
    }

    [Fact]
    public async Task LoadDataFromStreamAsync_ValidCsv_ReturnsSuccessResponse()
    {
        // Arrange
        var csvContent = @"ProductCode,ProductName,LineOfBusiness,ProductType,CompanyCode
1001,Seguro Residencial,1001,Habitacional,1
1002,Seguro Auto,1002,Automóvel,1";
        var stream = new MemoryStream(Encoding.UTF8.GetBytes(csvContent));

        // Act
        var response = await _service.LoadDataFromStreamAsync(
            stream,
            "products",
            DataFormat.Csv,
            clearExisting: false,
            validateForeignKeys: false,
            CancellationToken.None
        );

        // Assert
        response.Should().NotBeNull();
        response.Success.Should().BeTrue();
        response.RecordsInserted.Should().Be(2);
        response.RecordsFailed.Should().Be(0);
        response.EntityType.Should().Be("products");

        List<Product> products = await _context.Products.ToListAsync();
        products.Should().HaveCount(2);
        products.Should().Contain(p => p.ProductCode == 1001);
        products.Should().Contain(p => p.ProductCode == 1002);
    }

    [Fact]
    public async Task LoadDataFromStreamAsync_InvalidEntityType_ThrowsException()
    {
        // Arrange
        var csvContent = "test,data\n1,2";
        var stream = new MemoryStream(Encoding.UTF8.GetBytes(csvContent));

        // Act & Assert
        await Assert.ThrowsAsync<ArgumentException>(async () =>
            await _service.LoadDataFromStreamAsync(
                stream,
                "invalid_entity",
                DataFormat.Csv,
                clearExisting: false,
                validateForeignKeys: false,
                CancellationToken.None
            )
        );
    }

    [Fact]
    public async Task ClearEntityDataAsync_WithData_RemovesAllRecords()
    {
        // Arrange
        _context.Products.AddRange(
            new Product { ProductCode = 1001, ProductName = "Test 1", LineOfBusiness = 1001, ProductType = "Type1", CompanyCode = 1 },
            new Product { ProductCode = 1002, ProductName = "Test 2", LineOfBusiness = 1002, ProductType = "Type2", CompanyCode = 1 }
        );
        await _context.SaveChangesAsync();

        // Act
        var deletedCount = await _service.ClearEntityDataAsync("products", CancellationToken.None);

        // Assert
        deletedCount.Should().Be(2);
        List<Product> remainingProducts = await _context.Products.ToListAsync();
        remainingProducts.Should().BeEmpty();
    }

    [Fact]
    public async Task ResetDatabaseAsync_WithMultipleEntities_ClearsAllTables()
    {
        // Arrange
        _context.Products.Add(new Product { ProductCode = 1001, ProductName = "Test", LineOfBusiness = 1001, ProductType = "Type1", CompanyCode = 1 });
        _context.Clients.Add(new Client { ClientCode = 500001, ClientName = "Test Client", ClientType = "PF", DocumentNumber = "12345678901", CompanyCode = 1 });
        await _context.SaveChangesAsync();

        // Act
        var totalDeleted = await _service.ResetDatabaseAsync(CancellationToken.None);

        // Assert
        totalDeleted.Should().BeGreaterThan(0);
        var productCount = await _context.Products.CountAsync();
        var clientCount = await _context.Clients.CountAsync();
        productCount.Should().Be(0);
        clientCount.Should().Be(0);
    }

    [Fact]
    public async Task GetSchemaInfoAsync_ReturnsAllEntityTypes()
    {
        // Act
        Dictionary<string, object> schema = await _service.GetSchemaInfoAsync(null, CancellationToken.None);

        // Assert
        schema.Should().NotBeNull();
        schema.Should().ContainKey("EntityTypes");
        var entityTypes = schema["EntityTypes"] as List<string>;
        entityTypes.Should().NotBeNull();
        entityTypes.Should().Contain("Premiums");
        entityTypes.Should().Contain("Policies");
        entityTypes.Should().Contain("Products");
        entityTypes.Should().Contain("Clients");
        entityTypes.Should().HaveCountGreaterThanOrEqualTo(13);
    }

    [Fact]
    public async Task LoadDataFromStreamAsync_WithClearExisting_RemovesOldData()
    {
        // Arrange - Add initial data
        _context.Products.Add(new Product { ProductCode = 9999, ProductName = "Old Product", LineOfBusiness = 9999, ProductType = "Old", CompanyCode = 1 });
        await _context.SaveChangesAsync();

        var csvContent = @"ProductCode,ProductName,LineOfBusiness,ProductType,CompanyCode
1001,New Product,1001,New,1";
        var stream = new MemoryStream(Encoding.UTF8.GetBytes(csvContent));

        // Act
        var response = await _service.LoadDataFromStreamAsync(
            stream,
            "products",
            DataFormat.Csv,
            clearExisting: true,
            validateForeignKeys: false,
            CancellationToken.None
        );

        // Assert
        response.RecordsDeleted.Should().Be(1);
        response.RecordsInserted.Should().Be(1);

        List<Product> products = await _context.Products.ToListAsync();
        products.Should().HaveCount(1);
        products.First().ProductCode.Should().Be(1001);
        products.Should().NotContain(p => p.ProductCode == 9999);
    }

    [Fact]
    public async Task LoadDataFromStreamAsync_MalformedCsv_ReturnsValidationErrors()
    {
        // Arrange - Missing required column
        var csvContent = @"ProductCode,ProductName
1001,Test Product";
        var stream = new MemoryStream(Encoding.UTF8.GetBytes(csvContent));

        // Act
        var response = await _service.LoadDataFromStreamAsync(
            stream,
            "products",
            DataFormat.Csv,
            clearExisting: false,
            validateForeignKeys: false,
            CancellationToken.None
        );

        // Assert
        response.Success.Should().BeFalse();
        response.ValidationErrors.Should().NotBeEmpty();
    }
}
