using Xunit;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;
using Microsoft.Data.Sqlite;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.UnitTests.Data;

/// <summary>
/// Unit tests for ReadOnlyDbCommandInterceptor - User Story 2, Task T244
/// Tests that write operations are blocked when read-only mode is enabled (SC-007)
/// </summary>
public class ReadOnlyDbCommandInterceptorTests : IDisposable
{
    private readonly Mock<ILogger<ReadOnlyDbCommandInterceptor>> _mockLogger;
    private readonly SqliteConnection _connection;
    private readonly PremiumReportingDbContext _readOnlyContext;
    private readonly PremiumReportingDbContext _writableContext;

    public ReadOnlyDbCommandInterceptorTests()
    {
        _mockLogger = new Mock<ILogger<ReadOnlyDbCommandInterceptor>>();

        // Create in-memory SQLite database
        _connection = new SqliteConnection("DataSource=:memory:");
        _connection.Open();

        // Create context with read-only interceptor enabled
        var readOnlyInterceptor = new ReadOnlyDbCommandInterceptor(_mockLogger.Object, isEnabled: true);
        DbContextOptions<PremiumReportingDbContext> readOnlyOptions = new DbContextOptionsBuilder<PremiumReportingDbContext>()
            .UseSqlite(_connection)
            .AddInterceptors(readOnlyInterceptor)
            .Options;
        _readOnlyContext = new PremiumReportingDbContext(readOnlyOptions);

        // Create context without interceptor for setup
        DbContextOptions<PremiumReportingDbContext> writableOptions = new DbContextOptionsBuilder<PremiumReportingDbContext>()
            .UseSqlite(_connection)
            .Options;
        _writableContext = new PremiumReportingDbContext(writableOptions);

        // Ensure database is created
        _writableContext.Database.EnsureCreated();
    }

    public void Dispose()
    {
        _readOnlyContext?.Dispose();
        _writableContext?.Dispose();
        _connection?.Dispose();
    }

    #region Write Operation Blocking Tests

    [Fact]
    public async Task SaveChangesAsync_WithInsert_ThrowsInvalidOperationException()
    {
        // Arrange
        var product = new Product
        {
            ProductCode = 1001,
            ProductName = "Test Product",
            LineOfBusiness = 1001,
            ProductType = "TestType",
            CompanyCode = 1
        };
        _readOnlyContext.Products.Add(product);

        // Act
        Func<Task> act = async () => await _readOnlyContext.SaveChangesAsync();

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*INSERT*não permitida*SC-007*");
    }

    [Fact]
    public async Task SaveChangesAsync_WithUpdate_ThrowsInvalidOperationException()
    {
        // Arrange - First add data using writable context
        var product = new Product
        {
            ProductCode = 1001,
            ProductName = "Original Name",
            LineOfBusiness = 1001,
            ProductType = "TestType",
            CompanyCode = 1
        };
        _writableContext.Products.Add(product);
        await _writableContext.SaveChangesAsync();

        // Load and modify using read-only context
        Product? loadedProduct = await _readOnlyContext.Products.FindAsync(1001);
        loadedProduct!.ProductName = "Updated Name";

        // Act
        Func<Task> act = async () => await _readOnlyContext.SaveChangesAsync();

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*UPDATE*não permitida*SC-007*");
    }

    [Fact]
    public async Task SaveChangesAsync_WithDelete_ThrowsInvalidOperationException()
    {
        // Arrange - First add data using writable context
        var product = new Product
        {
            ProductCode = 1001,
            ProductName = "Test Product",
            LineOfBusiness = 1001,
            ProductType = "TestType",
            CompanyCode = 1
        };
        _writableContext.Products.Add(product);
        await _writableContext.SaveChangesAsync();

        // Load and delete using read-only context
        Product? loadedProduct = await _readOnlyContext.Products.FindAsync(1001);
        _readOnlyContext.Products.Remove(loadedProduct!);

        // Act
        Func<Task> act = async () => await _readOnlyContext.SaveChangesAsync();

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*DELETE*não permitida*SC-007*");
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_WithInsert_ThrowsInvalidOperationException()
    {
        // Arrange
        var sql = "INSERT INTO Products (ProductCode, ProductName, LineOfBusiness, ProductType, CompanyCode) VALUES (1001, 'Test', 1001, 'Type', 1)";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*INSERT*não permitida*SC-007*");
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_WithUpdate_ThrowsInvalidOperationException()
    {
        // Arrange
        var sql = "UPDATE Products SET ProductName = 'Updated' WHERE ProductCode = 1001";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*UPDATE*não permitida*SC-007*");
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_WithDelete_ThrowsInvalidOperationException()
    {
        // Arrange
        var sql = "DELETE FROM Products WHERE ProductCode = 1001";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*DELETE*não permitida*SC-007*");
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_WithDrop_ThrowsInvalidOperationException()
    {
        // Arrange
        var sql = "DROP TABLE Products";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*DROP*não permitida*SC-007*");
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_WithCreate_ThrowsInvalidOperationException()
    {
        // Arrange
        var sql = "CREATE TABLE TestTable (Id INTEGER PRIMARY KEY)";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*CREATE*não permitida*SC-007*");
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_WithAlter_ThrowsInvalidOperationException()
    {
        // Arrange
        var sql = "ALTER TABLE Products ADD COLUMN NewColumn TEXT";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*ALTER*não permitida*SC-007*");
    }

    #endregion

    #region Read Operation Allowing Tests

    [Fact]
    public async Task ToListAsync_WithSelect_DoesNotThrow()
    {
        // Arrange - Add some test data first
        _writableContext.Products.Add(new Product
        {
            ProductCode = 1001,
            ProductName = "Test Product",
            LineOfBusiness = 1001,
            ProductType = "TestType",
            CompanyCode = 1
        });
        await _writableContext.SaveChangesAsync();

        // Act
        Func<Task> act = async () => await _readOnlyContext.Products.ToListAsync();

        // Assert
        await act.Should().NotThrowAsync();
    }

    [Fact]
    public async Task CountAsync_WithSelect_DoesNotThrow()
    {
        // Act
        Func<Task> act = async () => await _readOnlyContext.Products.CountAsync();

        // Assert
        await act.Should().NotThrowAsync();
    }

    [Fact]
    public async Task FirstOrDefaultAsync_WithSelect_DoesNotThrow()
    {
        // Arrange - Add test data
        _writableContext.Products.Add(new Product
        {
            ProductCode = 1001,
            ProductName = "Test Product",
            LineOfBusiness = 1001,
            ProductType = "TestType",
            CompanyCode = 1
        });
        await _writableContext.SaveChangesAsync();

        // Act
        Func<Task> act = async () => await _readOnlyContext.Products.FirstOrDefaultAsync();

        // Assert
        await act.Should().NotThrowAsync();
    }

    [Fact]
    public async Task Where_WithComplexQuery_DoesNotThrow()
    {
        // Act
        Func<Task> act = async () =>
        {
            IQueryable<Product> query = _readOnlyContext.Products
                .Where(p => p.ProductCode > 1000)
                .OrderBy(p => p.ProductName)
                .Take(10);
            await query.ToListAsync();
        };

        // Assert
        await act.Should().NotThrowAsync();
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_WithSelect_DoesNotThrow()
    {
        // Arrange
        var sql = "SELECT * FROM Products";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert - SELECT won't throw, but will return -1 (no rows affected)
        await act.Should().NotThrowAsync();
    }

    #endregion

    #region SQL Comment Handling Tests

    [Fact]
    public async Task ExecuteSqlRawAsync_InsertWithComments_StillBlocks()
    {
        // Arrange
        var sql = @"
            -- This is a comment before INSERT
            INSERT INTO Products (ProductCode, ProductName, LineOfBusiness, ProductType, CompanyCode)
            VALUES (1001, 'Test', 1001, 'Type', 1) -- Comment after
        ";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*INSERT*não permitida*");
    }

    [Fact]
    public async Task ExecuteSqlRawAsync_MultilineCommentedInsert_StillBlocks()
    {
        // Arrange
        var sql = @"
            /*
               Multi-line comment
               before the statement
            */
            INSERT INTO Products (ProductCode, ProductName, LineOfBusiness, ProductType, CompanyCode)
            VALUES (1001, 'Test', 1001, 'Type', 1)
        ";

        // Act
        Func<Task> act = async () => await _readOnlyContext.Database.ExecuteSqlRawAsync(sql);

        // Assert
        await act.Should().ThrowAsync<InvalidOperationException>()
            .WithMessage("*INSERT*não permitida*");
    }

    #endregion

    #region Disabled Interceptor Tests

    [Fact]
    public async Task SaveChangesAsync_DisabledInterceptor_AllowsInsert()
    {
        // Arrange - Use writable context (no interceptor enabled)
        var product = new Product
        {
            ProductCode = 2001,
            ProductName = "Test Product",
            LineOfBusiness = 2001,
            ProductType = "TestType",
            CompanyCode = 1
        };
        _writableContext.Products.Add(product);

        // Act
        Func<Task> act = async () => await _writableContext.SaveChangesAsync();

        // Assert
        await act.Should().NotThrowAsync();

        // Verify product was inserted
        Product? inserted = await _writableContext.Products.FindAsync(2001);
        inserted.Should().NotBeNull();
        inserted!.ProductName.Should().Be("Test Product");
    }

    #endregion

    #region Edge Cases

    [Fact]
    public async Task AsNoTracking_WithReadOnlyContext_StillAllowsReads()
    {
        // Arrange
        _writableContext.Products.Add(new Product
        {
            ProductCode = 3001,
            ProductName = "Test",
            LineOfBusiness = 3001,
            ProductType = "Type",
            CompanyCode = 1
        });
        await _writableContext.SaveChangesAsync();

        // Act
        Func<Task> act = async () =>
        {
            await _readOnlyContext.Products
                .AsNoTracking()
                .ToListAsync();
        };

        // Assert
        await act.Should().NotThrowAsync();
    }

    #endregion
}
