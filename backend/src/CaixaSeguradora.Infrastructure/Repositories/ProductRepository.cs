using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for product information (V0PRODUTO, V0PRODUTOSVG views).
/// Maps to COBOL sections R0740-00-SELECT-V0PRODUTO, R1020-00-SELECT-V0PRODUTOSVG.
/// </summary>
public class ProductRepository : Repository<Product>, IProductRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public ProductRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async Task<Product?> GetByProductCodeAsync(int productCode, CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R0740-00-SELECT-V0PRODUTO:
        // SELECT * FROM V0PRODUTO WHERE COD_PROD = :productCode
        return await _premiumContext.Products
            .AsNoTracking()
            .FirstOrDefaultAsync(p => p.ProductCode == productCode, cancellationToken);
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Product> GetByLineOfBusinessAsync(
        int lineOfBusiness,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL: SELECT * FROM V0PRODUTO WHERE RAMO_SUSEP = :lineOfBusiness
        IOrderedQueryable<Product> query = _premiumContext.Products
            .AsNoTracking()
            .Where(p => p.LineOfBusiness == lineOfBusiness)
            .OrderBy(p => p.ProductCode);

        await foreach (Product? product in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return product;
        }
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Product> GetLifeInsuranceProductsAsync(
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R1020-00-SELECT-V0PRODUTOSVG:
        // SELECT * FROM V0PRODUTOSVG WHERE IND_VIDA_GRUPO = 'S'
        IOrderedQueryable<Product> query = _premiumContext.Products
            .AsNoTracking()
            .Where(p => p.IsLifeInsurance == "S")
            .OrderBy(p => p.ProductCode);

        await foreach (Product? product in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return product;
        }
    }

    /// <inheritdoc />
    public async Task<bool> IsAutoInsuranceProductAsync(int productCode, CancellationToken cancellationToken = default)
    {
        // Check if product belongs to auto insurance line of business
        // SUSEP code 0118 = Auto insurance
        var product = await _premiumContext.Products
            .AsNoTracking()
            .Where(p => p.ProductCode == productCode)
            .Select(p => p.LineOfBusiness)
            .FirstOrDefaultAsync(cancellationToken);

        return product == 118; // SUSEP Ramo 0118 = Auto
    }
}
