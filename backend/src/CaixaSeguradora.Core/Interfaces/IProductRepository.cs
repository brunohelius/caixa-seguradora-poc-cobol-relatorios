using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for product information (V0PRODUTO, V0PRODUTOSVG views).
/// Accessed at COBOL sections R0740-00-SELECT-V0PRODUTO, R1020-00-SELECT-V0PRODUTOSVG.
/// </summary>
public interface IProductRepository : IRepository<Product>
{
    /// <summary>
    /// Gets product by product code.
    /// </summary>
    Task<Product?> GetByProductCodeAsync(int productCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets products by line of business (Ramo).
    /// </summary>
    IAsyncEnumerable<Product> GetByLineOfBusinessAsync(int lineOfBusiness, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets life insurance products (Vida e Geração).
    /// Maps to COBOL section R1020-00-SELECT-V0PRODUTOSVG.
    /// </summary>
    IAsyncEnumerable<Product> GetLifeInsuranceProductsAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Checks if product is an auto insurance product.
    /// Used for specific processing rules.
    /// </summary>
    Task<bool> IsAutoInsuranceProductAsync(int productCode, CancellationToken cancellationToken = default);
}
