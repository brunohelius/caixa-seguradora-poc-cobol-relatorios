using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Product controller for querying insurance products.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
[ApiController]
[Route("api/products")]
[Produces("application/json")]
public class ProductController : ControllerBase
{
    private readonly IProductRepository _productRepository;
    private readonly ILogger<ProductController> _logger;

    public ProductController(
        IProductRepository productRepository,
        ILogger<ProductController> logger)
    {
        _productRepository = productRepository;
        _logger = logger;
    }

    /// <summary>
    /// Gets all products.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of all products</returns>
    [HttpGet]
    [ProducesResponseType(typeof(List<ProductDto>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<List<ProductDto>>> GetAllProducts(
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching all products");

            IReadOnlyList<Core.Entities.Product> products = await _productRepository.GetAllAsync(cancellationToken);

            var productDtos = products.Select(p => new ProductDto
            {
                ProductCode = p.ProductCode,
                ProductDescription = p.ProductDescription,
                LineOfBusinessCode = p.LineOfBusinessCode,
                LineOfBusinessDescription = p.LineOfBusinessDescription,
                IsActive = p.IsActive
            }).ToList();

            _logger.LogInformation("Retrieved {Count} products", productDtos.Count);

            return Ok(productDtos);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching products");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve products",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets a single product by code.
    /// </summary>
    /// <param name="code">Product code</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Product details</returns>
    [HttpGet("{code}")]
    [ProducesResponseType(typeof(ProductDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ProductDto>> GetProductByCode(
        int code,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching product: {ProductCode}", code);

            Core.Entities.Product? product = await _productRepository.GetByProductCodeAsync(code, cancellationToken);

            if (product == null)
            {
                _logger.LogWarning("Product not found: {ProductCode}", code);
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Product not found",
                    Details = $"No product found with code: {code}",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var productDto = new ProductDto
            {
                ProductCode = product.ProductCode,
                ProductDescription = product.ProductDescription,
                LineOfBusinessCode = product.LineOfBusinessCode,
                LineOfBusinessDescription = product.LineOfBusinessDescription,
                IsActive = product.IsActive
            };

            return Ok(productDto);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching product {ProductCode}", code);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve product",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Gets products by line of business.
    /// </summary>
    /// <param name="lineOfBusiness">Line of business code</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of products for the line of business</returns>
    [HttpGet("by-line-of-business/{lineOfBusiness}")]
    [ProducesResponseType(typeof(List<ProductDto>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<List<ProductDto>>> GetProductsByLineOfBusiness(
        int lineOfBusiness,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching products for line of business: {LOB}", lineOfBusiness);

            IReadOnlyList<Core.Entities.Product> products = await _productRepository
                .FindAsync(p => p.LineOfBusinessCode == lineOfBusiness, cancellationToken);

            var productDtos = products.Select(p => new ProductDto
            {
                ProductCode = p.ProductCode,
                ProductDescription = p.ProductDescription,
                LineOfBusinessCode = p.LineOfBusinessCode,
                LineOfBusinessDescription = p.LineOfBusinessDescription,
                IsActive = p.IsActive
            }).ToList();

            return Ok(productDtos);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching products for LOB {LOB}", lineOfBusiness);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve products",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }
}

/// <summary>
/// Product data transfer object.
/// </summary>
public class ProductDto
{
    public int ProductCode { get; set; }
    public string ProductDescription { get; set; } = string.Empty;
    public int LineOfBusinessCode { get; set; }
    public string? LineOfBusinessDescription { get; set; }
    public bool IsActive { get; set; }
}
