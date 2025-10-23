using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.AspNetCore.Mvc;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Client controller for querying client (insured) data.
/// Supports User Story 3 - Query and Visualize Premium Data.
/// </summary>
[ApiController]
[Route("api/clients")]
[Produces("application/json")]
public class ClientController : ControllerBase
{
    private readonly IClientRepository _clientRepository;
    private readonly ILogger<ClientController> _logger;

    public ClientController(
        IClientRepository clientRepository,
        ILogger<ClientController> logger)
    {
        _clientRepository = clientRepository;
        _logger = logger;
    }

    /// <summary>
    /// Gets a client by code.
    /// </summary>
    /// <param name="clientCode">Client code</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Client details</returns>
    [HttpGet("{clientCode}")]
    [ProducesResponseType(typeof(ClientDto), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status404NotFound)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<ClientDto>> GetClientByCode(
        long clientCode,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Fetching client: {ClientCode}", clientCode);

            var client = await _clientRepository.GetByClientCodeAsync((int)clientCode, cancellationToken);

            if (client == null)
            {
                _logger.LogWarning("Client not found: {ClientCode}", clientCode);
                return NotFound(new ErrorResponse
                {
                    StatusCode = 404,
                    Message = "Client not found",
                    Details = $"No client found with code: {clientCode}",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            var clientDto = new ClientDto
            {
                ClientCode = client.ClientCode,
                ClientName = client.ClientName,
                ClientType = client.ClientType,
                DocumentNumber = client.DocumentNumber,
                DocumentType = client.DocumentType,
                BirthDate = client.BirthDateParsed,
                Gender = client.Gender,
                Email = client.Email,
                PhoneNumber = client.PhoneNumber
            };

            return Ok(clientDto);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error fetching client {ClientCode}", clientCode);
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to retrieve client",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }

    /// <summary>
    /// Searches clients by name or document number.
    /// </summary>
    /// <param name="searchTerm">Name or document number to search</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of matching clients (max 50)</returns>
    [HttpGet("search")]
    [ProducesResponseType(typeof(List<ClientDto>), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(ErrorResponse), StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<List<ClientDto>>> SearchClients(
        [FromQuery] string searchTerm,
        CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("Searching clients with term: {SearchTerm}", searchTerm);

            if (string.IsNullOrWhiteSpace(searchTerm) || searchTerm.Length < 3)
            {
                return BadRequest(new ErrorResponse
                {
                    StatusCode = 400,
                    Message = "Invalid search term",
                    Details = "Search term must be at least 3 characters long",
                    Timestamp = DateTime.UtcNow.ToString("O")
                });
            }

            // Try to parse as client code
            if (long.TryParse(searchTerm, out var clientCode))
            {
                var client = await _clientRepository.GetByClientCodeAsync((int)clientCode, cancellationToken);
                if (client != null)
                {
                    return Ok(new List<ClientDto>
                    {
                        new ClientDto
                        {
                            ClientCode = client.ClientCode,
                            ClientName = client.ClientName,
                            ClientType = client.ClientType,
                            DocumentNumber = client.DocumentNumber,
                            DocumentType = client.DocumentType
                        }
                    });
                }
            }

            // Search by name or document (case-insensitive)
            var searchLower = searchTerm.ToLowerInvariant();
            var clients = await _clientRepository.FindAsync(
                c => c.ClientName.ToLower().Contains(searchLower) ||
                     c.DocumentNumber.Contains(searchTerm),
                cancellationToken);

            var clientDtos = clients
                .Take(50) // Limit results
                .Select(c => new ClientDto
                {
                    ClientCode = c.ClientCode,
                    ClientName = c.ClientName,
                    ClientType = c.ClientType,
                    DocumentNumber = c.DocumentNumber,
                    DocumentType = c.DocumentType,
                    BirthDate = c.BirthDateParsed,
                    Gender = c.Gender,
                    Email = c.Email,
                    PhoneNumber = c.PhoneNumber
                }).ToList();

            _logger.LogInformation("Found {Count} clients matching search term", clientDtos.Count);

            return Ok(clientDtos);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error searching clients");
            return StatusCode(500, new ErrorResponse
            {
                StatusCode = 500,
                Message = "Failed to search clients",
                Details = ex.Message,
                Timestamp = DateTime.UtcNow.ToString("O")
            });
        }
    }
}

/// <summary>
/// Client data transfer object.
/// </summary>
public class ClientDto
{
    public long ClientCode { get; set; }
    public string ClientName { get; set; } = string.Empty;
    public string ClientType { get; set; } = string.Empty;
    public string DocumentNumber { get; set; } = string.Empty;
    public string DocumentType { get; set; } = string.Empty;
    public DateTime? BirthDate { get; set; }
    public string? Gender { get; set; }
    public string? Email { get; set; }
    public string? PhoneNumber { get; set; }
}
