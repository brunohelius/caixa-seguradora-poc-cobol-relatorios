using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for client/customer data (V0CLIENTE, V0TOMADOR views).
/// Accessed at COBOL sections R0960-00-SELECT-V0CLIENTE, R1140-00-SELECT-V0TOMADOR.
/// </summary>
public interface IClientRepository : IRepository<Client>
{
    /// <summary>
    /// Gets client by client code.
    /// Maps to COBOL section R0960-00-SELECT-V0CLIENTE.
    /// </summary>
    Task<Client?> GetByClientCodeAsync(int clientCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets policy holder (tomador) information.
    /// Maps to COBOL section R1140-00-SELECT-V0TOMADOR.
    /// </summary>
    Task<Client?> GetPolicyHolderAsync(int policyHolderCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets client by document number (CPF/CNPJ).
    /// </summary>
    Task<Client?> GetByDocumentNumberAsync(string documentNumber, CancellationToken cancellationToken = default);

    /// <summary>
    /// Searches clients by name (partial match).
    /// </summary>
    IAsyncEnumerable<Client> SearchByNameAsync(string namePattern, CancellationToken cancellationToken = default);
}
