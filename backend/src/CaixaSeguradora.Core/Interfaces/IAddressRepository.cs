using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for address data (V0ENDERECOS view).
/// Accessed at COBOL sections R1160, R1170, R1220, R1230 (with cursor), R1240.
/// </summary>
public interface IAddressRepository : IRepository<Address>
{
    /// <summary>
    /// Gets addresses for a client using cursor pattern (for life insurance state processing).
    /// Maps to COBOL cursor: CUR-V0ENDERECOS declared at R1230-00-DECLARE-V0ENDERECOS.
    /// Used in R1220-00-PROCESSA-UF-VIDA for life insurance address processing.
    /// </summary>
    IAsyncEnumerable<Address> GetAddressesByClientAsync(int clientCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets the most recent/primary address for a client.
    /// Maps to COBOL section R1170-00-SELECT-MAX-ENDERECO.
    /// </summary>
    Task<Address?> GetPrimaryAddressAsync(int clientCode, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets addresses by client code and address type.
    /// Maps to COBOL section R1160-00-SELECT-V0ENDERECOS.
    /// </summary>
    Task<List<Address>> GetByClientAndTypeAsync(int clientCode, string addressType, CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets addresses by state/region code for geographic analysis.
    /// </summary>
    IAsyncEnumerable<Address> GetByStateCodeAsync(string stateCode, CancellationToken cancellationToken = default);
}
