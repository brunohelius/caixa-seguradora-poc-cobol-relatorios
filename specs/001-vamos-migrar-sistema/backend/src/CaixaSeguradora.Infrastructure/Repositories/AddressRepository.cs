using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for address data (V0ENDERECOS view).
/// Maps to COBOL sections R1160, R1170, R1220, R1230 (with cursor), R1240.
/// </summary>
public class AddressRepository : Repository<Address>, IAddressRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public AddressRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Address> GetAddressesByClientAsync(
        int clientCode,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // Maps to COBOL cursor: CUR-V0ENDERECOS declared at R1230-00-DECLARE-V0ENDERECOS
        // Used in R1220-00-PROCESSA-UF-VIDA for life insurance address processing
        // SELECT * FROM V0ENDERECOS WHERE COD_CLIEN = :clientCode ORDER BY SEQ_ENDER
        var query = _premiumContext.Addresses
            .AsNoTracking()
            .Where(a => a.ClientCode == clientCode)
            .OrderBy(a => a.AddressSequence);

        await foreach (var address in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return address;
        }
    }

    /// <inheritdoc />
    public async Task<Address?> GetPrimaryAddressAsync(int clientCode, CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R1170-00-SELECT-MAX-ENDERECO:
        // SELECT * FROM V0ENDERECOS WHERE COD_CLIEN = :clientCode AND SEQ_ENDER = (SELECT MAX(SEQ_ENDER) ...)
        var maxSequence = await _premiumContext.Addresses
            .AsNoTracking()
            .Where(a => a.ClientCode == clientCode)
            .MaxAsync(a => (int?)a.AddressSequence, cancellationToken);

        if (maxSequence == null)
        {
            return null;
        }

        return await _premiumContext.Addresses
            .AsNoTracking()
            .FirstOrDefaultAsync(a => a.ClientCode == clientCode && a.AddressSequence == maxSequence.Value, cancellationToken);
    }

    /// <inheritdoc />
    public async Task<List<Address>> GetByClientAndTypeAsync(
        int clientCode,
        string addressType,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R1160-00-SELECT-V0ENDERECOS:
        // SELECT * FROM V0ENDERECOS WHERE COD_CLIEN = :clientCode AND TIP_ENDER = :addressType
        return await _premiumContext.Addresses
            .AsNoTracking()
            .Where(a => a.ClientCode == clientCode && a.AddressType == addressType)
            .OrderBy(a => a.AddressSequence)
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Address> GetByStateCodeAsync(
        string stateCode,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // SELECT * FROM V0ENDERECOS WHERE COD_UF = :stateCode
        var query = _premiumContext.Addresses
            .AsNoTracking()
            .Where(a => a.State == stateCode)
            .OrderBy(a => a.City)
            .ThenBy(a => a.ClientCode);

        await foreach (var address in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return address;
        }
    }
}
