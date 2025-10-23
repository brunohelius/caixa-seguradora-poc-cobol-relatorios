using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for client/customer data (V0CLIENTE, V0TOMADOR views).
/// Maps to COBOL sections R0960-00-SELECT-V0CLIENTE, R1140-00-SELECT-V0TOMADOR.
/// </summary>
public class ClientRepository : Repository<Client>, IClientRepository
{
    private readonly PremiumReportingDbContext _premiumContext;

    public ClientRepository(PremiumReportingDbContext context) : base(context)
    {
        _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
    }

    /// <inheritdoc />
    public async Task<Client?> GetByClientCodeAsync(int clientCode, CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R0960-00-SELECT-V0CLIENTE:
        // SELECT * FROM V0CLIENTE WHERE COD_CLIEN = :clientCode
        return await _premiumContext.Clients
            .AsNoTracking()
            .FirstOrDefaultAsync(c => c.ClientCode == clientCode, cancellationToken);
    }

    /// <inheritdoc />
    public async Task<Client?> GetPolicyHolderAsync(int policyHolderCode, CancellationToken cancellationToken = default)
    {
        // Maps to COBOL section R1140-00-SELECT-V0TOMADOR:
        // SELECT * FROM V0TOMADOR WHERE COD_CLIEN = :policyHolderCode
        // Note: V0TOMADOR is typically the same as V0CLIENTE for policyholder role
        return await _premiumContext.Clients
            .AsNoTracking()
            .FirstOrDefaultAsync(c => c.ClientCode == policyHolderCode, cancellationToken);
    }

    /// <inheritdoc />
    public async Task<Client?> GetByDocumentNumberAsync(string documentNumber, CancellationToken cancellationToken = default)
    {
        // SELECT * FROM V0CLIENTE WHERE NUM_CPF_CNPJ = :documentNumber
        return await _premiumContext.Clients
            .AsNoTracking()
            .FirstOrDefaultAsync(c => c.DocumentNumber == documentNumber, cancellationToken);
    }

    /// <inheritdoc />
    public async IAsyncEnumerable<Client> SearchByNameAsync(
        string namePattern,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // SELECT * FROM V0CLIENTE WHERE NOM_CLIEN LIKE '%:namePattern%'
        var query = _premiumContext.Clients
            .AsNoTracking()
            .Where(c => c.ClientName.Contains(namePattern))
            .OrderBy(c => c.ClientName);

        await foreach (var client in query.AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return client;
        }
    }
}
