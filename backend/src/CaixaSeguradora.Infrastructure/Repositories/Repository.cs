using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories;

/// <summary>
/// Generic repository implementation using Entity Framework Core.
/// Provides COBOL cursor-style streaming capabilities for large datasets.
/// </summary>
/// <typeparam name="T">Entity type</typeparam>
public class Repository<T> : IRepository<T> where T : class
{
    protected readonly DbContext _context;
    protected readonly DbSet<T> _dbSet;

    public Repository(DbContext context)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _dbSet = context.Set<T>();
    }

    /// <inheritdoc />
    public virtual async Task<T?> GetByIdAsync(object id, CancellationToken cancellationToken = default)
    {
        return await _dbSet.FindAsync(new[] { id }, cancellationToken);
    }

    /// <inheritdoc />
    public virtual async Task<IReadOnlyList<T>> GetAllAsync(CancellationToken cancellationToken = default)
    {
        return await _dbSet
            .AsNoTracking()
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public virtual async IAsyncEnumerable<T> GetAllStreamingAsync([EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        // AsNoTracking for read-only queries improves performance
        // AsAsyncEnumerable enables COBOL-style cursor processing
        await foreach (T? entity in _dbSet.AsNoTracking().AsAsyncEnumerable().WithCancellation(cancellationToken))
        {
            yield return entity;
        }
    }

    /// <inheritdoc />
    public virtual async Task<IReadOnlyList<T>> FindAsync(Expression<Func<T, bool>> predicate, CancellationToken cancellationToken = default)
    {
        return await _dbSet
            .AsNoTracking()
            .Where(predicate)
            .ToListAsync(cancellationToken);
    }

    /// <inheritdoc />
    public virtual async IAsyncEnumerable<T> FindStreamingAsync(
        Expression<Func<T, bool>> predicate,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        await foreach (T? entity in _dbSet
            .AsNoTracking()
            .Where(predicate)
            .AsAsyncEnumerable()
            .WithCancellation(cancellationToken))
        {
            yield return entity;
        }
    }

    /// <inheritdoc />
    public virtual async Task<T?> SingleOrDefaultAsync(Expression<Func<T, bool>> predicate, CancellationToken cancellationToken = default)
    {
        return await _dbSet
            .AsNoTracking()
            .SingleOrDefaultAsync(predicate, cancellationToken);
    }

    /// <inheritdoc />
    public virtual async Task<T?> FirstOrDefaultAsync(Expression<Func<T, bool>> predicate, CancellationToken cancellationToken = default)
    {
        return await _dbSet
            .AsNoTracking()
            .FirstOrDefaultAsync(predicate, cancellationToken);
    }

    /// <inheritdoc />
    public virtual async Task<bool> AnyAsync(Expression<Func<T, bool>> predicate, CancellationToken cancellationToken = default)
    {
        return await _dbSet.AnyAsync(predicate, cancellationToken);
    }

    /// <inheritdoc />
    public virtual async Task<int> CountAsync(Expression<Func<T, bool>>? predicate = null, CancellationToken cancellationToken = default)
    {
        if (predicate == null)
        {
            return await _dbSet.CountAsync(cancellationToken);
        }

        return await _dbSet.CountAsync(predicate, cancellationToken);
    }

    /// <inheritdoc />
    public virtual async Task AddAsync(T entity, CancellationToken cancellationToken = default)
    {
        await _dbSet.AddAsync(entity, cancellationToken);
    }

    /// <inheritdoc />
    public virtual async Task AddRangeAsync(IEnumerable<T> entities, CancellationToken cancellationToken = default)
    {
        await _dbSet.AddRangeAsync(entities, cancellationToken);
    }

    /// <inheritdoc />
    public virtual void Update(T entity)
    {
        _dbSet.Update(entity);
    }

    /// <inheritdoc />
    public virtual void UpdateRange(IEnumerable<T> entities)
    {
        _dbSet.UpdateRange(entities);
    }

    /// <inheritdoc />
    public virtual void Remove(T entity)
    {
        _dbSet.Remove(entity);
    }

    /// <inheritdoc />
    public virtual void RemoveRange(IEnumerable<T> entities)
    {
        _dbSet.RemoveRange(entities);
    }

    /// <inheritdoc />
    public virtual async Task<int> SaveChangesAsync(CancellationToken cancellationToken = default)
    {
        return await _context.SaveChangesAsync(cancellationToken);
    }
}
