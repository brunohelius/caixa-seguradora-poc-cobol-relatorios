using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for batch job management.
    /// Handles scheduling, execution tracking, and job history.
    /// </summary>
    public class BatchJobRepository : Repository<BatchJob>, IBatchJobRepository
    {
        private readonly PremiumReportingDbContext _premiumContext;

        public BatchJobRepository(PremiumReportingDbContext context) : base(context)
        {
            _premiumContext = context ?? throw new ArgumentNullException(nameof(context));
        }

        /// <inheritdoc />
        public async Task<IReadOnlyList<BatchJob>> GetScheduledJobsAsync(
            DateTime currentTime,
            CancellationToken cancellationToken = default)
        {
            return await _premiumContext.BatchJobs
                .AsNoTracking()
                .Where(job =>
                    job.IsEnabled &&
                    job.Status == "ACTIVE" &&
                    job.NextExecutionTime.HasValue &&
                    job.NextExecutionTime.Value <= currentTime)
                .OrderBy(job => job.NextExecutionTime)
                .ToListAsync(cancellationToken);
        }

        /// <inheritdoc />
        public async Task<IReadOnlyList<BatchJobExecution>> GetJobHistoryAsync(
            int jobId,
            int limit = 50,
            CancellationToken cancellationToken = default)
        {
            return await _premiumContext.BatchJobExecutions
                .AsNoTracking()
                .Where(execution => execution.JobId == jobId)
                .OrderByDescending(execution => execution.StartTime)
                .Take(limit)
                .ToListAsync(cancellationToken);
        }

        /// <inheritdoc />
        public async Task<IReadOnlyList<BatchJob>> GetJobsByUserAsync(
            string createdBy,
            CancellationToken cancellationToken = default)
        {
            return await _premiumContext.BatchJobs
                .AsNoTracking()
                .Where(job => job.CreatedBy == createdBy && job.Status == "ACTIVE")
                .OrderBy(job => job.JobName)
                .ToListAsync(cancellationToken);
        }

        /// <inheritdoc />
        public async Task AddExecutionAsync(
            BatchJobExecution execution,
            CancellationToken cancellationToken = default)
        {
            await _premiumContext.BatchJobExecutions.AddAsync(execution, cancellationToken);
        }

        /// <inheritdoc />
        public void UpdateExecution(BatchJobExecution execution)
        {
            _premiumContext.BatchJobExecutions.Update(execution);
        }

        /// <inheritdoc />
        public async Task<BatchJobExecution?> GetLatestExecutionAsync(
            int jobId,
            CancellationToken cancellationToken = default)
        {
            return await _premiumContext.BatchJobExecutions
                .AsNoTracking()
                .Where(execution => execution.JobId == jobId)
                .OrderByDescending(execution => execution.StartTime)
                .FirstOrDefaultAsync(cancellationToken);
        }

        /// <inheritdoc />
        public async Task<IReadOnlyList<BatchJob>> GetJobsByStatusAsync(
            string status,
            CancellationToken cancellationToken = default)
        {
            return await _premiumContext.BatchJobs
                .AsNoTracking()
                .Where(job => job.Status == status)
                .OrderBy(job => job.JobName)
                .ToListAsync(cancellationToken);
        }

        /// <inheritdoc />
        public async Task<IReadOnlyList<BatchJobExecution>> GetRunningExecutionsAsync(
            CancellationToken cancellationToken = default)
        {
            return await _premiumContext.BatchJobExecutions
                .AsNoTracking()
                .Where(execution => execution.Status == "RUNNING")
                .Include(execution => execution.BatchJob)
                .OrderBy(execution => execution.StartTime)
                .ToListAsync(cancellationToken);
        }
    }
}
