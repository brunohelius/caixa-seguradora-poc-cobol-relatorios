using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;

namespace CaixaSeguradora.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for ReportExecution entity.
    /// Provides data access methods for report execution tracking.
    /// </summary>
    public class ReportExecutionRepository : Repository<ReportExecution>, IReportExecutionRepository
    {
        public ReportExecutionRepository(PremiumReportingDbContext context) : base(context)
        {
        }

        /// <summary>
        /// Gets a report execution by its ID with related logs and file outputs.
        /// </summary>
        public async Task<ReportExecution?> GetByIdWithDetailsAsync(Guid executionId, CancellationToken cancellationToken = default)
        {
            return await _context.Set<ReportExecution>()
                .Include(e => e.ProcessingLogs)
                .Include(e => e.FileOutputs)
                .FirstOrDefaultAsync(e => e.ExecutionId == executionId, cancellationToken);
        }

        /// <summary>
        /// Gets all report executions with pagination and optional filters.
        /// </summary>
        public async Task<(List<ReportExecution> Executions, int TotalCount)> GetAllWithPaginationAsync(
            int pageNumber,
            int pageSize,
            string? status = null,
            string? referenceMonth = null,
            CancellationToken cancellationToken = default)
        {
            var query = _context.Set<ReportExecution>().AsQueryable();

            // Apply filters
            if (!string.IsNullOrEmpty(status))
            {
                query = query.Where(e => e.Status == status);
            }

            if (!string.IsNullOrEmpty(referenceMonth))
            {
                query = query.Where(e => e.ReferenceMonth == referenceMonth);
            }

            // Get total count before pagination
            var totalCount = await query.CountAsync(cancellationToken);

            // Apply pagination and ordering
            var executions = await query
                .OrderByDescending(e => e.StartTime)
                .Skip((pageNumber - 1) * pageSize)
                .Take(pageSize)
                .ToListAsync(cancellationToken);

            return (executions, totalCount);
        }

        /// <summary>
        /// Checks if there is an active (Pending or Running) execution for the specified month.
        /// </summary>
        public async Task<bool> HasActiveExecutionForMonthAsync(string referenceMonth, CancellationToken cancellationToken = default)
        {
            return await _context.Set<ReportExecution>()
                .AnyAsync(e => e.ReferenceMonth == referenceMonth &&
                              (e.Status == "Pending" || e.Status == "Running"),
                         cancellationToken);
        }

        /// <summary>
        /// Gets the most recent execution for a specific month.
        /// </summary>
        public async Task<ReportExecution?> GetLatestExecutionForMonthAsync(string referenceMonth, CancellationToken cancellationToken = default)
        {
            return await _context.Set<ReportExecution>()
                .Where(e => e.ReferenceMonth == referenceMonth)
                .OrderByDescending(e => e.StartTime)
                .FirstOrDefaultAsync(cancellationToken);
        }

        /// <summary>
        /// Updates the progress of a report execution.
        /// </summary>
        public async Task UpdateProgressAsync(Guid executionId, int recordsProcessed, CancellationToken cancellationToken = default)
        {
            var execution = await _context.Set<ReportExecution>()
                .FirstOrDefaultAsync(e => e.ExecutionId == executionId, cancellationToken);

            if (execution != null)
            {
                execution.RecordsProcessed = recordsProcessed;
                await _context.SaveChangesAsync(cancellationToken);
            }
        }

        /// <summary>
        /// Completes a report execution with final statistics.
        /// </summary>
        public async Task CompleteExecutionAsync(
            Guid executionId,
            string returnCode,
            int premitRecords,
            int premcedRecords,
            int warnings,
            int errors,
            CancellationToken cancellationToken = default)
        {
            var execution = await _context.Set<ReportExecution>()
                .FirstOrDefaultAsync(e => e.ExecutionId == executionId, cancellationToken);

            if (execution != null)
            {
                execution.Status = errors > 0 ? "Failed" : "Completed";
                execution.EndTime = DateTime.UtcNow;
                execution.ReturnCode = returnCode;
                execution.PremitRecordsGenerated = premitRecords;
                execution.PremcedRecordsGenerated = premcedRecords;
                execution.WarningsCount = warnings;
                execution.ErrorsCount = errors;

                await _context.SaveChangesAsync(cancellationToken);
            }
        }
    }
}
