using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services
{
    /// <summary>
    /// Service implementation for tracking report execution progress and logging.
    /// Manages execution lifecycle from creation to completion with database persistence.
    /// </summary>
    public class ExecutionTrackingService : IExecutionTrackingService
    {
        private readonly PremiumReportingDbContext _context;
        private readonly ILogger<ExecutionTrackingService> _logger;

        public ExecutionTrackingService(
            PremiumReportingDbContext context,
            ILogger<ExecutionTrackingService> logger)
        {
            _context = context;
            _logger = logger;
        }

        /// <summary>
        /// Creates a new report execution record.
        /// </summary>
        public async Task<Guid> CreateExecutionAsync(
            string referenceMonth,
            string reportType,
            string executionMode,
            string triggeringUser,
            CancellationToken cancellationToken = default)
        {
            var execution = new ReportExecution
            {
                ExecutionId = Guid.NewGuid(),
                ReferenceMonth = referenceMonth,
                ReportType = reportType,
                ExecutionMode = executionMode,
                TriggeringUser = triggeringUser,
                StartTime = DateTime.UtcNow,
                Status = "Pending",
                ReturnCode = "0000",
                RecordsProcessed = 0,
                PremitRecordsGenerated = 0,
                PremcedRecordsGenerated = 0,
                WarningsCount = 0,
                ErrorsCount = 0
            };

            _context.Set<ReportExecution>().Add(execution);
            await _context.SaveChangesAsync(cancellationToken);

            _logger.LogInformation(
                "Created execution {ExecutionId} for month {ReferenceMonth} by user {User}",
                execution.ExecutionId,
                referenceMonth,
                triggeringUser);

            return execution.ExecutionId;
        }

        /// <summary>
        /// Updates the status of a report execution.
        /// </summary>
        public async Task UpdateStatusAsync(Guid executionId, string status, CancellationToken cancellationToken = default)
        {
            var execution = await _context.Set<ReportExecution>()
                .FirstOrDefaultAsync(e => e.ExecutionId == executionId, cancellationToken);

            if (execution == null)
            {
                var errorMessage = $"Falha ao atualizar status: execução {executionId} não encontrada";
                _logger.LogError(errorMessage);
                throw new InvalidOperationException(errorMessage);
            }

            execution.Status = status;

            if (status == "Running" && execution.StartTime == default)
            {
                execution.StartTime = DateTime.UtcNow;
            }

            await _context.SaveChangesAsync(cancellationToken);

            _logger.LogInformation(
                "Execution {ExecutionId} status updated to {Status}",
                executionId,
                status);
        }

        /// <summary>
        /// Updates the progress of a report execution.
        /// </summary>
        public async Task UpdateProgressAsync(Guid executionId, int recordsProcessed, CancellationToken cancellationToken = default)
        {
            var execution = await _context.Set<ReportExecution>()
                .FirstOrDefaultAsync(e => e.ExecutionId == executionId, cancellationToken);

            if (execution == null)
            {
                var errorMessage = $"Falha ao atualizar progresso: execução {executionId} não encontrada";
                _logger.LogError(errorMessage);
                throw new InvalidOperationException(errorMessage);
            }

            execution.RecordsProcessed = recordsProcessed;
            await _context.SaveChangesAsync(cancellationToken);

            // Log progress every 1000 records
            if (recordsProcessed % 1000 == 0)
            {
                _logger.LogInformation(
                    "Execution {ExecutionId}: Processed {RecordsProcessed} records",
                    executionId,
                    recordsProcessed);
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

            if (execution == null)
            {
                var errorMessage = $"Falha ao completar execução: execução {executionId} não encontrada";
                _logger.LogError(errorMessage);
                throw new InvalidOperationException(errorMessage);
            }

            execution.Status = errors > 0 ? "Failed" : "Completed";
            execution.EndTime = DateTime.UtcNow;
            execution.ReturnCode = returnCode;
            execution.PremitRecordsGenerated = premitRecords;
            execution.PremcedRecordsGenerated = premcedRecords;
            execution.WarningsCount = warnings;
            execution.ErrorsCount = errors;

            await _context.SaveChangesAsync(cancellationToken);

            var duration = (execution.EndTime.Value - execution.StartTime).TotalSeconds;

            _logger.LogInformation(
                "Execution {ExecutionId} completed with status {Status}, RC={ReturnCode}, " +
                "Duration={Duration}s, Processed={RecordsProcessed}, PREMIT={PremitRecords}, " +
                "PREMCED={PremcedRecords}, Warnings={Warnings}, Errors={Errors}",
                executionId,
                execution.Status,
                returnCode,
                duration,
                execution.RecordsProcessed,
                premitRecords,
                premcedRecords,
                warnings,
                errors);
        }

        /// <summary>
        /// Logs a processing event for an execution.
        /// </summary>
        public async Task LogEventAsync(
            Guid executionId,
            string severity,
            string cobolSection,
            string message,
            long? policyNumber = null,
            string? stackTrace = null,
            CancellationToken cancellationToken = default)
        {
            var log = new ProcessingLog
            {
                ExecutionId = executionId,
                Timestamp = DateTime.UtcNow,
                Severity = severity,
                CobolSection = cobolSection,
                Message = message,
                PolicyNumber = policyNumber,
                StackTrace = stackTrace
            };

            _context.Set<ProcessingLog>().Add(log);
            await _context.SaveChangesAsync(cancellationToken);

            // Also log to application logger
            var logLevel = severity switch
            {
                "ERROR" => LogLevel.Error,
                "WARNING" => LogLevel.Warning,
                "DEBUG" => LogLevel.Debug,
                _ => LogLevel.Information
            };

            _logger.Log(
                logLevel,
                "[{ExecutionId}] [{CobolSection}] {Message} {PolicyContext}",
                executionId,
                cobolSection,
                message,
                policyNumber.HasValue ? $"(Policy: {policyNumber})" : "");
        }

        /// <summary>
        /// Gets execution details by ID.
        /// </summary>
        public async Task<ReportExecution?> GetExecutionAsync(Guid executionId, CancellationToken cancellationToken = default)
        {
            return await _context.Set<ReportExecution>()
                .Include(e => e.ProcessingLogs)
                .Include(e => e.FileOutputs)
                .FirstOrDefaultAsync(e => e.ExecutionId == executionId, cancellationToken);
        }

        /// <summary>
        /// Gets execution history with pagination.
        /// </summary>
        public async Task<(List<ReportExecution> Executions, int TotalCount)> GetExecutionHistoryAsync(
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

            // Get total count
            var totalCount = await query.CountAsync(cancellationToken);

            // Apply pagination
            var executions = await query
                .OrderByDescending(e => e.StartTime)
                .Skip((pageNumber - 1) * pageSize)
                .Take(pageSize)
                .ToListAsync(cancellationToken);

            return (executions, totalCount);
        }
    }
}
