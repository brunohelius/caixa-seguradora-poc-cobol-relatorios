using System.Diagnostics;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Diagnostics.HealthChecks;
using CaixaSeguradora.Infrastructure.Data;

namespace CaixaSeguradora.Api.HealthChecks
{
    /// <summary>
    /// Health check para verificar conectividade e performance do banco de dados.
    /// Executa uma query simples e mede o tempo de resposta.
    /// T190 - Phase 10: Monitoring and Observability
    /// </summary>
    public class DatabaseHealthCheck : IHealthCheck
    {
        private readonly PremiumReportingDbContext _context;
        private readonly ILogger<DatabaseHealthCheck> _logger;

        // Thresholds em milissegundos conforme especificação T190
        private const int HealthyThresholdMs = 100;
        private const int DegradedThresholdMs = 500;

        public DatabaseHealthCheck(
            PremiumReportingDbContext context,
            ILogger<DatabaseHealthCheck> logger)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        public async Task<HealthCheckResult> CheckHealthAsync(
            HealthCheckContext context,
            CancellationToken cancellationToken = default)
        {
            var stopwatch = Stopwatch.StartNew();

            try
            {
                // Executa query simples SELECT 1 para verificar conectividade
                await _context.Database.ExecuteSqlRawAsync("SELECT 1", cancellationToken);

                stopwatch.Stop();
                var responseTimeMs = stopwatch.ElapsedMilliseconds;

                var data = new Dictionary<string, object>
                {
                    { "responseTime", $"{responseTimeMs}ms" },
                    { "threshold_healthy", $"{HealthyThresholdMs}ms" },
                    { "threshold_degraded", $"{DegradedThresholdMs}ms" }
                };

                // Determina status baseado no tempo de resposta
                if (responseTimeMs < HealthyThresholdMs)
                {
                    _logger.LogDebug(
                        "Database health check: Healthy (response time: {ResponseTime}ms)",
                        responseTimeMs);

                    return HealthCheckResult.Healthy(
                        $"Database respondeu em {responseTimeMs}ms",
                        data);
                }
                else if (responseTimeMs < DegradedThresholdMs)
                {
                    _logger.LogWarning(
                        "Database health check: Degraded (response time: {ResponseTime}ms)",
                        responseTimeMs);

                    return HealthCheckResult.Degraded(
                        $"Database com performance degradada ({responseTimeMs}ms)",
                        data: data);
                }
                else
                {
                    _logger.LogError(
                        "Database health check: Unhealthy (response time: {ResponseTime}ms exceeds {Threshold}ms)",
                        responseTimeMs,
                        DegradedThresholdMs);

                    return HealthCheckResult.Unhealthy(
                        $"Database com performance crítica ({responseTimeMs}ms)",
                        data: data);
                }
            }
            catch (Exception ex)
            {
                stopwatch.Stop();

                _logger.LogError(
                    ex,
                    "Database health check failed after {ElapsedMs}ms: {ErrorMessage}",
                    stopwatch.ElapsedMilliseconds,
                    ex.Message);

                var errorData = new Dictionary<string, object>
                {
                    { "error", ex.Message },
                    { "errorType", ex.GetType().Name },
                    { "elapsedTime", $"{stopwatch.ElapsedMilliseconds}ms" }
                };

                return HealthCheckResult.Unhealthy(
                    $"Falha ao conectar ao banco de dados: {ex.Message}",
                    exception: ex,
                    data: errorData);
            }
        }
    }
}
