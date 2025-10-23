using CaixaSeguradora.Core.DTOs;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for dashboard metrics and migration analysis.
/// Provides program analysis, function points estimation, and database dependency visualization.
/// </summary>
public interface IDashboardService
{
    /// <summary>
    /// Gets comprehensive dashboard metrics including program info, complexity, and migration progress.
    /// Data sourced from COBOL analysis (FINAL-ANALYSIS-REPORT.md) and real-time migration tracking.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Dashboard metrics DTO</returns>
    Task<DashboardMetricsDto> GetDashboardMetricsAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets function points analysis for project estimation.
    /// Calculates complexity based on IFPUG counting practices.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Function points breakdown</returns>
    Task<FunctionPointsDto> GetFunctionPointsAsync(CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets database dependencies analysis showing tables, cursors, and SQL operations.
    /// Critical for understanding data access patterns and migration complexity.
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Database dependencies information</returns>
    Task<DatabaseDependenciesDto> GetDatabaseDependenciesAsync(CancellationToken cancellationToken = default);
}
