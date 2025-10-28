using CaixaSeguradora.Infrastructure.Data;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using System.Diagnostics;

namespace CaixaSeguradora.Api.Controllers;

/// <summary>
/// Health check endpoint for monitoring application and database status.
/// Implements T156 - Connection monitoring for Phase 7 (US5).
/// </summary>
[ApiController]
[Route("api/v1/[controller]")]
[AllowAnonymous] // Health checks should be accessible without authentication
public class HealthController : ControllerBase
{
    private readonly PremiumReportingDbContext _context;
    private readonly ILogger<HealthController> _logger;

    public HealthController(
        PremiumReportingDbContext context,
        ILogger<HealthController> logger)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// Comprehensive health check including database connection performance.
    /// GET /api/v1/health
    /// </summary>
    /// <returns>Health status with database connection metrics</returns>
    [HttpGet]
    [ProducesResponseType(typeof(HealthCheckResponse), StatusCodes.Status200OK)]
    [ProducesResponseType(typeof(HealthCheckResponse), StatusCodes.Status503ServiceUnavailable)]
    public async Task<IActionResult> GetHealthStatus()
    {
        var response = new HealthCheckResponse
        {
            Status = "Healthy",
            Timestamp = DateTime.UtcNow,
            Version = "1.0.0"
        };

        try
        {
            // Check database connection and measure response time
            var dbStopwatch = Stopwatch.StartNew();

            // Execute simple query to test connection (SELECT 1)
            await _context.Database.ExecuteSqlRawAsync("SELECT 1");

            dbStopwatch.Stop();

            response.DatabaseStatus = dbStopwatch.ElapsedMilliseconds < 100
                ? "Healthy"
                : dbStopwatch.ElapsedMilliseconds < 500
                    ? "Degraded"
                    : "Unhealthy";

            response.DatabaseResponseTimeMs = dbStopwatch.ElapsedMilliseconds;

            // Get connection pool statistics (if available)
            response.Checks.Add("Database", new HealthCheckDetail
            {
                Status = response.DatabaseStatus,
                ResponseTimeMs = dbStopwatch.ElapsedMilliseconds,
                Message = response.DatabaseStatus == "Healthy"
                    ? "Database connection is healthy"
                    : $"Database response time is {dbStopwatch.ElapsedMilliseconds}ms (degraded threshold: 100ms, unhealthy: 500ms)"
            });

            // Check file system access for output directory
            var outputDirectory = Path.Combine(Directory.GetCurrentDirectory(), "output");
            var fileSystemStopwatch = Stopwatch.StartNew();

            if (!Directory.Exists(outputDirectory))
            {
                Directory.CreateDirectory(outputDirectory);
            }

            // Check write permissions
            var testFile = Path.Combine(outputDirectory, $".health_check_{Guid.NewGuid()}.tmp");
            await System.IO.File.WriteAllTextAsync(testFile, "health check");
            System.IO.File.Delete(testFile);

            fileSystemStopwatch.Stop();

            // Get available disk space
            var drive = new DriveInfo(Path.GetPathRoot(outputDirectory)!);
            var availableSpaceGB = drive.AvailableFreeSpace / (1024.0 * 1024.0 * 1024.0);

            response.Checks.Add("FileSystem", new HealthCheckDetail
            {
                Status = availableSpaceGB > 1.0 ? "Healthy" : "Degraded",
                ResponseTimeMs = fileSystemStopwatch.ElapsedMilliseconds,
                Message = $"Output directory writable. Available space: {availableSpaceGB:F2} GB"
            });

            // Overall status determination
            if (response.Checks.Values.Any(c => c.Status == "Unhealthy"))
            {
                response.Status = "Degraded";
                _logger.LogWarning("Health check degraded: database latency exceeded threshold");
            }
            else if (response.Checks.Values.Any(c => c.Status == "Degraded"))
            {
                response.Status = "Degraded";
                _logger.LogWarning("Health check degraded: One or more checks are degraded");
            }

            return Ok(response);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Health check encountered an exception; reporting degraded status");

            response.Status = "Degraded";
            response.DatabaseStatus = "Unhealthy";
            response.Checks.Add("Database", new HealthCheckDetail
            {
                Status = "Unhealthy",
                Message = $"Database connection failed: {ex.Message}"
            });

            return Ok(response);
        }
    }

    /// <summary>
    /// Simple liveness probe (returns 200 if API is running).
    /// GET /api/v1/health/live
    /// </summary>
    [HttpGet("live")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    public IActionResult GetLiveness()
    {
        return Ok(new { status = "Alive", timestamp = DateTime.UtcNow });
    }

    /// <summary>
    /// Readiness probe (returns 200 if API can serve traffic).
    /// GET /api/v1/health/ready
    /// </summary>
    [HttpGet("ready")]
    [ProducesResponseType(StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status503ServiceUnavailable)]
    public async Task<IActionResult> GetReadiness()
    {
        try
        {
            // Check if database is accessible
            await _context.Database.ExecuteSqlRawAsync("SELECT 1");

            return Ok(new { status = "Ready", timestamp = DateTime.UtcNow });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Readiness check failed");
            return StatusCode(
                StatusCodes.Status503ServiceUnavailable,
                new { status = "NotReady", error = ex.Message, timestamp = DateTime.UtcNow });
        }
    }
}

/// <summary>
/// Health check response model.
/// </summary>
public class HealthCheckResponse
{
    public string Status { get; set; } = "Unknown";
    public DateTime Timestamp { get; set; }
    public string Version { get; set; } = "1.0.0";
    public string DatabaseStatus { get; set; } = "Unknown";
    public long DatabaseResponseTimeMs { get; set; }
    public Dictionary<string, HealthCheckDetail> Checks { get; set; } = new();
}

/// <summary>
/// Individual health check detail.
/// </summary>
public class HealthCheckDetail
{
    public string Status { get; set; } = "Unknown";
    public long ResponseTimeMs { get; set; }
    public string Message { get; set; } = string.Empty;
}
