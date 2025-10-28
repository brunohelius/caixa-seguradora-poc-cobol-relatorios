using Microsoft.Extensions.Diagnostics.HealthChecks;

namespace CaixaSeguradora.Api.HealthChecks
{
    /// <summary>
    /// Health check para verificar disponibilidade e capacidade do sistema de arquivos.
    /// Verifica se o diretório de output existe, é gravável e possui espaço suficiente.
    /// T191 - Phase 10: Monitoring and Observability
    /// </summary>
    public class FileSystemHealthCheck : IHealthCheck
    {
        private readonly IConfiguration _configuration;
        private readonly ILogger<FileSystemHealthCheck> _logger;

        // Espaço mínimo requerido em bytes (1 GB conforme especificação T191)
        private const long MinimumDiskSpaceBytes = 1024L * 1024L * 1024L; // 1 GB

        public FileSystemHealthCheck(
            IConfiguration configuration,
            ILogger<FileSystemHealthCheck> logger)
        {
            _configuration = configuration ?? throw new ArgumentNullException(nameof(configuration));
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        public Task<HealthCheckResult> CheckHealthAsync(
            HealthCheckContext context,
            CancellationToken cancellationToken = default)
        {
            try
            {
                // Obtém o diretório de output da configuração
                var outputDirectory = _configuration["FileOutput:Directory"] ?? "./output";
                var fullPath = Path.GetFullPath(outputDirectory);

                var data = new Dictionary<string, object>
                {
                    { "outputDirectory", fullPath },
                    { "minimumDiskSpace", FormatBytes(MinimumDiskSpaceBytes) }
                };

                // Verifica se o diretório existe
                if (!Directory.Exists(fullPath))
                {
                    _logger.LogWarning(
                        "Output directory does not exist: {Directory}. Attempting to create.",
                        fullPath);

                    try
                    {
                        Directory.CreateDirectory(fullPath);
                        _logger.LogInformation("Output directory created successfully: {Directory}", fullPath);
                    }
                    catch (Exception createEx)
                    {
                        _logger.LogError(
                            createEx,
                            "Failed to create output directory: {Directory}",
                            fullPath);

                        return Task.FromResult(HealthCheckResult.Unhealthy(
                            $"Diretório de output não existe e não pôde ser criado: {fullPath}",
                            exception: createEx,
                            data: data));
                    }
                }

                // Verifica se o diretório é gravável
                var testFileName = Path.Combine(fullPath, $".healthcheck_{Guid.NewGuid():N}.tmp");
                try
                {
                    File.WriteAllText(testFileName, "health check test");
                    File.Delete(testFileName);

                    _logger.LogDebug("Write permission verified for directory: {Directory}", fullPath);
                }
                catch (Exception writeEx)
                {
                    _logger.LogError(
                        writeEx,
                        "Directory is not writable: {Directory}",
                        fullPath);

                    return Task.FromResult(HealthCheckResult.Unhealthy(
                        $"Diretório de output não possui permissão de escrita: {fullPath}",
                        exception: writeEx,
                        data: data));
                }

                // Verifica espaço em disco disponível
                var driveInfo = new DriveInfo(Path.GetPathRoot(fullPath)!);
                var availableSpace = driveInfo.AvailableFreeSpace;

                data["availableDiskSpace"] = FormatBytes(availableSpace);
                data["driveFormat"] = driveInfo.DriveFormat;
                data["driveName"] = driveInfo.Name;

                if (availableSpace < MinimumDiskSpaceBytes)
                {
                    _logger.LogWarning(
                        "Low disk space on {Drive}: {Available} available (minimum required: {Required})",
                        driveInfo.Name,
                        FormatBytes(availableSpace),
                        FormatBytes(MinimumDiskSpaceBytes));

                    return Task.FromResult(HealthCheckResult.Degraded(
                        $"Espaço em disco baixo: {FormatBytes(availableSpace)} disponível " +
                        $"(mínimo requerido: {FormatBytes(MinimumDiskSpaceBytes)})",
                        data: data));
                }

                _logger.LogDebug(
                    "File system health check: Healthy (available space: {Available} on {Drive})",
                    FormatBytes(availableSpace),
                    driveInfo.Name);

                return Task.FromResult(HealthCheckResult.Healthy(
                    $"Sistema de arquivos OK - {FormatBytes(availableSpace)} disponível",
                    data));
            }
            catch (Exception ex)
            {
                _logger.LogError(
                    ex,
                    "File system health check failed: {ErrorMessage}",
                    ex.Message);

                var errorData = new Dictionary<string, object>
                {
                    { "error", ex.Message },
                    { "errorType", ex.GetType().Name }
                };

                return Task.FromResult(HealthCheckResult.Unhealthy(
                    $"Falha ao verificar sistema de arquivos: {ex.Message}",
                    exception: ex,
                    data: errorData));
            }
        }

        /// <summary>
        /// Formata bytes em formato legível (KB, MB, GB)
        /// </summary>
        private static string FormatBytes(long bytes)
        {
            string[] sizes = { "B", "KB", "MB", "GB", "TB" };
            double len = bytes;
            int order = 0;

            while (len >= 1024 && order < sizes.Length - 1)
            {
                order++;
                len /= 1024;
            }

            return $"{len:0.##} {sizes[order]}";
        }
    }
}
