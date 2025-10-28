using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;
using System.Security.Cryptography;

namespace CaixaSeguradora.Infrastructure.Services
{
    /// <summary>
    /// File writer service for generating fixed-width PREMIT and PREMCED files.
    /// Phase 3 implementation is a stub - full implementation in Phase 6 (US4).
    /// </summary>
    public class FileWriterService : IFileWriterService
    {
        private readonly ILogger<FileWriterService> _logger;

        public FileWriterService(ILogger<FileWriterService> logger)
        {
            _logger = logger;
        }

        /// <summary>
        /// Writes premium records to a fixed-width file (stub for Phase 3).
        /// Full implementation will be added in Phase 6 (US4).
        /// </summary>
        public async Task<int> WriteToFileAsync<T>(
            IEnumerable<T> records,
            string filePath,
            string fileType,
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation(
                "FileWriterService.WriteToFileAsync called (stub) - FilePath={FilePath}, FileType={FileType}",
                filePath,
                fileType);

            // Phase 3: Stub implementation
            // Phase 6 will implement full fixed-width file generation
            var recordsList = records.ToList();
            _logger.LogInformation("Stub: Would write {Count} records to file", recordsList.Count);

            await Task.CompletedTask;
            return recordsList.Count;
        }

        /// <summary>
        /// Validates file format (stub for Phase 3).
        /// </summary>
        public async Task<bool> ValidateFileFormatAsync(
            string filePath,
            string fileType,
            CancellationToken cancellationToken = default)
        {
            _logger.LogInformation(
                "FileWriterService.ValidateFileFormatAsync called (stub) - FilePath={FilePath}",
                filePath);

            await Task.CompletedTask;
            return true; // Stub always returns true
        }

        /// <summary>
        /// Calculates SHA256 checksum for a file.
        /// </summary>
        public async Task<string> CalculateChecksumAsync(string filePath, CancellationToken cancellationToken = default)
        {
            if (!File.Exists(filePath))
            {
                throw new FileNotFoundException($"File not found: {filePath}");
            }

            using var stream = File.OpenRead(filePath);
            using var sha256 = SHA256.Create();
            var hashBytes = await sha256.ComputeHashAsync(stream, cancellationToken);
            return BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant();
        }
    }
}
