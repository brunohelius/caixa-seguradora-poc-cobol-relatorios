namespace CaixaSeguradora.Core.Interfaces
{
    /// <summary>
    /// Service interface for writing fixed-width report files.
    /// Handles PREMIT.TXT and PREMCED.TXT file generation with COBOL-compatible formatting.
    /// </summary>
    public interface IFileWriterService
    {
        /// <summary>
        /// Writes premium records to a fixed-width file.
        /// </summary>
        /// <param name="records">Premium records to write</param>
        /// <param name="filePath">Output file path</param>
        /// <param name="fileType">File type (PREMIT or PREMCED)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Number of records written</returns>
        Task<int> WriteToFileAsync<T>(
            IEnumerable<T> records,
            string filePath,
            string fileType,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Validates that a generated file matches expected format.
        /// </summary>
        /// <param name="filePath">File path to validate</param>
        /// <param name="fileType">File type (PREMIT or PREMCED)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>True if file is valid, false otherwise</returns>
        Task<bool> ValidateFileFormatAsync(
            string filePath,
            string fileType,
            CancellationToken cancellationToken = default);

        /// <summary>
        /// Calculates SHA256 checksum for a file.
        /// </summary>
        /// <param name="filePath">File path</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Hexadecimal checksum string</returns>
        Task<string> CalculateChecksumAsync(string filePath, CancellationToken cancellationToken = default);
    }
}
