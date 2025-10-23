using System.Collections.Concurrent;
using System.Security.Cryptography;
using System.Text;
using System.Transactions;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Formatters;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Implementation of report generation service for SUSEP Circular 360 premium reports.
/// Generates PREMIT.TXT and PREMCED.TXT files with byte-for-byte COBOL compatibility.
/// </summary>
/// <remarks>
/// Architecture:
/// - Async processing with background job support
/// - Progress tracking for long-running operations (10,000+ records)
/// - File storage with automatic cleanup (7-day retention)
/// - Byte-level comparison with legacy COBOL output
///
/// Processing Flow:
/// 1. Validate request parameters
/// 2. Create report tracking record
/// 3. Queue background job
/// 4. Stream data using IAsyncEnumerable (memory-efficient)
/// 5. Apply business logic calculations
/// 6. Format output using FixedWidthFormatter
/// 7. Generate files with COBOL-compatible encoding
/// 8. Store files and update status
/// </remarks>
public class ReportGenerationService : IReportGenerationService
{
    private readonly IPremiumRepository _premiumRepository;
    private readonly IPolicyRepository _policyRepository;
    private readonly IProductRepository _productRepository;
    private readonly IPremiumCalculationService _calculationService;
    private readonly ICossuranceService _cossuranceService;
    private readonly ILogger<ReportGenerationService> _logger;

    // In-memory report status tracking (in production, use Redis or database)
    private static readonly ConcurrentDictionary<Guid, ReportStatusDto> _reportStatuses = new();

    // File storage base path (in production, use Azure Blob Storage or S3)
    private readonly string _fileStorageBasePath;

    public ReportGenerationService(
        IPremiumRepository premiumRepository,
        IPolicyRepository policyRepository,
        IProductRepository productRepository,
        IPremiumCalculationService calculationService,
        ICossuranceService cossuranceService,
        ILogger<ReportGenerationService> logger)
    {
        _premiumRepository = premiumRepository;
        _policyRepository = policyRepository;
        _productRepository = productRepository;
        _calculationService = calculationService;
        _cossuranceService = cossuranceService;
        _logger = logger;

        // In production: inject via IConfiguration
        _fileStorageBasePath = Path.Combine(Path.GetTempPath(), "CaixaSeguradora", "Reports");
        Directory.CreateDirectory(_fileStorageBasePath);
    }

    /// <inheritdoc/>
    public Task<Guid> GenerateReportAsync(ReportGenerationRequestDto request, CancellationToken cancellationToken = default)
    {
        // Validate request
        ValidateRequest(request);

        // Create report ID and status
        var reportId = Guid.NewGuid();
        var status = new ReportStatusDto
        {
            ReportId = reportId,
            Status = "Queued",
            ProgressPercentage = 0,
            CurrentPhase = "Validation",
            RequestedAt = DateTime.UtcNow,
            Request = request,
            RequestedBy = request.RequestedBy,
            Notes = request.Notes
        };

        _reportStatuses[reportId] = status;

        // Queue background job (in production: use Hangfire, Azure Functions, or similar)
        _ = Task.Run(async () =>
        {
            try
            {
                await ProcessReportGenerationAsync(reportId, request, cancellationToken);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Report generation failed for {ReportId}", reportId);
                UpdateStatusAsFailed(reportId, ex);
            }
        }, cancellationToken);

        _logger.LogInformation("Report generation queued: {ReportId}", reportId);
        return Task.FromResult(reportId);
    }

    /// <inheritdoc/>
    public Task<ReportStatusDto> GetReportStatusAsync(Guid reportId, CancellationToken cancellationToken = default)
    {
        if (!_reportStatuses.TryGetValue(reportId, out ReportStatusDto? status))
        {
            throw new KeyNotFoundException($"Report not found: {reportId}");
        }

        return Task.FromResult(status);
    }

    /// <inheritdoc/>
    public async Task<(Stream FileStream, string FileName, string ContentType)> DownloadReportAsync(
        Guid reportId,
        string fileType,
        CancellationToken cancellationToken = default)
    {
        ReportStatusDto status = await GetReportStatusAsync(reportId, cancellationToken);

        if (status.Status != "Completed")
        {
            throw new InvalidOperationException($"Report is not ready for download. Current status: {status.Status}");
        }

        ReportFileDto? file = status.Files.FirstOrDefault(f => f.FileType.Equals(fileType, StringComparison.OrdinalIgnoreCase));
        if (file == null)
        {
            throw new FileNotFoundException($"File type '{fileType}' not found for report {reportId}");
        }

        if (!file.IsAvailable || (file.ExpiresAt.HasValue && file.ExpiresAt.Value < DateTime.UtcNow))
        {
            throw new FileNotFoundException($"File has expired or is no longer available");
        }

        if (string.IsNullOrEmpty(file.StoragePath) || !File.Exists(file.StoragePath))
        {
            throw new FileNotFoundException($"File not found on disk: {file.StoragePath}");
        }

        var fileStream = new FileStream(file.StoragePath, FileMode.Open, FileAccess.Read, FileShare.Read);
        return (fileStream, file.FileName, file.ContentType);
    }

    /// <inheritdoc/>
    public async Task<ReportComparisonDto> CompareWithCobolOutputAsync(
        Guid reportId,
        string cobolFilePath,
        CancellationToken cancellationToken = default)
    {
        DateTime startTime = DateTime.UtcNow;
        var comparisonId = Guid.NewGuid();

        _logger.LogInformation("Starting COBOL comparison for report {ReportId}", reportId);

        if (!File.Exists(cobolFilePath))
        {
            throw new FileNotFoundException($"COBOL output file not found: {cobolFilePath}");
        }

        ReportStatusDto status = await GetReportStatusAsync(reportId, cancellationToken);
        if (status.Status != "Completed")
        {
            throw new InvalidOperationException("Cannot compare incomplete report");
        }

        // For simplicity, compare PREMIT file (can extend to support both)
        ReportFileDto? netFile = status.Files.FirstOrDefault(f => f.FileType == "PREMIT");
        if (netFile == null || string.IsNullOrEmpty(netFile.StoragePath))
        {
            throw new FileNotFoundException("PREMIT file not found for comparison");
        }

        var comparison = new ReportComparisonDto
        {
            ComparisonId = comparisonId,
            ReportId = reportId,
            CobolFilePath = cobolFilePath,
            ComparedAt = DateTime.UtcNow
        };

        // Byte-level comparison
        var byteLevelMatch = await CompareByteLevelAsync(netFile.StoragePath, cobolFilePath);
        comparison.ByteLevelMatch = byteLevelMatch;

        // Line-by-line comparison
        List<ReportDifferenceDto> lineDifferences = await CompareLineLevelAsync(netFile.StoragePath, cobolFilePath);
        comparison.Differences = lineDifferences;

        // Semantic comparison (business logic totals)
        SemanticComparisonDto semanticComparison = await CompareSemanticAsync(status.Summary, cobolFilePath);
        comparison.SemanticComparison = semanticComparison;
        comparison.SemanticMatch = semanticComparison?.AllValuesMatch ?? false;

        // Calculate summary statistics
        var netLines = File.ReadAllLines(netFile.StoragePath);
        var cobolLines = File.ReadAllLines(cobolFilePath);

        comparison.Summary = new ComparisonSummaryDto
        {
            TotalLinesNet = netLines.Length,
            TotalLinesCobol = cobolLines.Length,
            MatchingLines = netLines.Length - lineDifferences.Count,
            DifferingLines = lineDifferences.Count,
            TotalDifferences = lineDifferences.Count,
            CriticalDifferences = lineDifferences.Count(d => d.Severity == "Critical"),
            WarningDifferences = lineDifferences.Count(d => d.Severity == "Warning"),
            InfoDifferences = lineDifferences.Count(d => d.Severity == "Info"),
            FileSizeDifferenceBytes = new FileInfo(netFile.StoragePath).Length - new FileInfo(cobolFilePath).Length
        };

        comparison.LineMatchPercentage = netLines.Length > 0
            ? (decimal)(comparison.Summary.MatchingLines * 100) / netLines.Length
            : 0;

        // Determine overall result
        if (byteLevelMatch)
        {
            comparison.ComparisonResult = "ExactMatch";
            comparison.PassedValidation = true;
        }
        else if (comparison.SemanticMatch && comparison.Summary.CriticalDifferences == 0)
        {
            comparison.ComparisonResult = "SemanticMatch";
            comparison.PassedValidation = true;
        }
        else
        {
            comparison.ComparisonResult = "Mismatch";
            comparison.PassedValidation = false;
            comparison.ValidationErrors.Add($"{comparison.Summary.CriticalDifferences} critical differences found");
        }

        comparison.ComparisonTimeSeconds = (DateTime.UtcNow - startTime).TotalSeconds;

        _logger.LogInformation(
            "COBOL comparison completed: {Result} ({MatchPercentage}% match)",
            comparison.ComparisonResult,
            comparison.LineMatchPercentage);

        return comparison;
    }

    /// <inheritdoc/>
    public Task<(List<ReportStatusDto> Reports, int TotalCount)> GetReportHistoryAsync(
        DateTime? startDate = null,
        DateTime? endDate = null,
        string? systemId = null,
        string? status = null,
        int pageNumber = 1,
        int pageSize = 20,
        CancellationToken cancellationToken = default)
    {
        IEnumerable<ReportStatusDto> query = _reportStatuses.Values.AsEnumerable();

        // Apply filters
        if (startDate.HasValue)
        {
            query = query.Where(r => r.RequestedAt >= startDate.Value);
        }

        if (endDate.HasValue)
        {
            query = query.Where(r => r.RequestedAt <= endDate.Value);
        }

        if (!string.IsNullOrEmpty(systemId))
        {
            query = query.Where(r => r.Request?.SystemId == systemId);
        }

        if (!string.IsNullOrEmpty(status))
        {
            query = query.Where(r => r.Status.Equals(status, StringComparison.OrdinalIgnoreCase));
        }

        var totalCount = query.Count();

        // Apply pagination
        var reports = query
            .OrderByDescending(r => r.RequestedAt)
            .Skip((pageNumber - 1) * pageSize)
            .Take(pageSize)
            .ToList();

        return Task.FromResult((reports, totalCount));
    }

    /// <inheritdoc/>
    public Task<bool> CancelReportGenerationAsync(Guid reportId, CancellationToken cancellationToken = default)
    {
        if (!_reportStatuses.TryGetValue(reportId, out ReportStatusDto? status))
        {
            return Task.FromResult(false);
        }

        if (status.Status == "Completed" || status.Status == "Failed")
        {
            return Task.FromResult(false);
        }

        status.Status = "Cancelled";
        status.CompletedAt = DateTime.UtcNow;
        status.CurrentPhase = "Cancelled";

        _logger.LogInformation("Report generation cancelled: {ReportId}", reportId);
        return Task.FromResult(true);
    }

    #region Private Methods

    /// <summary>
    /// Main report processing workflow with transaction scope matching COBOL COMMIT points.
    /// </summary>
    /// <remarks>
    /// Transaction Boundary: Matches COBOL section R0900-00-COMMIT logic
    /// - Reads all premium records (read-only, no transaction needed for reads)
    /// - Generates file output
    /// - Writes report metadata to database (transactional write)
    /// - COMMIT after all operations complete successfully (matches COBOL behavior)
    /// </remarks>
    private async Task ProcessReportGenerationAsync(
        Guid reportId,
        ReportGenerationRequestDto request,
        CancellationToken cancellationToken)
    {
        ReportStatusDto status = _reportStatuses[reportId];
        DateTime startTime = DateTime.UtcNow;

        // Transaction scope wraps the entire logical unit of work (matching COBOL COMMIT point)
        // Per research.md R5: Use TransactionScope with async flow enabled
        using var transactionScope = new TransactionScope(
            TransactionScopeOption.Required,
            new TransactionOptions
            {
                IsolationLevel = IsolationLevel.ReadCommitted, // Matches COBOL default isolation level
                Timeout = TimeSpan.FromMinutes(10) // Allow time for large report processing
            },
            TransactionScopeAsyncFlowOption.Enabled); // CRITICAL: Required for async/await

        try
        {
            // Update status to processing
            status.Status = "Processing";
            status.StartedAt = DateTime.UtcNow;
            status.CurrentPhase = "DataFetch";

            _logger.LogInformation("Starting report generation for {ReportId}", reportId);

            // Fetch data using streaming (IAsyncEnumerable) - Read-only operation
            IAsyncEnumerable<PremiumRecord> premiumStream = _premiumRepository.GetPremiumsForReportAsync(
                request.StartDate,
                request.EndDate,
                cancellationToken);

            // Initialize accumulators
            var recordsProcessed = 0;
            var premiumRecords = new List<PremiumRecord>(); // Store entities for file generation
            var summary = new ReportSummaryDto();

            // Process records (read-only data processing)
            status.CurrentPhase = "Calculation";
            await foreach (PremiumRecord? premium in premiumStream.WithCancellation(cancellationToken))
            {
                // Apply filters
                if (!ShouldIncludeRecord(premium, request))
                {
                    continue;
                }

                recordsProcessed++;
                status.RecordsProcessed = recordsProcessed;
                status.ProgressPercentage = Math.Min(90, (recordsProcessed * 90) / 10000); // Estimate

                // Store premium record for file generation
                premiumRecords.Add(premium);

                // Update summary
                UpdateSummary(summary, premium);
            }

            status.TotalRecords = recordsProcessed;
            status.CurrentPhase = "FileGeneration";

            // Generate output files using dedicated file generators
            var files = new List<ReportFileDto>();

            // Generate PREMIT file if requested
            if (request.ReportType == "PREMIT" || request.ReportType == "Both")
            {
                ReportFileDto premitFile = await GeneratePremitFileAsync(
                    reportId,
                    premiumRecords,
                    request,
                    cancellationToken);
                files.Add(premitFile);
                _logger.LogInformation("PREMIT file generated: {RecordCount} records", premiumRecords.Count);
            }

            // Generate PREMCED file if requested
            if (request.ReportType == "PREMCED" || request.ReportType == "Both")
            {
                ReportFileDto premcedFile = await GeneratePremcedFileAsync(
                    reportId,
                    premiumRecords,
                    request,
                    cancellationToken);
                files.Add(premcedFile);

                // PREMCED may have fewer records (only cossured policies)
                var premcedCount = premiumRecords.Count(PremcedFileGenerator.ShouldIncludeInPremced);
                _logger.LogInformation("PREMCED file generated: {RecordCount} cossured records", premcedCount);
            }

            // Update status as completed
            status.Status = "Completed";
            status.CompletedAt = DateTime.UtcNow;
            status.ExecutionTimeSeconds = (DateTime.UtcNow - startTime).TotalSeconds;
            status.CurrentPhase = "Complete";
            status.ProgressPercentage = 100;
            status.Files = files;
            status.Summary = summary;

            // COMMIT transaction (equivalent to COBOL EXEC SQL COMMIT END-EXEC)
            // This ensures report metadata writes are atomic with file generation
            transactionScope.Complete();

            _logger.LogInformation(
                "Report generation completed and committed: {ReportId} - {RecordsProcessed} records in {ExecutionTime:F2}s",
                reportId,
                recordsProcessed,
                status.ExecutionTimeSeconds);
        }
        catch (Exception ex)
        {
            // ROLLBACK automatic when scope disposes without Complete()
            // Matches COBOL: EXEC SQL ROLLBACK END-EXEC on error
            _logger.LogError(ex, "Report generation failed: {ReportId}. Transaction will be rolled back.", reportId);
            throw;
        }
    }

    /// <summary>
    /// Validates report generation request.
    /// </summary>
    private void ValidateRequest(ReportGenerationRequestDto request)
    {
        if (request.EndDate < request.StartDate)
        {
            throw new ArgumentException("End date must be greater than or equal to start date");
        }

        if (string.IsNullOrWhiteSpace(request.SystemId))
        {
            throw new ArgumentException("System ID is required");
        }

        var validReportTypes = new[] { "PREMIT", "PREMCED", "Both" };
        if (!validReportTypes.Contains(request.ReportType, StringComparer.OrdinalIgnoreCase))
        {
            throw new ArgumentException($"Invalid report type. Must be one of: {string.Join(", ", validReportTypes)}");
        }

        if (request.SendEmailNotification && string.IsNullOrWhiteSpace(request.NotificationEmail))
        {
            throw new ArgumentException("Notification email is required when SendEmailNotification is true");
        }
    }

    /// <summary>
    /// Determines if a premium record should be included based on request filters.
    /// </summary>
    private bool ShouldIncludeRecord(dynamic premium, ReportGenerationRequestDto request)
    {
        // Apply optional filters
        if (request.PolicyNumber != null && premium.PolicyNumber != request.PolicyNumber)
        {
            return false;
        }

        if (request.ProductCode.HasValue && premium.ProductCode != request.ProductCode.Value)
        {
            return false;
        }

        if (request.LineOfBusiness.HasValue && premium.LineOfBusiness != request.LineOfBusiness.Value)
        {
            return false;
        }

        if (!request.IncludeCancelled && premium.MovementType == "C")
        {
            return false;
        }

        if (!request.IncludeReversals && premium.MovementType == "R")
        {
            return false;
        }

        return true;
    }

    /// <summary>
    /// Generates PREMIT file from premium records using PremitFileGenerator.
    /// </summary>
    private async Task<ReportFileDto> GeneratePremitFileAsync(
        Guid reportId,
        IEnumerable<PremiumRecord> premiumRecords,
        ReportGenerationRequestDto request,
        CancellationToken cancellationToken)
    {
        var fileName = $"PREMIT_{reportId:N}.TXT";
        var filePath = Path.Combine(_fileStorageBasePath, reportId.ToString(), fileName);

        // Ensure directory exists
        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

        // Generate file using PremitFileGenerator
        await PremitFileGenerator.WriteFileAsync(filePath, premiumRecords, cancellationToken);

        // Calculate file hash for integrity verification
        var fileHash = await CalculateFileHashAsync(filePath);

        return new ReportFileDto
        {
            FileType = "PREMIT",
            FileName = fileName,
            StoragePath = filePath,
            SizeBytes = new FileInfo(filePath).Length,
            GeneratedAt = DateTime.UtcNow,
            ExpiresAt = DateTime.UtcNow.AddDays(7), // 7-day retention
            IsAvailable = true,
            ContentType = "text/plain; charset=ISO-8859-1",
            RecordCount = premiumRecords.Count(),
            FileHash = fileHash
        };
    }

    /// <summary>
    /// Generates PREMCED file from premium records using PremcedFileGenerator.
    /// </summary>
    private async Task<ReportFileDto> GeneratePremcedFileAsync(
        Guid reportId,
        IEnumerable<PremiumRecord> premiumRecords,
        ReportGenerationRequestDto request,
        CancellationToken cancellationToken)
    {
        var fileName = $"PREMCED_{reportId:N}.TXT";
        var filePath = Path.Combine(_fileStorageBasePath, reportId.ToString(), fileName);

        // Ensure directory exists
        Directory.CreateDirectory(Path.GetDirectoryName(filePath)!);

        // Generate file using PremcedFileGenerator (returns record count)
        var recordCount = await PremcedFileGenerator.WriteFileAsync(filePath, premiumRecords, cancellationToken);

        // Calculate file hash for integrity verification
        var fileHash = await CalculateFileHashAsync(filePath);

        return new ReportFileDto
        {
            FileType = "PREMCED",
            FileName = fileName,
            StoragePath = filePath,
            SizeBytes = new FileInfo(filePath).Length,
            GeneratedAt = DateTime.UtcNow,
            ExpiresAt = DateTime.UtcNow.AddDays(7), // 7-day retention
            IsAvailable = true,
            ContentType = "text/plain; charset=ISO-8859-1",
            RecordCount = recordCount, // Only cossured policies included
            FileHash = fileHash
        };
    }

    /// <summary>
    /// Calculates SHA-256 hash of file for integrity verification.
    /// </summary>
    private async Task<string> CalculateFileHashAsync(string filePath)
    {
        using FileStream stream = File.OpenRead(filePath);
        var hashBytes = await SHA256.HashDataAsync(stream);
        return BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant();
    }

    /// <summary>
    /// Updates summary statistics with processed premium record.
    /// </summary>
    private void UpdateSummary(ReportSummaryDto summary, dynamic premium)
    {
        summary.TotalRecordsProcessed++;

        if (premium.MovementType == "E")
        {
            summary.EmissionCount++;
        }
        else if (premium.MovementType == "C")
        {
            summary.CancellationCount++;
        }
        else if (premium.MovementType == "R")
        {
            summary.ReversalCount++;
        }

        // Accumulate totals (stub - use actual calculation service in production)
        summary.TotalNetPremium += (decimal)premium.NetPremium;
        // ... other accumulations
    }

    /// <summary>
    /// Generates output file and stores it on disk.
    /// </summary>
    private async Task<ReportFileDto> GenerateOutputFileAsync(
        Guid reportId,
        string fileType,
        List<string> records,
        ReportGenerationRequestDto request,
        CancellationToken cancellationToken)
    {
        var fileName = $"{fileType}_{request.SystemId}_{request.StartDate:yyyyMMdd}_{request.EndDate:yyyyMMdd}.TXT";
        var reportDir = Path.Combine(_fileStorageBasePath, reportId.ToString());
        Directory.CreateDirectory(reportDir);
        var filePath = Path.Combine(reportDir, fileName);

        // Write file with COBOL-compatible encoding (ISO-8859-1)
        var encoding = Encoding.GetEncoding("ISO-8859-1");
        await File.WriteAllLinesAsync(filePath, records, encoding, cancellationToken);

        var fileInfo = new FileInfo(filePath);
        var md5Hash = await ComputeMD5HashAsync(filePath);
        var sha256Hash = await ComputeSHA256HashAsync(filePath);

        return new ReportFileDto
        {
            FileType = fileType,
            FileName = fileName,
            SizeBytes = fileInfo.Length,
            SizeFormatted = FormatFileSize(fileInfo.Length),
            LineCount = records.Count,
            RecordCount = records.Count, // Adjust if headers/footers exist
            GeneratedAt = DateTime.UtcNow,
            DownloadUrl = $"/api/reports/{reportId}/download/{fileType}",
            ContentType = "text/plain",
            Encoding = "ISO-8859-1",
            MD5Hash = md5Hash,
            SHA256Hash = sha256Hash,
            ExpiresAt = DateTime.UtcNow.AddDays(7),
            StoragePath = filePath,
            IsAvailable = true
        };
    }

    /// <summary>
    /// Compares two files byte-for-byte.
    /// </summary>
    private async Task<bool> CompareByteLevelAsync(string file1, string file2)
    {
        var bytes1 = await File.ReadAllBytesAsync(file1);
        var bytes2 = await File.ReadAllBytesAsync(file2);

        return bytes1.SequenceEqual(bytes2);
    }

    /// <summary>
    /// Compares two files line-by-line and returns differences.
    /// </summary>
    private async Task<List<ReportDifferenceDto>> CompareLineLevelAsync(string file1, string file2)
    {
        var lines1 = await File.ReadAllLinesAsync(file1);
        var lines2 = await File.ReadAllLinesAsync(file2);

        var differences = new List<ReportDifferenceDto>();
        var maxLines = Math.Max(lines1.Length, lines2.Length);

        for (var i = 0; i < maxLines; i++)
        {
            var line1 = i < lines1.Length ? lines1[i] : null;
            var line2 = i < lines2.Length ? lines2[i] : null;

            if (line1 != line2)
            {
                differences.Add(new ReportDifferenceDto
                {
                    DifferenceType = "LineDifference",
                    Severity = "Warning",
                    LineNumber = i + 1,
                    ExpectedValue = line2,
                    ActualValue = line1,
                    Description = $"Line {i + 1} differs"
                });
            }
        }

        return differences;
    }

    /// <summary>
    /// Performs semantic comparison of business logic totals.
    /// </summary>
    private Task<SemanticComparisonDto> CompareSemanticAsync(ReportSummaryDto? netSummary, string cobolFilePath)
    {
        // TODO: Parse COBOL file to extract totals and compare with netSummary
        // This is a stub implementation

        var comparison = new SemanticComparisonDto
        {
            AllValuesMatch = true // Placeholder
        };

        return Task.FromResult(comparison);
    }

    /// <summary>
    /// Updates status as failed with error details.
    /// </summary>
    private void UpdateStatusAsFailed(Guid reportId, Exception ex)
    {
        if (_reportStatuses.TryGetValue(reportId, out ReportStatusDto? status))
        {
            status.Status = "Failed";
            status.CompletedAt = DateTime.UtcNow;
            status.ErrorMessage = ex.Message;
            status.ErrorDetails = ex.ToString();
        }
    }

    /// <summary>
    /// Computes MD5 hash of a file.
    /// </summary>
    private async Task<string> ComputeMD5HashAsync(string filePath)
    {
        using var md5 = MD5.Create();
        using FileStream stream = File.OpenRead(filePath);
        var hash = await md5.ComputeHashAsync(stream);
        return BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant();
    }

    /// <summary>
    /// Computes SHA256 hash of a file.
    /// </summary>
    private async Task<string> ComputeSHA256HashAsync(string filePath)
    {
        using var sha256 = SHA256.Create();
        using FileStream stream = File.OpenRead(filePath);
        var hash = await sha256.ComputeHashAsync(stream);
        return BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant();
    }

    /// <summary>
    /// Formats file size in human-readable format.
    /// </summary>
    private string FormatFileSize(long bytes)
    {
        string[] sizes = { "B", "KB", "MB", "GB" };
        double len = bytes;
        var order = 0;
        while (len >= 1024 && order < sizes.Length - 1)
        {
            order++;
            len = len / 1024;
        }
        return $"{len:0.##} {sizes[order]}";
    }

    #endregion
}
