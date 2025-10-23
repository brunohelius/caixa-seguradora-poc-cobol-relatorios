namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// DTO representing a generated report file.
/// Contains metadata and download information.
/// </summary>
public class ReportFileDto
{
    /// <summary>
    /// File type identifier.
    /// Values: PREMIT, PREMCED
    /// </summary>
    public string FileType { get; set; } = string.Empty;

    /// <summary>
    /// File name with extension.
    /// </summary>
    /// <example>PREMIT_GL_20251001_20251031.TXT</example>
    public string FileName { get; set; } = string.Empty;

    /// <summary>
    /// File size in bytes.
    /// </summary>
    public long SizeBytes { get; set; }

    /// <summary>
    /// File size in human-readable format (KB, MB).
    /// </summary>
    /// <example>245 KB</example>
    public string SizeFormatted { get; set; } = string.Empty;

    /// <summary>
    /// Number of lines in the file.
    /// </summary>
    public int LineCount { get; set; }

    /// <summary>
    /// Number of records in the file (excluding headers/footers).
    /// </summary>
    public int RecordCount { get; set; }

    /// <summary>
    /// File generation timestamp.
    /// </summary>
    public DateTime GeneratedAt { get; set; }

    /// <summary>
    /// Download URL or endpoint path.
    /// </summary>
    /// <example>/api/reports/abc123-def456/download/PREMIT</example>
    public string DownloadUrl { get; set; } = string.Empty;

    /// <summary>
    /// Content type for HTTP download.
    /// </summary>
    /// <example>text/plain</example>
    public string ContentType { get; set; } = "text/plain";

    /// <summary>
    /// Character encoding used in file.
    /// </summary>
    /// <example>ISO-8859-1</example>
    public string Encoding { get; set; } = "ISO-8859-1";

    /// <summary>
    /// MD5 hash of file contents for integrity verification.
    /// </summary>
    public string? MD5Hash { get; set; }

    /// <summary>
    /// SHA256 hash of file contents for integrity verification.
    /// </summary>
    public string? SHA256Hash { get; set; }

    /// <summary>
    /// Generic file hash for backward compatibility.
    /// Can be MD5 or SHA256 depending on configuration.
    /// </summary>
    public string? FileHash { get; set; }

    /// <summary>
    /// File expiration date (when it will be purged from storage).
    /// </summary>
    public DateTime? ExpiresAt { get; set; }

    /// <summary>
    /// File storage path (internal, not exposed to client).
    /// </summary>
    public string? StoragePath { get; set; }

    /// <summary>
    /// Whether file is still available for download.
    /// False if expired or deleted.
    /// </summary>
    public bool IsAvailable { get; set; } = true;
}
