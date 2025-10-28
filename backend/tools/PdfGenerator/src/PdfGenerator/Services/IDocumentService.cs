using PdfGenerator.Models;
using System.Threading.Tasks;

namespace PdfGenerator.Services;

/// <summary>
/// Interface for PDF document generation service
/// </summary>
public interface IDocumentService
{
    /// <summary>
    /// Generates the complete migration analysis PDF document
    /// </summary>
    /// <param name="outputPath">Path where the PDF should be saved</param>
    /// <param name="dataPath">Path to the data files directory</param>
    /// <returns>Path to the generated PDF file</returns>
    Task<string> GeneratePdfAsync(string outputPath, string dataPath);

    /// <summary>
    /// Validates that all required source files exist
    /// </summary>
    /// <param name="dataPath">Path to the data files directory</param>
    /// <returns>Validation result with any missing files</returns>
    Task<ValidationResult> ValidateSourceFilesAsync(string dataPath);

    /// <summary>
    /// Gets the current document version based on source file changes
    /// </summary>
    /// <param name="dataPath">Path to the data files directory</param>
    /// <returns>Document version information</returns>
    Task<DocumentVersion> GetDocumentVersionAsync(string dataPath);
}

/// <summary>
/// Result of source file validation
/// </summary>
public class ValidationResult
{
    public bool IsValid { get; set; }
    public List<string> MissingFiles { get; set; } = new();
    public List<string> Warnings { get; set; } = new();
}

/// <summary>
/// Document version information
/// </summary>
public class DocumentVersion
{
    public string Version { get; set; } = "1.0.0";
    public DateTime GeneratedDate { get; set; } = DateTime.Now;
    public string SourceHash { get; set; } = "";
    public bool HasChanges { get; set; }
}