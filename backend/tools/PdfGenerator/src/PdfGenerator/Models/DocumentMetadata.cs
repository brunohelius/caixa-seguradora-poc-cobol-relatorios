using System;
using System.Collections.Generic;

namespace PdfGenerator.Models
{
    /// <summary>
    /// Metadata for the migration analysis document
    /// </summary>
    public class DocumentMetadata
    {
        public string CompanyName { get; set; } = "Caixa Seguradora";
        public string ProductName { get; set; } = "SUSEP Circular 360 - Sistema de Apuração de Prêmios";
        public string DocumentTitle { get; set; } = "Análise de Migração COBOL para .NET 9";
        public string Version { get; set; } = "1.0.0";
        public DateTime GeneratedDate { get; set; } = DateTime.Now;
        public string Author { get; set; } = "Equipe de Migração COBOL";
        public string Language { get; set; } = "pt-BR";

        // Document properties
        public int TotalPages { get; set; }
        public long FileSizeBytes { get; set; }
        public TimeSpan GenerationTime { get; set; }

        // Version control
        public string PreviousVersion { get; set; }
        public List<string> ChangeLog { get; set; } = new List<string>();
        public string GitCommitHash { get; set; }

        // Source file tracking
        public Dictionary<string, DateTime> SourceFiles { get; set; } = new Dictionary<string, DateTime>();
        public Dictionary<string, string> FileHashes { get; set; } = new Dictionary<string, string>();

        /// <summary>
        /// Check if any source files have changed since last generation
        /// </summary>
        public bool HasSourceChanges()
        {
            // Implementation will compare file hashes
            return !string.IsNullOrEmpty(PreviousVersion) && ChangeLog.Count > 0;
        }

        /// <summary>
        /// Generate a unique document ID based on version and date
        /// </summary>
        public string GetDocumentId()
        {
            return $"MIGR-{Version}-{GeneratedDate:yyyyMMdd-HHmmss}";
        }
    }
}