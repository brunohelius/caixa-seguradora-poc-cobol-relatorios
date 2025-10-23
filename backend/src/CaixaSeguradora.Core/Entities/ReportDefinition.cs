using System;

namespace CaixaSeguradora.Core.Entities
{
    public class ReportDefinition
    {
        public int Id { get; set; }
        public string ReportCode { get; set; } = string.Empty; // PREMIT, PREMCED
        public string ReportName { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public string OutputFormat { get; set; } = string.Empty; // TXT, CSV, PDF
        public int RecordLength { get; set; }
        public bool IsActive { get; set; } = true;
        public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
        public DateTime? UpdatedAt { get; set; }
    }
}
