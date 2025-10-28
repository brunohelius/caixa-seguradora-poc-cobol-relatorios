using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Detailed processing log entry.
    /// Equivalent to COBOL DISPLAY statements and SYSPRINT output.
    /// </summary>
    [Table("PROCESSING_LOGS")]
    public class ProcessingLog
    {
        [Key]
        public long LogId { get; set; }

        public Guid ExecutionId { get; set; }

        public DateTime Timestamp { get; set; } = DateTime.UtcNow;

        [MaxLength(10)]
        public string Severity { get; set; } = "INFO"; // INFO, WARNING, ERROR, DEBUG

        [MaxLength(50)]
        public string CobolSection { get; set; } = string.Empty; // R0700-00, R1240-00, etc.

        public long? PolicyNumber { get; set; } // Context if applicable

        [Required]
        public string Message { get; set; } = string.Empty;

        public string? StackTrace { get; set; }

        // Navigation
        [ForeignKey("ExecutionId")]
        public virtual ReportExecution? Execution { get; set; }
    }
}
