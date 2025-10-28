using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Report generation execution metadata.
    /// Tracks each run of the SUSEP report generation process.
    /// </summary>
    [Table("REPORT_EXECUTIONS")]
    public class ReportExecution
    {
        [Key]
        public Guid ExecutionId { get; set; }

        [Required]
        [MaxLength(6)]
        public string ReferenceMonth { get; set; } = string.Empty; // YYYYMM format (e.g., "202510")

        public DateTime StartTime { get; set; }

        public DateTime? EndTime { get; set; }

        [MaxLength(20)]
        public string Status { get; set; } = "Pending"; // Pending, Running, Completed, Failed

        public int RecordsProcessed { get; set; }

        public int PremitRecordsGenerated { get; set; }

        public int PremcedRecordsGenerated { get; set; }

        public int WarningsCount { get; set; }

        public int ErrorsCount { get; set; }

        [MaxLength(4)]
        public string ReturnCode { get; set; } = "0000"; // 0000, 0004, 0008, 0012

        [MaxLength(100)]
        public string TriggeringUser { get; set; } = string.Empty;

        [MaxLength(10)]
        public string ReportType { get; set; } = "Both"; // PREMIT, PREMCED, Both

        [MaxLength(20)]
        public string ExecutionMode { get; set; } = "Monthly"; // Weekly, Monthly

        // Navigation properties
        public virtual ICollection<ProcessingLog> ProcessingLogs { get; set; } = new List<ProcessingLog>();
        public virtual ICollection<FileOutput> FileOutputs { get; set; } = new List<FileOutput>();
    }
}
