using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Generated output file metadata.
    /// </summary>
    [Table("FILE_OUTPUTS")]
    public class FileOutput
    {
        [Key]
        public Guid FileId { get; set; }

        public Guid ExecutionId { get; set; }

        [Required]
        [MaxLength(100)]
        public string FileName { get; set; } = string.Empty; // PREMIT_202510.TXT, PREMCED_202510.TXT

        [MaxLength(20)]
        public string FileType { get; set; } = string.Empty; // PREMIT, PREMCED

        [Required]
        public string FilePath { get; set; } = string.Empty;

        public long FileSizeBytes { get; set; }

        public int RecordCount { get; set; }

        public DateTime GeneratedAt { get; set; } = DateTime.UtcNow;

        [MaxLength(64)]
        public string Checksum { get; set; } = string.Empty; // SHA256

        public int DownloadCount { get; set; }

        // Navigation
        [ForeignKey("ExecutionId")]
        public virtual ReportExecution? Execution { get; set; }
    }
}
