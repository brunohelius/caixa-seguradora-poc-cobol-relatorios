using System;

namespace CaixaSeguradora.Core.Entities
{
    public class BatchJobExecution
    {
        public int ExecutionId { get; set; }
        public int JobId { get; set; }
        public DateTime StartTime { get; set; } = DateTime.UtcNow;
        public DateTime? EndTime { get; set; }
        public string Status { get; set; } = "RUNNING"; // RUNNING, SUCCESS, FAILED, CANCELLED
        public string ErrorMessage { get; set; } = string.Empty;
        public string OutputFilePath { get; set; } = string.Empty; // Path to generated report
        public int RecordsProcessed { get; set; } = 0;
        public string ExecutedBy { get; set; } = "SYSTEM"; // SYSTEM or username for manual executions
        public string ExecutionLog { get; set; } = string.Empty; // Detailed log messages

        // Navigation property
        public virtual BatchJob? BatchJob { get; set; }
    }
}
