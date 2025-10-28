namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// Simplified DTO for report execution tracking (Phase 3).
    /// Returns essential execution information for status polling.
    /// </summary>
    public class ReportExecutionDto
    {
        /// <summary>
        /// Unique execution identifier.
        /// </summary>
        public Guid ExecutionId { get; set; }

        /// <summary>
        /// Reference month (YYYYMM).
        /// </summary>
        public string ReferenceMonth { get; set; } = string.Empty;

        /// <summary>
        /// Execution status: Pending, Running, Completed, Failed
        /// </summary>
        public string Status { get; set; } = "Pending";

        /// <summary>
        /// Start time of execution.
        /// </summary>
        public DateTime StartTime { get; set; }

        /// <summary>
        /// End time (null if still running).
        /// </summary>
        public DateTime? EndTime { get; set; }

        /// <summary>
        /// Number of records processed so far.
        /// </summary>
        public int RecordsProcessed { get; set; }

        /// <summary>
        /// Number of PREMIT records generated.
        /// </summary>
        public int PremitRecordsGenerated { get; set; }

        /// <summary>
        /// Number of PREMCED records generated.
        /// </summary>
        public int PremcedRecordsGenerated { get; set; }

        /// <summary>
        /// Warning count.
        /// </summary>
        public int WarningsCount { get; set; }

        /// <summary>
        /// Error count.
        /// </summary>
        public int ErrorsCount { get; set; }

        /// <summary>
        /// COBOL return code (0000=success, 0004=warnings, 0008=errors, 0012=critical).
        /// </summary>
        public string ReturnCode { get; set; } = "0000";

        /// <summary>
        /// User who triggered the execution.
        /// </summary>
        public string TriggeringUser { get; set; } = string.Empty;

        /// <summary>
        /// Report type being generated.
        /// </summary>
        public string ReportType { get; set; } = "Both";

        /// <summary>
        /// Execution mode.
        /// </summary>
        public string ExecutionMode { get; set; } = "Monthly";

        /// <summary>
        /// Execution duration in seconds (null if still running).
        /// </summary>
        public double? DurationSeconds
        {
            get
            {
                if (EndTime.HasValue)
                {
                    return (EndTime.Value - StartTime).TotalSeconds;
                }
                return null;
            }
        }
    }
}
