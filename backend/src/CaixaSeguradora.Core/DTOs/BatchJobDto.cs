namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// DTO for batch job creation and updates.
    /// </summary>
    public class BatchJobRequestDto
    {
        /// <summary>
        /// Job name/title
        /// </summary>
        public string JobName { get; set; } = string.Empty;

        /// <summary>
        /// Job description
        /// </summary>
        public string Description { get; set; } = string.Empty;

        /// <summary>
        /// Recurrence pattern: DAILY, WEEKLY, MONTHLY, ONCE
        /// </summary>
        public string RecurrencePattern { get; set; } = "ONCE";

        /// <summary>
        /// Report generation parameters (JSON serialized ReportGenerationRequestDto)
        /// </summary>
        public string ReportParameters { get; set; } = string.Empty;

        /// <summary>
        /// Hour of day for execution (0-23)
        /// </summary>
        public int? ExecutionHour { get; set; }

        /// <summary>
        /// Minute for execution (0-59)
        /// </summary>
        public int? ExecutionMinute { get; set; }

        /// <summary>
        /// Day of week for WEEKLY jobs (0=Sunday, 6=Saturday)
        /// </summary>
        public int? DayOfWeek { get; set; }

        /// <summary>
        /// Day of month for MONTHLY jobs (1-31)
        /// </summary>
        public int? DayOfMonth { get; set; }

        /// <summary>
        /// Comma-separated email addresses for notifications
        /// </summary>
        public string NotificationRecipients { get; set; } = string.Empty;

        /// <summary>
        /// Maximum retry attempts for failed jobs
        /// </summary>
        public int MaxRetries { get; set; } = 3;

        /// <summary>
        /// User creating the job
        /// </summary>
        public string CreatedBy { get; set; } = string.Empty;
    }

    /// <summary>
    /// DTO for batch job response.
    /// </summary>
    public class BatchJobResponseDto
    {
        public int JobId { get; set; }
        public string JobName { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public string RecurrencePattern { get; set; } = string.Empty;
        public string Status { get; set; } = string.Empty;
        public DateTime? NextExecutionTime { get; set; }
        public DateTime? LastExecutionTime { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime CreatedDate { get; set; }
        public bool IsEnabled { get; set; }
        public int RetryCount { get; set; }
        public int MaxRetries { get; set; }
        public string NotificationRecipients { get; set; } = string.Empty;

        /// <summary>
        /// Latest execution summary (optional)
        /// </summary>
        public BatchJobExecutionSummaryDto? LatestExecution { get; set; }
    }

    /// <summary>
    /// DTO for batch job execution history.
    /// </summary>
    public class BatchJobExecutionDto
    {
        public int ExecutionId { get; set; }
        public int JobId { get; set; }
        public DateTime StartTime { get; set; }
        public DateTime? EndTime { get; set; }
        public string Status { get; set; } = string.Empty;
        public string ErrorMessage { get; set; } = string.Empty;
        public string OutputFilePath { get; set; } = string.Empty;
        public int RecordsProcessed { get; set; }
        public string ExecutedBy { get; set; } = string.Empty;
        public string ExecutionLog { get; set; } = string.Empty;

        /// <summary>
        /// Duration in seconds (calculated)
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

    /// <summary>
    /// Summary DTO for execution in job list.
    /// </summary>
    public class BatchJobExecutionSummaryDto
    {
        public int ExecutionId { get; set; }
        public DateTime StartTime { get; set; }
        public DateTime? EndTime { get; set; }
        public string Status { get; set; } = string.Empty;
        public int RecordsProcessed { get; set; }
        public string ErrorMessage { get; set; } = string.Empty;
    }

    /// <summary>
    /// DTO for manual job execution request.
    /// </summary>
    public class ExecuteJobRequestDto
    {
        /// <summary>
        /// User triggering the execution
        /// </summary>
        public string ExecutedBy { get; set; } = "MANUAL";
    }

    /// <summary>
    /// DTO for job schedule update.
    /// </summary>
    public class UpdateJobScheduleDto
    {
        public string? JobName { get; set; }
        public string? Description { get; set; }
        public string? RecurrencePattern { get; set; }
        public string? ReportParameters { get; set; }
        public int? ExecutionHour { get; set; }
        public int? ExecutionMinute { get; set; }
        public int? DayOfWeek { get; set; }
        public int? DayOfMonth { get; set; }
        public string? NotificationRecipients { get; set; }
        public int? MaxRetries { get; set; }
    }
}
