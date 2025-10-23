using System;

namespace CaixaSeguradora.Core.Entities
{
    public class BatchJob
    {
        public int JobId { get; set; }
        public string JobName { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public string RecurrencePattern { get; set; } = string.Empty; // DAILY, WEEKLY, MONTHLY, ONCE
        public string ReportParameters { get; set; } = string.Empty; // JSON serialized parameters
        public string Status { get; set; } = "ACTIVE"; // ACTIVE, PAUSED, COMPLETED, FAILED
        public DateTime? NextExecutionTime { get; set; }
        public DateTime? LastExecutionTime { get; set; }
        public string CreatedBy { get; set; } = string.Empty;
        public DateTime CreatedDate { get; set; } = DateTime.UtcNow;
        public DateTime? UpdatedDate { get; set; }

        // Optional fields for advanced scheduling
        public int? ExecutionHour { get; set; } // Hour of day (0-23)
        public int? ExecutionMinute { get; set; } // Minute (0-59)
        public int? DayOfWeek { get; set; } // For WEEKLY: 0=Sunday, 6=Saturday
        public int? DayOfMonth { get; set; } // For MONTHLY: 1-31
        public string NotificationRecipients { get; set; } = string.Empty; // Comma-separated emails
        public bool IsEnabled { get; set; } = true;
        public int MaxRetries { get; set; } = 3;
        public int RetryCount { get; set; } = 0;
    }
}
