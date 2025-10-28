using System;
using System.Collections.Generic;

namespace PdfGenerator.Models.ChartModels
{
    /// <summary>
    /// Represents data for generating a Gantt chart showing project timeline.
    /// </summary>
    public class GanttChartData
    {
        public string Title { get; set; } = "Cronograma de Migração COBOL para .NET";
        public DateTime ProjectStartDate { get; set; }
        public DateTime ProjectEndDate { get; set; }
        public List<GanttPhase> Phases { get; set; } = new();
        public List<GanttMilestone> Milestones { get; set; } = new();
        public string Currency { get; set; } = "BRL";

        /// <summary>
        /// Calculate total project duration in weeks
        /// </summary>
        public int GetDurationInWeeks()
        {
            if (ProjectStartDate == default || ProjectEndDate == default)
                return 0;

            return (int)Math.Ceiling((ProjectEndDate - ProjectStartDate).TotalDays / 7);
        }
    }

    /// <summary>
    /// Represents a project phase in the Gantt chart
    /// </summary>
    public class GanttPhase
    {
        public string Id { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
        public string Color { get; set; } = "#0047BB"; // Caixa blue by default
        public decimal PercentComplete { get; set; }
        public List<GanttTask> Tasks { get; set; } = new();
        public string Description { get; set; } = string.Empty;

        /// <summary>
        /// Calculate phase duration in days
        /// </summary>
        public int GetDurationInDays()
        {
            return (int)(EndDate - StartDate).TotalDays;
        }
    }

    /// <summary>
    /// Represents an individual task within a phase
    /// </summary>
    public class GanttTask
    {
        public string Id { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
        public decimal PercentComplete { get; set; }
        public string AssignedTo { get; set; } = string.Empty;
        public TaskStatus Status { get; set; }
        public List<string> Dependencies { get; set; } = new();
        public bool IsCriticalPath { get; set; }

        /// <summary>
        /// Calculate task duration in days
        /// </summary>
        public int GetDurationInDays()
        {
            return (int)(EndDate - StartDate).TotalDays;
        }
    }

    /// <summary>
    /// Represents a project milestone
    /// </summary>
    public class GanttMilestone
    {
        public string Id { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public DateTime Date { get; set; }
        public string Description { get; set; } = string.Empty;
        public bool IsCompleted { get; set; }
        public string Icon { get; set; } = "◆"; // Diamond symbol by default
        public string PaymentPercentage { get; set; } = string.Empty;
    }

    /// <summary>
    /// Task status enumeration
    /// </summary>
    public enum TaskStatus
    {
        NotStarted,
        InProgress,
        Completed,
        Delayed,
        OnHold
    }
}