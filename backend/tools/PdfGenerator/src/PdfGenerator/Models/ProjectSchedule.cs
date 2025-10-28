using System;
using System.Collections.Generic;
using System.Linq;

namespace PdfGenerator.Models
{
    /// <summary>
    /// Represents the complete project schedule with phases, tasks, and milestones.
    /// Used for timeline visualization and financial planning.
    /// </summary>
    public class ProjectSchedule
    {
        public string ProjectName { get; set; } = "Migração COBOL RG1866B para .NET";
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
        public List<Phase> Phases { get; set; } = new();
        public List<Milestone> Milestones { get; set; } = new();
        public decimal TotalBudget { get; set; }
        public string Currency { get; set; } = "BRL";
        public ProjectStatus Status { get; set; } = ProjectStatus.Planning;

        /// <summary>
        /// Calculate total duration in weeks
        /// </summary>
        public int GetDurationInWeeks()
        {
            return (int)Math.Ceiling((EndDate - StartDate).TotalDays / 7);
        }

        /// <summary>
        /// Calculate overall project completion percentage
        /// </summary>
        public decimal GetOverallCompletionPercentage()
        {
            if (!Phases.Any()) return 0;

            var totalWeight = Phases.Sum(p => p.Weight);
            if (totalWeight == 0) return 0;

            var weightedCompletion = Phases.Sum(p => p.GetCompletionPercentage() * p.Weight);
            return weightedCompletion / totalWeight;
        }

        /// <summary>
        /// Get current phase based on today's date
        /// </summary>
        public Phase GetCurrentPhase()
        {
            var today = DateTime.Today;
            return Phases.FirstOrDefault(p => p.StartDate <= today && p.EndDate >= today);
        }

        /// <summary>
        /// Get upcoming milestones
        /// </summary>
        public List<Milestone> GetUpcomingMilestones(int count = 3)
        {
            var today = DateTime.Today;
            return Milestones
                .Where(m => m.Date >= today && !m.IsCompleted)
                .OrderBy(m => m.Date)
                .Take(count)
                .ToList();
        }
    }

    /// <summary>
    /// Represents a project phase
    /// </summary>
    public class Phase
    {
        public string Id { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
        public List<ProjectTask> Tasks { get; set; } = new();
        public decimal Weight { get; set; } = 1.0m; // Weight for overall calculation
        public PhaseStatus Status { get; set; } = PhaseStatus.NotStarted;
        public string Owner { get; set; } = string.Empty;
        public List<string> Deliverables { get; set; } = new();

        /// <summary>
        /// Calculate phase duration in days
        /// </summary>
        public int GetDurationInDays()
        {
            return (int)(EndDate - StartDate).TotalDays;
        }

        /// <summary>
        /// Calculate phase completion percentage based on tasks
        /// </summary>
        public decimal GetCompletionPercentage()
        {
            if (!Tasks.Any()) return 0;

            var completedTasks = Tasks.Count(t => t.Status == TaskStatus.Completed);
            return (completedTasks * 100m) / Tasks.Count;
        }

        /// <summary>
        /// Get critical path tasks
        /// </summary>
        public List<ProjectTask> GetCriticalPathTasks()
        {
            return Tasks.Where(t => t.IsCriticalPath).ToList();
        }
    }

    /// <summary>
    /// Represents an individual task within a phase
    /// </summary>
    public class ProjectTask
    {
        public string Id { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
        public TaskStatus Status { get; set; } = TaskStatus.NotStarted;
        public string AssignedTo { get; set; } = string.Empty;
        public int EstimatedHours { get; set; }
        public int ActualHours { get; set; }
        public List<string> Dependencies { get; set; } = new();
        public bool IsCriticalPath { get; set; }
        public decimal PercentComplete { get; set; }

        /// <summary>
        /// Calculate task duration in days
        /// </summary>
        public int GetDurationInDays()
        {
            return (int)(EndDate - StartDate).TotalDays;
        }

        /// <summary>
        /// Check if task is delayed
        /// </summary>
        public bool IsDelayed()
        {
            return Status != TaskStatus.Completed && DateTime.Today > EndDate;
        }

        /// <summary>
        /// Get variance in hours (Actual - Estimated)
        /// </summary>
        public int GetHoursVariance()
        {
            return ActualHours - EstimatedHours;
        }
    }

    /// <summary>
    /// Represents a project milestone
    /// </summary>
    public class Milestone
    {
        public string Id { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public DateTime Date { get; set; }
        public bool IsCompleted { get; set; }
        public DateTime? CompletedDate { get; set; }
        public MilestoneType Type { get; set; } = MilestoneType.Delivery;
        public decimal PaymentPercentage { get; set; }
        public decimal PaymentAmount { get; set; }
        public List<string> Deliverables { get; set; } = new();
        public List<string> AcceptanceCriteria { get; set; } = new();

        /// <summary>
        /// Check if milestone is overdue
        /// </summary>
        public bool IsOverdue()
        {
            return !IsCompleted && DateTime.Today > Date;
        }

        /// <summary>
        /// Get days until milestone (negative if overdue)
        /// </summary>
        public int GetDaysUntil()
        {
            return (int)(Date - DateTime.Today).TotalDays;
        }
    }

    /// <summary>
    /// Project status
    /// </summary>
    public enum ProjectStatus
    {
        Planning,
        InProgress,
        OnHold,
        Completed,
        Cancelled
    }

    /// <summary>
    /// Phase status
    /// </summary>
    public enum PhaseStatus
    {
        NotStarted,
        InProgress,
        Completed,
        Delayed,
        Blocked
    }

    /// <summary>
    /// Task status
    /// </summary>
    public enum TaskStatus
    {
        NotStarted,
        InProgress,
        Completed,
        Delayed,
        Blocked,
        Cancelled
    }

    /// <summary>
    /// Milestone type
    /// </summary>
    public enum MilestoneType
    {
        Kickoff,
        Delivery,
        Payment,
        Review,
        Approval,
        GoLive
    }
}