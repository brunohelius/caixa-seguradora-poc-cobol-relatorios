using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service for scheduling and managing batch jobs.
/// Supports recurring jobs with cron expressions and one-time scheduled jobs.
/// </summary>
public interface IBatchSchedulingService
{
    /// <summary>
    /// Schedules a recurring job using a cron expression.
    /// </summary>
    /// <param name="jobName">Unique identifier for the job</param>
    /// <param name="cronExpression">Cron expression (e.g., "0 0 * * *" for daily at midnight)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Job ID for tracking</returns>
    Task<string> ScheduleRecurringJobAsync(
        string jobName,
        string cronExpression,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Schedules a one-time job to run at a specific time.
    /// </summary>
    /// <param name="jobName">Unique identifier for the job</param>
    /// <param name="scheduledTime">When to run the job</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Job ID for tracking</returns>
    Task<string> ScheduleDelayedJobAsync(
        string jobName,
        DateTimeOffset scheduledTime,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Cancels a scheduled or recurring job.
    /// </summary>
    /// <param name="jobId">Job ID returned from scheduling methods</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>True if successfully cancelled</returns>
    Task<bool> CancelJobAsync(
        string jobId,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets all scheduled jobs (both recurring and one-time).
    /// </summary>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of scheduled job information</returns>
    Task<IEnumerable<ScheduledJobInfo>> GetScheduledJobsAsync(
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets detailed information about a specific job.
    /// </summary>
    /// <param name="jobId">Job ID</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Job information or null if not found</returns>
    Task<ScheduledJobInfo?> GetJobInfoAsync(
        string jobId,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Triggers a job to run immediately (does not affect schedule).
    /// </summary>
    /// <param name="jobId">Job ID to trigger</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>True if successfully triggered</returns>
    Task<bool> TriggerJobNowAsync(
        string jobId,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Information about a scheduled job.
/// </summary>
public class ScheduledJobInfo
{
    /// <summary>
    /// Unique job identifier.
    /// </summary>
    public string JobId { get; set; } = string.Empty;

    /// <summary>
    /// Human-readable job name.
    /// </summary>
    public string JobName { get; set; } = string.Empty;

    /// <summary>
    /// Type of job (Recurring or Delayed).
    /// </summary>
    public JobType Type { get; set; }

    /// <summary>
    /// Cron expression for recurring jobs.
    /// </summary>
    public string? CronExpression { get; set; }

    /// <summary>
    /// Scheduled time for one-time jobs.
    /// </summary>
    public DateTimeOffset? ScheduledTime { get; set; }

    /// <summary>
    /// Last execution time (null if never executed).
    /// </summary>
    public DateTimeOffset? LastExecutionTime { get; set; }

    /// <summary>
    /// Next scheduled execution time.
    /// </summary>
    public DateTimeOffset? NextExecutionTime { get; set; }

    /// <summary>
    /// Current job state.
    /// </summary>
    public JobState State { get; set; }

    /// <summary>
    /// When the job was created.
    /// </summary>
    public DateTimeOffset CreatedAt { get; set; }
}

/// <summary>
/// Types of scheduled jobs.
/// </summary>
public enum JobType
{
    /// <summary>
    /// Job runs on a recurring schedule (cron expression).
    /// </summary>
    Recurring,

    /// <summary>
    /// Job runs once at a specific time.
    /// </summary>
    Delayed,

    /// <summary>
    /// Job runs immediately (fire-and-forget).
    /// </summary>
    Immediate
}

/// <summary>
/// Current state of a job.
/// </summary>
public enum JobState
{
    /// <summary>
    /// Job is scheduled and waiting to run.
    /// </summary>
    Scheduled,

    /// <summary>
    /// Job is currently executing.
    /// </summary>
    Running,

    /// <summary>
    /// Job completed successfully.
    /// </summary>
    Completed,

    /// <summary>
    /// Job failed during execution.
    /// </summary>
    Failed,

    /// <summary>
    /// Job was cancelled.
    /// </summary>
    Cancelled,

    /// <summary>
    /// Job is paused (recurring jobs only).
    /// </summary>
    Paused
}
