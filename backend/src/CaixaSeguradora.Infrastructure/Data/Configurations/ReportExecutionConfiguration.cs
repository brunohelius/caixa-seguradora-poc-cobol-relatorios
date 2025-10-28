using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    /// <summary>
    /// Entity Framework configuration for ReportExecution entity.
    /// </summary>
    public class ReportExecutionConfiguration : IEntityTypeConfiguration<ReportExecution>
    {
        public void Configure(EntityTypeBuilder<ReportExecution> builder)
        {
            // Table mapping
            builder.ToTable("REPORT_EXECUTIONS");

            // Primary key
            builder.HasKey(e => e.ExecutionId);

            // Indexes for query performance
            builder.HasIndex(e => e.ReferenceMonth)
                .HasDatabaseName("IX_ReportExecution_ReferenceMonth");

            builder.HasIndex(e => e.StartTime)
                .HasDatabaseName("IX_ReportExecution_StartTime");

            builder.HasIndex(e => e.Status)
                .HasDatabaseName("IX_ReportExecution_Status");

            builder.HasIndex(e => e.ReturnCode)
                .HasDatabaseName("IX_ReportExecution_ReturnCode");

            // Composite index for month + status queries
            builder.HasIndex(e => new { e.ReferenceMonth, e.Status })
                .HasDatabaseName("IX_ReportExecution_Month_Status");

            // Property configurations
            builder.Property(e => e.ExecutionId)
                .IsRequired()
                .ValueGeneratedNever(); // Guid will be generated in code

            builder.Property(e => e.ReferenceMonth)
                .IsRequired()
                .HasMaxLength(6)
                .IsFixedLength();

            builder.Property(e => e.StartTime)
                .IsRequired();

            builder.Property(e => e.EndTime);

            builder.Property(e => e.Status)
                .IsRequired()
                .HasMaxLength(20);

            builder.Property(e => e.RecordsProcessed)
                .HasDefaultValue(0);

            builder.Property(e => e.PremitRecordsGenerated)
                .HasDefaultValue(0);

            builder.Property(e => e.PremcedRecordsGenerated)
                .HasDefaultValue(0);

            builder.Property(e => e.WarningsCount)
                .HasDefaultValue(0);

            builder.Property(e => e.ErrorsCount)
                .HasDefaultValue(0);

            builder.Property(e => e.ReturnCode)
                .HasMaxLength(4);

            builder.Property(e => e.TriggeringUser)
                .HasMaxLength(100);

            builder.Property(e => e.ReportType)
                .HasMaxLength(10);

            builder.Property(e => e.ExecutionMode)
                .HasMaxLength(20);

            // Relationships
            builder.HasMany(e => e.ProcessingLogs)
                .WithOne(l => l.Execution)
                .HasForeignKey(l => l.ExecutionId)
                .OnDelete(DeleteBehavior.Cascade); // Delete logs when execution is deleted

            builder.HasMany(e => e.FileOutputs)
                .WithOne(f => f.Execution)
                .HasForeignKey(f => f.ExecutionId)
                .OnDelete(DeleteBehavior.Cascade); // Delete files when execution is deleted
        }
    }
}
