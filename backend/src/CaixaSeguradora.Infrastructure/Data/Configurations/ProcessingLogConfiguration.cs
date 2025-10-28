using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    /// <summary>
    /// Entity Framework configuration for ProcessingLog entity.
    /// </summary>
    public class ProcessingLogConfiguration : IEntityTypeConfiguration<ProcessingLog>
    {
        public void Configure(EntityTypeBuilder<ProcessingLog> builder)
        {
            // Table mapping
            builder.ToTable("PROCESSING_LOGS");

            // Primary key
            builder.HasKey(l => l.LogId);

            // Indexes for query performance
            builder.HasIndex(l => l.ExecutionId)
                .HasDatabaseName("IX_ProcessingLog_ExecutionId");

            builder.HasIndex(l => l.Timestamp)
                .HasDatabaseName("IX_ProcessingLog_Timestamp");

            builder.HasIndex(l => l.Severity)
                .HasDatabaseName("IX_ProcessingLog_Severity");

            builder.HasIndex(l => l.PolicyNumber)
                .HasDatabaseName("IX_ProcessingLog_PolicyNumber");

            builder.HasIndex(l => l.CobolSection)
                .HasDatabaseName("IX_ProcessingLog_CobolSection");

            // Composite indexes for common query patterns
            builder.HasIndex(l => new { l.ExecutionId, l.Severity })
                .HasDatabaseName("IX_ProcessingLog_Execution_Severity");

            builder.HasIndex(l => new { l.ExecutionId, l.Timestamp })
                .HasDatabaseName("IX_ProcessingLog_Execution_Timestamp");

            // Property configurations
            builder.Property(l => l.LogId)
                .IsRequired()
                .ValueGeneratedOnAdd();

            builder.Property(l => l.ExecutionId)
                .IsRequired();

            builder.Property(l => l.Timestamp)
                .IsRequired();

            builder.Property(l => l.Severity)
                .IsRequired()
                .HasMaxLength(10);

            builder.Property(l => l.CobolSection)
                .HasMaxLength(50);

            builder.Property(l => l.PolicyNumber)
                .HasColumnType("BIGINT");

            builder.Property(l => l.Message)
                .IsRequired()
                .HasColumnType("TEXT");

            builder.Property(l => l.StackTrace)
                .HasColumnType("TEXT");

            // Foreign key relationship to ReportExecution
            builder.HasOne(l => l.Execution)
                .WithMany(e => e.ProcessingLogs)
                .HasForeignKey(l => l.ExecutionId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
