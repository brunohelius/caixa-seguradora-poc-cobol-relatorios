using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class BatchJobExecutionConfiguration : IEntityTypeConfiguration<BatchJobExecution>
    {
        public void Configure(EntityTypeBuilder<BatchJobExecution> builder)
        {
            builder.ToTable("BatchJobExecutions");

            builder.HasKey(e => e.ExecutionId);

            builder.Property(e => e.JobId).IsRequired();
            builder.Property(e => e.StartTime).IsRequired();
            builder.Property(e => e.Status).IsRequired().HasMaxLength(20).HasDefaultValue("RUNNING");
            builder.Property(e => e.ErrorMessage).HasMaxLength(1000);
            builder.Property(e => e.OutputFilePath).HasMaxLength(500);
            builder.Property(e => e.RecordsProcessed).HasDefaultValue(0);
            builder.Property(e => e.ExecutedBy).IsRequired().HasMaxLength(100).HasDefaultValue("SYSTEM");
            builder.Property(e => e.ExecutionLog).HasColumnType("text");

            // Relationship with BatchJob
            builder.HasOne(e => e.BatchJob)
                .WithMany()
                .HasForeignKey(e => e.JobId)
                .OnDelete(DeleteBehavior.Cascade);

            builder.HasIndex(e => e.JobId);
            builder.HasIndex(e => e.Status);
            builder.HasIndex(e => e.StartTime);
        }
    }
}
