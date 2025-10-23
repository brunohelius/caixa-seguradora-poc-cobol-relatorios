using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class BatchJobConfiguration : IEntityTypeConfiguration<BatchJob>
    {
        public void Configure(EntityTypeBuilder<BatchJob> builder)
        {
            builder.ToTable("BatchJobs");

            builder.HasKey(b => b.JobId);

            builder.Property(b => b.JobName).IsRequired().HasMaxLength(100);
            builder.Property(b => b.Description).HasMaxLength(500);
            builder.Property(b => b.RecurrencePattern).IsRequired().HasMaxLength(20);
            builder.Property(b => b.ReportParameters).HasColumnType("text");
            builder.Property(b => b.Status).IsRequired().HasMaxLength(20).HasDefaultValue("ACTIVE");
            builder.Property(b => b.CreatedBy).IsRequired().HasMaxLength(100);
            builder.Property(b => b.CreatedDate).IsRequired();
            builder.Property(b => b.NotificationRecipients).HasMaxLength(500);
            builder.Property(b => b.IsEnabled).HasDefaultValue(true);
            builder.Property(b => b.MaxRetries).HasDefaultValue(3);
            builder.Property(b => b.RetryCount).HasDefaultValue(0);

            builder.HasIndex(b => b.JobName);
            builder.HasIndex(b => new { b.Status, b.IsEnabled });
            builder.HasIndex(b => b.NextExecutionTime);
        }
    }
}
