using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class ReportDefinitionConfiguration : IEntityTypeConfiguration<ReportDefinition>
    {
        public void Configure(EntityTypeBuilder<ReportDefinition> builder)
        {
            builder.ToTable("ReportDefinitions");

            builder.HasKey(r => r.Id);

            builder.Property(r => r.ReportCode).IsRequired().HasMaxLength(10);
            builder.Property(r => r.ReportName).IsRequired().HasMaxLength(100);
            builder.Property(r => r.Description).HasMaxLength(200);
            builder.Property(r => r.OutputFormat).HasMaxLength(10).HasDefaultValue("TXT");
            builder.Property(r => r.RecordLength).IsRequired();
            builder.Property(r => r.IsActive).HasDefaultValue(true);

            builder.HasIndex(r => r.ReportCode).IsUnique();
        }
    }
}
