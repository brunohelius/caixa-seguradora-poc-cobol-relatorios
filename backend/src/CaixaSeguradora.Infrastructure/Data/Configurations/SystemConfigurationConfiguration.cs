using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class SystemConfigurationConfiguration : IEntityTypeConfiguration<SystemConfiguration>
    {
        public void Configure(EntityTypeBuilder<SystemConfiguration> builder)
        {
            builder.ToTable("SystemConfigurations");

            builder.HasKey(s => s.Id);

            builder.Property(s => s.ConfigKey).IsRequired().HasMaxLength(50);
            builder.Property(s => s.ConfigValue).IsRequired().HasMaxLength(500);
            builder.Property(s => s.Description).HasMaxLength(200);
            builder.Property(s => s.Category).HasMaxLength(50);

            builder.HasIndex(s => s.ConfigKey).IsUnique();
        }
    }
}
