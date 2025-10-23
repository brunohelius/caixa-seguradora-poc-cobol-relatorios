using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class AgencyConfiguration : IEntityTypeConfiguration<Agency>
    {
        public void Configure(EntityTypeBuilder<Agency> builder)
        {
            builder.ToTable("Agencies");

            builder.HasKey(a => a.AgencyCode);

            builder.Property(a => a.AgencyName).IsRequired().HasMaxLength(60);
            builder.Property(a => a.RegionalCode).IsRequired();
            builder.Property(a => a.RegionalName).HasMaxLength(50);
            builder.Property(a => a.Status).HasMaxLength(1).HasDefaultValue("A");
        }
    }
}
