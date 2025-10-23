using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class ProducerConfiguration : IEntityTypeConfiguration<Producer>
    {
        public void Configure(EntityTypeBuilder<Producer> builder)
        {
            builder.ToTable("Producers");

            builder.HasKey(p => p.ProducerCode);

            builder.Property(p => p.ProducerName).IsRequired().HasMaxLength(60);
            builder.Property(p => p.TaxId).HasMaxLength(11);
            builder.Property(p => p.DefaultCommissionPercentage).HasColumnType("decimal(5,2)");
            builder.Property(p => p.Status).HasMaxLength(1).HasDefaultValue("A");

            builder.HasOne(p => p.Agency)
                .WithMany(a => a.Producers)
                .HasForeignKey(p => p.AgencyId)
                .OnDelete(DeleteBehavior.SetNull);
        }
    }
}
