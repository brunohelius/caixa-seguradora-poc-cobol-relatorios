using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class CoverageConfiguration : IEntityTypeConfiguration<Coverage>
    {
        public void Configure(EntityTypeBuilder<Coverage> builder)
        {
            builder.ToTable("Coverages");

            builder.HasKey(c => c.Id);

            builder.Property(c => c.CoverageCode).IsRequired();
            builder.Property(c => c.CoverageName).IsRequired().HasMaxLength(50);
            builder.Property(c => c.InsuredAmount).HasColumnType("decimal(15,2)");
            builder.Property(c => c.PremiumAmount).HasColumnType("decimal(15,2)");
            builder.Property(c => c.DeductiblePercentage).HasColumnType("decimal(5,2)");
            builder.Property(c => c.Status).HasMaxLength(1).HasDefaultValue("A");

            builder.HasOne(c => c.Policy)
                .WithMany(p => p.Coverages)
                .HasForeignKey(c => c.PolicyNumber)
                .HasPrincipalKey(p => p.PolicyNumber)
                .OnDelete(DeleteBehavior.Cascade);

            builder.HasOne(c => c.Product)
                .WithMany(p => p.Coverages)
                .HasForeignKey(c => c.ProductCode)
                .HasPrincipalKey(p => p.ProductCode)
                .OnDelete(DeleteBehavior.Restrict);
        }
    }
}
