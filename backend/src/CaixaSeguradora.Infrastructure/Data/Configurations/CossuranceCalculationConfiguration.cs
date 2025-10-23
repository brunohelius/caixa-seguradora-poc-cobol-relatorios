using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class CossuranceCalculationConfiguration : IEntityTypeConfiguration<CossuranceCalculation>
    {
        public void Configure(EntityTypeBuilder<CossuranceCalculation> builder)
        {
            builder.ToTable("CossuranceCalculations");

            builder.HasKey(c => c.Id);

            builder.Property(c => c.TotalGrossPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.TotalNetPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.TotalCommission).HasColumnType("decimal(15,2)");
            builder.Property(c => c.TotalIOF).HasColumnType("decimal(15,2)");
            builder.Property(c => c.CossurerPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.CossurerCommission).HasColumnType("decimal(15,2)");

            builder.HasOne(c => c.CossuredPolicy)
                .WithMany()
                .HasForeignKey(c => c.CossuredPolicyId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
