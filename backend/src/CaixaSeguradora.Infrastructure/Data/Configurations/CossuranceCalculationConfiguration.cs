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

            builder.HasKey(c => c.CalculationId);

            builder.Property(c => c.PolicyNumber).IsRequired();
            builder.Property(c => c.CossuranceCode).IsRequired();
            builder.Property(c => c.QuotaPercentage).HasColumnType("decimal(13,9)");
            builder.Property(c => c.RetainedPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.CededPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.CededCommission).HasColumnType("decimal(15,2)");
            builder.Property(c => c.TotalGrossPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.TotalNetPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.TotalIOF).HasColumnType("decimal(15,2)");

            builder.HasIndex(c => new { c.PolicyNumber, c.CossuranceCode })
                .HasDatabaseName("IX_CossuranceCalculations_PolicyCossurance");

            builder.HasOne(c => c.Policy)
                .WithMany()
                .HasForeignKey(c => c.PolicyNumber)
                .HasPrincipalKey(p => p.PolicyNumber)
                .OnDelete(DeleteBehavior.Restrict);

            builder.HasOne(c => c.CossuredPolicy)
                .WithMany()
                .HasForeignKey(c => c.CossuredPolicyId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
