using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class CossuredPolicyConfiguration : IEntityTypeConfiguration<CossuredPolicy>
    {
        public void Configure(EntityTypeBuilder<CossuredPolicy> builder)
        {
            builder.ToTable("CossuredPolicies");

            builder.HasKey(c => c.CossuranceId);

            builder.Property(c => c.PolicyNumber).IsRequired();
            builder.Property(c => c.CossuranceCode).IsRequired();
            builder.Property(c => c.CossurerCode).IsRequired();
            builder.Property(c => c.CossurerName).IsRequired().HasMaxLength(100);
            builder.Property(c => c.CossuranceType).HasMaxLength(1).IsRequired();
            builder.Property(c => c.CedingCompanyCode).IsRequired();
            builder.Property(c => c.AcquiringCompanyCode).IsRequired();
            builder.Property(c => c.PercentageShare).HasColumnType("decimal(13,9)");
            builder.Property(c => c.CededInsuredAmount).HasColumnType("decimal(15,2)");
            builder.Property(c => c.CededPremium).HasColumnType("decimal(15,2)");
            builder.Property(c => c.IsLeader).HasMaxLength(1).HasDefaultValue("N");
            builder.Property(c => c.Status).HasMaxLength(1).HasDefaultValue("A");

            builder.HasIndex(c => new { c.PolicyNumber, c.CossuranceCode })
                .HasDatabaseName("IX_CossuredPolicies_PolicyCossurance");

            builder.HasOne(c => c.Policy)
                .WithMany(p => p.CossuredPolicies)
                .HasForeignKey(c => c.PolicyNumber)
                .HasPrincipalKey(p => p.PolicyNumber)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
