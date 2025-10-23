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

            builder.HasKey(c => c.Id);

            builder.Property(c => c.PolicyNumber).IsRequired();
            builder.Property(c => c.CossurerCode).IsRequired();
            builder.Property(c => c.CossurerName).IsRequired().HasMaxLength(60);
            builder.Property(c => c.ParticipationPercentage).HasColumnType("decimal(5,2)");
            builder.Property(c => c.CossuranceType).HasMaxLength(1).IsRequired();
            builder.Property(c => c.Status).HasMaxLength(1).HasDefaultValue("A");

            builder.HasOne(c => c.Policy)
                .WithMany()
                .HasForeignKey(c => c.PolicyId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
