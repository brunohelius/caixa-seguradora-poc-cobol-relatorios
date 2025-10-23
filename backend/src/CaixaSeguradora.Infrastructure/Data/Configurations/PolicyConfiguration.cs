using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class PolicyConfiguration : IEntityTypeConfiguration<Policy>
    {
        public void Configure(EntityTypeBuilder<Policy> builder)
        {
            builder.ToTable("Policies");

            builder.HasKey(p => p.PolicyNumber);

            builder.Property(p => p.SystemCode).IsRequired().HasMaxLength(2);
            builder.Property(p => p.ProductCode).IsRequired();
            builder.Property(p => p.TotalPremium).HasColumnType("decimal(15,2)").IsRequired();
            builder.Property(p => p.NetPremium).HasColumnType("decimal(15,2)").IsRequired();
            builder.Property(p => p.PolicyStatus).HasMaxLength(1);

            builder.HasIndex(p => new { p.PolicyNumber, p.EndorsementNumber });

            // Relationships using COBOL business keys
            builder.HasOne(p => p.Client)
                .WithMany(c => c.Policies)
                .HasForeignKey(p => p.ClientCode)
                .HasPrincipalKey(c => c.ClientCode)
                .OnDelete(DeleteBehavior.Restrict);

            builder.HasOne(p => p.Agency)
                .WithMany(a => a.Policies)
                .HasForeignKey(p => p.AgencyCode)
                .HasPrincipalKey(a => a.AgencyCode)
                .OnDelete(DeleteBehavior.Restrict);

            builder.HasOne(p => p.Producer)
                .WithMany(pr => pr.Policies)
                .HasForeignKey(p => p.ProducerCode)
                .HasPrincipalKey(pr => pr.ProducerCode)
                .OnDelete(DeleteBehavior.Restrict);

            builder.HasOne(p => p.Product)
                .WithMany()
                .HasForeignKey(p => p.ProductCode)
                .HasPrincipalKey(pr => pr.ProductCode)
                .OnDelete(DeleteBehavior.Restrict);
        }
    }
}
