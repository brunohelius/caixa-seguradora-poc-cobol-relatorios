using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class PremiumRecordConfiguration : IEntityTypeConfiguration<PremiumRecord>
    {
        public void Configure(EntityTypeBuilder<PremiumRecord> builder)
        {
            builder.ToTable("PremiumRecords");

            builder.HasKey(p => p.Id);

            // Main identifiers
            builder.Property(p => p.PolicyNumber).IsRequired();
            builder.Property(p => p.EndorsementNumber).IsRequired();
            builder.Property(p => p.SystemCode).IsRequired().HasMaxLength(2);

            // Financial fields - CRITICAL: decimal for precision
            builder.Property(p => p.GrossPremium).HasColumnType("decimal(15,2)");
            builder.Property(p => p.NetPremium).HasColumnType("decimal(15,2)");
            builder.Property(p => p.CommissionAmount).HasColumnType("decimal(15,2)");
            builder.Property(p => p.IOFAmount).HasColumnType("decimal(15,2)");
            builder.Property(p => p.InstallmentFee).HasColumnType("decimal(15,2)");
            builder.Property(p => p.PolicyCost).HasColumnType("decimal(15,2)");
            builder.Property(p => p.InsuredAmount).HasColumnType("decimal(15,2)");
            builder.Property(p => p.InstallmentAmount).HasColumnType("decimal(15,2)");
            builder.Property(p => p.ProducerCommissionPercentage).HasColumnType("decimal(5,2)");
            builder.Property(p => p.CossurancePercentage).HasColumnType("decimal(5,2)");
            builder.Property(p => p.CossurancePremium).HasColumnType("decimal(15,2)");

            // String fields with max lengths
            builder.Property(p => p.InsuredName).HasMaxLength(60);
            builder.Property(p => p.InsuredTaxId).HasMaxLength(14);
            builder.Property(p => p.InsuredPersonType).HasMaxLength(1);
            builder.Property(p => p.ProductName).HasMaxLength(50);
            builder.Property(p => p.AgencyName).HasMaxLength(60);
            builder.Property(p => p.ProducerName).HasMaxLength(60);
            builder.Property(p => p.CossuranceIndicator).HasMaxLength(1).HasDefaultValue("N");
            builder.Property(p => p.PolicyStatus).HasMaxLength(2);
            builder.Property(p => p.CurrencyCode).HasMaxLength(3).HasDefaultValue("BRL");
            builder.Property(p => p.CalculationType).HasMaxLength(1);
            builder.Property(p => p.PostalCode).HasMaxLength(8);
            builder.Property(p => p.Street).HasMaxLength(100);
            builder.Property(p => p.AddressNumber).HasMaxLength(10);
            builder.Property(p => p.City).HasMaxLength(50);
            builder.Property(p => p.State).HasMaxLength(2);
            builder.Property(p => p.CreatedBy).HasMaxLength(10);
            builder.Property(p => p.UpdatedBy).HasMaxLength(10);
            builder.Property(p => p.RecordChecksum).HasMaxLength(32);

            // Indexes for performance
            builder.HasIndex(p => p.PolicyNumber);
            builder.HasIndex(p => new { p.PolicyNumber, p.EndorsementNumber });
            builder.HasIndex(p => p.SystemCode);
            builder.HasIndex(p => p.PolicyStartDate);

            // Relationship with Policy (optional)
            builder.HasOne(p => p.Policy)
                .WithMany()
                .HasForeignKey(p => p.PolicyId)
                .OnDelete(DeleteBehavior.SetNull);
        }
    }
}
