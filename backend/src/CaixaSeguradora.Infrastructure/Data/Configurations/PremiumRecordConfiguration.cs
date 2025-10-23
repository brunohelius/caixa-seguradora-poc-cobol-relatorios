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

            builder.HasKey(p => p.PremiumId);

            // Main identifiers
            builder.Property(p => p.PolicyNumber).IsRequired();
            builder.Property(p => p.EndorsementNumber).IsRequired();
            builder.Property(p => p.MovementType).HasMaxLength(1);

            // Company and product classification
            builder.Property(p => p.CompanyCode).IsRequired();
            builder.Property(p => p.LineOfBusiness).IsRequired();
            builder.Property(p => p.ProductModality).IsRequired();
            builder.Property(p => p.OperationType).IsRequired();
            builder.Property(p => p.BusinessOperationType).IsRequired();
            builder.Property(p => p.ClientCode).IsRequired();

            // Financial fields - CRITICAL: decimal for precision (using actual COBOL properties)
            builder.Property(p => p.ExchangeRate).HasColumnType("decimal(15,9)");

            // Item totals
            builder.Property(p => p.InsuredAmountItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.BasePremiumItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.FixedPremiumItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.TariffPremiumItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.DiscountItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.NetPremiumItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AdditionalFractionalItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.IssuanceCostItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.IofItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.TotalPremiumItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.CommissionItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AdministrationFeeItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AgencyCommissionItem).HasColumnType("decimal(15,5)");
            builder.Property(p => p.PreferentialCommissionItem).HasColumnType("decimal(15,5)");

            // Net totals
            builder.Property(p => p.InsuredAmountNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.BasePremiumNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.FixedPremiumNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.TariffPremiumNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.DiscountNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.NetPremiumNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AdditionalFractionalNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.IssuanceCostNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.IofNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.TotalPremiumNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.CommissionNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AdministrationFeeNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AgencyCommissionNet).HasColumnType("decimal(15,5)");
            builder.Property(p => p.PreferentialCommissionNet).HasColumnType("decimal(15,5)");

            // Cossurance totals
            builder.Property(p => p.InsuredAmountCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.BasePremiumCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.FixedPremiumCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.TariffPremiumCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.DiscountCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.NetPremiumCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AdditionalFractionalCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.CommissionCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AdministrationFeeCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AgencyCommissionCossurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.PreferentialCommissionCossurance).HasColumnType("decimal(15,5)");

            // Reinsurance totals
            builder.Property(p => p.InsuredAmountReinsurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.TariffPremiumReinsurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.DiscountReinsurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.NetPremiumReinsurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.AdditionalFractionalReinsurance).HasColumnType("decimal(15,5)");
            builder.Property(p => p.CommissionReinsurance).HasColumnType("decimal(15,5)");

            // Grand totals
            builder.Property(p => p.InsuredAmountTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.BasePremiumTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.FixedPremiumTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.TariffPremiumTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.DiscountTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.NetPremiumTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.AdditionalFractionalTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.IssuanceCostTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.IofTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.TotalPremiumTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.CommissionTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.AdministrationFeeTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.AgencyCommissionTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.PreferentialCommissionTotal).HasColumnType("decimal(15,2)");

            // Local currency totals
            builder.Property(p => p.InsuredAmountLocalTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.BasePremiumLocalTotal).HasColumnType("decimal(15,2)");
            builder.Property(p => p.FixedPremiumLocalTotal).HasColumnType("decimal(15,2)");

            // Indexes for performance
            builder.HasIndex(p => p.PolicyNumber);
            builder.HasIndex(p => new { p.PolicyNumber, p.EndorsementNumber });
            builder.HasIndex(p => new { p.CompanyCode, p.ReferenceYear, p.ReferenceMonth, p.ReferenceDay })
                .HasDatabaseName("IX_PremiumRecords_DateRange");

            // Relationship with Policy (optional)
            builder.HasOne(p => p.Policy)
                .WithMany()
                .HasForeignKey(p => p.PolicyNumber)
                .HasPrincipalKey(p => p.PolicyNumber)
                .OnDelete(DeleteBehavior.SetNull);
        }
    }
}
