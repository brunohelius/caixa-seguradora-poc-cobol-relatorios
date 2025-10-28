using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    /// <summary>
    /// Entity Framework configuration for ReinsuranceData entity.
    /// </summary>
    public class ReinsuranceDataConfiguration : IEntityTypeConfiguration<ReinsuranceData>
    {
        public void Configure(EntityTypeBuilder<ReinsuranceData> builder)
        {
            // Table mapping
            builder.ToTable("REINSURANCE_DATA");

            // Primary key
            builder.HasKey(r => r.ReinsuranceId);

            // Indexes for query performance
            builder.HasIndex(r => r.PolicyNumber)
                .HasDatabaseName("IX_ReinsuranceData_PolicyNumber");

            builder.HasIndex(r => r.EffectiveDate)
                .HasDatabaseName("IX_ReinsuranceData_EffectiveDate");

            builder.HasIndex(r => r.TreatyCode)
                .HasDatabaseName("IX_ReinsuranceData_TreatyCode");

            builder.HasIndex(r => r.ReturnCode)
                .HasDatabaseName("IX_ReinsuranceData_ReturnCode");

            // Composite index for policy + effective date lookups
            builder.HasIndex(r => new { r.PolicyNumber, r.EffectiveDate })
                .HasDatabaseName("IX_ReinsuranceData_Policy_EffectiveDate");

            // Property configurations
            builder.Property(r => r.PolicyNumber)
                .IsRequired()
                .HasColumnType("BIGINT");

            builder.Property(r => r.EffectiveDate)
                .IsRequired();

            builder.Property(r => r.PremiumAmount)
                .IsRequired()
                .HasColumnType("DECIMAL(15,2)")
                .HasPrecision(15, 2);

            builder.Property(r => r.ReinsuredAmount)
                .HasColumnType("DECIMAL(15,2)")
                .HasPrecision(15, 2);

            builder.Property(r => r.ReinsurancePercentage)
                .HasColumnType("DECIMAL(5,2)")
                .HasPrecision(5, 2);

            builder.Property(r => r.TreatyCode)
                .HasMaxLength(10);

            builder.Property(r => r.ReturnCode)
                .IsRequired()
                .HasMaxLength(2);

            builder.Property(r => r.ErrorMessage)
                .HasMaxLength(100);

            // Computed column - ignored by EF (calculated in entity)
            builder.Ignore(r => r.RetainedAmount);
            builder.Ignore(r => r.IsSuccessful);
            builder.Ignore(r => r.HasReinsurance);

            // Foreign key relationship to Policy
            builder.HasOne(r => r.Policy)
                .WithMany()
                .HasForeignKey(r => r.PolicyNumber)
                .OnDelete(DeleteBehavior.Restrict);

            // Audit fields (inherited from AuditableEntity)
            builder.Property(r => r.CreatedAt)
                .IsRequired();

            builder.Property(r => r.CreatedBy)
                .HasMaxLength(100);

            builder.Property(r => r.UpdatedBy)
                .HasMaxLength(100);
        }
    }
}
