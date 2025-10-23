using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class ProductConfiguration : IEntityTypeConfiguration<Product>
    {
        public void Configure(EntityTypeBuilder<Product> builder)
        {
            builder.ToTable("Products");

            builder.HasKey(p => p.ProductCode);

            builder.Property(p => p.CompanyCode).IsRequired();
            builder.Property(p => p.ProductName).IsRequired().HasMaxLength(100);
            builder.Property(p => p.LineOfBusiness).IsRequired();
            builder.Property(p => p.LineOfBusinessGroup).IsRequired();
            builder.Property(p => p.SusepProcessNumber).HasMaxLength(20);
            builder.Property(p => p.ProductType).HasMaxLength(1);
            builder.Property(p => p.ProductStatus).HasMaxLength(1).HasDefaultValue("A");
            builder.Property(p => p.ProductModality).IsRequired();
            builder.Property(p => p.IsLifeInsurance).HasMaxLength(1).HasDefaultValue("N");
            builder.Property(p => p.CommissionPercentage).HasColumnType("decimal(6,2)");

            builder.HasIndex(p => new { p.CompanyCode, p.LineOfBusiness })
                .HasDatabaseName("IX_Products_LineOfBusiness");
            builder.HasIndex(p => p.ProductStatus)
                .HasDatabaseName("IX_Products_Status");
        }
    }
}
