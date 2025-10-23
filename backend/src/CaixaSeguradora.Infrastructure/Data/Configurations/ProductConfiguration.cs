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

            builder.HasKey(p => p.Id);

            builder.Property(p => p.ProductCode).IsRequired();
            builder.Property(p => p.ProductName).IsRequired().HasMaxLength(50);
            builder.Property(p => p.Description).HasMaxLength(200);
            builder.Property(p => p.ProductType).HasMaxLength(20);
            builder.Property(p => p.Status).HasMaxLength(1).HasDefaultValue("A");
            builder.Property(p => p.CommissionPercentage).HasColumnType("decimal(5,2)");

            builder.HasIndex(p => p.ProductCode).IsUnique();
        }
    }
}
