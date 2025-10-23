using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class InvoiceConfiguration : IEntityTypeConfiguration<Invoice>
    {
        public void Configure(EntityTypeBuilder<Invoice> builder)
        {
            builder.ToTable("Invoices");

            builder.HasKey(i => i.Id);

            builder.Property(i => i.InvoiceNumber).IsRequired();
            builder.Property(i => i.TotalAmount).HasColumnType("decimal(15,2)");
            builder.Property(i => i.PaidAmount).HasColumnType("decimal(15,2)");
            builder.Property(i => i.Status).HasMaxLength(2).HasDefaultValue("PE");
            builder.Property(i => i.NumberOfInstallments).IsRequired();

            builder.HasIndex(i => i.InvoiceNumber).IsUnique();

            builder.HasOne(i => i.Policy)
                .WithMany(p => p.Invoices)
                .HasForeignKey(i => i.PolicyNumber)
                .HasPrincipalKey(p => p.PolicyNumber)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
