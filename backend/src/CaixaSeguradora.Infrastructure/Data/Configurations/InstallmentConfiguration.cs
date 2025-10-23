using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class InstallmentConfiguration : IEntityTypeConfiguration<Installment>
    {
        public void Configure(EntityTypeBuilder<Installment> builder)
        {
            builder.ToTable("Installments");

            builder.HasKey(i => i.Id);

            builder.Property(i => i.InstallmentNumber).IsRequired();
            builder.Property(i => i.InstallmentAmount).HasColumnType("decimal(15,2)");
            builder.Property(i => i.PaidAmount).HasColumnType("decimal(15,2)");
            builder.Property(i => i.Status).HasMaxLength(2).HasDefaultValue("PE");
            builder.Property(i => i.BarcodeNumber).HasMaxLength(47);

            builder.HasIndex(i => new { i.InvoiceId, i.InstallmentNumber });

            builder.HasOne(i => i.Invoice)
                .WithMany(inv => inv.Installments)
                .HasForeignKey(i => i.InvoiceId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
