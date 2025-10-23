using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class AddressConfiguration : IEntityTypeConfiguration<Address>
    {
        public void Configure(EntityTypeBuilder<Address> builder)
        {
            builder.ToTable("Addresses");

            builder.HasKey(a => a.Id);

            builder.Property(a => a.PostalCode).IsRequired().HasMaxLength(8);
            builder.Property(a => a.Street).IsRequired().HasMaxLength(100);
            builder.Property(a => a.Number).HasMaxLength(10);
            builder.Property(a => a.Complement).HasMaxLength(50);
            builder.Property(a => a.Neighborhood).HasMaxLength(50);
            builder.Property(a => a.City).IsRequired().HasMaxLength(50);
            builder.Property(a => a.State).IsRequired().HasMaxLength(2);
            builder.Property(a => a.Country).HasMaxLength(50).HasDefaultValue("Brasil");

            builder.HasOne(a => a.Client)
                .WithMany(c => c.Addresses)
                .HasForeignKey(a => a.ClientId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
