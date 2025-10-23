using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class ClientConfiguration : IEntityTypeConfiguration<Client>
    {
        public void Configure(EntityTypeBuilder<Client> builder)
        {
            builder.ToTable("Clients");

            builder.HasKey(c => c.Id);

            builder.Property(c => c.TaxId).IsRequired().HasMaxLength(14);
            builder.Property(c => c.Name).IsRequired().HasMaxLength(60);
            builder.Property(c => c.PersonType).IsRequired().HasMaxLength(1);
            builder.Property(c => c.Email).HasMaxLength(100);
            builder.Property(c => c.PhoneNumber).HasMaxLength(20);

            builder.HasIndex(c => c.TaxId).IsUnique();
        }
    }
}
