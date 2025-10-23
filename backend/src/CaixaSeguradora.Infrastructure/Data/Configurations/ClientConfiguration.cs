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

            builder.HasKey(c => c.ClientCode);

            builder.Property(c => c.DocumentNumber).IsRequired().HasMaxLength(14);
            builder.Property(c => c.ClientName).IsRequired().HasMaxLength(100);
            builder.Property(c => c.ClientType).IsRequired().HasMaxLength(1);
            builder.Property(c => c.Email).HasMaxLength(50);
            builder.Property(c => c.PhoneNumber).HasMaxLength(20);
            builder.Property(c => c.IdentityDocument).HasMaxLength(20);
            builder.Property(c => c.BirthDate).HasMaxLength(10);
            builder.Property(c => c.Gender).HasMaxLength(1);
            builder.Property(c => c.ClientStatus).HasMaxLength(1).HasDefaultValue("A");

            builder.HasIndex(c => c.DocumentNumber).IsUnique();
        }
    }
}
