using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    public class EndorsementConfiguration : IEntityTypeConfiguration<Endorsement>
    {
        public void Configure(EntityTypeBuilder<Endorsement> builder)
        {
            builder.ToTable("Endorsements");

            builder.HasKey(e => e.Id);

            builder.Property(e => e.EndorsementNumber).IsRequired();
            builder.Property(e => e.EndorsementType).IsRequired().HasMaxLength(2);
            builder.Property(e => e.PremiumAmount).HasColumnType("decimal(15,2)");
            builder.Property(e => e.Reason).HasMaxLength(200);
            builder.Property(e => e.Status).HasMaxLength(1).HasDefaultValue("A");

            builder.HasIndex(e => new { e.PolicyId, e.EndorsementNumber });

            builder.HasOne(e => e.Policy)
                .WithMany(p => p.Endorsements)
                .HasForeignKey(e => e.PolicyId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
