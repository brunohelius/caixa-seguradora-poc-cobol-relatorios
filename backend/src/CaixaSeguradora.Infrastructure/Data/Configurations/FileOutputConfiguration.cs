using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data.Configurations
{
    /// <summary>
    /// Entity Framework configuration for FileOutput entity.
    /// </summary>
    public class FileOutputConfiguration : IEntityTypeConfiguration<FileOutput>
    {
        public void Configure(EntityTypeBuilder<FileOutput> builder)
        {
            // Table mapping
            builder.ToTable("FILE_OUTPUTS");

            // Primary key
            builder.HasKey(f => f.FileId);

            // Indexes for query performance
            builder.HasIndex(f => f.ExecutionId)
                .HasDatabaseName("IX_FileOutput_ExecutionId");

            builder.HasIndex(f => f.FileType)
                .HasDatabaseName("IX_FileOutput_FileType");

            builder.HasIndex(f => f.GeneratedAt)
                .HasDatabaseName("IX_FileOutput_GeneratedAt");

            builder.HasIndex(f => f.FileName)
                .HasDatabaseName("IX_FileOutput_FileName");

            // Composite index for execution + file type
            builder.HasIndex(f => new { f.ExecutionId, f.FileType })
                .HasDatabaseName("IX_FileOutput_Execution_FileType");

            // Property configurations
            builder.Property(f => f.FileId)
                .IsRequired()
                .ValueGeneratedNever(); // Guid will be generated in code

            builder.Property(f => f.ExecutionId)
                .IsRequired();

            builder.Property(f => f.FileName)
                .IsRequired()
                .HasMaxLength(100);

            builder.Property(f => f.FileType)
                .IsRequired()
                .HasMaxLength(20);

            builder.Property(f => f.FilePath)
                .IsRequired()
                .HasColumnType("TEXT");

            builder.Property(f => f.FileSizeBytes)
                .IsRequired();

            builder.Property(f => f.RecordCount)
                .IsRequired();

            builder.Property(f => f.GeneratedAt)
                .IsRequired();

            builder.Property(f => f.Checksum)
                .HasMaxLength(64); // SHA256 hash length

            builder.Property(f => f.DownloadCount)
                .HasDefaultValue(0);

            // Foreign key relationship to ReportExecution
            builder.HasOne(f => f.Execution)
                .WithMany(e => e.FileOutputs)
                .HasForeignKey(f => f.ExecutionId)
                .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
