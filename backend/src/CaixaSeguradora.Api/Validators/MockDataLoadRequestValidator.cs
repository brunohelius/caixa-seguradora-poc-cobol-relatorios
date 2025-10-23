using CaixaSeguradora.Core.DTOs;
using FluentValidation;

namespace CaixaSeguradora.Api.Validators;

/// <summary>
/// Validator for MockDataLoadRequest.
/// Validates mock data loading parameters including file format and entity types.
/// </summary>
public class MockDataLoadRequestValidator : AbstractValidator<MockDataLoadRequest>
{
    private static readonly string[] ValidEntityTypes =
    {
        "premiums", "policies", "clients", "products", "endorsements",
        "coverages", "addresses", "agencies", "producers", "cossurance"
    };

    private const long MaxFileSizeBytes = 50 * 1024 * 1024; // 50 MB

    public MockDataLoadRequestValidator()
    {
        RuleFor(x => x.EntityType)
            .NotEmpty()
            .WithMessage("Tipo de entidade é obrigatório")
            .MaximumLength(100)
            .WithMessage("Tipo de entidade não pode exceder 100 caracteres")
            .Must(entityType => ValidEntityTypes.Contains(entityType.ToLower()))
            .WithMessage($"Tipo de entidade deve ser um dos seguintes: {string.Join(", ", ValidEntityTypes)}");

        RuleFor(x => x.Format)
            .IsInEnum()
            .WithMessage("Formato de dados deve ser CSV ou JSON");

        // Validate that at least one data source is provided
        RuleFor(x => x)
            .Must(x => !string.IsNullOrEmpty(x.FileContentBase64) || !string.IsNullOrEmpty(x.RawDataContent))
            .WithMessage("Arquivo ou conteúdo de dados deve ser fornecido");

        // Validate file content base64 size (rough estimate: base64 is ~33% larger than binary)
        RuleFor(x => x.FileContentBase64)
            .Must(base64 => string.IsNullOrEmpty(base64) || EstimateBase64Size(base64) <= MaxFileSizeBytes)
            .WithMessage($"Tamanho do arquivo não pode exceder {MaxFileSizeBytes / (1024 * 1024)} MB")
            .When(x => !string.IsNullOrEmpty(x.FileContentBase64));

        // Validate raw data content size
        RuleFor(x => x.RawDataContent)
            .Must(content => string.IsNullOrEmpty(content) ||
                           System.Text.Encoding.UTF8.GetByteCount(content) <= MaxFileSizeBytes)
            .WithMessage($"Tamanho do conteúdo não pode exceder {MaxFileSizeBytes / (1024 * 1024)} MB")
            .When(x => !string.IsNullOrEmpty(x.RawDataContent));

        // CSV delimiter validation
        RuleFor(x => x.CsvDelimiter)
            .Must(delimiter => delimiter == ',' || delimiter == ';' || delimiter == '\t' || delimiter == '|')
            .WithMessage("Delimitador CSV deve ser vírgula (,), ponto e vírgula (;), tabulação (\\t) ou barra vertical (|)")
            .When(x => x.Format == DataFormat.Csv);

        // Max records validation
        RuleFor(x => x.MaxRecords)
            .GreaterThanOrEqualTo(0)
            .WithMessage("Número máximo de registros deve ser maior ou igual a zero")
            .LessThanOrEqualTo(100000)
            .WithMessage("Número máximo de registros não pode exceder 100.000");
    }

    /// <summary>
    /// Estimates the decoded size of a base64-encoded string.
    /// Base64 encoding increases size by approximately 33% (4 bytes for every 3 bytes of data).
    /// </summary>
    private static long EstimateBase64Size(string base64String)
    {
        if (string.IsNullOrEmpty(base64String))
            return 0;

        // Remove padding characters to get accurate estimate
        int paddingChars = 0;
        if (base64String.EndsWith("=="))
            paddingChars = 2;
        else if (base64String.EndsWith("="))
            paddingChars = 1;

        // Calculate decoded size: (length * 3) / 4 - padding
        long estimatedSize = ((base64String.Length * 3) / 4) - paddingChars;
        return estimatedSize;
    }
}
