using System;
using System.Text;
using System.Text.RegularExpressions;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Implementação do serviço de formatação (equivalente ao módulo COBOL GE0009S).
/// Formata e valida documentos brasileiros (CPF/CNPJ) e datas.
/// </summary>
public class FormattingService : IFormattingService
{
    private readonly ILogger<FormattingService> _logger;

    // Regex para remover caracteres não numéricos
    private static readonly Regex NonDigitRegex = new(@"\D", RegexOptions.Compiled);

    public FormattingService(ILogger<FormattingService> logger)
    {
        _logger = logger;
    }

    /// <inheritdoc/>
    public string FormatCpf(string cpf)
    {
        if (string.IsNullOrWhiteSpace(cpf))
        {
            _logger.LogWarning("Tentativa de formatar CPF vazio ou nulo");
            return string.Empty;
        }

        // Remove formatação existente
        var cleaned = RemoveFormatting(cpf);

        if (cleaned.Length != 11)
        {
            _logger.LogWarning("CPF inválido: tamanho incorreto. Esperado 11 dígitos, recebido {Length}", cleaned.Length);
            return cpf; // Retorna original se inválido
        }

        // Formata: XXX.XXX.XXX-XX
        return $"{cleaned.Substring(0, 3)}.{cleaned.Substring(3, 3)}.{cleaned.Substring(6, 3)}-{cleaned.Substring(9, 2)}";
    }

    /// <inheritdoc/>
    public string FormatCnpj(string cnpj)
    {
        if (string.IsNullOrWhiteSpace(cnpj))
        {
            _logger.LogWarning("Tentativa de formatar CNPJ vazio ou nulo");
            return string.Empty;
        }

        // Remove formatação existente
        var cleaned = RemoveFormatting(cnpj);

        if (cleaned.Length != 14)
        {
            _logger.LogWarning("CNPJ inválido: tamanho incorreto. Esperado 14 dígitos, recebido {Length}", cleaned.Length);
            return cnpj; // Retorna original se inválido
        }

        // Formata: XX.XXX.XXX/XXXX-XX
        return $"{cleaned.Substring(0, 2)}.{cleaned.Substring(2, 3)}.{cleaned.Substring(5, 3)}/{cleaned.Substring(8, 4)}-{cleaned.Substring(12, 2)}";
    }

    /// <inheritdoc/>
    public string FormatDate(DateTime date, string format = "dd/MM/yyyy")
    {
        return date.ToString(format);
    }

    /// <inheritdoc/>
    public bool ValidateCpfCheckDigit(string cpf)
    {
        if (string.IsNullOrWhiteSpace(cpf))
            return false;

        var cleaned = RemoveFormatting(cpf);

        if (cleaned.Length != 11)
            return false;

        Span<int> digits = stackalloc int[11];
        for (int i = 0; i < cleaned.Length; i++)
        {
            char c = cleaned[i];
            if (!char.IsDigit(c))
            {
                return false;
            }

            digits[i] = c - '0';
        }

        if (IsRepeatedSequence(digits))
            return false;

        var firstCheckDigit = CalculateCpfCheckDigit(digits, 9);

        if (digits[9] != firstCheckDigit)
            return false;

        var secondCheckDigit = CalculateCpfCheckDigit(digits, 10);

        var isValid = digits[10] == secondCheckDigit;

        if (!isValid)
        {
            _logger.LogDebug("CPF {Cpf} falhou na validação de dígitos verificadores", cpf);
        }

        return isValid;
    }

    /// <inheritdoc/>
    public bool ValidateCnpjCheckDigit(string cnpj)
    {
        if (string.IsNullOrWhiteSpace(cnpj))
            return false;

        var cleaned = RemoveFormatting(cnpj);

        if (cleaned.Length != 14)
            return false;

        // CNPJs inválidos conhecidos (todos dígitos iguais)
        if (cleaned == new string(cleaned[0], 14))
            return false;

        // Calcula primeiro dígito verificador
        var multipliers1 = new[] { 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2 };
        var sum = 0;
        for (int i = 0; i < 12; i++)
        {
            sum += int.Parse(cleaned[i].ToString()) * multipliers1[i];
        }
        var firstCheckDigit = sum % 11 < 2 ? 0 : 11 - (sum % 11);

        if (int.Parse(cleaned[12].ToString()) != firstCheckDigit)
            return false;

        // Calcula segundo dígito verificador
        var multipliers2 = new[] { 6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2 };
        sum = 0;
        for (int i = 0; i < 13; i++)
        {
            sum += int.Parse(cleaned[i].ToString()) * multipliers2[i];
        }
        var secondCheckDigit = sum % 11 < 2 ? 0 : 11 - (sum % 11);

        var isValid = int.Parse(cleaned[13].ToString()) == secondCheckDigit;

        if (!isValid)
        {
            _logger.LogDebug("CNPJ {Cnpj} falhou na validação de dígitos verificadores", cnpj);
        }

        return isValid;
    }

    /// <inheritdoc/>
    public string RemoveFormatting(string document)
    {
        if (string.IsNullOrWhiteSpace(document))
            return string.Empty;

        return NonDigitRegex.Replace(document, string.Empty);
    }

    private static bool IsRepeatedSequence(ReadOnlySpan<int> digits)
    {
        for (int i = 1; i < digits.Length; i++)
        {
            if (digits[i] != digits[0])
            {
                return false;
            }
        }

        return true;
    }

    private static int CalculateCpfCheckDigit(ReadOnlySpan<int> digits, int length)
    {
        var sum = 0;
        var weight = length + 1;

        for (int i = 0; i < length; i++)
        {
            sum += digits[i] * (weight - i);
        }

        var remainder = sum % 11;
        return remainder < 2 ? 0 : 11 - remainder;
    }
}
