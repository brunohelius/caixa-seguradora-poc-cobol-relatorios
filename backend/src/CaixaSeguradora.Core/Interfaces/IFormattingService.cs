namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Interface para serviço de formatação (equivalente ao módulo COBOL GE0009S).
/// Formata documentos (CPF/CNPJ), datas e valida dígitos verificadores.
/// </summary>
public interface IFormattingService
{
    /// <summary>
    /// Formata CPF para o padrão visual brasileiro (XXX.XXX.XXX-XX).
    /// </summary>
    /// <param name="cpf">CPF sem formatação (11 dígitos)</param>
    /// <returns>CPF formatado (ex: "123.456.789-01")</returns>
    string FormatCpf(string cpf);

    /// <summary>
    /// Formata CNPJ para o padrão visual brasileiro (XX.XXX.XXX/XXXX-XX).
    /// </summary>
    /// <param name="cnpj">CNPJ sem formatação (14 dígitos)</param>
    /// <returns>CNPJ formatado (ex: "12.345.678/0001-95")</returns>
    string FormatCnpj(string cnpj);

    /// <summary>
    /// Formata data conforme formato especificado.
    /// </summary>
    /// <param name="date">Data a ser formatada</param>
    /// <param name="format">Formato desejado (padrão: "dd/MM/yyyy")</param>
    /// <returns>Data formatada</returns>
    string FormatDate(DateTime date, string format = "dd/MM/yyyy");

    /// <summary>
    /// Valida dígitos verificadores do CPF.
    /// </summary>
    /// <param name="cpf">CPF sem formatação (11 dígitos)</param>
    /// <returns>True se os dígitos verificadores estão corretos</returns>
    bool ValidateCpfCheckDigit(string cpf);

    /// <summary>
    /// Valida dígitos verificadores do CNPJ.
    /// </summary>
    /// <param name="cnpj">CNPJ sem formatação (14 dígitos)</param>
    /// <returns>True se os dígitos verificadores estão corretos</returns>
    bool ValidateCnpjCheckDigit(string cnpj);

    /// <summary>
    /// Remove formatação de CPF ou CNPJ (mantém apenas dígitos).
    /// </summary>
    /// <param name="document">Documento formatado ou não</param>
    /// <returns>Apenas os dígitos do documento</returns>
    string RemoveFormatting(string document);
}
