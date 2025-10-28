namespace CaixaSeguradora.Core.Exceptions;

/// <summary>
/// Exceção lançada quando dados de entrada falham na validação de regras de negócio.
/// Utilizada por serviços de validação externa e regras de negócio.
/// </summary>
public class ValidationException : Exception
{
    /// <summary>
    /// Campo que falhou na validação
    /// </summary>
    public string? FieldName { get; }

    /// <summary>
    /// Valor que falhou na validação
    /// </summary>
    public object? FieldValue { get; }

    /// <summary>
    /// Código de erro associado (para mapeamento com códigos COBOL)
    /// </summary>
    public string? ErrorCode { get; }

    public ValidationException(string message)
        : base(message)
    {
    }

    public ValidationException(string message, Exception innerException)
        : base(message, innerException)
    {
    }

    public ValidationException(string fieldName, object? fieldValue, string message)
        : base(message)
    {
        FieldName = fieldName;
        FieldValue = fieldValue;
    }

    public ValidationException(string fieldName, object? fieldValue, string message, string errorCode)
        : base(message)
    {
        FieldName = fieldName;
        FieldValue = fieldValue;
        ErrorCode = errorCode;
    }
}
