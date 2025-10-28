namespace CaixaSeguradora.Core.Exceptions;

/// <summary>
/// Exceção lançada quando um serviço externo está temporariamente indisponível.
/// Utilizada para indicar falhas em módulos externos (RE0001S, GE0009S, GE0010S)
/// após esgotamento de tentativas de retry.
/// </summary>
public class ServiceUnavailableException : Exception
{
    /// <summary>
    /// Nome do serviço que está indisponível
    /// </summary>
    public string ServiceName { get; }

    /// <summary>
    /// Número de tentativas realizadas antes da falha
    /// </summary>
    public int AttemptCount { get; }

    public ServiceUnavailableException(string serviceName, int attemptCount)
        : base($"Serviço '{serviceName}' está temporariamente indisponível após {attemptCount} tentativas.")
    {
        ServiceName = serviceName;
        AttemptCount = attemptCount;
    }

    public ServiceUnavailableException(string serviceName, int attemptCount, Exception innerException)
        : base($"Serviço '{serviceName}' está temporariamente indisponível após {attemptCount} tentativas.", innerException)
    {
        ServiceName = serviceName;
        AttemptCount = attemptCount;
    }

    public ServiceUnavailableException(string message)
        : base(message)
    {
        ServiceName = "Unknown";
        AttemptCount = 0;
    }

    public ServiceUnavailableException(string message, Exception innerException)
        : base(message, innerException)
    {
        ServiceName = "Unknown";
        AttemptCount = 0;
    }
}
