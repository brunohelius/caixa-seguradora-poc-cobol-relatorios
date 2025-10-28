using System.Diagnostics;
using System.Security.Cryptography;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Exceptions;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;
using Polly;
using Polly.Retry;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Implementação MOCK do serviço de cálculo de resseguro (equivalente ao módulo COBOL RE0001S).
/// Esta é uma implementação simplificada para desenvolvimento/testes.
/// Em produção, deve ser substituída por integração com sistema real de resseguro.
/// </summary>
public class ReinsuranceCalculationService : IReinsuranceCalculationService
{
    private readonly ILogger<ReinsuranceCalculationService> _logger;
    private readonly ResiliencePipeline _retryPipeline;

    // Ramos GARANTIA conforme COBOL (CADMUS-154263)
    private static readonly HashSet<int> GarantiaBranches = new() { 40, 45, 75, 76 };

    public ReinsuranceCalculationService(ILogger<ReinsuranceCalculationService> logger)
    {
        _logger = logger;

        // Configura política de retry: 3 tentativas com backoff exponencial (1s, 2s, 4s)
        _retryPipeline = new ResiliencePipelineBuilder()
            .AddRetry(new RetryStrategyOptions
            {
                MaxRetryAttempts = 3,
                Delay = TimeSpan.FromSeconds(1),
                BackoffType = DelayBackoffType.Exponential,
                UseJitter = true,
                OnRetry = args =>
                {
                    _logger.LogWarning(
                        "Tentativa {AttemptNumber} de cálculo de resseguro falhou. Aguardando {Delay}ms antes de nova tentativa. Exceção: {Exception}",
                        args.AttemptNumber + 1,
                        args.RetryDelay.TotalMilliseconds,
                        args.Outcome.Exception?.Message);
                    return ValueTask.CompletedTask;
                }
            })
            .Build();
    }

    /// <inheritdoc/>
    public async Task<ReinsuranceResponse> CalculateReinsuranceAsync(
        long policyNumber,
        decimal premiumAmount,
        int productCode,
        DateTime effectiveDate,
        int susepBranchCode,
        CancellationToken cancellationToken = default)
    {
        var stopwatch = Stopwatch.StartNew();
        _logger.LogInformation(
            "Iniciando cálculo de resseguro para apólice {PolicyNumber}, produto {ProductCode}, valor {PremiumAmount:C}",
            policyNumber, productCode, premiumAmount);

        try
        {
            // Executa cálculo com política de retry
            var response = await _retryPipeline.ExecuteAsync(
                async ct =>
                {
                    return await CalculateReinsuranceInternalAsync(
                        policyNumber, premiumAmount, productCode, effectiveDate, susepBranchCode, ct);
                },
                cancellationToken);

            stopwatch.Stop();
            _logger.LogInformation(
                "Cálculo de resseguro concluído para apólice {PolicyNumber}. ReturnCode={ReturnCode}, Percentual={Percentage}%, Tempo={ElapsedMs}ms",
                policyNumber, response.ReturnCode, response.ReinsurancePercentage, stopwatch.ElapsedMilliseconds);

            return response;
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            _logger.LogError(ex,
                "Erro fatal no cálculo de resseguro para apólice {PolicyNumber} após 3 tentativas. Tempo={ElapsedMs}ms",
                policyNumber, stopwatch.ElapsedMilliseconds);

            throw new ServiceUnavailableException("ReinsuranceCalculationService", 3, ex);
        }
    }

    /// <summary>
    /// Implementação interna do cálculo de resseguro (MOCK).
    /// </summary>
    private Task<ReinsuranceResponse> CalculateReinsuranceInternalAsync(
        long policyNumber,
        decimal premiumAmount,
        int productCode,
        DateTime effectiveDate,
        int susepBranchCode,
        CancellationToken cancellationToken)
    {
        // Validações de entrada
        if (premiumAmount <= 0)
        {
            return Task.FromResult(new ReinsuranceResponse
            {
                ReturnCode = "08",
                ErrorMessage = "Valor do prêmio deve ser maior que zero"
            });
        }

        if (policyNumber <= 0)
        {
            return Task.FromResult(new ReinsuranceResponse
            {
                ReturnCode = "08",
                ErrorMessage = "Número de apólice inválido"
            });
        }

        // MOCK: Percentual de resseguro baseado em regras simplificadas
        decimal reinsurancePercentage = CalculateMockReinsurancePercentage(
            premiumAmount, productCode, susepBranchCode);

        // Calcula valor ressegurado
        decimal reinsuredAmount = Math.Round(premiumAmount * (reinsurancePercentage / 100m), 2);

        // Gera códigos mock de tratado e contrato
        string treatyCode = GenerateMockTreatyCode(susepBranchCode);
        string contractCode = GenerateMockContractCode(productCode, effectiveDate.Year);

        // Data de corte (cutoff) - mock usando primeiro dia do mês seguinte
        DateTime cutoffDate = new DateTime(effectiveDate.Year, effectiveDate.Month, 1).AddMonths(1);

        _logger.LogDebug(
            "MOCK: Resseguro calculado - Apólice={PolicyNumber}, Percentual={Percentage}%, Valor Ressegurado={ReinsuredAmount:C}, Tratado={TreatyCode}",
            policyNumber, reinsurancePercentage, reinsuredAmount, treatyCode);

        return Task.FromResult(new ReinsuranceResponse
        {
            ReinsuredAmount = reinsuredAmount,
            ReinsurancePercentage = reinsurancePercentage,
            TreatyCode = treatyCode,
            ContractCode = contractCode,
            CutoffDate = cutoffDate,
            ReturnCode = "00",
            ErrorMessage = null
        });
    }

    /// <summary>
    /// Calcula percentual de resseguro (MOCK - lógica simplificada).
    /// Em produção, este cálculo seria feito pelo módulo COBOL RE0001S real.
    /// </summary>
    private decimal CalculateMockReinsurancePercentage(decimal premiumAmount, int productCode, int susepBranchCode)
    {
        // Regra simplificada:
        // - Prêmios altos (> 100.000): 40% ressegurado
        // - Prêmios médios (10.000 - 100.000): 30% ressegurado
        // - Prêmios baixos (< 10.000): 20% ressegurado
        // - Ramos GARANTIA: +5% adicional

        decimal basePercentage = premiumAmount switch
        {
            > 100000m => 40m,
            > 10000m => 30m,
            _ => 20m
        };

        // Ajuste para ramos GARANTIA (CADMUS-154263)
        if (GarantiaBranches.Contains(susepBranchCode))
        {
            basePercentage += 5m;
            _logger.LogDebug("Ramo GARANTIA detectado ({SusepBranchCode}), percentual ajustado para {Percentage}%",
                susepBranchCode, basePercentage);
        }

        return basePercentage;
    }

    /// <summary>
    /// Gera código de tratado mock (10 caracteres).
    /// Formato: "TRT{SusepBranchCode:D4}{RandomSuffix:D3}"
    /// </summary>
    private string GenerateMockTreatyCode(int susepBranchCode)
    {
        var random = new Random();
        return $"TRT{susepBranchCode:D4}{random.Next(100, 999):D3}";
    }

    /// <summary>
    /// Gera código de contrato mock (15 caracteres).
    /// Formato: "CTR{ProductCode:D4}{Year}{RandomSuffix:D4}"
    /// </summary>
    private string GenerateMockContractCode(int productCode, int year)
    {
        int suffix = RandomNumberGenerator.GetInt32(0, 10000);
        return $"CTR{productCode:D4}{year}{suffix:D4}";
    }
}
