# 05 - Business Logic

[← Voltar ao Índice](README.md)

## Índice

- [Visão Geral](#visão-geral)
- [Estrutura do Programa](#estrutura-do-programa)
- [Seções COBOL (R0000-R9999)](#seções-cobol-r0000-r9999)
- [Regras de Negócio por Tipo de Movimento](#regras-de-negócio-por-tipo-de-movimento)
- [Cálculos de Prêmio](#cálculos-de-prêmio)
- [Processamento de Cosseguro](#processamento-de-cosseguro)
- [Validações por Ramo SUSEP](#validações-por-ramo-susep)
- [Migração para .NET](#migração-para-net)

---

## Visão Geral

O programa RG1866B contém **63 seções** (paragraphs) organizadas em grupos funcionais numerados de R0000 a R9999. Cada seção implementa uma parte específica da lógica de negócio para geração de relatórios SUSEP Circular 360.

### Métricas de Lógica de Negócio

| Métrica | Valor |
|---------|-------|
| **Total de Seções** | 63 |
| **Linhas de Código** | 5.046 |
| **Regras de Negócio** | 147+ |
| **Tipos de Movimento** | 6 (101-106) |
| **Ramos SUSEP** | 20+ |
| **Cálculos Financeiros** | 38 fórmulas |
| **Validações** | 52 checks |

### Fluxo de Processamento Principal

```text
R0000-INICIO
    ↓
R0100-INICIALIZACAO
    ↓
R0200-ABRIR-ARQUIVOS
    ↓
R0300-LER-PARAMETROS
    ↓
R0400-ABRIR-CURSORES
    ↓
R0500-PROCESSAR-LOTE ← Loop Principal
    ↓
    R0600-PROCESSAR-PREMIO
        ↓
        R0700-BUSCAR-APOLICE
        ↓
        R0800-BUSCAR-PRODUTO
        ↓
        R0900-BUSCAR-CLIENTES
        ↓
        R1000-CALCULAR-PREMIO
        ↓
        R3000-PROCESSAR-COSSEGURO (se aplicável)
        ↓
        R4000-FORMATAR-PREMIT
        ↓
        R5000-ESCREVER-REGISTRO
    ↓ (até fim do cursor)
    ↓
R8000-FECHAR-CURSORES
    ↓
R8100-FECHAR-ARQUIVOS
    ↓
R8200-GERAR-TOTALIZADORES
    ↓
R9999-FIM
```

---

## Estrutura do Programa

### Divisões COBOL

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. RG1866B.
*----------------------------------------------------------------
* PROGRAMA: RG1866B - RELATORIO PREMIOS EMITIDOS SUSEP CIRC 360
* AUTOR: TIME DE DESENVOLVIMENTO CAIXA SEGURADORA
* DATA CRIACAO: 2014-03-15
* ULTIMA ALTERACAO: 2022-09-30
*----------------------------------------------------------------

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SPECIAL-NAMES.
    DECIMAL-POINT IS COMMA.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> 687 variáveis de trabalho

PROCEDURE DIVISION.
    *> 63 seções de lógica de negócio
```

---

## Seções COBOL (R0000-R9999)

### Grupo R0000-R0999: Inicialização e Setup

#### R0000-INICIO

**Propósito**: Ponto de entrada do programa

```cobol
R0000-INICIO.
    PERFORM R0100-INICIALIZACAO.
    PERFORM R0200-ABRIR-ARQUIVOS.
    PERFORM R0300-LER-PARAMETROS.
    PERFORM R0400-ABRIR-CURSORES.
    PERFORM R0500-PROCESSAR-LOTE.
    PERFORM R8000-FECHAR-CURSORES.
    PERFORM R8100-FECHAR-ARQUIVOS.
    PERFORM R8200-GERAR-TOTALIZADORES.
    STOP RUN.
```

**Migração .NET**:

```csharp
public class PremiumReportService : IPremiumReportService
{
    public async Task<ReportResult> GenerateReportAsync(ReportParameters parameters)
    {
        // R0100: Inicialização
        await InitializeAsync(parameters);

        // R0200-R0300: Abrir conexões e ler parâmetros
        await using var connection = await OpenConnectionAsync();
        var config = await LoadConfigurationAsync(parameters);

        // R0400: Abrir cursores (streams)
        await using var premiumStream = GetPremiumStreamAsync(config);

        // R0500: Processar lote
        var result = await ProcessBatchAsync(premiumStream, config);

        // R8200: Gerar totalizadores
        await GenerateSummaryAsync(result);

        return result;
    }
}
```

#### R0100-INICIALIZACAO

**Propósito**: Inicializar variáveis de trabalho

```cobol
R0100-INICIALIZACAO.
    MOVE ZEROS TO WS-TOTAL-REGISTROS
                   WS-TOTAL-PREMIO-LIQUIDO
                   WS-TOTAL-PREMIO-TOTAL
                   WS-TOTAL-IOF
                   WS-CONTADOR-ERROS.

    MOVE SPACES TO WS-MENSAGEM-ERRO
                    WS-NUMERO-APOLICE-ANTERIOR.

    MOVE 'N' TO WS-FIM-CURSOR-PREMIOS
                 WS-FIM-CURSOR-ENDERECOS
                 WS-FIM-CURSOR-COSSEGURO.

    ACCEPT WS-DATA-PROCESSAMENTO FROM DATE YYYYMMDD.
    MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP.
```

**Migração .NET**:

```csharp
private void Initialize(ReportParameters parameters)
{
    _totalRecords = 0;
    _totalNetPremium = 0m;
    _totalGrossPremium = 0m;
    _totalIOF = 0m;
    _errorCount = 0;

    _processingDate = parameters.ProcessingDate;
    _timestamp = DateTime.Now;

    _logger.LogInformation("Initialized report generation at {Timestamp}", _timestamp);
}
```

#### R0200-ABRIR-ARQUIVOS

**Propósito**: Abrir arquivos de saída PREMIT.TXT e PREMCED.TXT

```cobol
R0200-ABRIR-ARQUIVOS.
    OPEN OUTPUT ARQUIVO-PREMIT.
    IF WS-FILE-STATUS NOT = '00'
        MOVE 'ERRO AO ABRIR ARQUIVO-PREMIT' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-ARQUIVO
    END-IF.

    OPEN OUTPUT ARQUIVO-PREMCED.
    IF WS-FILE-STATUS NOT = '00'
        MOVE 'ERRO AO ABRIR ARQUIVO-PREMCED' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-ARQUIVO
    END-IF.
```

**Migração .NET**:

```csharp
private async Task<(StreamWriter premit, StreamWriter premced)> OpenFilesAsync(
    ReportParameters parameters)
{
    var premitPath = Path.Combine(parameters.OutputPath, "PREMIT.TXT");
    var premcedPath = Path.Combine(parameters.OutputPath, "PREMCED.TXT");

    try
    {
        var premitWriter = new StreamWriter(premitPath, append: false, Encoding.ASCII);
        var premcedWriter = new StreamWriter(premcedPath, append: false, Encoding.ASCII);

        _logger.LogInformation("Opened output files: {PremitPath}, {PremcedPath}",
            premitPath, premcedPath);

        return (premitWriter, premcedWriter);
    }
    catch (IOException ex)
    {
        _logger.LogError(ex, "Failed to open output files");
        throw new ReportGenerationException("Erro ao abrir arquivos de saída", ex);
    }
}
```

#### R0300-LER-PARAMETROS

**Propósito**: Ler parâmetros de execução (data, companhia, etc.)

```cobol
R0300-LER-PARAMETROS.
    ACCEPT WS-DATA-PROCESSAMENTO FROM SYSIN.
    ACCEPT WS-COD-CIA FROM SYSIN.

    IF WS-DATA-PROCESSAMENTO = ZEROS OR SPACES
        MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATA-PROCESSAMENTO
    END-IF.

    IF WS-COD-CIA = ZEROS OR SPACES
        MOVE 1 TO WS-COD-CIA  *> Default company code
    END-IF.
```

**Migração .NET**:

```csharp
public class ReportParameters
{
    public DateTime ProcessingDate { get; set; }
    public int CompanyCode { get; set; }
    public string OutputPath { get; set; }

    public static ReportParameters FromConfiguration(IConfiguration config)
    {
        return new ReportParameters
        {
            ProcessingDate = config.GetValue<DateTime?>("ProcessingDate")
                ?? DateTime.Today,
            CompanyCode = config.GetValue<int?>("CompanyCode") ?? 1,
            OutputPath = config.GetValue<string>("OutputPath")
                ?? "/tmp/reports"
        };
    }
}
```

### Grupo R0500-R0999: Processamento Principal

#### R0500-PROCESSAR-LOTE

**Propósito**: Loop principal de processamento de prêmios

```cobol
R0500-PROCESSAR-LOTE.
    PERFORM R0600-FETCH-PREMIO

    PERFORM UNTIL WS-FIM-CURSOR-PREMIOS = 'S'
        ADD 1 TO WS-TOTAL-REGISTROS

        PERFORM R0700-BUSCAR-APOLICE
        PERFORM R0800-BUSCAR-PRODUTO
        PERFORM R0900-BUSCAR-CLIENTES
        PERFORM R1000-CALCULAR-PREMIO

        IF WS-PRODUTO-TEM-COSSEGURO = 'S'
            PERFORM R3000-PROCESSAR-COSSEGURO
        END-IF

        PERFORM R4000-FORMATAR-PREMIT
        PERFORM R5000-ESCREVER-REGISTRO

        PERFORM R0600-FETCH-PREMIO
    END-PERFORM.
```

**Migração .NET**:

```csharp
private async Task<ReportResult> ProcessBatchAsync(
    IAsyncEnumerable<Premium> premiumStream,
    ReportConfiguration config)
{
    var result = new ReportResult();

    await foreach (var premium in premiumStream)
    {
        result.TotalRecords++;

        // Buscar dados relacionados
        var policy = await _policyRepository.GetByNumberAsync(premium.PolicyNumber);
        var product = await _productRepository.GetByCodeAsync(premium.ProductCode);
        var clients = await _clientRepository.GetByPolicyAsync(premium.PolicyNumber);

        // Calcular prêmio
        var calculation = await CalculatePremiumAsync(premium, policy, product);

        // Processar cosseguro se aplicável
        if (product.CosuranceIndicator == "S")
        {
            await ProcessCosuranceAsync(premium, policy, calculation);
        }

        // Formatar e escrever registros
        var premitRecord = FormatPremitRecord(premium, policy, calculation);
        await WritePremitRecordAsync(premitRecord);

        // Atualizar totalizadores
        result.TotalNetPremium += calculation.NetPremium;
        result.TotalGrossPremium += calculation.GrossPremium;
        result.TotalIOF += calculation.IOF;
    }

    return result;
}
```

---

## Regras de Negócio por Tipo de Movimento

### Tipos de Movimento (COD_TIPO_MOVIMENTO)

| Código | Descrição | Impacto no Prêmio | Seção COBOL |
|--------|-----------|-------------------|-------------|
| **101** | Emissão | +Prêmio Integral | R1100 |
| **102** | Endosso Aumento | +Prêmio Adicional | R1200 |
| **103** | Endosso Redução | -Prêmio Devolvido | R1300 |
| **104** | Cancelamento | -Prêmio Integral | R1400 |
| **105** | Renovação | +Prêmio Nova Vigência | R1500 |
| **106** | Substituição | ±Diferença Prêmio | R1600 |

### R1100: Emissão (Tipo 101)

**Regra**: Prêmio integral da apólice nova

```cobol
R1100-PROCESSAR-EMISSAO.
    MOVE REGISTRO-PREMIO-LIQUIDO TO WS-PREMIO-CALCULADO.
    MOVE REGISTRO-PREMIO-TOTAL TO WS-PREMIO-EMITIDO.
    MOVE REGISTRO-IOF TO WS-IOF-CALCULADO.

    *> Calcular adicional de fracionamento
    IF APOLICE-NUM-PARCELAS > 1
        COMPUTE WS-ADICIONAL-FRAC =
            REGISTRO-PREMIO-LIQUIDO * 0.0538  *> 5.38% taxa padrão
        ADD WS-ADICIONAL-FRAC TO WS-PREMIO-EMITIDO
    END-IF.

    *> Validar vigência
    IF APOLICE-DATA-VIG-INI > WS-DATA-PROCESSAMENTO
        MOVE 'W' TO WS-STATUS-VALIDACAO  *> Warning: vigência futura
    END-IF.
```

**Migração .NET**:

```csharp
public class EmissionCalculationService
{
    private const decimal DEFAULT_INSTALLMENT_FEE_RATE = 0.0538m; // 5.38%

    public PremiumCalculation CalculateEmission(
        Premium premium,
        Policy policy)
    {
        var calculation = new PremiumCalculation
        {
            NetPremium = premium.NetPremium,
            GrossPremium = premium.TotalPremium,
            IOF = premium.IOF
        };

        // Adicional de fracionamento
        if (policy.InstallmentCount > 1)
        {
            calculation.InstallmentFee =
                premium.NetPremium * DEFAULT_INSTALLMENT_FEE_RATE;
            calculation.GrossPremium += calculation.InstallmentFee;
        }

        // Validações
        if (policy.EffectiveStartDate > _processingDate)
        {
            calculation.Warnings.Add("Vigência futura");
        }

        return calculation;
    }
}
```

### R1200: Endosso Aumento (Tipo 102)

**Regra**: Calcular prêmio proporcional ao período remanescente

```cobol
R1200-PROCESSAR-ENDOSSO-AUMENTO.
    *> Calcular dias remanescentes de vigência
    COMPUTE WS-DIAS-VIGENCIA =
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-FIM) -
        FUNCTION INTEGER-OF-DATE(WS-DATA-PROCESSAMENTO).

    *> Dias totais da apólice
    COMPUTE WS-DIAS-TOTAIS =
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-FIM) -
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-INI).

    *> Prêmio proporcional (pro-rata die)
    COMPUTE WS-PREMIO-CALCULADO =
        REGISTRO-PREMIO-LIQUIDO *
        (WS-DIAS-VIGENCIA / WS-DIAS-TOTAIS).

    *> IOF proporcional
    COMPUTE WS-IOF-CALCULADO =
        REGISTRO-IOF *
        (WS-DIAS-VIGENCIA / WS-DIAS-TOTAIS).
```

**Migração .NET**:

```csharp
public PremiumCalculation CalculateEndorsementIncrease(
    Premium premium,
    Policy policy,
    DateTime processingDate)
{
    // Calcular dias remanescentes
    var remainingDays = (policy.EffectiveEndDate - processingDate).Days;
    var totalDays = (policy.EffectiveEndDate - policy.EffectiveStartDate).Days;

    // Validar período
    if (remainingDays <= 0)
    {
        throw new BusinessRuleException(
            "Endosso não permitido: vigência já encerrada");
    }

    // Pro-rata die (proporcional aos dias)
    var proportionFactor = (decimal)remainingDays / totalDays;

    return new PremiumCalculation
    {
        NetPremium = premium.NetPremium * proportionFactor,
        IOF = premium.IOF * proportionFactor,
        GrossPremium = (premium.NetPremium + premium.IOF) * proportionFactor,
        ProportionFactor = proportionFactor
    };
}
```

### R1300: Endosso Redução (Tipo 103)

**Regra**: Calcular devolução de prêmio (negativo)

```cobol
R1300-PROCESSAR-ENDOSSO-REDUCAO.
    *> Mesmo cálculo de R1200, mas com sinal negativo
    PERFORM R1200-PROCESSAR-ENDOSSO-AUMENTO.

    MULTIPLY WS-PREMIO-CALCULADO BY -1.
    MULTIPLY WS-IOF-CALCULADO BY -1.
    MULTIPLY WS-PREMIO-EMITIDO BY -1.
```

**Migração .NET**:

```csharp
public PremiumCalculation CalculateEndorsementDecrease(
    Premium premium,
    Policy policy,
    DateTime processingDate)
{
    // Mesmo cálculo de aumento, mas com valores negativos
    var increase = CalculateEndorsementIncrease(premium, policy, processingDate);

    return new PremiumCalculation
    {
        NetPremium = -increase.NetPremium,
        IOF = -increase.IOF,
        GrossPremium = -increase.GrossPremium,
        ProportionFactor = increase.ProportionFactor
    };
}
```

### R1400: Cancelamento (Tipo 104)

**Regra**: Devolver prêmio integral (negativo), validar carência

```cobol
R1400-PROCESSAR-CANCELAMENTO.
    *> Verificar carência de 7 dias
    COMPUTE WS-DIAS-DESDE-EMISSAO =
        FUNCTION INTEGER-OF-DATE(WS-DATA-PROCESSAMENTO) -
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-EMISSAO).

    IF WS-DIAS-DESDE-EMISSAO < 7
        *> Devolução integral (direito de arrependimento)
        COMPUTE WS-PREMIO-CALCULADO =
            REGISTRO-PREMIO-TOTAL * -1
    ELSE
        *> Devolução proporcional (descontar taxa administrativa 10%)
        COMPUTE WS-PREMIO-CALCULADO =
            REGISTRO-PREMIO-TOTAL * -0.90
    END-IF.
```

**Migração .NET**:

```csharp
public PremiumCalculation CalculateCancellation(
    Premium premium,
    Policy policy,
    DateTime processingDate)
{
    var daysSinceIssue = (processingDate - policy.IssueDate).Days;

    decimal refundAmount;

    if (daysSinceIssue < 7)
    {
        // Direito de arrependimento: devolução integral
        refundAmount = premium.TotalPremium;
        _logger.LogInformation(
            "Full refund applied (regret period): Policy {PolicyNumber}",
            policy.PolicyNumber);
    }
    else
    {
        // Devolução com desconto de taxa administrativa (10%)
        const decimal ADMIN_FEE_RATE = 0.10m;
        refundAmount = premium.TotalPremium * (1 - ADMIN_FEE_RATE);
        _logger.LogInformation(
            "Partial refund applied (admin fee {FeeRate}%): Policy {PolicyNumber}",
            ADMIN_FEE_RATE * 100, policy.PolicyNumber);
    }

    return new PremiumCalculation
    {
        NetPremium = -refundAmount,
        GrossPremium = -refundAmount,
        IOF = 0m, // IOF não é devolvido
        RefundReason = daysSinceIssue < 7 ? "Regret" : "Cancellation"
    };
}
```

---

## Cálculos de Prêmio

### R1000-CALCULAR-PREMIO (Seção Mestre)

```cobol
R1000-CALCULAR-PREMIO.
    EVALUATE COD-TIPO-MOVIMENTO
        WHEN 101  PERFORM R1100-PROCESSAR-EMISSAO
        WHEN 102  PERFORM R1200-PROCESSAR-ENDOSSO-AUMENTO
        WHEN 103  PERFORM R1300-PROCESSAR-ENDOSSO-REDUCAO
        WHEN 104  PERFORM R1400-PROCESSAR-CANCELAMENTO
        WHEN 105  PERFORM R1500-PROCESSAR-RENOVACAO
        WHEN 106  PERFORM R1600-PROCESSAR-SUBSTITUICAO
        WHEN OTHER
            MOVE 'TIPO DE MOVIMENTO INVALIDO' TO WS-MENSAGEM-ERRO
            PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-EVALUATE.

    *> Aplicar conversão de moeda se necessário
    IF COD-MOEDA NOT = 'BRL'
        PERFORM R1700-CONVERTER-MOEDA
    END-IF.

    *> Validar limites
    PERFORM R1800-VALIDAR-LIMITES.
```

**Migração .NET**:

```csharp
public async Task<PremiumCalculation> CalculatePremiumAsync(
    Premium premium,
    Policy policy,
    Product product)
{
    PremiumCalculation calculation = premium.MovementType switch
    {
        101 => CalculateEmission(premium, policy),
        102 => CalculateEndorsementIncrease(premium, policy, _processingDate),
        103 => CalculateEndorsementDecrease(premium, policy, _processingDate),
        104 => CalculateCancellation(premium, policy, _processingDate),
        105 => CalculateRenewal(premium, policy),
        106 => CalculateReplacement(premium, policy),
        _ => throw new BusinessRuleException(
            $"Tipo de movimento inválido: {premium.MovementType}")
    };

    // Conversão de moeda
    if (premium.CurrencyCode != "BRL")
    {
        calculation = await ConvertCurrencyAsync(calculation, premium.ExchangeRate);
    }

    // Validações de limites
    ValidateLimits(calculation, product);

    return calculation;
}
```

### R1700-CONVERTER-MOEDA

**Regra**: Conversão para BRL usando taxa de câmbio

```cobol
R1700-CONVERTER-MOEDA.
    IF WS-TAXA-CAMBIO = ZEROS
        MOVE 'TAXA DE CAMBIO NAO INFORMADA' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.

    COMPUTE WS-PREMIO-CALCULADO =
        WS-PREMIO-CALCULADO * WS-TAXA-CAMBIO.

    COMPUTE WS-PREMIO-EMITIDO =
        WS-PREMIO-EMITIDO * WS-TAXA-CAMBIO.

    COMPUTE WS-IOF-CALCULADO =
        WS-IOF-CALCULADO * WS-TAXA-CAMBIO.
```

**Migração .NET**:

```csharp
private async Task<PremiumCalculation> ConvertCurrencyAsync(
    PremiumCalculation calculation,
    decimal exchangeRate)
{
    if (exchangeRate <= 0)
    {
        throw new BusinessRuleException("Taxa de câmbio inválida");
    }

    return new PremiumCalculation
    {
        NetPremium = calculation.NetPremium * exchangeRate,
        GrossPremium = calculation.GrossPremium * exchangeRate,
        IOF = calculation.IOF * exchangeRate,
        ExchangeRate = exchangeRate,
        OriginalCurrency = calculation.OriginalCurrency ?? "USD"
    };
}
```

### R1800-VALIDAR-LIMITES

**Regra**: Validar valores contra limites do produto

```cobol
R1800-VALIDAR-LIMITES.
    *> Limite mínimo de prêmio
    IF WS-PREMIO-CALCULADO < PRODUTO-PREMIO-MINIMO
        MOVE 'E' TO WS-STATUS-VALIDACAO  *> Error
        MOVE 'PREMIO ABAIXO DO MINIMO' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.

    *> Limite máximo de prêmio
    IF WS-PREMIO-CALCULADO > PRODUTO-PREMIO-MAXIMO
        MOVE 'W' TO WS-STATUS-VALIDACAO  *> Warning
        MOVE 'PREMIO ACIMA DO MAXIMO' TO WS-MENSAGEM-ERRO
        PERFORM R9100-TRATAR-WARNING
    END-IF.
```

**Migração .NET**:

```csharp
private void ValidateLimits(PremiumCalculation calculation, Product product)
{
    // Limite mínimo
    if (calculation.NetPremium < product.MinimumPremium)
    {
        throw new BusinessRuleException(
            $"Prêmio R$ {calculation.NetPremium:N2} abaixo do mínimo " +
            $"R$ {product.MinimumPremium:N2}");
    }

    // Limite máximo (warning, não erro)
    if (calculation.NetPremium > product.MaximumPremium)
    {
        _logger.LogWarning(
            "Prêmio R$ {NetPremium:N2} acima do máximo R$ {MaxPremium:N2} " +
            "para produto {ProductCode}",
            calculation.NetPremium, product.MaximumPremium, product.ProductCode);

        calculation.Warnings.Add(
            $"Prêmio acima do máximo: R$ {product.MaximumPremium:N2}");
    }
}
```

---

## Processamento de Cosseguro

### R3000-PROCESSAR-COSSEGURO (Seção Mestre)

```cobol
R3000-PROCESSAR-COSSEGURO.
    *> Buscar dados de cosseguro/cessão
    PERFORM R3100-ABRIR-CURSOR-COSSEGURO.
    PERFORM R3200-FETCH-COSSEGURO.

    MOVE ZEROS TO WS-PREMIO-LIDER
                   WS-PREMIO-CEDIDO
                   WS-QTD-COSSEGURADORES.

    PERFORM UNTIL WS-FIM-CURSOR-COSSEGURO = 'S'
        ADD 1 TO WS-QTD-COSSEGURADORES

        PERFORM R3500-CALCULAR-PARTICIPACAO
        PERFORM R3600-GERAR-REGISTRO-PREMCED

        PERFORM R3200-FETCH-COSSEGURO
    END-PERFORM.

    PERFORM R3900-FECHAR-CURSOR-COSSEGURO.

    *> Validar soma de participações = 100%
    PERFORM R3800-VALIDAR-PARTICIPACOES.
```

**Migração .NET**:

```csharp
private async Task ProcessCosuranceAsync(
    Premium premium,
    Policy policy,
    PremiumCalculation calculation)
{
    // Buscar participações de cosseguro
    var cosurances = await _cosuranceRepository.GetCosurancesAsync(
        policy.PolicyNumber,
        premium.EndorsementNumber);

    if (!cosurances.Any())
    {
        _logger.LogWarning(
            "Produto marcado com cosseguro mas sem participações: {PolicyNumber}",
            policy.PolicyNumber);
        return;
    }

    decimal totalLeaderPremium = 0m;
    decimal totalCededPremium = 0m;

    foreach (var cosurance in cosurances)
    {
        var participation = CalculateParticipation(
            calculation,
            cosurance);

        await GeneratePremcedRecordAsync(
            premium,
            policy,
            cosurance,
            participation);

        if (cosurance.ParticipationType == "L") // Líder
        {
            totalLeaderPremium += participation.PremiumAmount;
        }
        else // Cedido
        {
            totalCededPremium += participation.PremiumAmount;
        }
    }

    // Validar soma de participações
    ValidateParticipations(cosurances, calculation.GrossPremium);
}
```

### R3500-CALCULAR-PARTICIPACAO

**Regra**: Calcular prêmio por percentual de participação

```cobol
R3500-CALCULAR-PARTICIPACAO.
    COMPUTE WS-PREMIO-PARTICIPACAO =
        WS-PREMIO-EMITIDO *
        (COSSEGURO-PERCENTUAL / 100).

    IF COSSEGURO-TIPO = 'A'  *> Aceito (líder)
        ADD WS-PREMIO-PARTICIPACAO TO WS-PREMIO-LIDER
    ELSE  *> Cedido
        ADD WS-PREMIO-PARTICIPACAO TO WS-PREMIO-CEDIDO
    END-IF.
```

**Migração .NET**:

```csharp
private CosuranceParticipation CalculateParticipation(
    PremiumCalculation calculation,
    Cosurance cosurance)
{
    var premiumAmount = calculation.GrossPremium *
        (cosurance.ParticipationPercentage / 100m);

    return new CosuranceParticipation
    {
        CoinsurerCompanyCode = cosurance.CoinsurerCompanyCode,
        ParticipationPercentage = cosurance.ParticipationPercentage,
        PremiumAmount = premiumAmount,
        ParticipationType = cosurance.ParticipationType
    };
}
```

### R3800-VALIDAR-PARTICIPACOES

**Regra**: Soma de percentuais deve ser 100%

```cobol
R3800-VALIDAR-PARTICIPACOES.
    MOVE ZEROS TO WS-SOMA-PARTICIPACOES.

    *> Somar todos os percentuais
    EXEC SQL
        SELECT SUM(PERCENTUAL_PARTICIPACAO)
        INTO :WS-SOMA-PARTICIPACOES
        FROM V0APOLCOSCED
        WHERE NUM_APOLICE = :WS-NUM-APOLICE
          AND NUM_ENDOSSO = :WS-NUM-ENDOSSO
    END-EXEC.

    IF WS-SOMA-PARTICIPACOES NOT = 100
        MOVE 'SOMA DE PARTICIPACOES DIFERENTE DE 100%'
            TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.
```

**Migração .NET**:

```csharp
private void ValidateParticipations(
    List<Cosurance> cosurances,
    decimal totalPremium)
{
    var totalPercentage = cosurances.Sum(c => c.ParticipationPercentage);

    // Tolerância de 0.01% para arredondamento
    const decimal TOLERANCE = 0.01m;

    if (Math.Abs(totalPercentage - 100m) > TOLERANCE)
    {
        throw new BusinessRuleException(
            $"Soma de participações ({totalPercentage:N2}%) diferente de 100%");
    }

    // Validar soma de prêmios (reconciliação)
    var totalCalculatedPremium = cosurances
        .Sum(c => totalPremium * (c.ParticipationPercentage / 100m));

    if (Math.Abs(totalCalculatedPremium - totalPremium) > 0.01m)
    {
        _logger.LogWarning(
            "Diferença na soma de prêmios de cosseguro: " +
            "Esperado={Expected:N2}, Calculado={Calculated:N2}",
            totalPremium, totalCalculatedPremium);
    }
}
```

---

## Validações por Ramo SUSEP

### R2000-VALIDAR-RAMO-SUSEP

**Regra**: Validações específicas por ramo SUSEP

```cobol
R2000-VALIDAR-RAMO-SUSEP.
    EVALUATE PRODUTO-RAMO-SUSEP
        WHEN 0531  *> Vida Individual
            PERFORM R2100-VALIDAR-VIDA
        WHEN 0532  *> Vida em Grupo
            PERFORM R2200-VALIDAR-VIDA-GRUPO
        WHEN 0553  *> Acidentes Pessoais
            PERFORM R2300-VALIDAR-ACIDENTES
        WHEN 0571  *> Previdência Privada
            PERFORM R2400-VALIDAR-PREVIDENCIA
        WHEN OTHER
            PERFORM R2900-VALIDAR-RAMO-GENERICO
    END-EVALUATE.
```

**Migração .NET**:

```csharp
private void ValidateBySusepBranch(
    Premium premium,
    Policy policy,
    Product product,
    PremiumCalculation calculation)
{
    switch (product.SusepBranch)
    {
        case 531: // Vida Individual
            ValidateLifeInsurance(premium, policy, calculation);
            break;

        case 532: // Vida em Grupo
            ValidateGroupLifeInsurance(premium, policy, calculation);
            break;

        case 553: // Acidentes Pessoais
            ValidatePersonalAccidents(premium, policy, calculation);
            break;

        case 571: // Previdência Privada
            ValidatePensionPlan(premium, policy, calculation);
            break;

        default:
            ValidateGenericBranch(premium, policy, calculation);
            break;
    }
}
```

### R2100-VALIDAR-VIDA (Ramo 0531)

**Regras**:
- Cliente deve ter idade entre 18 e 70 anos
- Vigência máxima de 1 ano
- Exigir documentação médica acima de R$ 100.000

```cobol
R2100-VALIDAR-VIDA.
    *> Validar idade do segurado
    COMPUTE WS-IDADE-SEGURADO =
        FUNCTION INTEGER-OF-DATE(WS-DATA-PROCESSAMENTO) -
        FUNCTION INTEGER-OF-DATE(CLIENTE-DATA-NASCIMENTO).

    DIVIDE WS-IDADE-SEGURADO BY 365 GIVING WS-IDADE-ANOS.

    IF WS-IDADE-ANOS < 18 OR WS-IDADE-ANOS > 70
        MOVE 'IDADE FORA DO LIMITE PERMITIDO' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.

    *> Validar vigência máxima
    COMPUTE WS-DIAS-VIGENCIA =
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-FIM) -
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-INI).

    IF WS-DIAS-VIGENCIA > 365
        MOVE 'VIGENCIA SUPERIOR A 1 ANO' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.
```

**Migração .NET**:

```csharp
private void ValidateLifeInsurance(
    Premium premium,
    Policy policy,
    PremiumCalculation calculation)
{
    var insured = _clientRepository.GetById(policy.InsuredClientCode);

    // Validar idade
    var age = CalculateAge(insured.BirthDate, _processingDate);
    if (age < 18 || age > 70)
    {
        throw new BusinessRuleException(
            $"Idade {age} fora do limite permitido (18-70 anos)");
    }

    // Validar vigência máxima
    var policyDuration = (policy.EffectiveEndDate - policy.EffectiveStartDate).Days;
    if (policyDuration > 365)
    {
        throw new BusinessRuleException(
            $"Vigência de {policyDuration} dias superior ao máximo de 365 dias");
    }

    // Exigir documentação médica
    if (calculation.GrossPremium > 100000m)
    {
        calculation.Warnings.Add(
            "Documentação médica obrigatória para prêmio acima de R$ 100.000");
    }
}
```

---

## Migração para .NET

### Arquitetura de Serviços

```csharp
// Service Layer: CaixaSeguradora.Core/Services/
public interface IPremiumCalculationService
{
    Task<PremiumCalculation> CalculateAsync(
        Premium premium,
        Policy policy,
        Product product);
}

public class PremiumCalculationService : IPremiumCalculationService
{
    private readonly ILogger<PremiumCalculationService> _logger;
    private readonly DateTime _processingDate;

    // Métodos privados para cada tipo de movimento
    private PremiumCalculation CalculateEmission(...);
    private PremiumCalculation CalculateEndorsementIncrease(...);
    private PremiumCalculation CalculateEndorsementDecrease(...);
    private PremiumCalculation CalculateCancellation(...);

    // Métodos de validação
    private void ValidateLimits(...);
    private void ValidateBySusepBranch(...);

    // Métodos de conversão
    private Task<PremiumCalculation> ConvertCurrencyAsync(...);
}
```

### Testes de Comparação com COBOL

```csharp
[Fact]
public async Task EmissionCalculation_ShouldMatch_COBOLOutput()
{
    // Arrange: dados de teste do COBOL
    var premium = new Premium
    {
        NetPremium = 1250.50m,
        TotalPremium = 1393.05m,
        IOF = 142.55m,
        MovementType = 101 // Emissão
    };

    var policy = new Policy
    {
        InstallmentCount = 1,
        EffectiveStartDate = new DateTime(2025, 10, 1),
        EffectiveEndDate = new DateTime(2026, 09, 30)
    };

    // Act: cálculo .NET
    var calculation = await _service.CalculateAsync(premium, policy, _product);

    // Assert: comparar com saída COBOL esperada
    Assert.Equal(1250.50m, calculation.NetPremium);
    Assert.Equal(1393.05m, calculation.GrossPremium);
    Assert.Equal(142.55m, calculation.IOF);
}
```

---

## Referências

- **Estruturas COBOL**: `docs/legacy-system/03-data-structures.md`
- **Modelo de Dados**: `docs/legacy-system/04-database-model.md`
- **Código Fonte Original**: `LEGACY_SYSTEM_DOCUMENTATION.md`
- **Especificação Migração**: `specs/001-vamos-migrar-sistema/spec.md`

---

**Documento criado em**: 2025-10-27
**Última atualização**: 2025-10-27
**Versão**: 1.0
