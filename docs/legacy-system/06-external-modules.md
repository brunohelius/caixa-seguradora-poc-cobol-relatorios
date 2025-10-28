# 06 - External Modules

[← Voltar ao Índice](README.md)

## Índice

- [Visão Geral](#visão-geral)
- [RE0001S - Módulo de Resseguro](#re0001s---módulo-de-resseguro)
- [GE0009S - Módulo de Formatação](#ge0009s---módulo-de-formatação)
- [GE0010S - Módulo de Validação](#ge0010s---módulo-de-validação)
- [Estratégia de Migração](#estratégia-de-migração)
- [Testes de Integração](#testes-de-integração)

---

## Visão Geral

O programa RG1866B depende de **3 módulos externos** (subprogramas COBOL) que fornecem funcionalidades reutilizáveis. Estes módulos são chamados via `CALL` statement e seguem o padrão de comunicação por área de linkage.

### Módulos Utilizados

| Módulo | Propósito | Chamadas/Execução | Localização |
|--------|-----------|-------------------|-------------|
| **RE0001S** | Cálculos de resseguro | ~500-1000 | PROD.LOADLIB |
| **GE0009S** | Formatação de campos | ~10.000 | SYS1.COBLIB |
| **GE0010S** | Validação de dados | ~8.000 | SYS1.COBLIB |

### Padrão de Comunicação

```cobol
*> Padrão de chamada COBOL
CALL 'MODULENAME' USING
    BY REFERENCE AREA-ENTRADA
    BY REFERENCE AREA-SAIDA
    BY REFERENCE AREA-RETORNO.

IF RETORNO-STATUS NOT = '00'
    PERFORM TRATAR-ERRO-MODULO
END-IF.
```

**Migração .NET**:

```csharp
// Padrão de serviço .NET
public interface IModuleService
{
    Task<ModuleResponse> ExecuteAsync(ModuleRequest request);
}

public class ModuleResponse
{
    public string StatusCode { get; set; }
    public string ErrorMessage { get; set; }
    public object Result { get; set; }
}
```

---

## RE0001S - Módulo de Resseguro

### Propósito

Calcular valores de resseguro (reinsurance) para apólices que excedem limites de retenção da seguradora. Implementa regras complexas de distribuição proporcional e por camadas (layers).

### Interface COBOL

**Área de Entrada (LINKAGE SECTION)**:

```cobol
01  RE0001S-ENTRADA.
    05  RE-COD-CIA              PIC 9(5).
    05  RE-NUM-APOLICE          PIC X(20).
    05  RE-PREMIO-TOTAL         PIC 9(15)V99 COMP-3.
    05  RE-IMPORTANCIA-SEGURADA PIC 9(15)V99 COMP-3.
    05  RE-RAMO-SUSEP           PIC 9(4).
    05  RE-TIPO-CALCULO         PIC X(1).
        88  RE-CALC-PROPORCIONAL   VALUE 'P'.
        88  RE-CALC-EXCEDENTE      VALUE 'E'.
        88  RE-CALC-NAO-PROPORCIONAL VALUE 'N'.
```

**Área de Saída**:

```cobol
01  RE0001S-SAIDA.
    05  RE-PREMIO-RETIDO        PIC 9(15)V99 COMP-3.
    05  RE-PREMIO-CEDIDO        PIC 9(15)V99 COMP-3.
    05  RE-PERCENTUAL-CEDIDO    PIC 9(3)V99 COMP-3.
    05  RE-QTD-RESSEGURADORES   PIC 9(3).
    05  RE-RESSEGURADORES OCCURS 10 TIMES.
        10  RE-COD-RESSEGURADOR PIC 9(5).
        10  RE-NOME-RESSEGURADOR PIC X(50).
        10  RE-PREMIO-RESSEG    PIC 9(15)V99 COMP-3.
        10  RE-PERC-RESSEG      PIC 9(3)V99 COMP-3.
```

**Área de Retorno**:

```cobol
01  RE0001S-RETORNO.
    05  RE-STATUS               PIC X(2).
        88  RE-SUCESSO             VALUE '00'.
        88  RE-ERRO-PARAMETRO      VALUE '10'.
        88  RE-ERRO-CALCULO        VALUE '20'.
        88  RE-ERRO-LIMITES        VALUE '30'.
    05  RE-MENSAGEM-ERRO        PIC X(100).
```

### Chamada no RG1866B

**Seção R4500-CALCULAR-RESSEGURO**:

```cobol
R4500-CALCULAR-RESSEGURO.
    *> Preparar entrada
    MOVE WS-COD-CIA TO RE-COD-CIA.
    MOVE WS-NUM-APOLICE TO RE-NUM-APOLICE.
    MOVE WS-PREMIO-TOTAL TO RE-PREMIO-TOTAL.
    MOVE WS-IMPORTANCIA-SEGURADA TO RE-IMPORTANCIA-SEGURADA.
    MOVE PRODUTO-RAMO-SUSEP TO RE-RAMO-SUSEP.

    *> Determinar tipo de cálculo baseado no produto
    IF PRODUTO-TIPO-RESSEGURO = 'PROP'
        SET RE-CALC-PROPORCIONAL TO TRUE
    ELSE IF PRODUTO-TIPO-RESSEGURO = 'EXCD'
        SET RE-CALC-EXCEDENTE TO TRUE
    ELSE
        SET RE-CALC-NAO-PROPORCIONAL TO TRUE
    END-IF.

    *> Chamar módulo
    CALL 'RE0001S' USING
        BY REFERENCE RE0001S-ENTRADA
        BY REFERENCE RE0001S-SAIDA
        BY REFERENCE RE0001S-RETORNO.

    *> Validar retorno
    IF NOT RE-SUCESSO
        MOVE RE-MENSAGEM-ERRO TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-MODULO
    END-IF.

    *> Processar resultado
    MOVE RE-PREMIO-RETIDO TO WS-PREMIO-RETIDO.
    MOVE RE-PREMIO-CEDIDO TO WS-PREMIO-CEDIDO.

    *> Gerar registros para cada ressegurador
    PERFORM VARYING WS-IDX FROM 1 BY 1
        UNTIL WS-IDX > RE-QTD-RESSEGURADORES
        PERFORM R4600-GERAR-REGISTRO-RESSEGURO
    END-PERFORM.
```

### Migração .NET

**Interface**:

```csharp
public interface IReinsuranceService
{
    Task<ReinsuranceCalculation> CalculateAsync(ReinsuranceRequest request);
}

public class ReinsuranceRequest
{
    public int CompanyCode { get; set; }
    public string PolicyNumber { get; set; }
    public decimal TotalPremium { get; set; }
    public decimal InsuredAmount { get; set; }
    public int SusepBranch { get; set; }
    public ReinsuranceCalculationType CalculationType { get; set; }
}

public enum ReinsuranceCalculationType
{
    Proportional,      // Proporcional
    SurplusShare,      // Excedente
    NonProportional    // Não-proporcional
}

public class ReinsuranceCalculation
{
    public decimal RetainedPremium { get; set; }
    public decimal CededPremium { get; set; }
    public decimal CededPercentage { get; set; }
    public List<ReinsurerParticipation> Reinsurers { get; set; }
}

public class ReinsurerParticipation
{
    public int ReinsurerCode { get; set; }
    public string ReinsurerName { get; set; }
    public decimal CededPremium { get; set; }
    public decimal Percentage { get; set; }
}
```

**Implementação**:

```csharp
public class ReinsuranceService : IReinsuranceService
{
    private readonly ILogger<ReinsuranceService> _logger;
    private readonly IReinsuranceRepository _repository;

    // Limites de retenção por ramo SUSEP
    private static readonly Dictionary<int, decimal> RetentionLimits = new()
    {
        { 531, 1000000m },   // Vida Individual: R$ 1.000.000
        { 532, 5000000m },   // Vida em Grupo: R$ 5.000.000
        { 553, 500000m },    // Acidentes Pessoais: R$ 500.000
        { 571, 10000000m }   // Previdência: R$ 10.000.000
    };

    public async Task<ReinsuranceCalculation> CalculateAsync(
        ReinsuranceRequest request)
    {
        // Buscar limite de retenção
        var retentionLimit = GetRetentionLimit(request.SusepBranch);

        // Calcular valores
        var calculation = request.CalculationType switch
        {
            ReinsuranceCalculationType.Proportional =>
                CalculateProportional(request, retentionLimit),

            ReinsuranceCalculationType.SurplusShare =>
                CalculateSurplusShare(request, retentionLimit),

            ReinsuranceCalculationType.NonProportional =>
                CalculateNonProportional(request, retentionLimit),

            _ => throw new ArgumentException("Tipo de cálculo inválido")
        };

        // Distribuir entre resseguradores
        await DistributeToReinsurersAsync(calculation, request);

        return calculation;
    }

    private ReinsuranceCalculation CalculateProportional(
        ReinsuranceRequest request,
        decimal retentionLimit)
    {
        // Resseguro proporcional: percentual fixo
        const decimal RETENTION_PERCENTAGE = 0.80m; // 80% retenção

        var retainedPremium = request.TotalPremium * RETENTION_PERCENTAGE;
        var cededPremium = request.TotalPremium * (1 - RETENTION_PERCENTAGE);

        return new ReinsuranceCalculation
        {
            RetainedPremium = retainedPremium,
            CededPremium = cededPremium,
            CededPercentage = (1 - RETENTION_PERCENTAGE) * 100,
            Reinsurers = new List<ReinsurerParticipation>()
        };
    }

    private ReinsuranceCalculation CalculateSurplusShare(
        ReinsuranceRequest request,
        decimal retentionLimit)
    {
        // Resseguro por excedente: valor acima do limite
        decimal retainedPremium;
        decimal cededPremium;

        if (request.InsuredAmount <= retentionLimit)
        {
            // Dentro do limite: retenção total
            retainedPremium = request.TotalPremium;
            cededPremium = 0m;
        }
        else
        {
            // Acima do limite: ceder proporcionalmente
            var excessRatio = (request.InsuredAmount - retentionLimit) /
                request.InsuredAmount;

            retainedPremium = request.TotalPremium * (1 - excessRatio);
            cededPremium = request.TotalPremium * excessRatio;
        }

        return new ReinsuranceCalculation
        {
            RetainedPremium = retainedPremium,
            CededPremium = cededPremium,
            CededPercentage = request.TotalPremium > 0
                ? (cededPremium / request.TotalPremium) * 100
                : 0,
            Reinsurers = new List<ReinsurerParticipation>()
        };
    }

    private async Task DistributeToReinsurersAsync(
        ReinsuranceCalculation calculation,
        ReinsuranceRequest request)
    {
        if (calculation.CededPremium <= 0)
            return;

        // Buscar resseguradores ativos
        var reinsurers = await _repository.GetActiveReinsurersAsync(
            request.CompanyCode,
            request.SusepBranch);

        if (!reinsurers.Any())
        {
            throw new BusinessRuleException(
                "Nenhum ressegurador ativo encontrado");
        }

        // Distribuir proporcionalmente
        var totalCapacity = reinsurers.Sum(r => r.Capacity);

        foreach (var reinsurer in reinsurers)
        {
            var participationPercentage = reinsurer.Capacity / totalCapacity;
            var cededPremium = calculation.CededPremium * participationPercentage;

            calculation.Reinsurers.Add(new ReinsurerParticipation
            {
                ReinsurerCode = reinsurer.Code,
                ReinsurerName = reinsurer.Name,
                CededPremium = cededPremium,
                Percentage = participationPercentage * 100
            });
        }
    }

    private decimal GetRetentionLimit(int susepBranch)
    {
        if (RetentionLimits.TryGetValue(susepBranch, out var limit))
            return limit;

        // Default para ramos não mapeados
        return 1000000m;
    }
}
```

---

## GE0009S - Módulo de Formatação

### Propósito

Formatar campos numéricos e alfanuméricos para saída em arquivos fixed-width (PREMIT.TXT, PREMCED.TXT). Garante padding correto, alinhamento e conversão de tipos.

### Interface COBOL

**Área de Entrada**:

```cobol
01  GE0009S-ENTRADA.
    05  GE-TIPO-FORMATO         PIC X(1).
        88  GE-FORMATO-NUMERICO    VALUE 'N'.
        88  GE-FORMATO-ALFANUMERICO VALUE 'A'.
        88  GE-FORMATO-DATA        VALUE 'D'.
        88  GE-FORMATO-MOEDA       VALUE 'M'.
    05  GE-VALOR-ENTRADA        PIC X(50).
    05  GE-TAMANHO-SAIDA        PIC 9(3).
    05  GE-CASAS-DECIMAIS       PIC 9(2).
    05  GE-CARACTERE-PREENCHIMENTO PIC X(1).
```

**Área de Saída**:

```cobol
01  GE0009S-SAIDA.
    05  GE-VALOR-FORMATADO      PIC X(100).
```

### Chamadas no RG1866B

**Exemplo 1: Formatar Prêmio (R4000-FORMATAR-PREMIT)**:

```cobol
R4000-FORMATAR-PREMIT.
    *> Formatar prêmio total (15 posições, 2 decimais)
    SET GE-FORMATO-MOEDA TO TRUE.
    MOVE WS-PREMIO-TOTAL TO GE-VALOR-ENTRADA.
    MOVE 15 TO GE-TAMANHO-SAIDA.
    MOVE 2 TO GE-CASAS-DECIMAIS.
    MOVE '0' TO GE-CARACTERE-PREENCHIMENTO.

    CALL 'GE0009S' USING
        BY REFERENCE GE0009S-ENTRADA
        BY REFERENCE GE0009S-SAIDA.

    MOVE GE-VALOR-FORMATADO TO PREMIT-PREMIO-TOTAL.
```

**Exemplo 2: Formatar Número de Apólice**:

```cobol
R4010-FORMATAR-APOLICE.
    SET GE-FORMATO-ALFANUMERICO TO TRUE.
    MOVE WS-NUM-APOLICE TO GE-VALOR-ENTRADA.
    MOVE 20 TO GE-TAMANHO-SAIDA.
    MOVE SPACES TO GE-CARACTERE-PREENCHIMENTO.

    CALL 'GE0009S' USING
        BY REFERENCE GE0009S-ENTRADA
        BY REFERENCE GE0009S-SAIDA.

    MOVE GE-VALOR-FORMATADO TO PREMIT-NUM-APOLICE.
```

### Migração .NET

**Interface**:

```csharp
public interface IFixedWidthFormatter
{
    string FormatNumeric(decimal value, int totalWidth, int decimalPlaces);
    string FormatAlphanumeric(string value, int width);
    string FormatDate(DateTime date, string format, int width);
    string FormatMoney(decimal amount, int totalWidth, int decimalPlaces);
}
```

**Implementação**:

```csharp
public class FixedWidthFormatter : IFixedWidthFormatter
{
    public string FormatNumeric(decimal value, int totalWidth, int decimalPlaces)
    {
        // Remove ponto decimal e preenche com zeros à esquerda
        var scaledValue = (long)(value * (decimal)Math.Pow(10, decimalPlaces));
        return scaledValue.ToString().PadLeft(totalWidth, '0');
    }

    public string FormatAlphanumeric(string value, int width)
    {
        // Trunca ou preenche com espaços à direita
        if (string.IsNullOrEmpty(value))
            return new string(' ', width);

        return value.Length > width
            ? value.Substring(0, width)
            : value.PadRight(width, ' ');
    }

    public string FormatDate(DateTime date, string format, int width)
    {
        var formatted = date.ToString(format);
        return FormatAlphanumeric(formatted, width);
    }

    public string FormatMoney(decimal amount, int totalWidth, int decimalPlaces)
    {
        // Mesmo que FormatNumeric, mas com validações específicas de moeda
        if (amount < 0)
        {
            throw new ArgumentException("Valores monetários não podem ser negativos");
        }

        return FormatNumeric(amount, totalWidth, decimalPlaces);
    }
}
```

**Uso no Serviço**:

```csharp
public class PremitRecordFormatter
{
    private readonly IFixedWidthFormatter _formatter;

    public string FormatPremitRecord(PremitRecord record)
    {
        var sb = new StringBuilder(1200); // Tamanho fixo PREMIT

        // Campos numéricos
        sb.Append(_formatter.FormatNumeric(record.CompanyCode, 5, 0));
        sb.Append(_formatter.FormatNumeric(record.SusepBranch, 4, 0));

        // Campos alfanuméricos
        sb.Append(_formatter.FormatAlphanumeric(record.PolicyNumber, 20));

        // Campos monetários
        sb.Append(_formatter.FormatMoney(record.TotalPremium, 15, 2));
        sb.Append(_formatter.FormatMoney(record.NetPremium, 15, 2));
        sb.Append(_formatter.FormatMoney(record.IOF, 13, 2));

        // Datas
        sb.Append(_formatter.FormatDate(record.IssueDate, "yyyyMMdd", 8));

        // Garantir 1200 bytes exatos
        var result = sb.ToString();
        if (result.Length != 1200)
        {
            throw new InvalidOperationException(
                $"Registro PREMIT com tamanho incorreto: {result.Length} bytes");
        }

        return result;
    }
}
```

---

## GE0010S - Módulo de Validação

### Propósito

Validar dados de entrada (CPF, CNPJ, datas, códigos) usando regras padrão da Caixa Seguradora. Centraliza lógica de validação para reuso.

### Interface COBOL

**Área de Entrada**:

```cobol
01  GE0010S-ENTRADA.
    05  GE-TIPO-VALIDACAO       PIC X(2).
        88  GE-VALIDAR-CPF         VALUE 'CP'.
        88  GE-VALIDAR-CNPJ        VALUE 'CN'.
        88  GE-VALIDAR-DATA        VALUE 'DT'.
        88  GE-VALIDAR-CODIGO      VALUE 'CD'.
    05  GE-VALOR-VALIDAR        PIC X(50).
    05  GE-PARAMETRO-VALIDACAO  PIC X(20).
```

**Área de Saída**:

```cobol
01  GE0010S-SAIDA.
    05  GE-VALIDACAO-OK         PIC X(1).
        88  GE-VALIDO              VALUE 'S'.
        88  GE-INVALIDO            VALUE 'N'.
    05  GE-MENSAGEM-VALIDACAO   PIC X(100).
```

### Chamadas no RG1866B

**Exemplo: Validar CPF do Segurado**:

```cobol
R1900-VALIDAR-CPF-SEGURADO.
    SET GE-VALIDAR-CPF TO TRUE.
    MOVE CLIENTE-CPF TO GE-VALOR-VALIDAR.

    CALL 'GE0010S' USING
        BY REFERENCE GE0010S-ENTRADA
        BY REFERENCE GE0010S-SAIDA.

    IF GE-INVALIDO
        MOVE GE-MENSAGEM-VALIDACAO TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.
```

### Migração .NET

**Interface**:

```csharp
public interface IValidationService
{
    ValidationResult ValidateCPF(string cpf);
    ValidationResult ValidateCNPJ(string cnpj);
    ValidationResult ValidateDate(DateTime date, DateValidationType type);
    ValidationResult ValidateCode(string code, string codeType);
}

public class ValidationResult
{
    public bool IsValid { get; set; }
    public string ErrorMessage { get; set; }

    public static ValidationResult Success() =>
        new ValidationResult { IsValid = true };

    public static ValidationResult Failure(string message) =>
        new ValidationResult { IsValid = false, ErrorMessage = message };
}
```

**Implementação**:

```csharp
public class ValidationService : IValidationService
{
    public ValidationResult ValidateCPF(string cpf)
    {
        if (string.IsNullOrWhiteSpace(cpf))
            return ValidationResult.Failure("CPF não informado");

        // Remover caracteres não numéricos
        cpf = new string(cpf.Where(char.IsDigit).ToArray());

        if (cpf.Length != 11)
            return ValidationResult.Failure("CPF deve conter 11 dígitos");

        // CPFs inválidos conhecidos
        if (cpf.All(c => c == cpf[0]))
            return ValidationResult.Failure("CPF com dígitos repetidos");

        // Calcular dígitos verificadores
        var digits = cpf.Select(c => int.Parse(c.ToString())).ToArray();

        // Primeiro dígito
        var sum1 = 0;
        for (int i = 0; i < 9; i++)
            sum1 += digits[i] * (10 - i);

        var remainder1 = sum1 % 11;
        var digit1 = remainder1 < 2 ? 0 : 11 - remainder1;

        if (digits[9] != digit1)
            return ValidationResult.Failure("CPF inválido (1º dígito)");

        // Segundo dígito
        var sum2 = 0;
        for (int i = 0; i < 10; i++)
            sum2 += digits[i] * (11 - i);

        var remainder2 = sum2 % 11;
        var digit2 = remainder2 < 2 ? 0 : 11 - remainder2;

        if (digits[10] != digit2)
            return ValidationResult.Failure("CPF inválido (2º dígito)");

        return ValidationResult.Success();
    }

    public ValidationResult ValidateCNPJ(string cnpj)
    {
        if (string.IsNullOrWhiteSpace(cnpj))
            return ValidationResult.Failure("CNPJ não informado");

        // Remover caracteres não numéricos
        cnpj = new string(cnpj.Where(char.IsDigit).ToArray());

        if (cnpj.Length != 14)
            return ValidationResult.Failure("CNPJ deve conter 14 dígitos");

        // CNPJ com dígitos repetidos
        if (cnpj.All(c => c == cnpj[0]))
            return ValidationResult.Failure("CNPJ com dígitos repetidos");

        // Algoritmo de validação CNPJ
        var digits = cnpj.Select(c => int.Parse(c.ToString())).ToArray();

        // Primeiro dígito
        var multipliers1 = new[] { 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2 };
        var sum1 = 0;
        for (int i = 0; i < 12; i++)
            sum1 += digits[i] * multipliers1[i];

        var remainder1 = sum1 % 11;
        var digit1 = remainder1 < 2 ? 0 : 11 - remainder1;

        if (digits[12] != digit1)
            return ValidationResult.Failure("CNPJ inválido (1º dígito)");

        // Segundo dígito
        var multipliers2 = new[] { 6, 5, 4, 3, 2, 9, 8, 7, 6, 5, 4, 3, 2 };
        var sum2 = 0;
        for (int i = 0; i < 13; i++)
            sum2 += digits[i] * multipliers2[i];

        var remainder2 = sum2 % 11;
        var digit2 = remainder2 < 2 ? 0 : 11 - remainder2;

        if (digits[13] != digit2)
            return ValidationResult.Failure("CNPJ inválido (2º dígito)");

        return ValidationResult.Success();
    }

    public ValidationResult ValidateDate(
        DateTime date,
        DateValidationType type)
    {
        return type switch
        {
            DateValidationType.NotFuture when date > DateTime.Today =>
                ValidationResult.Failure("Data não pode ser futura"),

            DateValidationType.NotPast when date < DateTime.Today =>
                ValidationResult.Failure("Data não pode ser passada"),

            DateValidationType.BusinessDay when !IsBusinessDay(date) =>
                ValidationResult.Failure("Data deve ser dia útil"),

            _ => ValidationResult.Success()
        };
    }

    private bool IsBusinessDay(DateTime date)
    {
        // Sábado ou domingo
        if (date.DayOfWeek == DayOfWeek.Saturday ||
            date.DayOfWeek == DayOfWeek.Sunday)
            return false;

        // Adicionar validação de feriados aqui
        // (omitido para brevidade)

        return true;
    }
}
```

---

## Estratégia de Migração

### Fase 1: Mapeamento de Módulos

| Módulo COBOL | Serviço .NET | Localização |
|--------------|--------------|-------------|
| RE0001S | `ReinsuranceService` | `CaixaSeguradora.Core/Services/` |
| GE0009S | `FixedWidthFormatter` | `CaixaSeguradora.Infrastructure/Formatters/` |
| GE0010S | `ValidationService` | `CaixaSeguradora.Core/Services/` |

### Fase 2: Dependency Injection

**Program.cs**:

```csharp
// Registrar serviços
builder.Services.AddScoped<IReinsuranceService, ReinsuranceService>();
builder.Services.AddSingleton<IFixedWidthFormatter, FixedWidthFormatter>();
builder.Services.AddScoped<IValidationService, ValidationService>();
```

### Fase 3: Testes de Compatibilidade

**Estratégia**:
1. Capturar inputs/outputs de chamadas COBOL reais
2. Executar mesmos inputs nos serviços .NET
3. Comparar outputs byte-a-byte
4. Validar 100% de compatibilidade

---

## Testes de Integração

### Teste de Resseguro

```csharp
[Fact]
public async Task ReinsuranceCalculation_ShouldMatch_COBOLOutput()
{
    // Arrange: capturado do mainframe
    var request = new ReinsuranceRequest
    {
        CompanyCode = 1,
        PolicyNumber = "000000012345678",
        TotalPremium = 5000000.00m,
        InsuredAmount = 10000000.00m,
        SusepBranch = 531,
        CalculationType = ReinsuranceCalculationType.SurplusShare
    };

    // Act
    var result = await _service.CalculateAsync(request);

    // Assert: valores esperados do COBOL
    Assert.Equal(1000000.00m, result.RetainedPremium); // Limite de retenção
    Assert.Equal(4000000.00m, result.CededPremium);
    Assert.Equal(80.00m, result.CededPercentage);
}
```

### Teste de Formatação

```csharp
[Theory]
[InlineData(12345.67, 15, 2, "000000001234567")] // COBOL output
[InlineData(0.00, 15, 2, "000000000000000")]
[InlineData(999999999999.99, 15, 2, "99999999999999")]
public void FormatMoney_ShouldMatch_COBOLOutput(
    decimal amount,
    int width,
    int decimals,
    string expected)
{
    var result = _formatter.FormatMoney(amount, width, decimals);
    Assert.Equal(expected, result);
}
```

### Teste de Validação

```csharp
[Theory]
[InlineData("12345678909", true)]  // CPF válido
[InlineData("00000000000", false)] // Dígitos repetidos
[InlineData("123", false)]         // Tamanho incorreto
public void ValidateCPF_ShouldMatch_COBOLBehavior(
    string cpf,
    bool expectedValid)
{
    var result = _service.ValidateCPF(cpf);
    Assert.Equal(expectedValid, result.IsValid);
}
```

---

## Referências

- **Lógica de Negócio**: `docs/legacy-system/05-business-logic.md`
- **Estruturas de Dados**: `docs/legacy-system/03-data-structures.md`
- **IBM COBOL CALL Statement**: Enterprise COBOL Programming Guide
- **Clean Architecture**: Uncle Bob Martin's Clean Architecture

---

**Documento criado em**: 2025-10-27
**Última atualização**: 2025-10-27
**Versão**: 1.0
