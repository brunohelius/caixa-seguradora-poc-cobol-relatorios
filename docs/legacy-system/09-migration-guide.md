# 09 - Migration Guide

[← Voltar ao Índice](README.md)

## Índice

- [Visão Geral](#visão-geral)
- [Complexidades Técnicas Críticas](#complexidades-técnicas-críticas)
- [Riscos e Mitigações](#riscos-e-mitigações)
- [Estratégia de Validação](#estratégia-de-validação)
- [Checklist de Migração](#checklist-de-migração)
- [Plano de Rollback](#plano-de-rollback)

---

## Visão Geral

A migração do programa RG1866B de COBOL/Mainframe para .NET 9 apresenta desafios únicos devido à natureza **regulatória crítica** do sistema (SUSEP Circular 360). Este guia documenta todas as complexidades técnicas, riscos e estratégias de mitigação.

### Complexidade Geral

| Aspecto | Nível de Complexidade | Justificativa |
|---------|----------------------|---------------|
| **Precisão Decimal** | ⚠️⚠️⚠️ CRÍTICO | COMP-3 → decimal: tolerância zero |
| **Lógica de Negócio** | ⚠️⚠️⚠️ ALTO | 147+ regras, 6 tipos de movimento |
| **Módulos Externos** | ⚠️⚠️ MÉDIO | 3 módulos (RE0001S, GE0009S, GE0010S) |
| **Formatação Fixed-Width** | ⚠️⚠️⚠️ CRÍTICO | Byte-a-byte match obrigatório |
| **Database Migration** | ⚠️⚠️ MÉDIO | DB2 → SQL Server/SQLite |
| **Agendamento** | ⚠️ BAIXO | TWS → Hangfire |
| **Monitoramento** | ⚠️ BAIXO | SDSF → Application Insights |

### Criticidade Regulatória

**Exigência SUSEP**: Arquivos PREMIT.TXT e PREMCED.TXT devem ser **byte-a-byte idênticos** aos gerados pelo COBOL durante período de validação paralela (mínimo 3 meses).

**Penalidades por Divergência**:
- 1ª divergência: Multa de R$ 50.000
- 2ª divergência: Multa de R$ 200.000
- 3ª divergência: Suspensão de operar

---

## Complexidades Técnicas Críticas

### 1. Precisão Decimal (COMP-3 vs decimal)

#### **Complexidade**: ⚠️⚠️⚠️ CRÍTICO

**Problema**:
COBOL COMP-3 (packed decimal) armazena valores com precisão exata. C# `decimal` usa representação binária diferente que pode causar diferenças em operações de arredondamento.

**Exemplo do Problema**:

```cobol
*> COBOL (COMP-3)
01  WS-PREMIO-TOTAL    PIC 9(15)V99 COMP-3 VALUE 1234567890.12.
01  WS-TAXA            PIC 9(1)V9999 COMP-3 VALUE 0.0538.
01  WS-RESULTADO       PIC 9(15)V99 COMP-3.

COMPUTE WS-RESULTADO = WS-PREMIO-TOTAL * WS-TAXA.
*> Resultado COBOL: 66419872.78656 → arredondado para 66419872.79
```

```csharp
// C# (decimal)
decimal premioTotal = 1234567890.12m;
decimal taxa = 0.0538m;
decimal resultado = premioTotal * taxa;
// Resultado C#: 66419872.786456m → arredondado para 66419872.79

// ✅ MAS: dependendo da operação, pode dar 66419872.78 (diferença de 1 centavo)
```

**Mitigação**:

```csharp
public class CobolDecimalCalculator
{
    // Replicar comportamento COMP-3 ROUNDED
    public static decimal ComputeRounded(decimal value1, decimal value2,
        int decimalPlaces = 2)
    {
        var result = value1 * value2;

        // Usar MidpointRounding.AwayFromZero (comportamento COBOL ROUNDED)
        return Math.Round(result, decimalPlaces, MidpointRounding.AwayFromZero);
    }
}

// Uso:
decimal resultado = CobolDecimalCalculator.ComputeRounded(
    premioTotal, taxa, decimalPlaces: 2);
```

**Validação Obrigatória**:
- Criar 1000+ casos de teste com valores extremos
- Comparar saída COBOL vs .NET byte-a-byte
- Testar edge cases: valores muito pequenos (< 0.01), muito grandes (> 1 bilhão)

---

### 2. Formatação Fixed-Width (LRECL=1200)

#### **Complexidade**: ⚠️⚠️⚠️ CRÍTICO

**Problema**:
Arquivos PREMIT.TXT e PREMCED.TXT têm layout fixed-width com regras específicas de padding, alinhamento e conversão.

**Exemplo do Problema**:

```cobol
*> COBOL: Formatação de prêmio (15 posições, 2 decimais implícitos)
05  PREMIT-PREMIO-TOTAL  PIC 9(15) VALUE 000000001234567.
*> Saída: "000000001234567" (sem ponto decimal)
```

```csharp
// ❌ ERRADO (C# naive approach):
string formatted = totalPremium.ToString("000000000000000");
// Resultado: "000000001234567.00" (17 caracteres, contém ponto decimal!)

// ✅ CORRETO (replicar comportamento COBOL):
long scaledValue = (long)(totalPremium * 100); // Multiplicar por 10^2
string formatted = scaledValue.ToString().PadLeft(15, '0');
// Resultado: "000000001234567" (15 caracteres, sem ponto decimal)
```

**Armadilhas Comuns**:

1. **Valores Negativos**:
```cobol
*> COBOL: Sinal na última posição (overpunch)
PREMIT-VALOR PIC S9(13)V99 VALUE -12345.67.
*> Saída: "000000001234567}" (último dígito 7 → } indica negativo)
```

```csharp
// C# precisa replicar overpunch notation
public static string FormatSignedCobol(decimal value, int totalWidth)
{
    bool isNegative = value < 0;
    long absoluteValue = Math.Abs((long)(value * 100));
    string digits = absoluteValue.ToString().PadLeft(totalWidth, '0');

    if (isNegative)
    {
        // Overpunch: último dígito + 16 na tabela ASCII
        char lastDigit = digits[totalWidth - 1];
        char overpunched = (char)(lastDigit + 16); // '0'→'p', '1'→'q', ..., '7'→'}'
        digits = digits.Substring(0, totalWidth - 1) + overpunched;
    }

    return digits;
}
```

2. **Campos Alfanuméricos (Padding à Direita)**:
```cobol
*> COBOL: PIC X(20) padded com espaços à direita
05  PREMIT-NUM-APOLICE  PIC X(20) VALUE "ABC123".
*> Saída: "ABC123              " (20 caracteres)
```

```csharp
// C# deve preencher com espaços à direita (não à esquerda!)
string formatted = policyNumber.PadRight(20, ' ');
```

**Mitigação**:
- Implementar `FixedWidthFormatter` que replica **exatamente** o comportamento COBOL
- Testar com 100% dos casos de teste do COBOL (arquivos sample)
- Validação SHA-256 checksum dos arquivos gerados

---

### 3. Conversão de Datas (COBOL vs .NET)

#### **Complexidade**: ⚠️⚠️ MÉDIO

**Problema**:
COBOL armazena datas como `PIC 9(8)` (YYYYMMDD) ou usa funções intrínsecas como `INTEGER-OF-DATE`.

**Exemplo do Problema**:

```cobol
*> COBOL: Calcular dias entre datas
COMPUTE WS-DIAS-VIGENCIA =
    FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-FIM) -
    FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-INI).
*> INTEGER-OF-DATE converte YYYYMMDD para "dias desde 01/01/1601"
```

```csharp
// C# equivalente
public static int CalculateDaysDifference(DateTime endDate, DateTime startDate)
{
    // Simples: usar TimeSpan
    return (endDate - startDate).Days;
}

// MAS ATENÇÃO: INTEGER-OF-DATE do COBOL tem base 01/01/1601
// Se for usar conversão direta, precisa ajustar:
public static int CobolIntegerOfDate(DateTime date)
{
    DateTime cobolEpoch = new DateTime(1601, 1, 1);
    return (date - cobolEpoch).Days;
}
```

**Armadilhas**:
- COBOL aceita datas inválidas (ex: 20251332) sem erro → .NET lança exception
- COBOL `CURRENT-DATE` retorna 21 bytes (YYYY-MM-DD-HH.MM.SS.NN+HH.MM) → .NET DateTime tem formato diferente

**Mitigação**:
```csharp
public static DateTime ParseCobolDate(string cobolDate, bool throwOnError = false)
{
    // COBOL: PIC 9(8) formato YYYYMMDD
    if (cobolDate.Length != 8)
    {
        if (throwOnError) throw new FormatException("Data inválida");
        return DateTime.MinValue;
    }

    int year = int.Parse(cobolDate.Substring(0, 4));
    int month = int.Parse(cobolDate.Substring(4, 2));
    int day = int.Parse(cobolDate.Substring(6, 2));

    // Validar como COBOL faz (aceita valores inválidos)
    if (month > 12) month = 12;
    if (day > DateTime.DaysInMonth(year, month))
        day = DateTime.DaysInMonth(year, month);

    return new DateTime(year, month, day);
}
```

---

### 4. Cursores DB2 vs IAsyncEnumerable

#### **Complexidade**: ⚠️⚠️ MÉDIO

**Problema**:
COBOL usa cursores DB2 com fetch explícito. .NET usa `IAsyncEnumerable<T>` com comportamento lazy loading.

**Exemplo do Problema**:

```cobol
*> COBOL: Cursor com FETCH explícito
EXEC SQL
    DECLARE CURSOR-PREMIOS CURSOR FOR
    SELECT COD_CIA, NUM_APOLICE, PREMIO_TOTAL
    FROM V0PREMIOS
    WHERE DATA_PROCESSAMENTO = :WS-DATA-PROCESSAMENTO
END-EXEC.

EXEC SQL OPEN CURSOR-PREMIOS END-EXEC.

PERFORM UNTIL WS-FIM-CURSOR = 'S'
    EXEC SQL
        FETCH CURSOR-PREMIOS
        INTO :WS-COD-CIA, :WS-NUM-APOLICE, :WS-PREMIO-TOTAL
    END-EXEC

    IF SQLCODE = 100
        MOVE 'S' TO WS-FIM-CURSOR
    ELSE
        PERFORM PROCESSAR-PREMIO
    END-IF
END-PERFORM.

EXEC SQL CLOSE CURSOR-PREMIOS END-EXEC.
```

```csharp
// .NET equivalente (comportamento diferente!)
public async IAsyncEnumerable<Premium> GetPremiumsAsync(
    DateTime processingDate,
    [EnumeratorCancellation] CancellationToken ct = default)
{
    var query = _context.Premiums
        .AsNoTracking()
        .Where(p => p.ProcessingDate == processingDate);

    // ⚠️ Query é lazy! Não executa até consumir o enumerator
    await foreach (var premium in query.AsAsyncEnumerable().WithCancellation(ct))
    {
        yield return premium; // Fetch on-demand
    }
}

// Uso:
await foreach (var premium in _repo.GetPremiumsAsync(date))
{
    await ProcessPremiumAsync(premium); // Processa um a um (como COBOL)
}
```

**Armadilhas**:
1. **Ordem de Registros**: COBOL `ORDER BY` deve ser replicado exatamente
2. **Timeouts**: COBOL timeout de 120s → configurar em .NET
3. **Connection Pooling**: COBOL mantém conexão aberta → .NET fecha/reabre

**Mitigação**:
```csharp
// Configurar timeout e ordering explicitamente
var query = _context.Premiums
    .AsNoTracking()
    .Where(p => p.ProcessingDate == processingDate)
    .OrderBy(p => p.PolicyNumber)       // ⚠️ Mesma ordem do COBOL
    .ThenBy(p => p.EndorsementNumber);  // ⚠️ Mesma ordem do COBOL

// Configurar timeout (EF Core)
_context.Database.SetCommandTimeout(TimeSpan.FromSeconds(120));
```

---

### 5. Módulos Externos (CALL Statement)

#### **Complexidade**: ⚠️⚠️ MÉDIO

**Problema**:
COBOL chama módulos externos (RE0001S, GE0009S, GE0010S) via `CALL` statement com áreas de linkage. .NET usa interfaces e dependency injection.

**Exemplo do Problema**:

```cobol
*> COBOL: Chamar módulo externo
CALL 'RE0001S' USING
    BY REFERENCE RE0001S-ENTRADA
    BY REFERENCE RE0001S-SAIDA
    BY REFERENCE RE0001S-RETORNO.

IF NOT RE-SUCESSO
    PERFORM R9000-TRATAR-ERRO-MODULO
END-IF.
```

```csharp
// .NET: Interface + Dependency Injection
public interface IReinsuranceService
{
    Task<ReinsuranceCalculation> CalculateAsync(ReinsuranceRequest request);
}

// Uso:
var request = new ReinsuranceRequest
{
    PolicyNumber = policyNumber,
    TotalPremium = totalPremium,
    // ...
};

var calculation = await _reinsuranceService.CalculateAsync(request);

if (!calculation.Success)
{
    throw new BusinessRuleException(calculation.ErrorMessage);
}
```

**Armadilha**: COBOL módulos são **stateless** (cada CALL é independente). .NET services podem ter state → garantir stateless.

**Mitigação**:
```csharp
// Registrar como Scoped (não Singleton) para evitar state compartilhado
builder.Services.AddScoped<IReinsuranceService, ReinsuranceService>();
```

---

## Riscos e Mitigações

### Matriz de Riscos

| ID | Risco | Probabilidade | Impacto | Severidade | Mitigação |
|----|-------|---------------|---------|------------|-----------|
| **R01** | Divergência COBOL vs .NET (decimal) | ALTA | CRÍTICO | 🔴 CRÍTICO | Testes de comparação byte-a-byte (1000+ casos) |
| **R02** | Perda de regras de negócio | MÉDIA | CRÍTICO | 🔴 CRÍTICO | Documentação completa + code review por SME |
| **R03** | Performance degradada | MÉDIA | ALTO | 🟡 ALTO | Benchmarks pré/pós migração + otimizações |
| **R04** | Erros de formatação fixed-width | ALTA | CRÍTICO | 🔴 CRÍTICO | Formatter testado com 100% dos samples COBOL |
| **R05** | Falha no agendamento mensal | BAIXA | ALTO | 🟡 MÉDIO | Testes de integração Hangfire + monitoramento |
| **R06** | Incompatibilidade DB2 → SQL Server | MÉDIA | MÉDIO | 🟡 MÉDIO | Testes com dados reais + scripts de migração |
| **R07** | Falta de conhecimento do time | ALTA | MÉDIO | 🟡 MÉDIO | Treinamento + documentação técnica completa |
| **R08** | Rollback complexo | BAIXA | CRÍTICO | 🔴 ALTO | Plano de rollback detalhado + testes |

---

### R01: Divergência Decimal (CRÍTICO)

**Descrição**: Cálculos financeiros .NET divergem do COBOL por problemas de arredondamento/precisão.

**Impacto**: Penalidades SUSEP (R$ 50.000 a R$ 200.000), suspensão de operar.

**Mitigação**:

1. **Fase 1: Análise**
   - Identificar todos os `COMPUTE` e operações aritméticas no COBOL (147 ocorrências)
   - Documentar regras de arredondamento (ROUNDED vs truncamento)

2. **Fase 2: Implementação**
   - Criar `CobolDecimalCalculator` que replica comportamento COMP-3
   - Usar `decimal` (nunca `float` ou `double`)
   - Aplicar `Math.Round(MidpointRounding.AwayFromZero)` consistentemente

3. **Fase 3: Validação**
   - Criar 1000+ casos de teste (valores extremos, edge cases)
   - Executar COBOL e .NET em paralelo
   - Comparar resultados byte-a-byte (SHA-256 checksum)
   - Tolerância: **ZERO BYTES DE DIFERENÇA**

**Critério de Aceitação**: 100% dos testes passando por 3 meses consecutivos em paralelo.

---

### R02: Perda de Regras de Negócio (CRÍTICO)

**Descrição**: Regras de negócio implementadas em COBOL não são migradas ou são mal interpretadas.

**Impacto**: Cálculos incorretos, não conformidade regulatória.

**Mitigação**:

1. **Documentação Completa**
   - ✅ Todas as 63 seções COBOL documentadas (`05-business-logic.md`)
   - ✅ 147+ regras de negócio identificadas
   - ✅ 6 tipos de movimento (101-106) com fórmulas

2. **Code Review Especializado**
   - Revisão por Subject Matter Experts (SMEs) de negócio
   - Checklist de validação para cada regra
   - Aprovação formal antes do deploy

3. **Testes de Aceitação**
   - Criar cenários de teste cobrindo todas as regras
   - Validação com área de negócio
   - Testes com dados reais (anonimizados)

**Critério de Aceitação**: Aprovação formal de SMEs + 100% de cobertura de testes de regras de negócio.

---

### R04: Formatação Fixed-Width (CRÍTICO)

**Descrição**: Arquivos PREMIT.TXT/PREMCED.TXT gerados pelo .NET diferem do COBOL.

**Impacto**: Rejeição pela SUSEP, penalidades financeiras.

**Mitigação**:

1. **Implementação Rigorosa**
   ```csharp
   // FixedWidthFormatter testado com 100% dos samples COBOL
   public class FixedWidthFormatter : IFixedWidthFormatter
   {
       // Implementação documentada em 06-external-modules.md
   }
   ```

2. **Validação Automática**
   ```csharp
   [Fact]
   public void PremitFile_ShouldMatchCOBOL_ByteForByte()
   {
       // Comparar com arquivo COBOL sample
       var cobolOutput = File.ReadAllBytes("TestData/PREMIT_COBOL.TXT");
       var dotnetOutput = File.ReadAllBytes("TestData/PREMIT_DOTNET.TXT");

       Assert.Equal(cobolOutput.Length, dotnetOutput.Length);
       Assert.True(cobolOutput.SequenceEqual(dotnetOutput));
   }
   ```

3. **Checksum Validation**
   ```csharp
   public static string CalculateSHA256(string filePath)
   {
       using var sha256 = SHA256.Create();
       using var stream = File.OpenRead(filePath);
       var hash = sha256.ComputeHash(stream);
       return BitConverter.ToString(hash).Replace("-", "");
   }

   // Validar:
   Assert.Equal(cobolChecksum, dotnetChecksum);
   ```

**Critério de Aceitação**: SHA-256 checksum idêntico por 3 meses em produção paralela.

---

## Estratégia de Validação

### Execução Paralela (Shadow Mode)

**Duração**: Mínimo 3 meses (idealmente 6 meses)

**Processo**:

```text
┌─────────────────────────────────────────────────┐
│  1º Dia Útil do Mês (03:00 AM)                  │
└─────────────────────────────────────────────────┘
                    │
        ┌───────────┴───────────┐
        │                       │
        ▼                       ▼
┌───────────────┐       ┌───────────────┐
│  COBOL (PROD) │       │  .NET (TEST)  │
│  RG1866B      │       │  API/Service  │
└───────┬───────┘       └───────┬───────┘
        │                       │
        │ Gera                  │ Gera
        ▼                       ▼
┌────────────────┐      ┌────────────────┐
│ PREMIT.TXT     │      │ PREMIT_NET.TXT │
│ (produção)     │      │ (teste)        │
└────────┬───────┘      └───────┬────────┘
         │                      │
         └──────────┬───────────┘
                    │
                    ▼
          ┌─────────────────┐
          │ File Comparator │
          │ (SHA-256)       │
          └────────┬────────┘
                   │
       ┌───────────┴───────────┐
       │                       │
       ▼                       ▼
  ✅ Match                 ❌ Difference
  (continuar)              (alert + análise)
```

**Critério de Sucesso**:
- 3 meses consecutivos: 100% match (0 divergências)
- 0 alertas críticos
- Performance dentro do SLA (< 60min)

---

### Testes de Comparação

#### 1. Unit Tests (Cálculos)

```csharp
[Theory]
[InlineData(101, 1250.50, 1393.05)] // Emissão
[InlineData(102, 500.00, 525.00)]   // Endosso Aumento
[InlineData(103, -500.00, -525.00)] // Endosso Redução
[InlineData(104, -1393.05, -1393.05)] // Cancelamento
public async Task PremiumCalculation_ShouldMatchCOBOL(
    int movementType,
    decimal expectedNet,
    decimal expectedGross)
{
    // Arrange: dados capturados do COBOL
    var premium = new Premium { MovementType = movementType, /* ... */ };

    // Act
    var result = await _service.CalculatePremiumAsync(premium, _policy, _product);

    // Assert
    Assert.Equal(expectedNet, result.NetPremium);
    Assert.Equal(expectedGross, result.GrossPremium);
}
```

#### 2. Integration Tests (Formatação)

```csharp
[Fact]
public async Task PremitRecord_ShouldMatchCOBOL_ExactFormat()
{
    // Arrange: record de teste
    var record = new PremitRecord
    {
        CompanyCode = 1,
        PolicyNumber = "12345678",
        TotalPremium = 1234567.89m,
        // ...
    };

    // Act
    string formatted = _formatter.FormatPremitRecord(record);

    // Assert: comparar com output COBOL esperado
    string expectedCobol = LoadCobolSample("PREMIT_SAMPLE_001.txt");
    Assert.Equal(expectedCobol, formatted);
    Assert.Equal(1200, formatted.Length); // LRECL=1200
}
```

#### 3. End-to-End Tests (Arquivo Completo)

```csharp
[Fact]
public async Task FullReport_ShouldMatchCOBOL_FileChecksum()
{
    // Arrange: gerar relatório completo
    var parameters = new ReportParameters
    {
        ProcessingDate = new DateTime(2025, 10, 1),
        CompanyCode = 1
    };

    // Act: executar .NET
    await _service.GenerateReportAsync(parameters);

    // Assert: comparar checksum
    string dotnetChecksum = CalculateSHA256("output/PREMIT.TXT");
    string cobolChecksum = LoadCobolChecksum("PREMIT_202510_CHECKSUM.txt");

    Assert.Equal(cobolChecksum, dotnetChecksum);
}
```

---

## Checklist de Migração

### Fase 1: Preparação (Semanas 1-2)

- [ ] **Setup Ambiente**
  - [ ] Provisionar SQL Server/SQLite
  - [ ] Configurar CI/CD pipeline
  - [ ] Setup Application Insights
  - [ ] Criar ambientes: DEV, QA, STAGING, PROD

- [ ] **Análise de Código**
  - [ ] Revisar documentação completa (docs/legacy-system/)
  - [ ] Identificar todas as regras de negócio (147+)
  - [ ] Mapear dependências externas (3 módulos)

- [ ] **Preparação de Dados**
  - [ ] Exportar dados de teste do DB2
  - [ ] Carregar em SQL Server/SQLite
  - [ ] Validar integridade referencial

### Fase 2: Desenvolvimento (Semanas 3-8)

- [ ] **Core Implementation**
  - [ ] Implementar entidades (15 classes)
  - [ ] Implementar repositórios (7 repositórios)
  - [ ] Implementar serviços de negócio (5 serviços)
  - [ ] Implementar cálculos (6 tipos de movimento)

- [ ] **Módulos Externos**
  - [ ] Migrar RE0001S → ReinsuranceService
  - [ ] Migrar GE0009S → FixedWidthFormatter
  - [ ] Migrar GE0010S → ValidationService

- [ ] **Fixed-Width Output**
  - [ ] Implementar PremitRecordFormatter (1200 bytes)
  - [ ] Implementar PremcedRecordFormatter (800 bytes)
  - [ ] Validar com 100% dos samples COBOL

### Fase 3: Testes (Semanas 9-10)

- [ ] **Unit Tests**
  - [ ] Cobertura > 90% em Core/Services
  - [ ] Todos os cálculos com casos de teste COBOL
  - [ ] 1000+ casos de teste de precisão decimal

- [ ] **Integration Tests**
  - [ ] Testes de repositório com dados reais
  - [ ] Testes de formatação (byte-a-byte)
  - [ ] Testes de módulos externos

- [ ] **E2E Tests**
  - [ ] Geração completa de relatório
  - [ ] Validação SHA-256 checksum
  - [ ] Performance benchmarks

### Fase 4: Shadow Mode (Meses 1-3)

- [ ] **Mês 1**
  - [ ] Executar .NET em paralelo com COBOL
  - [ ] Comparar outputs diariamente
  - [ ] Corrigir divergências (se houver)
  - [ ] Meta: 0 divergências

- [ ] **Mês 2**
  - [ ] Continuar execução paralela
  - [ ] Validar com dados de volumes variados
  - [ ] Performance tuning
  - [ ] Meta: 100% match + performance dentro SLA

- [ ] **Mês 3**
  - [ ] Validação final com stakeholders
  - [ ] Aprovação formal de SMEs
  - [ ] Preparar plano de cutover
  - [ ] Meta: Aprovação go-live

### Fase 5: Go-Live (Semana 14)

- [ ] **Pré-Go-Live**
  - [ ] Backup completo do sistema COBOL
  - [ ] Testar plano de rollback
  - [ ] Comunicação a stakeholders (SUSEP, áreas internas)
  - [ ] Treinamento da equipe de operações

- [ ] **Cutover**
  - [ ] Desativar job COBOL em TWS
  - [ ] Ativar agendamento .NET (Hangfire)
  - [ ] Monitorar primeira execução em tempo real
  - [ ] Validar arquivo gerado vs último COBOL

- [ ] **Pós-Go-Live**
  - [ ] Monitorar primeiras 5 execuções (5 meses)
  - [ ] Manter COBOL como backup por 6 meses
  - [ ] Documentar lições aprendidas
  - [ ] Descomissionar mainframe após 6 meses

---

## Plano de Rollback

### Cenários de Rollback

#### Cenário 1: Divergência Detectada (CRÍTICO)

**Trigger**: SHA-256 checksum divergente entre .NET e COBOL

**Ação Imediata** (dentro de 1 hora):

1. **Pausar Processamento .NET**
   ```bash
   # Desabilitar job Hangfire
   RecurringJob.RemoveIfExists("rg1866b-monthly");
   ```

2. **Reativar COBOL**
   ```jcl
   // Submeter RG1866B.JCL manualmente
   SUBMIT PROD.JCL(RG1866BM)
   ```

3. **Análise de Root Cause**
   - Comparar arquivos byte-a-byte (identificar posição exata da divergência)
   - Analisar logs .NET e COBOL
   - Identificar registro problemático

4. **Decisão**
   - Se problema conhecido: corrigir .NET e retestar
   - Se problema desconhecido: rollback completo (ver Cenário 3)

---

#### Cenário 2: Performance Inaceitável

**Trigger**: Tempo de execução > 60 minutos (SLA excedido)

**Ação Imediata**:

1. **Avaliar Causa**
   - Verificar Application Insights: query lenta? CPU alta?
   - Analisar SQL Server: queries lentas? locks?

2. **Otimização Rápida**
   - Adicionar índices se necessário
   - Aumentar recursos (CPU/RAM)
   - Ajustar batch size (chunks menores)

3. **Se Não Resolver** (dentro de 2 horas):
   - Reativar COBOL para este mês
   - Agendar otimização para próximo ciclo

---

#### Cenário 3: Rollback Completo

**Trigger**: Problemas críticos não resolvidos em 3 execuções consecutivas

**Procedimento** (dentro de 24 horas):

1. **Backup Estado Atual**
   ```bash
   # Backup database .NET
   docker exec sql-server /opt/mssql-tools/bin/sqlcmd \
     -S localhost -U sa -P $SA_PASSWORD \
     -Q "BACKUP DATABASE PremiumReporting TO DISK='/backup/premiumreporting_rollback.bak'"
   ```

2. **Desativar Infraestrutura .NET**
   ```bash
   # Parar containers Docker
   docker-compose down

   # Desabilitar agendamento Hangfire
   # (via configuração)
   ```

3. **Reativar Sistema COBOL**
   ```jcl
   // Restaurar job TWS
   JOBD RG1866B_MENSAL ACTIVATE

   // Validar próxima execução agendada
   ```

4. **Comunicação**
   - Email para SUSEP: informar retorno ao sistema anterior
   - Comunicado interno: motivo do rollback
   - Plano de ação: correção e nova tentativa

5. **Post-Mortem** (dentro de 1 semana)
   - Análise detalhada de falhas
   - Plano de correção
   - Cronograma de nova tentativa (mínimo 3 meses)

---

### Critérios de No-Rollback (Sucesso)

**Após 3 meses de shadow mode com 100% match**:
- ✅ 0 divergências de checksum
- ✅ Performance dentro SLA (< 60min)
- ✅ 0 incidentes críticos
- ✅ Aprovação formal de stakeholders

**Declaração de Sucesso**: Sistema .NET torna-se produção primária, COBOL vira backup por mais 6 meses.

---

## Referências

- **Lógica de Negócio**: `docs/legacy-system/05-business-logic.md`
- **Módulos Externos**: `docs/legacy-system/06-external-modules.md`
- **Guia de Operações**: `docs/legacy-system/07-operations-guide.md`
- **Histórico de Manutenção**: `docs/legacy-system/08-maintenance-history.md`
- **SUSEP Circular 360/2007**: Resolução oficial SUSEP
- **IBM COBOL COMP-3**: Enterprise COBOL Language Reference

---

**Documento criado em**: 2025-10-27
**Última atualização**: 2025-10-27
**Versão**: 1.0
