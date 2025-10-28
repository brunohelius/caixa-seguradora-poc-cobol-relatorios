# 03 - Estruturas de Dados do Sistema COBOL RG1866B

[← Voltar ao Índice](README.md) | [← Anterior](02-architecture.md) | [Próximo →](04-database-model.md)

---

## 1. Working Storage Section (687 Variáveis)

### 1.1 Distribuição por Nível

| Nível COBOL | Quantidade | Percentual | Propósito |
|-------------|-----------|-----------|-----------|
| **01** | 7 | 1.0% | Estruturas de dados principais |
| **05** | 83 | 12.1% | Sub-grupos e campos principais |
| **10** | 179 | 26.1% | Campos padrão |
| **77** | 390 | 56.8% | Variáveis independentes |
| **88** | 0 | 0.0% | Condições nomeadas (não utilizado) |
| **Outros** | 28 | 4.0% | Níveis diversos |
| **TOTAL** | **687** | **100%** | - |

**Observação Importante**: 56.8% de variáveis Level 77 indica padrão COBOL antigo (independentes vs. estruturadas). Modernização para .NET deve consolidar em classes.

---

## 2. Estruturas Level 01 (7 Principais)

### 2.1 WS-ARQUIVOS - Controle de Arquivos

```cobol
01  WS-ARQUIVOS.
    05  EMI-STATUS          PIC XX.
        88  EMI-OK          VALUE '00'.
        88  EMI-EOF         VALUE '10'.
        88  EMI-ERROR       VALUE '90'.
    05  CED-STATUS          PIC XX.
        88  CED-OK          VALUE '00'.
        88  CED-EOF         VALUE '10'.
        88  CED-ERROR       VALUE '90'.
    05  WS-SQLCODE          PIC S9(9) COMP.
    05  WS-SQLSTATE         PIC X(5).
```

**Mapeamento .NET**:
```csharp
public class FileStatusControl
{
    public string EmitStatus { get; set; }  // EMI-STATUS
    public bool IsEmitOk => EmitStatus == "00";
    public bool IsEmitEof => EmitStatus == "10";

    public string CededStatus { get; set; }  // CED-STATUS
    public bool IsCededOk => CededStatus == "00";

    public int SqlCode { get; set; }  // WS-SQLCODE
    public string SqlState { get; set; }  // WS-SQLSTATE
}
```

---

### 2.2 AREA-DE-WORK - Área Principal de Trabalho

```cobol
01  AREA-DE-WORK.
    *> Controle de data
    05  WS-DATA-PROCESSAMENTO    PIC 9(8).
    05  WS-ANO-REFER             PIC 9(4).
    05  WS-MES-REFER             PIC 9(2).
    05  WS-DIA-REFER             PIC 9(2).

    *> Contadores
    05  WS-CONTADOR-REGISTROS    PIC 9(7) COMP-3.
    05  WS-CONTADOR-PREMIOS      PIC 9(7) COMP-3.
    05  WS-CONTADOR-COSSEGURO    PIC 9(7) COMP-3.
    05  WS-CONTADOR-REJEICOES    PIC 9(5) COMP-3.

    *> Acumuladores financeiros (COMP-3 = packed decimal)
    05  WS-TOTAL-PREMIO          PIC 9(15)V99 COMP-3.
    05  WS-TOTAL-IOF             PIC 9(15)V99 COMP-3.
    05  WS-TOTAL-ADICIONAL       PIC 9(15)V99 COMP-3.
    05  WS-TOTAL-PREMIO-LIQ      PIC 9(15)V99 COMP-3.
    05  WS-TOTAL-COSSEG-CED      PIC 9(15)V99 COMP-3.

    *> Flags de controle
    05  WS-FIM-CURSOR-PREMIOS    PIC X VALUE 'N'.
        88  FIM-PREMIOS          VALUE 'S'.
    05  WS-FIM-CURSOR-ENDERECO   PIC X VALUE 'N'.
        88  FIM-ENDERECO         VALUE 'S'.
    05  WS-ENCONTROU-REGISTRO    PIC X VALUE 'N'.
        88  REGISTRO-ENCONTRADO  VALUE 'S'.
```

**Mapeamento .NET**:
```csharp
public class WorkArea
{
    // Controle de data
    public DateTime ProcessingDate { get; set; }
    public int ReferenceYear { get; set; }
    public int ReferenceMonth { get; set; }
    public int ReferenceDay { get; set; }

    // Contadores
    public int RecordCount { get; set; }
    public int PremiumCount { get; set; }
    public int CossuranceCount { get; set; }
    public int RejectionCount { get; set; }

    // Acumuladores financeiros (IMPORTANTE: decimal, não double!)
    [CobolField(PicClause = "9(15)V99", DecimalPlaces = 2)]
    public decimal TotalPremium { get; set; }

    [CobolField(PicClause = "9(15)V99", DecimalPlaces = 2)]
    public decimal TotalIOF { get; set; }

    [CobolField(PicClause = "9(15)V99", DecimalPlaces = 2)]
    public decimal TotalAdditional { get; set; }

    [CobolField(PicClause = "9(15)V99", DecimalPlaces = 2)]
    public decimal TotalNetPremium { get; set; }

    [CobolField(PicClause = "9(15)V99", DecimalPlaces = 2)]
    public decimal TotalCededCossurance { get; set; }

    // Flags
    public bool IsPremiumCursorEnd { get; set; }
    public bool IsAddressCursorEnd { get; set; }
    public bool RecordFound { get; set; }
}
```

---

### 2.3 LKRE-PARM-RE0001S - Parâmetros Módulo Resseguro

```cobol
01  LKRE-PARM-RE0001S.
    *> INPUT PARAMETERS
    05  LKRE-I-APOLICE               PIC 9(10).
    05  LKRE-I-DATA-VIGENCIA         PIC 9(8).
    05  LKRE-I-VALOR-PREMIO          PIC 9(13)V99 COMP-3.
    05  LKRE-I-CODIGO-PRODUTO        PIC 9(4).
    05  LKRE-I-RAMO-SUSEP            PIC 9(4).
    05  LKRE-I-CODIGO-CIA            PIC 9(5).
    05  LKRE-I-TIPO-OPERACAO         PIC X(3).

    *> OUTPUT PARAMETERS
    05  LKRE-O-VALOR-RESSEG          PIC 9(13)V99 COMP-3.
    05  LKRE-O-PERC-RESSEG           PIC 9(3)V99 COMP-3.
    05  LKRE-O-COD-TRATADO           PIC X(10).
    05  LKRE-O-TIPO-TRATADO          PIC X(2).
    05  LKRE-O-COD-RESSEGURADORA     PIC 9(5).
    05  LKRE-O-RETURN-CODE           PIC 9(2).
        88  LKRE-OK                  VALUE 00.
        88  LKRE-ERROR               VALUE 99.
    05  LKRE-O-ERROR-MESSAGE         PIC X(100).
```

**Mapeamento .NET**:
```csharp
public class ReinsuranceModuleParameters
{
    // Input
    public long PolicyNumber { get; set; }
    public DateTime EffectiveDate { get; set; }

    [CobolField(PicClause = "9(13)V99", DecimalPlaces = 2)]
    public decimal PremiumValue { get; set; }

    public int ProductCode { get; set; }
    public int SusepBranch { get; set; }
    public int CompanyCode { get; set; }
    public string OperationType { get; set; }

    // Output
    [CobolField(PicClause = "9(13)V99", DecimalPlaces = 2)]
    public decimal ReinsuranceValue { get; set; }

    [CobolField(PicClause = "9(3)V99", DecimalPlaces = 2)]
    public decimal ReinsurancePercentage { get; set; }

    public string TreatyCode { get; set; }
    public string TreatyType { get; set; }
    public int ReinsurerCode { get; set; }
    public int ReturnCode { get; set; }
    public string ErrorMessage { get; set; }

    public bool IsSuccess => ReturnCode == 0;
}
```

---

### 2.4 WS-TABELAS - Estruturas de Tabela (Arrays)

```cobol
01  WS-TABELAS.
    05  WS-TAB-PRODUTOS OCCURS 100 TIMES INDEXED BY IDX-PROD.
        10  WS-TAB-COD-PRODUTO       PIC 9(4).
        10  WS-TAB-RAMO-SUSEP        PIC 9(4).
        10  WS-TAB-GRUPO-RAMO        PIC 9(2).
        10  WS-TAB-DESCRICAO         PIC X(50).
        10  WS-TAB-STATUS            PIC X(1).
            88  PRODUTO-ATIVO        VALUE 'A'.
            88  PRODUTO-INATIVO      VALUE 'I'.

    05  WS-TAB-EMPRESAS OCCURS 10 TIMES INDEXED BY IDX-EMP.
        10  WS-TAB-COD-EMPRESA       PIC 9(2).
        10  WS-TAB-COD-CIA-SUSEP     PIC 9(5).
        10  WS-TAB-RAZAO-SOCIAL      PIC X(60).
```

**Mapeamento .NET**:
```csharp
public class ProductTable
{
    public int ProductCode { get; set; }
    public int SusepBranch { get; set; }
    public int BranchGroup { get; set; }
    public string Description { get; set; }
    public char Status { get; set; }

    public bool IsActive => Status == 'A';
    public bool IsInactive => Status == 'I';
}

public class CompanyTable
{
    public int CompanyCode { get; set; }
    public int SusepCompanyCode { get; set; }
    public string CompanyName { get; set; }
}

public class Tables
{
    public List<ProductTable> Products { get; set; } = new(100);
    public List<CompanyTable> Companies { get; set; } = new(10);
}
```

---

## 3. File Section - Arquivos de Saída

### 3.1 PREMIT - Prêmios Emitidos (1200 bytes/registro)

```cobol
FD  PREMIT
    LABEL RECORDS STANDARD
    RECORDING MODE F
    BLOCK CONTAINS 0 RECORDS.

01  REGISTRO-PREMIT.
    *> Identificação (posições 1-50)
    05  EMI-COD-CIA             PIC 9(5).         *> 1-5
    05  EMI-RAMO-SUSEP          PIC 9(4).         *> 6-9
    05  EMI-NUM-APOLICE         PIC X(20).        *> 10-29
    05  EMI-NUM-ENDOSSO         PIC 9(10).        *> 30-39
    05  EMI-NUM-PROPOSTA        PIC X(20).        *> 40-59

    *> Datas (posições 60-95)
    05  EMI-DT-EMISSAO          PIC 9(8).         *> 60-67 (YYYYMMDD)
    05  EMI-DT-INI-VIG          PIC 9(8).         *> 68-75
    05  EMI-DT-FIM-VIG          PIC 9(8).         *> 76-83
    05  EMI-DT-PROPOSTA         PIC 9(8).         *> 84-91
    05  FILLER                  PIC X(4).         *> 92-95

    *> Valores (posições 96-250)
    05  EMI-TIPO-MOV            PIC 9(3).         *> 96-98
    05  EMI-PREMIO-TOTAL        PIC S9(13)V99.    *> 99-113 (sem ponto decimal!)
    05  EMI-PREMIO-LIQUIDO      PIC S9(13)V99.    *> 114-128
    05  EMI-IOF                 PIC S9(13)V99.    *> 129-143
    05  EMI-ADICIONAL-FRACIO    PIC S9(13)V99.    *> 144-158
    05  EMI-PREMIO-TARIFARIO    PIC S9(13)V99.    *> 159-173

    *> Cliente (posições 251-350)
    05  EMI-COD-CLIENTE         PIC 9(10).        *> 251-260
    05  EMI-TIPO-PESSOA         PIC X(1).         *> 261 (F=Física, J=Jurídica)
    05  EMI-CPF-CNPJ            PIC X(14).        *> 262-275
    05  EMI-NOME-CLIENTE        PIC X(70).        *> 276-345
    05  FILLER                  PIC X(5).         *> 346-350

    *> Endereço (posições 351-500)
    05  EMI-LOGRADOURO          PIC X(50).        *> 351-400
    05  EMI-NUMERO              PIC X(10).        *> 401-410
    05  EMI-COMPLEMENTO         PIC X(30).        *> 411-440
    05  EMI-BAIRRO              PIC X(30).        *> 441-470
    05  EMI-CIDADE              PIC X(30).        *> 471-500
    05  EMI-UF                  PIC X(2).         *> 501-502
    05  EMI-CEP                 PIC 9(8).         *> 503-510

    *> Produto (posições 511-600)
    05  EMI-COD-PRODUTO         PIC 9(4).         *> 511-514
    05  EMI-DESC-PRODUTO        PIC X(50).        *> 515-564
    05  EMI-GRUPO-RAMO          PIC 9(2).         *> 565-566
    05  EMI-COD-MODALIDADE      PIC 9(4).         *> 567-570
    05  FILLER                  PIC X(30).        *> 571-600

    *> Coberturas e riscos (posições 601-800)
    05  EMI-IMPORTANCIA-SEG     PIC S9(13)V99.    *> 601-615
    05  EMI-QTD-SEGURADOS       PIC 9(7).         *> 616-622
    05  EMI-QTD-PARCELAS        PIC 9(3).         *> 623-625
    05  EMI-NUM-BILHETE         PIC 9(15).        *> 626-640
    05  FILLER                  PIC X(160).       *> 641-800

    *> Comissões e distribução (posições 801-1000)
    05  EMI-COD-PRODUTOR        PIC 9(10).        *> 801-810
    05  EMI-NOME-PRODUTOR       PIC X(70).        *> 811-880
    05  EMI-PERC-COMISSAO       PIC 9(3)V99.      *> 881-885
    05  EMI-VALOR-COMISSAO      PIC S9(13)V99.    *> 886-900
    05  EMI-COD-AGENCIA         PIC 9(6).         *> 901-906
    05  EMI-NOME-AGENCIA        PIC X(50).        *> 907-956
    05  FILLER                  PIC X(44).        *> 957-1000

    *> Campos específicos SUSEP (posições 1001-1200)
    05  EMI-TIPO-RENOVACAO      PIC X(1).         *> 1001
    05  EMI-CANAL-VENDAS        PIC 9(2).         *> 1002-1003
    05  EMI-TIPO-OPERACAO       PIC X(3).         *> 1004-1006
    05  EMI-COD-FONTE           PIC 9(4).         *> 1007-1010
    05  EMI-EMPRESA             PIC 9(2).         *> 1011-1012
    05  FILLER                  PIC X(188).       *> 1013-1200
```

**Mapeamento .NET (Entity)**:
```csharp
[CobolRecord(Length = 1200)]
public class PremitRecord
{
    // Identificação
    [CobolField(Position = 1, Length = 5, PicClause = "9(5)")]
    public int CompanyCode { get; set; }

    [CobolField(Position = 6, Length = 4, PicClause = "9(4)")]
    public int SusepBranch { get; set; }

    [CobolField(Position = 10, Length = 20, PicClause = "X(20)")]
    public string PolicyNumber { get; set; }

    [CobolField(Position = 30, Length = 10, PicClause = "9(10)")]
    public long EndorsementNumber { get; set; }

    [CobolField(Position = 40, Length = 20, PicClause = "X(20)")]
    public string ProposalNumber { get; set; }

    // Datas (YYYYMMDD format)
    [CobolField(Position = 60, Length = 8, PicClause = "9(8)")]
    public int IssueDateRaw { get; set; }

    public DateTime IssueDate
    {
        get => ParseCobolDate(IssueDateRaw);
        set => IssueDateRaw = FormatCobolDate(value);
    }

    [CobolField(Position = 68, Length = 8, PicClause = "9(8)")]
    public int StartDateRaw { get; set; }

    public DateTime StartDate
    {
        get => ParseCobolDate(StartDateRaw);
        set => StartDateRaw = FormatCobolDate(value);
    }

    // Valores financeiros (15 dígitos + 2 decimais, SEM ponto decimal)
    [CobolField(Position = 96, Length = 3, PicClause = "9(3)")]
    public int MovementType { get; set; }

    [CobolField(Position = 99, Length = 15, PicClause = "S9(13)V99", DecimalPlaces = 2)]
    public decimal TotalPremium { get; set; }

    [CobolField(Position = 114, Length = 15, PicClause = "S9(13)V99", DecimalPlaces = 2)]
    public decimal NetPremium { get; set; }

    [CobolField(Position = 129, Length = 15, PicClause = "S9(13)V99", DecimalPlaces = 2)]
    public decimal IOF { get; set; }

    // ... (mais 60+ campos)

    // Métodos auxiliares
    private static DateTime ParseCobolDate(int cobolDate)
    {
        if (cobolDate == 0) return DateTime.MinValue;

        int year = cobolDate / 10000;
        int month = (cobolDate % 10000) / 100;
        int day = cobolDate % 100;

        return new DateTime(year, month, day);
    }

    private static int FormatCobolDate(DateTime date)
    {
        if (date == DateTime.MinValue) return 0;
        return date.Year * 10000 + date.Month * 100 + date.Day;
    }
}
```

---

### 3.2 PREMCED - Prêmios Cedidos (800 bytes/registro)

```cobol
FD  PREMCED
    LABEL RECORDS STANDARD
    RECORDING MODE F
    BLOCK CONTAINS 0 RECORDS.

01  REGISTRO-PREMCED.
    *> Identificação (posições 1-50)
    05  CED-COD-CIA             PIC 9(5).         *> 1-5 (cedente)
    05  CED-RAMO-SUSEP          PIC 9(4).         *> 6-9
    05  CED-NUM-APOLICE         PIC X(20).        *> 10-29
    05  CED-NUM-ENDOSSO         PIC 9(10).        *> 30-39
    05  CED-TIPO-CESSAO         PIC X(1).         *> 40 ('C'=Cedido, 'O'=Obtido)
    05  FILLER                  PIC X(10).        *> 41-50

    *> Cosseguradora/Resseguradora (posições 51-100)
    05  CED-COD-CIA-COPART      PIC 9(5).         *> 51-55
    05  CED-NOME-COPART         PIC X(40).        *> 56-95
    05  FILLER                  PIC X(5).         *> 96-100

    *> Valores (posições 101-200)
    05  CED-PERC-PARTICIPACAO   PIC 9(3)V99.      *> 101-105 (ex: 025.50)
    05  CED-PREMIO-CEDIDO       PIC S9(13)V99.    *> 106-120
    05  CED-PREMIO-RETIDO       PIC S9(13)V99.    *> 121-135
    05  CED-IOF-CEDIDO          PIC S9(13)V99.    *> 136-150
    05  CED-COMISSAO            PIC S9(13)V99.    *> 151-165
    05  FILLER                  PIC X(35).        *> 166-200

    *> Datas (posições 201-250)
    05  CED-DT-EMISSAO          PIC 9(8).         *> 201-208
    05  CED-DT-INI-VIG          PIC 9(8).         *> 209-216
    05  CED-DT-FIM-VIG          PIC 9(8).         *> 217-224
    05  FILLER                  PIC X(26).        *> 225-250

    *> Tratado de Resseguro (posições 251-350)
    05  CED-COD-TRATADO         PIC X(10).        *> 251-260
    05  CED-TIPO-TRATADO        PIC X(2).         *> 261-262
        88  TRATADO-QUOTA       VALUE 'QT'.
        88  TRATADO-EXCESSO     VALUE 'EX'.
        88  TRATADO-FACULTATIVO VALUE 'FA'.
    05  CED-DESC-TRATADO        PIC X(50).        *> 263-312
    05  FILLER                  PIC X(38).        *> 313-350

    *> Produto (posições 351-400)
    05  CED-COD-PRODUTO         PIC 9(4).         *> 351-354
    05  CED-GRUPO-RAMO          PIC 9(2).         *> 355-356
    05  FILLER                  PIC X(44).        *> 357-400

    *> Campos de controle (posições 401-800)
    05  CED-TIPO-MOV            PIC 9(3).         *> 401-403
    05  CED-EMPRESA             PIC 9(2).         *> 404-405
    05  FILLER                  PIC X(395).       *> 406-800
```

**Mapeamento .NET**:
```csharp
[CobolRecord(Length = 800)]
public class PremcedRecord
{
    [CobolField(Position = 1, Length = 5)]
    public int CedingCompanyCode { get; set; }

    [CobolField(Position = 6, Length = 4)]
    public int SusepBranch { get; set; }

    [CobolField(Position = 10, Length = 20)]
    public string PolicyNumber { get; set; }

    [CobolField(Position = 40, Length = 1)]
    public char CessionType { get; set; }  // 'C' ou 'O'

    public bool IsCeded => CessionType == 'C';
    public bool IsObtained => CessionType == 'O';

    [CobolField(Position = 51, Length = 5)]
    public int CoparticipantCompanyCode { get; set; }

    [CobolField(Position = 56, Length = 40)]
    public string CoparticipantName { get; set; }

    [CobolField(Position = 101, Length = 5, PicClause = "9(3)V99", DecimalPlaces = 2)]
    public decimal ParticipationPercentage { get; set; }

    [CobolField(Position = 106, Length = 15, PicClause = "S9(13)V99", DecimalPlaces = 2)]
    public decimal CededPremium { get; set; }

    [CobolField(Position = 121, Length = 15, PicClause = "S9(13)V99", DecimalPlaces = 2)]
    public decimal RetainedPremium { get; set; }

    // ... (mais campos)
}
```

---

## 4. Tipos de Dados COBOL → .NET

### 4.1 Mapeamento de Tipos Numéricos

| COBOL PIC | Exemplo | Tamanho | .NET Type | Observações |
|-----------|---------|---------|-----------|-------------|
| `9(n)` | `PIC 9(5)` | n bytes | `int` / `long` | Inteiro sem sinal |
| `S9(n)` | `PIC S9(10)` | n bytes | `int` / `long` | Inteiro com sinal |
| `9(n)V99` | `PIC 9(13)V99` | n+2 bytes | `decimal` | ⚠️ CRÍTICO: usar decimal! |
| `9(n)V99 COMP-3` | `PIC 9(15)V99 COMP-3` | (n+3)/2 bytes | `decimal` | Packed decimal |
| `9(n) COMP` | `PIC 9(5) COMP` | 2/4/8 bytes | `short`/`int`/`long` | Binary |

**⚠️ IMPORTANTE PARA CÁLCULOS FINANCEIROS**:
```csharp
// ❌ ERRADO - perde precisão
public double PremiumAmount { get; set; }

// ✅ CORRETO - mantém precisão COBOL
[CobolField(PicClause = "9(13)V99", DecimalPlaces = 2)]
public decimal PremiumAmount { get; set; }
```

### 4.2 Mapeamento de Tipos Alfanuméricos

| COBOL PIC | Exemplo | .NET Type | Padding |
|-----------|---------|-----------|---------|
| `X(n)` | `PIC X(20)` | `string` | Direita (espaços) |
| `A(n)` | `PIC A(50)` | `string` | Direita (espaços) |

**Exemplo de Padding**:
```csharp
public static string FormatAlphanumeric(string value, int length)
{
    if (value == null) value = "";

    // Trunca se maior
    if (value.Length > length)
        return value.Substring(0, length);

    // Pad com espaços à direita
    return value.PadRight(length, ' ');
}

// Uso:
string policyNumber = FormatAlphanumeric("ABC123", 20);
// Resultado: "ABC123              " (14 espaços)
```

---

## 5. Formatação Fixed-Width

### 5.1 Regras de Formatação

#### Campos Numéricos (PIC 9)
```csharp
public static string FormatNumeric(decimal value, int totalWidth, int decimalPlaces)
{
    // Remove ponto decimal e formata
    long scaledValue = (long)(value * (decimal)Math.Pow(10, decimalPlaces));

    // Pad com zeros à esquerda
    return scaledValue.ToString().PadLeft(totalWidth, '0');
}

// Exemplo:
decimal amount = 1234.56m;
string formatted = FormatNumeric(amount, 15, 2);
// Resultado: "000000000123456" (sem ponto decimal!)
```

#### Campos Alfanuméricos (PIC X)
```csharp
public static string FormatAlphanumeric(string value, int length)
{
    value ??= "";

    if (value.Length > length)
        return value.Substring(0, length);

    // Pad com espaços à direita
    return value.PadRight(length, ' ');
}

// Exemplo:
string name = "João Silva";
string formatted = FormatAlphanumeric(name, 30);
// Resultado: "João Silva                    " (20 espaços)
```

### 5.2 Exemplo de Geração de Registro Completo

```csharp
public string ToPremitFixedWidth(PremitRecord record)
{
    var sb = new StringBuilder(1200);

    // Posições 1-5: Código da Cia (numérico)
    sb.Append(record.CompanyCode.ToString().PadLeft(5, '0'));

    // Posições 6-9: Ramo SUSEP (numérico)
    sb.Append(record.SusepBranch.ToString().PadLeft(4, '0'));

    // Posições 10-29: Número da apólice (alfanumérico)
    sb.Append(record.PolicyNumber.PadRight(20, ' '));

    // Posições 30-39: Número endosso (numérico)
    sb.Append(record.EndorsementNumber.ToString().PadLeft(10, '0'));

    // ... continua para todos os 1200 bytes

    // Posições 99-113: Prêmio total (decimal sem ponto)
    long premiumScaled = (long)(record.TotalPremium * 100);
    sb.Append(premiumScaled.ToString().PadLeft(15, '0'));

    // ... todos os demais campos

    // Garantir exatamente 1200 bytes
    if (sb.Length < 1200)
        sb.Append(' ', 1200 - sb.Length);

    return sb.ToString();
}
```

---

## 6. Validações Críticas

### 6.1 Validação de Precisão Decimal

```csharp
[Test]
public void ValidateDecimalPrecision()
{
    // COBOL: PIC 9(13)V99 = 13 dígitos inteiros + 2 decimais
    decimal maxValue = 9999999999999.99m;
    decimal minValue = -9999999999999.99m;

    Assert.IsTrue(record.TotalPremium <= maxValue);
    Assert.IsTrue(record.TotalPremium >= minValue);

    // Validar apenas 2 casas decimais
    decimal rounded = Math.Round(record.TotalPremium, 2);
    Assert.AreEqual(rounded, record.TotalPremium);
}
```

### 6.2 Validação de Tamanho de String

```csharp
[Test]
public void ValidateStringLengths()
{
    Assert.IsTrue(record.PolicyNumber.Length <= 20);
    Assert.IsTrue(record.ClientName.Length <= 70);
    Assert.IsTrue(record.Address.Length <= 50);
}
```

---

## 7. Atributo Customizado CobolField

```csharp
[AttributeUsage(AttributeTargets.Property)]
public class CobolFieldAttribute : Attribute
{
    public string PicClause { get; set; }
    public int Length { get; set; }
    public int DecimalPlaces { get; set; }
    public int Position { get; set; }

    public CobolFieldAttribute() { }

    public CobolFieldAttribute(string picClause)
    {
        PicClause = picClause;
        ParsePicClause(picClause);
    }

    private void ParsePicClause(string pic)
    {
        // Parse "9(13)V99" → Length=15, DecimalPlaces=2
        // Parse "X(20)" → Length=20
        // ... implementação
    }
}
```

---

## Próximos Passos

1. ✅ Mapear todas as 687 variáveis COBOL para classes C#
2. ✅ Implementar FixedWidthFormatter completo
3. ✅ Criar testes de comparação byte-a-byte
4. ✅ Validar precisão de cálculos financeiros

---

**Documento**: 03-data-structures.md
**Versão**: 1.0
**Próximo**: [04-database-model.md](04-database-model.md) →
