# 04 - Database Model

[← Voltar ao Índice](README.md)

## Índice

- [Visão Geral](#visão-geral)
- [Views DB2 Acessadas](#views-db2-acessadas)
- [Cursores COBOL](#cursores-cobol)
- [Relacionamentos Entre Tabelas](#relacionamentos-entre-tabelas)
- [Modelo de Dados .NET](#modelo-de-dados-net)
- [Estratégia de Migração](#estratégia-de-migração)
- [Considerações de Performance](#considerações-de-performance)

---

## Visão Geral

O programa RG1866B acessa **26+ views DB2** do sistema legado através de **4 cursores ativos**. Todas as views começam com prefixo `V0` (views da aplicação) ou `GE` (views genéricas corporativas).

### Características do Acesso a Dados

| Característica | Valor |
|----------------|-------|
| **Total de Views Acessadas** | 26+ |
| **Cursores Simultâneos** | 4 ativos |
| **Tipo de Acesso** | READ-ONLY (nenhuma atualização) |
| **Volume de Dados** | ~10.000 registros/execução |
| **Isolamento** | UR (Uncommitted Read) |
| **Padrão de Acesso** | Sequential scan via cursores |

### Views Críticas (Acesso Primário)

1. **V0PREMIOS**: Prêmios emitidos (cursor principal)
2. **V0APOLICE**: Dados da apólice
3. **V0PRODUTO**: Informações do produto
4. **V0CLIENTE**: Dados cadastrais do cliente
5. **V0ENDERECOS**: Endereços (3 tipos: segurado, estipulante, corretor)
6. **V0APOLCOSCED**: Cosseguro/cessão
7. **GE399**: Cálculo de cosseguro

---

## Views DB2 Acessadas

### 1. V0PREMIOS (View Principal)

**Cursor**: `CURSOR-PREMIOS`

**Definição SQL (Extraída do COBOL)**:

```sql
DECLARE CURSOR-PREMIOS CURSOR FOR
  SELECT
    COD_CIA,
    RAMO_SUSEP,
    NUM_APOLICE,
    NUM_ENDOSSO,
    COD_TIPO_MOVIMENTO,
    DATA_EMISSAO,
    DATA_VIGENCIA_INICIAL,
    DATA_VIGENCIA_FINAL,
    PREMIO_LIQUIDO,
    PREMIO_TOTAL,
    IOF,
    ADICIONAL_FRACIONAMENTO,
    COD_PRODUTO,
    COD_MOEDA,
    TAXA_CAMBIO
  FROM V0PREMIOS
  WHERE DATA_PROCESSAMENTO = :WS-DATA-PROCESSAMENTO
    AND COD_CIA = :WS-COD-CIA
  ORDER BY NUM_APOLICE, NUM_ENDOSSO
```

**Estrutura da View**:

```sql
CREATE VIEW V0PREMIOS AS
SELECT
  p.COD_CIA           AS COD_CIA,           -- SMALLINT NOT NULL
  p.RAMO_SUSEP        AS RAMO_SUSEP,        -- SMALLINT NOT NULL
  p.NUM_APOLICE       AS NUM_APOLICE,       -- BIGINT NOT NULL
  p.NUM_ENDOSSO       AS NUM_ENDOSSO,       -- INTEGER NOT NULL
  p.COD_TIPO_MOVIMENTO AS COD_TIPO_MOVIMENTO, -- SMALLINT
  p.DATA_EMISSAO      AS DATA_EMISSAO,      -- DATE
  p.DATA_VIG_INI      AS DATA_VIGENCIA_INICIAL, -- DATE
  p.DATA_VIG_FIM      AS DATA_VIGENCIA_FINAL,   -- DATE
  p.PREMIO_LIQUIDO    AS PREMIO_LIQUIDO,    -- DECIMAL(15,2)
  p.PREMIO_TOTAL      AS PREMIO_TOTAL,      -- DECIMAL(15,2)
  p.IOF               AS IOF,               -- DECIMAL(13,2)
  p.ADIC_FRACIONAMENTO AS ADICIONAL_FRACIONAMENTO, -- DECIMAL(13,2)
  p.COD_PRODUTO       AS COD_PRODUTO,       -- INTEGER
  p.COD_MOEDA         AS COD_MOEDA,         -- CHAR(3)
  p.TAXA_CAMBIO       AS TAXA_CAMBIO        -- DECIMAL(9,6)
FROM TB_PREMIOS p
WHERE p.STATUS = 'A'  -- Ativo
  AND p.TIPO = 'E'    -- Emissão
```

**Entity Framework Mapping**:

```csharp
public class Premium
{
    [Key]
    public long PremiumId { get; set; }

    [Required]
    public int CompanyCode { get; set; }

    [Required]
    public int SusepBranch { get; set; }

    [Required]
    [MaxLength(20)]
    public string PolicyNumber { get; set; }

    [Required]
    public int EndorsementNumber { get; set; }

    public int MovementType { get; set; }

    public DateTime IssueDate { get; set; }
    public DateTime EffectiveStartDate { get; set; }
    public DateTime EffectiveEndDate { get; set; }

    [Column(TypeName = "decimal(15,2)")]
    public decimal NetPremium { get; set; }

    [Column(TypeName = "decimal(15,2)")]
    public decimal TotalPremium { get; set; }

    [Column(TypeName = "decimal(13,2)")]
    public decimal IOF { get; set; }

    [Column(TypeName = "decimal(13,2)")]
    public decimal InstallmentFee { get; set; }

    public int ProductCode { get; set; }

    [MaxLength(3)]
    public string CurrencyCode { get; set; }

    [Column(TypeName = "decimal(9,6)")]
    public decimal ExchangeRate { get; set; }

    // Navigation properties
    public virtual Policy Policy { get; set; }
    public virtual Product Product { get; set; }
}
```

### 2. V0APOLICE (Apólices)

**Definição SQL**:

```sql
CREATE VIEW V0APOLICE AS
SELECT
  a.NUM_APOLICE       AS NUM_APOLICE,       -- BIGINT NOT NULL
  a.COD_CIA           AS COD_CIA,           -- SMALLINT NOT NULL
  a.RAMO_SUSEP        AS RAMO_SUSEP,        -- SMALLINT NOT NULL
  a.DATA_EMISSAO      AS DATA_EMISSAO,      -- DATE
  a.DATA_VIG_INI      AS DATA_VIGENCIA_INICIAL, -- DATE
  a.DATA_VIG_FIM      AS DATA_VIGENCIA_FINAL,   -- DATE
  a.COD_PRODUTO       AS COD_PRODUTO,       -- INTEGER
  a.NUM_PROPOSTA      AS NUM_PROPOSTA,      -- BIGINT
  a.COD_SEGURADO      AS COD_SEGURADO,      -- BIGINT
  a.COD_ESTIPULANTE   AS COD_ESTIPULANTE,   -- BIGINT
  a.COD_CORRETOR      AS COD_CORRETOR,      -- INTEGER
  a.FORMA_PAGAMENTO   AS FORMA_PAGAMENTO,   -- SMALLINT
  a.NUM_PARCELAS      AS NUM_PARCELAS,      -- SMALLINT
  a.TIPO_APOLICE      AS TIPO_APOLICE       -- CHAR(1)
FROM TB_APOLICES a
WHERE a.STATUS = 'V'  -- Vigente
```

**Entity Framework Mapping**:

```csharp
public class Policy
{
    [Key]
    [MaxLength(20)]
    public string PolicyNumber { get; set; }

    [Required]
    public int CompanyCode { get; set; }

    [Required]
    public int SusepBranch { get; set; }

    public DateTime IssueDate { get; set; }
    public DateTime EffectiveStartDate { get; set; }
    public DateTime EffectiveEndDate { get; set; }

    public int ProductCode { get; set; }
    public long ProposalNumber { get; set; }

    public long InsuredClientCode { get; set; }
    public long PolicyholderCode { get; set; }
    public int BrokerCode { get; set; }

    public int PaymentMethod { get; set; }
    public int InstallmentCount { get; set; }

    [MaxLength(1)]
    public string PolicyType { get; set; }

    // Navigation properties
    public virtual Product Product { get; set; }
    public virtual Client InsuredClient { get; set; }
    public virtual Client Policyholder { get; set; }
    public virtual ICollection<Premium> Premiums { get; set; }
    public virtual ICollection<Endorsement> Endorsements { get; set; }
    public virtual ICollection<Coverage> Coverages { get; set; }
}
```

### 3. V0PRODUTO (Produtos)

**Definição SQL**:

```sql
CREATE VIEW V0PRODUTO AS
SELECT
  p.COD_PRODUTO       AS COD_PRODUTO,       -- INTEGER NOT NULL
  p.NOME_PRODUTO      AS NOME_PRODUTO,      -- VARCHAR(100)
  p.RAMO_SUSEP        AS RAMO_SUSEP,        -- SMALLINT
  p.TIPO_PRODUTO      AS TIPO_PRODUTO,      -- CHAR(2)
  p.COD_CATEGORIA     AS COD_CATEGORIA,     -- SMALLINT
  p.IND_COSSEGURO     AS IND_COSSEGURO,     -- CHAR(1)
  p.IND_RESSEGURO     AS IND_RESSEGURO      -- CHAR(1)
FROM TB_PRODUTOS p
WHERE p.STATUS = 'A'  -- Ativo
```

**Entity Framework Mapping**:

```csharp
public class Product
{
    [Key]
    public int ProductCode { get; set; }

    [Required]
    [MaxLength(100)]
    public string ProductName { get; set; }

    public int SusepBranch { get; set; }

    [MaxLength(2)]
    public string ProductType { get; set; }

    public int CategoryCode { get; set; }

    [MaxLength(1)]
    public string CosuranceIndicator { get; set; }

    [MaxLength(1)]
    public string ReinsuranceIndicator { get; set; }

    // Navigation properties
    public virtual ICollection<Policy> Policies { get; set; }
}
```

### 4. V0CLIENTE (Clientes)

**Definição SQL**:

```sql
CREATE VIEW V0CLIENTE AS
SELECT
  c.COD_CLIENTE       AS COD_CLIENTE,       -- BIGINT NOT NULL
  c.TIPO_PESSOA       AS TIPO_PESSOA,       -- CHAR(1) - F/J
  c.CPF_CNPJ          AS CPF_CNPJ,          -- VARCHAR(14)
  c.NOME_RAZAO        AS NOME_RAZAO,        -- VARCHAR(100)
  c.DATA_NASCIMENTO   AS DATA_NASCIMENTO,   -- DATE
  c.SEXO              AS SEXO,              -- CHAR(1) - M/F
  c.ESTADO_CIVIL      AS ESTADO_CIVIL,      -- SMALLINT
  c.EMAIL             AS EMAIL,             -- VARCHAR(100)
  c.TELEFONE          AS TELEFONE           -- VARCHAR(20)
FROM TB_CLIENTES c
WHERE c.STATUS = 'A'  -- Ativo
```

**Entity Framework Mapping**:

```csharp
public class Client
{
    [Key]
    public long ClientCode { get; set; }

    [Required]
    [MaxLength(1)]
    public string PersonType { get; set; }  // F=Física, J=Jurídica

    [Required]
    [MaxLength(14)]
    public string TaxId { get; set; }  // CPF or CNPJ

    [Required]
    [MaxLength(100)]
    public string FullName { get; set; }

    public DateTime? BirthDate { get; set; }

    [MaxLength(1)]
    public string Gender { get; set; }  // M/F

    public int? MaritalStatus { get; set; }

    [MaxLength(100)]
    public string Email { get; set; }

    [MaxLength(20)]
    public string Phone { get; set; }

    // Navigation properties
    public virtual ICollection<Address> Addresses { get; set; }
}
```

### 5. V0ENDERECOS (Endereços)

**Cursor**: `CURSOR-ENDERECOS`

**Definição SQL**:

```sql
DECLARE CURSOR-ENDERECOS CURSOR FOR
  SELECT
    COD_CLIENTE,
    TIPO_ENDERECO,
    LOGRADOURO,
    NUMERO,
    COMPLEMENTO,
    BAIRRO,
    CIDADE,
    UF,
    CEP
  FROM V0ENDERECOS
  WHERE COD_CLIENTE IN (:WS-COD-SEGURADO,
                        :WS-COD-ESTIPULANTE,
                        :WS-COD-CORRETOR)
    AND TIPO_ENDERECO = 'R'  -- Residencial
  ORDER BY COD_CLIENTE
```

**Entity Framework Mapping**:

```csharp
public class Address
{
    [Key]
    public long AddressId { get; set; }

    [Required]
    public long ClientCode { get; set; }

    [Required]
    [MaxLength(1)]
    public string AddressType { get; set; }  // R=Residencial, C=Comercial

    [Required]
    [MaxLength(100)]
    public string Street { get; set; }

    [MaxLength(10)]
    public string Number { get; set; }

    [MaxLength(50)]
    public string Complement { get; set; }

    [MaxLength(50)]
    public string Neighborhood { get; set; }

    [Required]
    [MaxLength(50)]
    public string City { get; set; }

    [Required]
    [MaxLength(2)]
    public string State { get; set; }

    [Required]
    [MaxLength(8)]
    public string ZipCode { get; set; }

    // Navigation property
    public virtual Client Client { get; set; }
}
```

### 6. V0APOLCOSCED (Cosseguro/Cessão)

**Cursor**: `CURSOR-COSSEGURO`

**Definição SQL**:

```sql
DECLARE CURSOR-COSSEGURO CURSOR FOR
  SELECT
    NUM_APOLICE,
    NUM_ENDOSSO,
    SEQ_COSSEGURO,
    COD_CIA_COSSEGURADORA,
    PERCENTUAL_PARTICIPACAO,
    TIPO_PARTICIPACAO,
    PREMIO_CEDIDO
  FROM V0APOLCOSCED
  WHERE NUM_APOLICE = :WS-NUM-APOLICE
    AND NUM_ENDOSSO = :WS-NUM-ENDOSSO
  ORDER BY SEQ_COSSEGURO
```

**Entity Framework Mapping**:

```csharp
public class Cosurance
{
    [Key]
    public long CosuranceId { get; set; }

    [Required]
    [MaxLength(20)]
    public string PolicyNumber { get; set; }

    [Required]
    public int EndorsementNumber { get; set; }

    [Required]
    public int Sequence { get; set; }

    [Required]
    public int CoinsurerCompanyCode { get; set; }

    [Required]
    [Column(TypeName = "decimal(5,2)")]
    public decimal ParticipationPercentage { get; set; }

    [Required]
    [MaxLength(1)]
    public string ParticipationType { get; set; }  // A=Aceito, C=Cedido

    [Column(TypeName = "decimal(15,2)")]
    public decimal CededPremium { get; set; }

    // Navigation property
    public virtual Policy Policy { get; set; }
}
```

### 7. GE399 (Cálculos de Cosseguro)

**View Genérica Corporativa**

**Definição SQL**:

```sql
CREATE VIEW GE399 AS
SELECT
  g.NUM_APOLICE       AS NUM_APOLICE,
  g.NUM_ENDOSSO       AS NUM_ENDOSSO,
  g.PREMIO_TOTAL      AS PREMIO_TOTAL,
  g.PERC_LIDER        AS PERC_LIDER,
  g.PREMIO_LIDER      AS PREMIO_LIDER,
  g.PREMIO_CEDIDO     AS PREMIO_CEDIDO,
  g.QTD_COSSEGURADORES AS QTD_COSSEGURADORES
FROM TB_GE399 g
```

**Entity Framework Mapping**:

```csharp
public class CosuranceCalculation
{
    [Key]
    public long CalculationId { get; set; }

    [Required]
    [MaxLength(20)]
    public string PolicyNumber { get; set; }

    [Required]
    public int EndorsementNumber { get; set; }

    [Column(TypeName = "decimal(15,2)")]
    public decimal TotalPremium { get; set; }

    [Column(TypeName = "decimal(5,2)")]
    public decimal LeaderPercentage { get; set; }

    [Column(TypeName = "decimal(15,2)")]
    public decimal LeaderPremium { get; set; }

    [Column(TypeName = "decimal(15,2)")]
    public decimal CededPremium { get; set; }

    public int CoinsurerCount { get; set; }
}
```

---

## Cursores COBOL

### Cursor 1: CURSOR-PREMIOS (Principal)

**Seção COBOL**: R0400-ABRIR-CURSORES até R0700-PROCESSAR-PREMIOS

```cobol
R0400-ABRIR-CURSORES.
    EXEC SQL
        OPEN CURSOR-PREMIOS
    END-EXEC.

    IF SQLCODE NOT = 0
        MOVE 'ERRO AO ABRIR CURSOR-PREMIOS' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-SQL
    END-IF.

R0500-FETCH-PREMIO.
    EXEC SQL
        FETCH CURSOR-PREMIOS
        INTO :REGISTRO-PREMIO
    END-EXEC.

    EVALUATE SQLCODE
        WHEN 0
            MOVE 'N' TO WS-FIM-CURSOR-PREMIOS
        WHEN 100
            MOVE 'S' TO WS-FIM-CURSOR-PREMIOS
        WHEN OTHER
            PERFORM R9000-TRATAR-ERRO-SQL
    END-EVALUATE.
```

**Migração para .NET**:

```csharp
public async IAsyncEnumerable<Premium> GetPremiumsAsync(
    DateTime processingDate,
    int companyCode,
    [EnumeratorCancellation] CancellationToken cancellationToken = default)
{
    var query = _context.Premiums
        .AsNoTracking()
        .Where(p => p.ProcessingDate == processingDate && p.CompanyCode == companyCode)
        .OrderBy(p => p.PolicyNumber)
        .ThenBy(p => p.EndorsementNumber);

    await foreach (var premium in query.AsAsyncEnumerable()
        .WithCancellation(cancellationToken))
    {
        yield return premium;
    }
}
```

### Cursor 2: CURSOR-ENDERECOS

**Seção COBOL**: R1500-BUSCAR-ENDERECOS

```cobol
R1500-BUSCAR-ENDERECOS.
    EXEC SQL
        DECLARE CURSOR-ENDERECOS CURSOR FOR
        SELECT COD_CLIENTE, TIPO_ENDERECO, LOGRADOURO, NUMERO,
               COMPLEMENTO, BAIRRO, CIDADE, UF, CEP
        FROM V0ENDERECOS
        WHERE COD_CLIENTE IN (:WS-COD-SEGURADO,
                              :WS-COD-ESTIPULANTE,
                              :WS-COD-CORRETOR)
          AND TIPO_ENDERECO = 'R'
        ORDER BY COD_CLIENTE
    END-EXEC.

    EXEC SQL OPEN CURSOR-ENDERECOS END-EXEC.
```

**Migração para .NET**:

```csharp
public async Task<Dictionary<long, Address>> GetAddressesAsync(
    long insuredCode,
    long policyholderCode,
    int brokerCode)
{
    var clientCodes = new[] { insuredCode, policyholderCode, (long)brokerCode };

    var addresses = await _context.Addresses
        .AsNoTracking()
        .Where(a => clientCodes.Contains(a.ClientCode) && a.AddressType == "R")
        .OrderBy(a => a.ClientCode)
        .ToListAsync();

    return addresses.ToDictionary(a => a.ClientCode, a => a);
}
```

### Cursor 3: CURSOR-COSSEGURO

**Seção COBOL**: R3000-PROCESSAR-COSSEGURO

```cobol
R3000-PROCESSAR-COSSEGURO.
    EXEC SQL
        DECLARE CURSOR-COSSEGURO CURSOR FOR
        SELECT NUM_APOLICE, NUM_ENDOSSO, SEQ_COSSEGURO,
               COD_CIA_COSSEGURADORA, PERCENTUAL_PARTICIPACAO,
               TIPO_PARTICIPACAO, PREMIO_CEDIDO
        FROM V0APOLCOSCED
        WHERE NUM_APOLICE = :WS-NUM-APOLICE
          AND NUM_ENDOSSO = :WS-NUM-ENDOSSO
        ORDER BY SEQ_COSSEGURO
    END-EXEC.

    EXEC SQL OPEN CURSOR-COSSEGURO END-EXEC.
```

**Migração para .NET**:

```csharp
public async Task<List<Cosurance>> GetCosurancesAsync(
    string policyNumber,
    int endorsementNumber)
{
    return await _context.Cosurances
        .AsNoTracking()
        .Where(c => c.PolicyNumber == policyNumber &&
                    c.EndorsementNumber == endorsementNumber)
        .OrderBy(c => c.Sequence)
        .ToListAsync();
}
```

### Cursor 4: CURSOR-GE399 (Cálculos)

**Seção COBOL**: R3500-CALCULAR-COSSEGURO

```cobol
R3500-CALCULAR-COSSEGURO.
    EXEC SQL
        SELECT PREMIO_TOTAL, PERC_LIDER, PREMIO_LIDER,
               PREMIO_CEDIDO, QTD_COSSEGURADORES
        INTO :WS-PREMIO-TOTAL, :WS-PERC-LIDER, :WS-PREMIO-LIDER,
             :WS-PREMIO-CEDIDO, :WS-QTD-COSSEG
        FROM GE399
        WHERE NUM_APOLICE = :WS-NUM-APOLICE
          AND NUM_ENDOSSO = :WS-NUM-ENDOSSO
    END-EXEC.
```

**Migração para .NET**:

```csharp
public async Task<CosuranceCalculation> GetCosuranceCalculationAsync(
    string policyNumber,
    int endorsementNumber)
{
    return await _context.CosuranceCalculations
        .AsNoTracking()
        .FirstOrDefaultAsync(c => c.PolicyNumber == policyNumber &&
                                   c.EndorsementNumber == endorsementNumber);
}
```

---

## Relacionamentos Entre Tabelas

### Diagrama Entidade-Relacionamento (ER)

```text
┌─────────────────┐
│    V0PREMIOS    │───┐
│  (Premium)      │   │
│                 │   │ N:1
│ - COD_CIA       │   │
│ - NUM_APOLICE   │◄──┼────────┐
│ - NUM_ENDOSSO   │   │        │
│ - PREMIO_TOTAL  │   │        │
└─────────────────┘   │        │
         │            │        │
         │ N:1        │        │
         ▼            │        │
┌─────────────────┐   │   ┌────▼───────────┐
│   V0APOLICE     │   │   │   V0PRODUTO    │
│   (Policy)      │   │   │   (Product)    │
│                 │   │   │                │
│ - NUM_APOLICE   │   │   │ - COD_PRODUTO  │
│ - COD_PRODUTO   │───┘   │ - NOME_PRODUTO │
│ - COD_SEGURADO  │        │ - RAMO_SUSEP   │
│ - COD_ESTIPUL.  │        └────────────────┘
└────┬───┬────┬───┘
     │   │    │
     │   │    │ N:1
     │   │    └─────────────┐
     │   │                  │
     │   │ N:1              │
     │   └────────┐         │
     │            │         │
     │ N:1        ▼         ▼
     │     ┌─────────────────────┐
     │     │    V0CLIENTE        │
     │     │    (Client)         │
     │     │                     │
     │     │ - COD_CLIENTE       │
     │     │ - CPF_CNPJ          │
     │     │ - NOME_RAZAO        │
     │     └──────┬──────────────┘
     │            │
     │            │ 1:N
     │            ▼
     │     ┌─────────────────────┐
     │     │   V0ENDERECOS       │
     │     │   (Address)         │
     │     │                     │
     │     │ - COD_CLIENTE       │
     │     │ - TIPO_ENDERECO     │
     │     │ - LOGRADOURO        │
     │     └─────────────────────┘
     │
     │ 1:N
     ▼
┌─────────────────────┐         ┌─────────────────────┐
│  V0APOLCOSCED       │   1:1   │      GE399          │
│  (Cosurance)        │◄────────│  (Calculation)      │
│                     │         │                     │
│ - NUM_APOLICE       │         │ - NUM_APOLICE       │
│ - NUM_ENDOSSO       │         │ - PREMIO_TOTAL      │
│ - PERC_PARTICIPACAO │         │ - PREMIO_LIDER      │
│ - PREMIO_CEDIDO     │         │ - QTD_COSSEGURADORES│
└─────────────────────┘         └─────────────────────┘
```

### Relacionamentos em Entity Framework

```csharp
// PremiumReportingDbContext.cs
protected override void OnModelCreating(ModelBuilder modelBuilder)
{
    // Premium -> Policy (N:1)
    modelBuilder.Entity<Premium>()
        .HasOne(p => p.Policy)
        .WithMany(pol => pol.Premiums)
        .HasForeignKey(p => new { p.PolicyNumber, p.CompanyCode })
        .OnDelete(DeleteBehavior.Restrict);

    // Premium -> Product (N:1)
    modelBuilder.Entity<Premium>()
        .HasOne(p => p.Product)
        .WithMany()
        .HasForeignKey(p => p.ProductCode)
        .OnDelete(DeleteBehavior.Restrict);

    // Policy -> Product (N:1)
    modelBuilder.Entity<Policy>()
        .HasOne(p => p.Product)
        .WithMany(prod => prod.Policies)
        .HasForeignKey(p => p.ProductCode)
        .OnDelete(DeleteBehavior.Restrict);

    // Policy -> Client (N:1, multiple)
    modelBuilder.Entity<Policy>()
        .HasOne(p => p.InsuredClient)
        .WithMany()
        .HasForeignKey(p => p.InsuredClientCode)
        .OnDelete(DeleteBehavior.Restrict);

    modelBuilder.Entity<Policy>()
        .HasOne(p => p.Policyholder)
        .WithMany()
        .HasForeignKey(p => p.PolicyholderCode)
        .OnDelete(DeleteBehavior.Restrict);

    // Client -> Address (1:N)
    modelBuilder.Entity<Address>()
        .HasOne(a => a.Client)
        .WithMany(c => c.Addresses)
        .HasForeignKey(a => a.ClientCode)
        .OnDelete(DeleteBehavior.Cascade);

    // Policy -> Cosurance (1:N)
    modelBuilder.Entity<Cosurance>()
        .HasOne(c => c.Policy)
        .WithMany()
        .HasForeignKey(c => new { c.PolicyNumber, c.CompanyCode })
        .OnDelete(DeleteBehavior.Cascade);

    // Indexes for performance
    modelBuilder.Entity<Premium>()
        .HasIndex(p => new { p.ProcessingDate, p.CompanyCode })
        .HasDatabaseName("IX_Premium_ProcessingDate_CompanyCode");

    modelBuilder.Entity<Premium>()
        .HasIndex(p => new { p.PolicyNumber, p.EndorsementNumber })
        .HasDatabaseName("IX_Premium_Policy_Endorsement");

    modelBuilder.Entity<Address>()
        .HasIndex(a => new { a.ClientCode, a.AddressType })
        .HasDatabaseName("IX_Address_Client_Type");
}
```

---

## Modelo de Dados .NET

### DbContext Principal

```csharp
public class PremiumReportingDbContext : DbContext
{
    public PremiumReportingDbContext(DbContextOptions<PremiumReportingDbContext> options)
        : base(options)
    {
    }

    // DbSets (tabelas)
    public DbSet<Premium> Premiums { get; set; }
    public DbSet<Policy> Policies { get; set; }
    public DbSet<Product> Products { get; set; }
    public DbSet<Client> Clients { get; set; }
    public DbSet<Address> Addresses { get; set; }
    public DbSet<Endorsement> Endorsements { get; set; }
    public DbSet<Coverage> Coverages { get; set; }
    public DbSet<Cosurance> Cosurances { get; set; }
    public DbSet<CosuranceCalculation> CosuranceCalculations { get; set; }
    public DbSet<BatchJob> BatchJobs { get; set; }
    public DbSet<BatchJobExecution> BatchJobExecutions { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        base.OnModelCreating(modelBuilder);

        // Aplicar todas as configurações
        modelBuilder.ApplyConfigurationsFromAssembly(typeof(PremiumReportingDbContext).Assembly);
    }
}
```

### Configurações por Entidade

**Exemplo: PremiumConfiguration.cs**

```csharp
public class PremiumConfiguration : IEntityTypeConfiguration<Premium>
{
    public void Configure(EntityTypeBuilder<Premium> builder)
    {
        builder.ToTable("Premiums");

        builder.HasKey(p => p.PremiumId);

        builder.Property(p => p.CompanyCode)
            .IsRequired();

        builder.Property(p => p.PolicyNumber)
            .IsRequired()
            .HasMaxLength(20);

        builder.Property(p => p.TotalPremium)
            .HasColumnType("decimal(15,2)")
            .IsRequired();

        builder.Property(p => p.NetPremium)
            .HasColumnType("decimal(15,2)")
            .IsRequired();

        // Indexes
        builder.HasIndex(p => new { p.ProcessingDate, p.CompanyCode })
            .HasDatabaseName("IX_Premium_ProcessingDate_CompanyCode");

        builder.HasIndex(p => new { p.PolicyNumber, p.EndorsementNumber })
            .HasDatabaseName("IX_Premium_Policy_Endorsement");

        // Relationships configurados em OnModelCreating
    }
}
```

---

## Estratégia de Migração

### Fase 1: Mapeamento de Views para Tabelas

| View DB2 | Tabela .NET | Estratégia |
|----------|-------------|------------|
| V0PREMIOS | Premiums | Mapeamento 1:1 + campos auditoria |
| V0APOLICE | Policies | Mapeamento 1:1 + relacionamentos |
| V0PRODUTO | Products | Carga inicial + sincronização |
| V0CLIENTE | Clients | Carga inicial + sincronização |
| V0ENDERECOS | Addresses | Mapeamento 1:1 |
| V0APOLCOSCED | Cosurances | Mapeamento 1:1 |
| GE399 | CosuranceCalculations | Migração de lógica calculada |

### Fase 2: Carga Inicial de Dados

**Script de Migração SQLite**:

```sql
-- 1. Criar estrutura
CREATE TABLE Premiums (
    PremiumId INTEGER PRIMARY KEY AUTOINCREMENT,
    CompanyCode INTEGER NOT NULL,
    SusepBranch INTEGER NOT NULL,
    PolicyNumber TEXT(20) NOT NULL,
    EndorsementNumber INTEGER NOT NULL,
    MovementType INTEGER,
    IssueDate TEXT NOT NULL,
    EffectiveStartDate TEXT NOT NULL,
    EffectiveEndDate TEXT NOT NULL,
    NetPremium REAL NOT NULL,
    TotalPremium REAL NOT NULL,
    IOF REAL,
    InstallmentFee REAL,
    ProductCode INTEGER NOT NULL,
    CurrencyCode TEXT(3),
    ExchangeRate REAL,
    ProcessingDate TEXT NOT NULL,
    CreatedAt TEXT DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IX_Premium_ProcessingDate_CompanyCode
ON Premiums(ProcessingDate, CompanyCode);

CREATE INDEX IX_Premium_Policy_Endorsement
ON Premiums(PolicyNumber, EndorsementNumber);

-- 2. Popular com dados de teste
INSERT INTO Premiums (CompanyCode, SusepBranch, PolicyNumber, EndorsementNumber, ...)
SELECT ... FROM V0PREMIOS_EXPORT;
```

### Fase 3: Sincronização Incremental

**Estratégia**:

1. **Export diário de views DB2** para arquivos CSV
2. **ETL Job** carrega CSVs para SQLite/SQL Server
3. **Validação** de integridade referencial
4. **Reconciliação** com arquivos PREMIT.TXT/PREMCED.TXT

**C# ETL Service**:

```csharp
public class DataSyncService
{
    public async Task SyncPremiumsAsync(string csvFilePath)
    {
        using var reader = new StreamReader(csvFilePath);
        using var csv = new CsvReader(reader, CultureInfo.InvariantCulture);

        var records = csv.GetRecords<PremiumCsvRecord>();

        var premiums = records.Select(r => new Premium
        {
            CompanyCode = r.COD_CIA,
            PolicyNumber = r.NUM_APOLICE.ToString(),
            EndorsementNumber = r.NUM_ENDOSSO,
            TotalPremium = r.PREMIO_TOTAL,
            // ... outros campos
        });

        _context.Premiums.AddRange(premiums);
        await _context.SaveChangesAsync();
    }
}
```

---

## Considerações de Performance

### Índices Recomendados

```sql
-- Premiums: busca por data de processamento (query principal)
CREATE INDEX IX_Premium_ProcessingDate_CompanyCode
ON Premiums(ProcessingDate, CompanyCode);

-- Premiums: busca por apólice/endosso
CREATE INDEX IX_Premium_Policy_Endorsement
ON Premiums(PolicyNumber, EndorsementNumber);

-- Policies: busca por cliente
CREATE INDEX IX_Policy_InsuredClient
ON Policies(InsuredClientCode);

-- Addresses: busca por cliente e tipo
CREATE INDEX IX_Address_Client_Type
ON Addresses(ClientCode, AddressType);

-- Cosurances: busca por apólice
CREATE INDEX IX_Cosurance_Policy
ON Cosurances(PolicyNumber, EndorsementNumber);
```

### Otimizações de Query

**1. AsNoTracking para Read-Only**:

```csharp
// ✅ Correto: leitura sem tracking
var premiums = await _context.Premiums
    .AsNoTracking()
    .Where(p => p.ProcessingDate == date)
    .ToListAsync();

// ❌ Evitar: tracking desnecessário
var premiums = await _context.Premiums
    .Where(p => p.ProcessingDate == date)
    .ToListAsync();
```

**2. Projeções com Select**:

```csharp
// ✅ Correto: buscar apenas campos necessários
var summaries = await _context.Premiums
    .Where(p => p.ProcessingDate == date)
    .Select(p => new PremiumSummary
    {
        PolicyNumber = p.PolicyNumber,
        TotalPremium = p.TotalPremium
    })
    .ToListAsync();
```

**3. Include com ThenInclude para Joins**:

```csharp
// ✅ Correto: carregamento eager de relacionamentos
var policies = await _context.Policies
    .Include(p => p.Product)
    .Include(p => p.InsuredClient)
        .ThenInclude(c => c.Addresses)
    .Where(p => p.PolicyNumber == policyNumber)
    .FirstOrDefaultAsync();
```

### Estimativas de Volume

| Tabela | Registros/Mês | Tamanho/Registro | Crescimento/Ano |
|--------|---------------|------------------|-----------------|
| Premiums | 10.000 | 250 bytes | 2.5 MB |
| Policies | 2.000 | 300 bytes | 600 KB |
| Clients | 5.000 | 200 bytes | 1 MB |
| Addresses | 15.000 | 150 bytes | 2.25 MB |
| Cosurances | 1.000 | 100 bytes | 100 KB |

**Total estimado**: ~6.5 MB/ano em SQLite (desenvolvimento), compactável com indexação.

---

## Referências

- **Especificação Completa**: `specs/001-vamos-migrar-sistema/data-model.md`
- **Estruturas COBOL**: `docs/legacy-system/03-data-structures.md`
- **Entity Framework Core**: https://learn.microsoft.com/ef/core/
- **COBOL DB2 SQL**: IBM DB2 for z/OS SQL Reference

---

**Documento criado em**: 2025-10-27
**Última atualização**: 2025-10-27
**Versão**: 1.0
