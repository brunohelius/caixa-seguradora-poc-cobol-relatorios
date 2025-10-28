# Glossário Técnico e de Negócio
## Sistema RG1866B - Prêmios Emitidos SUSEP Circular 360

**Documento**: 10-glossary.md
**Versão**: 1.0.0
**Data**: 27 de outubro de 2025
**Projeto**: Migração COBOL RG1866B para .NET 9

[← Voltar ao Índice](README.md)

---

## Índice

1. [Termos Técnicos COBOL/Mainframe](#termos-técnicos-cobolmainframe)
2. [Termos .NET/Tecnologia Moderna](#termos-nettecnologia-moderna)
3. [Termos de Negócio - Seguros](#termos-de-negócio-seguros)
4. [Termos Regulatórios SUSEP](#termos-regulatórios-susep)
5. [Acrônimos e Siglas](#acrônimos-e-siglas)
6. [Mapeamento COBOL → .NET](#mapeamento-cobol-net)
7. [Tipos de Dados](#tipos-de-dados)
8. [Conceitos de Arquitetura](#conceitos-de-arquitetura)

---

## Termos Técnicos COBOL/Mainframe

### A

**ABEND (ABnormal END)**
- **Definição**: Término anormal de um programa mainframe, equivalente a uma exceção não tratada
- **Exemplo**: `ABEND S0C7` indica dados numéricos inválidos
- **Impacto**: Causa falha do job JCL e requer intervenção operacional
- **.NET Equivalente**: `System.Exception` não capturada que termina a aplicação

**ACCEPT**
- **Definição**: Instrução COBOL para ler dados de entrada (console, arquivo de sistema)
- **Sintaxe**: `ACCEPT WS-DATA-ATUAL FROM DATE YYYYMMDD`
- **.NET Equivalente**: `Console.ReadLine()` ou `DateTime.Now`

### B

**BLANK WHEN ZERO**
- **Definição**: Cláusula COBOL que exibe espaços quando um campo numérico é zero
- **Exemplo**: `05 WS-VALOR PIC 9(5) BLANK WHEN ZERO.`
- **.NET Equivalente**: `value == 0 ? "" : value.ToString()`

**BLKSIZE (Block Size)**
- **Definição**: Tamanho do bloco de dados em bytes para otimizar I/O em arquivos mainframe
- **Uso típico**: `BLKSIZE=12000` para `LRECL=1200` (10 registros por bloco)
- **Impacto**: Multiplica eficiência de leitura/escrita por 10x ou mais

### C

**CALL**
- **Definição**: Instrução COBOL para invocar subprogramas (módulos externos)
- **Sintaxe**: `CALL 'RE0001S' USING WS-PARAMETRO-01 WS-PARAMETRO-02`
- **.NET Equivalente**: Invocação de método ou serviço externo via DI

**CLOSE**
- **Definição**: Fecha um arquivo COBOL aberto previamente com OPEN
- **Sintaxe**: `CLOSE ARQUIVO-ENTRADA`
- **Importante**: Libera recursos do sistema operacional
- **.NET Equivalente**: `stream.Close()` ou `await stream.DisposeAsync()`

**COBOL (COmmon Business-Oriented Language)**
- **Definição**: Linguagem de programação de alto nível criada em 1959 para aplicações comerciais
- **Características**: Verbosa, legível, orientada a registros, forte em cálculos decimais
- **Uso**: Dominante em sistemas bancários, seguros e governo (mainframes)

**COMP (COMPutational)**
- **Definição**: Formato de armazenamento numérico binário em COBOL
- **Variações**:
  - `COMP` ou `COMP-4`: Binário (2, 4 ou 8 bytes)
  - `COMP-1`: Ponto flutuante de precisão simples (4 bytes)
  - `COMP-2`: Ponto flutuante de precisão dupla (8 bytes)
  - `COMP-3`: Packed decimal (mais usado - veja abaixo)
- **.NET Equivalente**: `short`, `int`, `long`, `float`, `double`

**COMP-3 (Packed Decimal)**
- **Definição**: Formato de armazenamento numérico onde cada dígito ocupa 4 bits (meio byte)
- **Estrutura**: Dois dígitos por byte, último nibble contém o sinal (C=positivo, D=negativo, F=unsigned)
- **Exemplo**: `PIC 9(5)V99 COMP-3` → número de 7 dígitos com 2 decimais implícitos, ocupa 4 bytes
  - Valor 12345.67 → hex `01 23 45 67 C` (5 nibbles = 4 bytes)
- **Vantagem**: Economiza 50% de espaço vs. zoned decimal, cálculos rápidos em hardware mainframe
- **.NET Equivalente**: `decimal` (128 bits, até 28-29 dígitos, precisão exata)

**CONTINUE**
- **Definição**: Instrução COBOL que não faz nada (placeholder ou NOP)
- **Uso**: Em estruturas IF/ELSE quando uma branch deve ser vazia
- **.NET Equivalente**: Bloco vazio `{}` ou comentário

**CURSOR**
- **Definição**: Estrutura DB2 que permite leitura linha a linha de resultados SQL
- **Ciclo de vida**: DECLARE → OPEN → FETCH (loop) → CLOSE
- **Exemplo**:
  ```cobol
  DECLARE C1 CURSOR FOR
      SELECT NUM_APOLICE, VLR_PREMIO FROM V0PREMIOS
      WHERE DTA_EMISSAO BETWEEN :WS-DATA-INI AND :WS-DATA-FIM

  OPEN C1
  FETCH C1 INTO :WS-NUM-APOLICE, :WS-VLR-PREMIO
  ... (processar)
  CLOSE C1
  ```
- **.NET Equivalente**: `IAsyncEnumerable<T>` ou `IDataReader`

### D

**DATA DIVISION**
- **Definição**: Seção do programa COBOL onde todas as variáveis são declaradas
- **Subseções**:
  - `FILE SECTION`: Estruturas de arquivos de entrada/saída
  - `WORKING-STORAGE SECTION`: Variáveis internas (memória durante execução)
  - `LINKAGE SECTION`: Parâmetros recebidos de programas chamadores
- **.NET Equivalente**: Declarações de campos/propriedades em classes

**DB2 (Database 2)**
- **Definição**: Sistema de gerenciamento de banco de dados relacional da IBM para mainframe z/OS
- **Características**: ACID compliant, otimizado para COBOL, SQL embarcado
- **Versões**: DB2 for z/OS vs. DB2 LUW (Linux/Unix/Windows)
- **.NET Equivalente**: SQL Server, PostgreSQL, Oracle

**DCB (Data Control Block)**
- **Definição**: Parâmetros JCL que definem características físicas de um arquivo
- **Atributos**:
  - `RECFM`: Format (FB=Fixed Blocked, VB=Variable Blocked)
  - `LRECL`: Tamanho lógico do registro
  - `BLKSIZE`: Tamanho do bloco físico
- **Exemplo**: `DCB=(RECFM=FB,LRECL=1200,BLKSIZE=12000)`

**DISPLAY**
- **Definição**: Instrução COBOL para exibir mensagens no console ou sysout
- **Sintaxe**: `DISPLAY 'TOTAL DE REGISTROS: ' WS-CONTADOR`
- **.NET Equivalente**: `Console.WriteLine()` ou `ILogger.LogInformation()`

### E

**EBCDIC (Extended Binary Coded Decimal Interchange Code)**
- **Definição**: Codificação de caracteres usada em mainframes IBM (alternativa ao ASCII)
- **Diferenças**: Ordem de classificação diferente (maiúsculas < minúsculas), códigos de controle distintos
- **Conversão**: Necessária ao transferir dados entre mainframe e sistemas ASCII/.NET
- **.NET Equivalente**: `Encoding.GetEncoding(37)` para EBCDIC-US

**ENVIRONMENT DIVISION**
- **Definição**: Seção do programa COBOL que define interação com ambiente (arquivos, sistema)
- **Subseções**:
  - `CONFIGURATION SECTION`: Características do computador
  - `INPUT-OUTPUT SECTION`: Mapeia arquivos lógicos para arquivos físicos
- **.NET Equivalente**: Configuração de `appsettings.json` e DI container

**EXEC SQL**
- **Definição**: Delimitador COBOL para incluir comandos SQL embarcados
- **Sintaxe**:
  ```cobol
  EXEC SQL
      SELECT NUM_APOLICE INTO :WS-NUM-APOLICE
      FROM V0APOLICE WHERE COD_APOLICE = :WS-COD-APOLICE
  END-EXEC.
  ```
- **Importante**: Variáveis COBOL usam `:` como prefixo dentro do SQL
- **.NET Equivalente**: LINQ to Entities ou Entity Framework Core queries

### F

**FD (File Description)**
- **Definição**: Cláusula COBOL que descreve estrutura de um arquivo na FILE SECTION
- **Sintaxe**:
  ```cobol
  FD  ARQUIVO-ENTRADA
      LABEL RECORDS ARE STANDARD
      RECORDING MODE IS F
      BLOCK CONTAINS 0 RECORDS.
  01  REGISTRO-ENTRADA.
      05 CAMPO-1  PIC X(10).
      05 CAMPO-2  PIC 9(5)V99 COMP-3.
  ```
- **.NET Equivalente**: Class definition para serialização/deserialização

**FETCH**
- **Definição**: Instrução SQL COBOL para recuperar próxima linha de um cursor aberto
- **Sintaxe**: `EXEC SQL FETCH C1 INTO :WS-VAR1, :WS-VAR2 END-EXEC`
- **Controle**: Usa `SQLCODE` para detectar fim de dados (SQLCODE=100)
- **.NET Equivalente**: `await foreach (var item in asyncEnumerable)` ou `reader.Read()`

### G

**GO TO**
- **Definição**: Instrução COBOL para desvio incondicional de fluxo
- **Sintaxe**: `GO TO 2000-PROCESSAR-PROXIMO`
- **Uso**: Comum em COBOL estruturado com seções numeradas (e.g., R0100, R0200)
- **Crítica**: Dificulta manutenção quando usado em excesso (spaghetti code)
- **.NET Equivalente**: `goto` (desaconselhado), preferir estruturas de controle (`if`, `while`)

### I

**IDENTIFICATION DIVISION**
- **Definição**: Primeira seção obrigatória de um programa COBOL, contém metadados
- **Campos**:
  - `PROGRAM-ID`: Nome do programa (e.g., RG1866B)
  - `AUTHOR`: Desenvolvedor
  - `DATE-WRITTEN`: Data de criação
  - `DATE-COMPILED`: Data de última compilação
- **.NET Equivalente**: Assembly attributes `[AssemblyTitle]`, `[AssemblyVersion]`

**IF**
- **Definição**: Estrutura condicional COBOL
- **Sintaxe verbosa**:
  ```cobol
  IF WS-VALOR > 1000
      DISPLAY 'ALTO'
  ELSE
      IF WS-VALOR > 500
          DISPLAY 'MEDIO'
      ELSE
          DISPLAY 'BAIXO'
      END-IF
  END-IF.
  ```
- **.NET Equivalente**: `if/else if/else` ou `switch` expression

**INCLUDE**
- **Definição**: Diretiva de pré-processador COBOL para incluir código externo (copybooks)
- **Sintaxe**: `EXEC SQL INCLUDE SQLCA END-EXEC` (para incluir SQL Communication Area)
- **.NET Equivalente**: `using` directive ou `#include` em C/C++

### J

**JCL (Job Control Language)**
- **Definição**: Linguagem de scripting para controlar execução de jobs em mainframe z/OS
- **Componentes**:
  - `JOB`: Define o job (prioridade, classe, tempo limite)
  - `EXEC`: Executa um programa ou procedimento
  - `DD` (Data Definition): Aloca arquivos, datasets
- **Exemplo**:
  ```jcl
  //MYJOB    JOB (ACCT),'DESCRIPTION',CLASS=A
  //STEP1    EXEC PGM=RG1866B,PARM='202510,1'
  //STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
  //SYSOUT   DD SYSOUT=*
  ```
- **.NET Equivalente**: Docker Compose, Kubernetes manifests, ou scripts bash/PowerShell

**JOB**
- **Definição**: Unidade de trabalho no mainframe, composta de um ou mais steps
- **Ciclo de vida**: Submetido → Fila → Execução → Saída (logs)
- **Controle**: TWS (Tivoli Workload Scheduler) ou JES2/JES3

### L

**LINKAGE SECTION**
- **Definição**: Seção da DATA DIVISION que declara parâmetros recebidos via CALL
- **Exemplo**:
  ```cobol
  LINKAGE SECTION.
  01  LK-PARAMETRO-ENTRADA.
      05 LK-COD-EMPRESA     PIC 9(3).
      05 LK-ANO-MES         PIC 9(6).
  ```
- **Uso**: Permite comunicação entre programa principal e subrotinas
- **.NET Equivalente**: Parâmetros de método ou construtor

**LRECL (Logical RECord Length)**
- **Definição**: Tamanho lógico de um registro em bytes
- **Exemplos**:
  - PREMIT.TXT: `LRECL=1200` (1200 bytes por linha)
  - PREMCED.TXT: `LRECL=800` (800 bytes por linha)
- **Importância**: Define layout fixo dos arquivos SUSEP

### M

**MOVE**
- **Definição**: Instrução COBOL para copiar/atribuir valor a uma variável
- **Sintaxes**:
  - `MOVE 100 TO WS-CONTADOR` (atribuição literal)
  - `MOVE WS-VALOR-ORIGEM TO WS-VALOR-DESTINO` (cópia)
  - `MOVE SPACES TO WS-CAMPO-TEXTO` (limpar string)
  - `MOVE ZEROS TO WS-CAMPO-NUMERO` (zerar número)
- **Conversões automáticas**: COBOL converte tipos implicitamente (numérico ↔ alfanumérico)
- **.NET Equivalente**: Operador `=` ou `string.Empty`, `0`

### O

**OCCURS**
- **Definição**: Cláusula COBOL para definir arrays (estruturas repetidas)
- **Sintaxe**:
  ```cobol
  01  WS-TABELA-PRODUTOS.
      05 WS-PRODUTO OCCURS 50 TIMES.
         10 WS-COD-PRODUTO    PIC 9(5).
         10 WS-NOME-PRODUTO   PIC X(30).
  ```
- **Acesso**: `MOVE 'PRODUTO1' TO WS-NOME-PRODUTO(1)`
- **.NET Equivalente**: Arrays `T[]` ou `List<T>`

**OPEN**
- **Definição**: Abre um arquivo COBOL para leitura/escrita
- **Modos**:
  - `OPEN INPUT ARQUIVO-ENTRADA` (leitura)
  - `OPEN OUTPUT ARQUIVO-SAIDA` (escrita, cria novo)
  - `OPEN EXTEND ARQUIVO-LOG` (append)
  - `OPEN I-O ARQUIVO-RANDOM` (leitura e escrita)
- **.NET Equivalente**: `File.OpenRead()`, `File.OpenWrite()`, `FileStream`

### P

**PERFORM**
- **Definição**: Instrução COBOL para invocar seções/parágrafos (equivalente a chamada de função)
- **Variações**:
  - `PERFORM 3000-CALCULAR-TOTAL` (executa uma vez)
  - `PERFORM UNTIL WS-EOF = 'S'` (loop condicional)
  - `PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 50` (loop indexado)
- **.NET Equivalente**: Chamada de método, `while`, `for`

**PIC (PICTURE)**
- **Definição**: Cláusula COBOL que define tipo e tamanho de um campo
- **Códigos**:
  - `9`: Dígito numérico (0-9)
  - `X`: Caractere alfanumérico (qualquer)
  - `A`: Caractere alfabético (A-Z, a-z, espaço)
  - `S`: Sinal (+ ou -)
  - `V`: Decimal implícito (não ocupa espaço)
  - `Z`: Zero suprimido (exibido como espaço)
- **Exemplos**:
  - `PIC 9(5)`: 5 dígitos numéricos (00000-99999)
  - `PIC X(30)`: String de 30 caracteres
  - `PIC 9(13)V99`: 15 dígitos com 2 casas decimais (decimal implícito)
  - `PIC S9(7) COMP-3`: Número signed de 7 dígitos em packed decimal
- **.NET Mapeamento**:
  - `PIC 9(n)` → `int`, `long` (dependendo de n)
  - `PIC X(n)` → `string`
  - `PIC 9(n)V99` → `decimal`

**PROCEDURE DIVISION**
- **Definição**: Seção do programa COBOL onde a lógica executável reside
- **Estrutura**: Dividida em seções (SECTION) e parágrafos (PARAGRAPH)
- **Exemplo**:
  ```cobol
  PROCEDURE DIVISION.
  0000-MAIN SECTION.
      PERFORM 1000-INICIALIZAR
      PERFORM 2000-PROCESSAR
      PERFORM 9000-FINALIZAR
      STOP RUN.

  1000-INICIALIZAR SECTION.
      OPEN INPUT ARQUIVO-ENTRADA
      ...
  ```
- **.NET Equivalente**: Métodos de uma classe

### R

**READ**
- **Definição**: Lê próximo registro de um arquivo sequencial COBOL
- **Sintaxe**:
  ```cobol
  READ ARQUIVO-ENTRADA INTO WS-REGISTRO-TRABALHO
      AT END
          MOVE 'S' TO WS-EOF
      NOT AT END
          ADD 1 TO WS-CONTADOR
  END-READ.
  ```
- **.NET Equivalente**: `StreamReader.ReadLine()` ou `reader.Read()`

**REDEFINES**
- **Definição**: Cláusula COBOL que permite múltiplas interpretações da mesma área de memória (union)
- **Exemplo**:
  ```cobol
  01  WS-DATA-NUMERICA     PIC 9(8).
  01  WS-DATA-FORMATADA REDEFINES WS-DATA-NUMERICA.
      05 WS-ANO            PIC 9(4).
      05 WS-MES            PIC 9(2).
      05 WS-DIA            PIC 9(2).
  ```
- **Uso**: Economiza memória, converte formatos
- **.NET Equivalente**: `StructLayout` com `FieldOffset` ou pattern matching

**ROUNDED**
- **Definição**: Cláusula COBOL para arredondar resultado de operações aritméticas
- **Comportamento**: Arredonda para o dígito menos significativo do campo de destino
- **Exemplo**: `COMPUTE WS-RESULTADO ROUNDED = WS-VALOR1 / WS-VALOR2`
- **Regra**: Arredonda 0.5 para cima (MidpointRounding.AwayFromZero)
- **.NET Equivalente**: `Math.Round(value, decimals, MidpointRounding.AwayFromZero)`

### S

**SELECT**
- **Definição**: Cláusula COBOL na INPUT-OUTPUT SECTION que associa nome lógico a arquivo físico
- **Sintaxe**:
  ```cobol
  SELECT ARQUIVO-ENTRADA
      ASSIGN TO ENTRADA
      ORGANIZATION IS SEQUENTIAL
      ACCESS MODE IS SEQUENTIAL
      FILE STATUS IS WS-FILE-STATUS.
  ```
- **.NET Equivalente**: File path configuration em `appsettings.json`

**SQLCA (SQL Communication Area)**
- **Definição**: Estrutura DB2 que contém informações sobre última operação SQL
- **Campos principais**:
  - `SQLCODE`: Código de retorno (0=sucesso, 100=não encontrado, <0=erro)
  - `SQLERRM`: Mensagem de erro
- **Exemplo**:
  ```cobol
  EXEC SQL SELECT ... END-EXEC.
  IF SQLCODE NOT = 0
      DISPLAY 'ERRO SQL: ' SQLCODE
  END-IF.
  ```
- **.NET Equivalente**: `DbException` ou `try/catch` em Entity Framework

**SQLCODE**
- **Definição**: Código de retorno de operações SQL embarcadas em COBOL
- **Valores**:
  - `0`: Sucesso
  - `100`: Não encontrado (NOT FOUND)
  - `-803`: Violação de chave duplicada
  - `-811`: Retornou múltiplas linhas quando esperava uma
  - `-904`: Recurso indisponível
- **.NET Equivalente**: `SqlException.Number` ou `PostgresException.SqlState`

**STOP RUN**
- **Definição**: Instrução COBOL que termina execução do programa
- **Sintaxe**: `STOP RUN.`
- **Comportamento**: Fecha arquivos, libera recursos, retorna controle ao sistema
- **.NET Equivalente**: `Environment.Exit(0)` ou `return` do método Main

**STRING**
- **Definição**: Instrução COBOL para concatenar strings
- **Sintaxe**:
  ```cobol
  STRING WS-NOME DELIMITED BY SIZE
         '-' DELIMITED BY SIZE
         WS-SOBRENOME DELIMITED BY SIZE
      INTO WS-NOME-COMPLETO
  END-STRING.
  ```
- **.NET Equivalente**: `string.Concat()` ou `$"{nome}-{sobrenome}"`

### T

**TWS (Tivoli Workload Scheduler)**
- **Definição**: Software IBM para agendamento e orquestração de jobs em ambientes enterprise
- **Funcionalidades**:
  - Calendarização complexa (workdays, feriados)
  - Dependências entre jobs
  - Monitoramento e alertas
  - Recuperação de falhas
- **.NET Equivalente**: Hangfire, Quartz.NET, Azure Functions com Timer Trigger

### U

**UNSTRING**
- **Definição**: Instrução COBOL para dividir string em múltiplas variáveis (parsing)
- **Sintaxe**:
  ```cobol
  UNSTRING WS-DATA-STRING DELIMITED BY '/'
      INTO WS-DIA WS-MES WS-ANO
  END-UNSTRING.
  ```
- **.NET Equivalente**: `string.Split('/')` ou regex

### V

**V (Virtual Decimal Point)**
- **Definição**: Símbolo no PIC clause que indica posição do ponto decimal (não armazenado)
- **Exemplo**: `PIC 9(13)V99` → 15 dígitos, últimos 2 são decimais
  - Valor 12345.67 armazenado como `000000000001234567` (15 dígitos)
- **Importante**: `.NET decimal` armazena o ponto, COBOL não (economia de 1 byte)

### W

**WORKING-STORAGE SECTION**
- **Definição**: Seção da DATA DIVISION onde variáveis de trabalho são declaradas
- **Características**: Alocadas na memória durante toda execução do programa
- **Níveis**:
  - `01`: Nível raiz (grupo ou campo independente)
  - `05`, `10`, `15`, etc.: Níveis hierárquicos (subcampos)
  - `77`: Campo independente (não pode ter subcampos)
  - `88`: Condição nomeada (value test)
- **Exemplo**:
  ```cobol
  WORKING-STORAGE SECTION.
  01  WS-CONTADORES.
      05 WS-TOTAL-REGISTROS    PIC 9(7) VALUE ZERO.
      05 WS-TOTAL-ERROS        PIC 9(5) VALUE ZERO.

  77  WS-EOF                   PIC X VALUE 'N'.
      88 EOF-ATINGIDO          VALUE 'S'.
  ```
- **.NET Equivalente**: Campos privados ou propriedades de instância

**WRITE**
- **Definição**: Escreve registro em arquivo de saída COBOL
- **Sintaxe**:
  ```cobol
  WRITE REGISTRO-SAIDA FROM WS-REGISTRO-TRABALHO
      AFTER ADVANCING 1 LINES
  END-WRITE.
  ```
- **Comportamento**: Adiciona registro ao final do arquivo (se aberto em OUTPUT/EXTEND)
- **.NET Equivalente**: `StreamWriter.WriteLine()` ou `writer.Write()`

### Z

**ZERO/ZEROS/ZEROES**
- **Definição**: Constante figurativa COBOL que representa zero numérico ou string de zeros
- **Uso**: `MOVE ZEROS TO WS-CONTADOR` (zera variável)
- **.NET Equivalente**: `0` ou `default(T)`

---

## Termos .NET/Tecnologia Moderna

### A

**API (Application Programming Interface)**
- **Definição**: Interface que permite comunicação entre sistemas via HTTP/HTTPS
- **Tipos**:
  - REST API: Usa métodos HTTP (GET, POST, PUT, DELETE)
  - SOAP API: Usa XML e WSDL
  - GraphQL: Query language para APIs
- **Neste projeto**: ASP.NET Core Web API com 28 endpoints

**ASP.NET Core**
- **Definição**: Framework web multiplataforma da Microsoft para construir APIs e web apps
- **Versão**: 9.0 (LTS - Long Term Support até novembro 2027)
- **Características**: Alto desempenho, modular, cloud-ready

**Async/Await**
- **Definição**: Padrão C# para programação assíncrona não bloqueante
- **Sintaxe**:
  ```csharp
  public async Task<List<Premium>> GetPremiumsAsync()
  {
      return await _context.Premiums.ToListAsync();
  }
  ```
- **Benefício**: Libera threads durante operações I/O (DB, files, HTTP)
- **Equivalente COBOL**: Não existe - COBOL é síncrono/bloqueante

**AutoMapper**
- **Definição**: Biblioteca .NET para mapeamento objeto-objeto (conversão de DTOs)
- **Uso**:
  ```csharp
  var dto = _mapper.Map<PremiumDto>(premiumEntity);
  ```
- **Neste projeto**: Converte entre entidades EF Core e DTOs de API

### C

**Clean Architecture**
- **Definição**: Padrão arquitetural que separa responsabilidades em camadas concêntricas
- **Camadas** (dependências apontam para dentro):
  1. **Core** (centro): Entidades, interfaces, regras de negócio
  2. **Infrastructure**: Implementações (DB, APIs externas, file I/O)
  3. **API**: Controllers, middleware, apresentação
- **Benefícios**: Testabilidade, independência de frameworks, manutenibilidade

**Controller**
- **Definição**: Classe ASP.NET Core que expõe endpoints HTTP
- **Exemplo**:
  ```csharp
  [ApiController]
  [Route("api/v1/[controller]")]
  public class PremiumsController : ControllerBase
  {
      [HttpGet]
      public async Task<ActionResult<List<PremiumDto>>> GetAll()
      {
          // ...
      }
  }
  ```
- **Responsabilidade**: Receber requisições HTTP, validar, chamar serviços, retornar respostas

**CORS (Cross-Origin Resource Sharing)**
- **Definição**: Mecanismo de segurança que permite APIs aceitar requisições de diferentes domínios
- **Configuração**:
  ```csharp
  builder.Services.AddCors(options =>
  {
      options.AddPolicy("AllowFrontend", policy =>
          policy.WithOrigins("http://localhost:5173")
                .AllowAnyMethod()
                .AllowAnyHeader());
  });
  ```

### D

**Decimal (C#)**
- **Definição**: Tipo numérico .NET de 128 bits com precisão exata para cálculos financeiros
- **Características**:
  - 28-29 dígitos de precisão
  - Sem erros de arredondamento binário (vs. float/double)
  - Aloca 16 bytes (vs. 8 para double)
- **Equivalente COBOL**: COMP-3 (packed decimal)
- **Uso obrigatório**: Todos os cálculos monetários neste projeto

**Dependency Injection (DI)**
- **Definição**: Padrão de design onde dependências são fornecidas externamente (injeção via construtor)
- **Configuração** (Program.cs):
  ```csharp
  builder.Services.AddScoped<IPremiumRepository, PremiumRepository>();
  builder.Services.AddScoped<IPremiumService, PremiumService>();
  ```
- **Uso** (Controller):
  ```csharp
  public PremiumsController(IPremiumService premiumService)
  {
      _premiumService = premiumService;
  }
  ```
- **Benefícios**: Testabilidade (mocking), desacoplamento, gerenciamento de lifetime

**DTO (Data Transfer Object)**
- **Definição**: Objeto simples usado para transferir dados entre camadas (API ↔ Cliente)
- **Exemplo**:
  ```csharp
  public class PremiumDto
  {
      public long PolicyNumber { get; set; }
      public decimal Amount { get; set; }
      public DateTime EffectiveDate { get; set; }
  }
  ```
- **Diferença de Entity**: DTO não tem lógica, não é rastreado pelo EF Core

### E

**Entity Framework Core (EF Core)**
- **Definição**: ORM (Object-Relational Mapper) da Microsoft para .NET
- **Versão**: 9.0
- **Funcionalidades**:
  - Mapeia classes C# para tabelas SQL
  - LINQ to Entities para queries
  - Change tracking
  - Migrations (versionamento de schema)
- **Neste projeto**: Mapeia 15 entidades para views/tabelas DB2

**Environment Variables**
- **Definição**: Variáveis do sistema operacional usadas para configuração (secrets, endpoints)
- **Leitura** (.NET):
  ```csharp
  var connString = Environment.GetEnvironmentVariable("DATABASE_URL");
  ```
- **Uso**: Separar configuração de código (12-factor app)

### H

**Hangfire**
- **Definição**: Biblioteca .NET para agendamento de jobs em background
- **Funcionalidades**:
  - Jobs recorrentes (cron expressions)
  - Retry automático
  - Dashboard web para monitoramento
- **Neste projeto**: Substitui TWS para execução mensal do RG1866B

**HTTP Status Codes**
- **Definição**: Códigos numéricos em respostas HTTP que indicam resultado
- **Principais**:
  - `200 OK`: Sucesso
  - `201 Created`: Recurso criado
  - `400 Bad Request`: Erro de validação
  - `404 Not Found`: Recurso não encontrado
  - `500 Internal Server Error`: Erro no servidor

### I

**IAsyncEnumerable<T>**
- **Definição**: Interface .NET para streaming assíncrono de dados
- **Uso**:
  ```csharp
  public async IAsyncEnumerable<Premium> GetPremiumsAsync()
  {
      await foreach (var premium in _context.Premiums.AsAsyncEnumerable())
      {
          yield return premium;
      }
  }
  ```
- **Benefício**: Processa milhões de registros sem carregar tudo na memória
- **Equivalente COBOL**: CURSOR (FETCH em loop)

**IoC (Inversion of Control)**
- **Definição**: Princípio de design onde o framework controla o fluxo (não o código do desenvolvedor)
- **Implementação**: Dependency Injection container (.NET)

### L

**LINQ (Language Integrated Query)**
- **Definição**: Sintaxe SQL-like dentro de C# para queries em coleções/DB
- **Exemplo**:
  ```csharp
  var premiums = await _context.Premiums
      .Where(p => p.EffectiveDate >= startDate && p.EffectiveDate <= endDate)
      .OrderBy(p => p.PolicyNumber)
      .ToListAsync();
  ```
- **Equivalente COBOL**: Consultas SQL embarcadas com `EXEC SQL`

### M

**Middleware**
- **Definição**: Componentes ASP.NET Core que processam requisições HTTP em pipeline
- **Exemplos**: Logging, autenticação, tratamento de exceções, CORS
- **Ordem importa**: Executam na ordem definida em `Program.cs`

**Migration (EF Core)**
- **Definição**: Arquivo C# que descreve mudanças no schema do banco de dados
- **Comandos**:
  - `dotnet ef migrations add InitialCreate`: Cria nova migration
  - `dotnet ef database update`: Aplica migrations pendentes
- **Versionamento**: Permite rollback e histórico de mudanças

### O

**ORM (Object-Relational Mapper)**
- **Definição**: Framework que mapeia objetos (classes) para tabelas relacionais
- **Exemplos**: Entity Framework Core, Dapper, NHibernate
- **Benefício**: Elimina SQL manual, type-safety, produtividade

### R

**Repository Pattern**
- **Definição**: Padrão que encapsula acesso a dados, abstrai persistência
- **Exemplo**:
  ```csharp
  public interface IPremiumRepository
  {
      Task<Premium> GetByIdAsync(long id);
      IAsyncEnumerable<Premium> GetAllAsync();
      Task AddAsync(Premium premium);
  }
  ```
- **Benefício**: Substitui implementação (mock em testes, troca de DB)

**REST (Representational State Transfer)**
- **Definição**: Estilo arquitetural para APIs web baseado em HTTP
- **Princípios**:
  - Stateless (sem sessão no servidor)
  - Recursos identificados por URIs (`/api/v1/premiums/123`)
  - Métodos HTTP semânticos (GET=leitura, POST=criação, PUT=atualização, DELETE=remoção)
  - Representações (JSON, XML)

### S

**Serilog**
- **Definição**: Biblioteca .NET para logging estruturado
- **Características**:
  - Logs em formato JSON com propriedades
  - Múltiplos sinks (console, arquivos, Application Insights, Seq)
  - Performance otimizada
- **Exemplo**:
  ```csharp
  _logger.LogInformation("Processando {Count} prêmios para período {StartDate}-{EndDate}",
      count, startDate, endDate);
  ```

**Swagger/OpenAPI**
- **Definição**: Especificação para descrever APIs REST + UI interativa para testar endpoints
- **Neste projeto**: Documentação automática em https://localhost:5001/swagger
- **Ferramenta**: Swashbuckle.AspNetCore

### T

**Task<T>**
- **Definição**: Tipo .NET que representa operação assíncrona que retorna `T`
- **Uso**: Todos os métodos async retornam `Task<T>` ou `Task` (void)
- **Exemplo**: `Task<List<Premium>>` representa operação que retornará lista de prêmios

### U

**Unit of Work Pattern**
- **Definição**: Padrão que agrupa múltiplas operações de repositório em uma transação
- **EF Core**: DbContext já implementa Unit of Work (SaveChangesAsync commita tudo)

---

## Termos de Negócio - Seguros

### A

**Apólice**
- **Definição**: Contrato de seguro que formaliza acordo entre seguradora e segurado
- **Identificador**: Número da apólice (único por companhia)
- **Componentes**: Coberturas, prêmio, vigência, dados do segurado
- **Tabela DB2**: V0APOLICE

**Aviso de Sinistro**
- **Definição**: Comunicação formal de ocorrência de sinistro à seguradora
- **Prazo**: Varia por produto (geralmente 7 dias úteis)

### C

**Cancelamento**
- **Definição**: Término antecipado da apólice por solicitação do segurado ou seguradora
- **Tipos**:
  - **A pedido do segurado**: Com direito a restituição proporcional
  - **Por inadimplência**: Atraso no pagamento do prêmio
  - **Por perda total**: Sinistro que esgota capital segurado
- **Impacto no prêmio**: Movimentação negativa (crédito/estorno)

**Capital Segurado**
- **Definição**: Valor máximo que a seguradora se compromete a pagar em caso de sinistro
- **Exemplo**: Seguro residencial com capital de R$ 500.000
- **Relação**: Prêmio é calculado como % do capital segurado

**Circular SUSEP**
- **Definição**: Norma regulatória emitida pela SUSEP (equivalente a resolução)
- **Circular 360/2008**: Estabelece regras para envio de prêmios emitidos
- **Obrigatoriedade**: Seguradoras devem reportar mensalmente

**Cliente/Segurado**
- **Definição**: Pessoa física ou jurídica que contrata seguro
- **Identificação**: CPF (pessoa física) ou CNPJ (pessoa jurídica)
- **Tabela DB2**: V0CLIENTE

**Cobertura**
- **Definição**: Risco específico incluído na apólice (incêndio, roubo, danos elétricos, etc.)
- **Tipos**:
  - **Básica**: Cobertura principal obrigatória
  - **Adicional**: Coberturas opcionais contratadas
- **Tabela DB2**: V0COBERTURAS

**Comissão**
- **Definição**: Remuneração paga ao corretor de seguros pela intermediação
- **Base de cálculo**: Percentual sobre o prêmio líquido
- **Exemplo**: 15% de R$ 1.000 = R$ 150 de comissão

**Cosseguro**
- **Definição**: Operação em que múltiplas seguradoras compartilham um mesmo risco
- **Componentes**:
  - **Líder**: Seguradora que administra a apólice (Caixa Seguradora)
  - **Participantes**: Demais seguradoras (com % de participação)
- **Exemplo**: Risco de R$ 10 milhões dividido entre 3 seguradoras (40%, 35%, 25%)
- **Tabela DB2**: V0COSSEGURO, GE399

### E

**Emissão**
- **Definição**: Ato de formalizar nova apólice ou endosso
- **Data de emissão**: Data em que o documento foi gerado
- **Prêmio emitido**: Valor total cobrado na emissão
- **Tabela DB2**: V0PREMIOS (COD_MOVIMENTO=1 para emissão)

**Endosso**
- **Definição**: Alteração contratual em apólice vigente (não renovação)
- **Tipos**:
  - **Endosso de inclusão**: Adiciona cobertura/bem
  - **Endosso de exclusão**: Remove cobertura/bem
  - **Endosso de alteração**: Modifica dados cadastrais, vigência, etc.
- **Impacto no prêmio**: Pode gerar prêmio adicional ou restituição
- **Tabela DB2**: V0ENDOSSO

**Estipulante**
- **Definição**: Pessoa jurídica que contrata seguro em grupo (em nome de terceiros)
- **Exemplo**: Empresa que contrata seguro de vida para funcionários
- **Diferença**: Estipulante paga, segurado é beneficiado

### F

**Franquia**
- **Definição**: Valor ou percentual que o segurado assume em caso de sinistro (participação obrigatória)
- **Exemplo**: Franquia de R$ 2.000 em seguro auto - segurado paga esse valor, seguradora paga o restante
- **Tipos**: Simples (fixa), dedutível, proporcional

### I

**Indenização**
- **Definição**: Valor pago pela seguradora ao segurado em caso de sinistro coberto
- **Limite**: Não pode exceder o capital segurado
- **Cálculo**: Considera valor do dano, franquia, percentual de cobertura

**IOF (Imposto sobre Operações Financeiras)**
- **Definição**: Imposto federal incidente sobre prêmios de seguro
- **Alíquota**: 7,38% para seguros de dano (exceto saúde)
- **Base de cálculo**: Prêmio líquido (antes do IOF)
- **Fórmula**: `Prêmio total = Prêmio líquido + IOF + adicional fracionamento`

### M

**Movimento**
- **Definição**: Tipo de operação que altera dados da apólice
- **Códigos** (sistema):
  - `1`: Emissão (nova apólice)
  - `2`: Endosso
  - `3`: Cancelamento
  - `4`: Renovação
- **Campo**: COD_MOVIMENTO na tabela V0PREMIOS

### P

**Prêmio**
- **Definição**: Valor pago pelo segurado à seguradora pela transferência de risco
- **Componentes**:
  - **Prêmio líquido**: Valor puro do risco (cálculos atuariais)
  - **IOF**: Imposto (7,38%)
  - **Adicional de fracionamento**: Quando pago parcelado
  - **Prêmio total**: Soma dos componentes
- **Fórmula simplificada**: `Prêmio total = Prêmio líquido × (1 + taxa_IOF) + adicional_fracionamento`
- **Tabela DB2**: V0PREMIOS

**Prêmio Cedido**
- **Definição**: Parcela do prêmio transferida a resseguradoras
- **Motivo**: Seguradora repassa parte do risco para limitar exposição
- **Arquivo SUSEP**: PREMCED.TXT (LRECL=800)

**Prêmio Emitido**
- **Definição**: Valor total de prêmios gerados em determinado período (regime de competência)
- **Importante**: Contabilizado na data de emissão, não na data de pagamento
- **Relatório**: Base da Circular SUSEP 360 (arquivo PREMIT.TXT)

**Produto**
- **Definição**: Modalidade de seguro comercializada (seguro auto, residencial, vida, etc.)
- **Identificador**: Código do produto + código SUSEP
- **Tabela DB2**: V0PRODUTO

### R

**Resseguro**
- **Definição**: Operação em que seguradora transfere parte do risco para resseguradora
- **Tipos**:
  - **Facultativo**: Negociado caso a caso
  - **Proporcional**: Resseguradora assume % fixo do risco
  - **Não-proporcional**: Resseguradora paga excesso sobre limite (XL - Excess of Loss)
- **Exemplo**: Risco de R$ 50 milhões - seguradora retém R$ 10 milhões, cede R$ 40 milhões ao resseguro
- **Prêmio cedido**: Parte do prêmio transferida à resseguradora

**Renovação**
- **Definição**: Criação de nova apólice ao fim da vigência anterior (continuidade)
- **Diferença de endosso**: Renovação = nova apólice; Endosso = alteração na apólice atual
- **Prêmio**: Emissão normal, não é ajuste

### S

**Sinistro**
- **Definição**: Ocorrência do evento coberto pela apólice (incêndio, roubo, acidente, etc.)
- **Processo**: Aviso → Regulação → Indenização
- **Impacto**: Reduz lucro técnico da seguradora

**SUSEP (Superintendência de Seguros Privados)**
- **Definição**: Autarquia federal que regula e fiscaliza mercado de seguros no Brasil
- **Função**: Proteger consumidor, garantir solidez das seguradoras, normatizar operações
- **Vinculação**: Ministério da Fazenda
- **Circular 360/2008**: Principal norma relacionada a este projeto

### V

**Vigência**
- **Definição**: Período em que a apólice está ativa (cobertura vigente)
- **Datas**: Data de início e data de fim
- **Exemplo**: Vigência de 01/10/2025 a 01/10/2026 (12 meses)
- **Importante**: Sinistros só são cobertos se ocorrerem dentro da vigência

---

## Termos Regulatórios SUSEP

### C

**Circular SUSEP 360/2008**
- **Definição**: Norma que estabelece envio mensal de prêmios emitidos e cedidos
- **Objetivo**: Monitorar mercado, calcular arrecadação, fiscalizar operações
- **Arquivos obrigatórios**:
  - **PREMIT.TXT**: Prêmios emitidos (LRECL=1200)
  - **PREMCED.TXT**: Prêmios cedidos a resseguradoras (LRECL=800)
- **Prazo**: Até o 15º dia útil do mês seguinte
- **Penalidades**: Multa de R$ 50.000 a R$ 200.000 por atraso/divergência

### L

**Layout SUSEP**
- **Definição**: Especificação oficial do formato dos arquivos de envio
- **Características**:
  - Fixed-width (largura fixa) - sem delimitadores
  - EBCDIC (mainframe) ou ASCII (sistemas modernos)
  - Campos com posições fixas (início-fim)
  - Padding obrigatório (zeros à esquerda para números, espaços à direita para textos)
- **Exemplo** (PREMIT.TXT - posições 1-50):
  ```
  Pos 1-3:    Código da empresa (999)
  Pos 4-13:   Número da apólice (9999999999)
  Pos 14-21:  Data de emissão (YYYYMMDD)
  Pos 22-36:  Prêmio total (999999999999999, últimos 2 = decimais)
  ...
  ```

### P

**Penalidades SUSEP**
- **Definição**: Multas aplicadas por descumprimento de normas
- **Valores** (Circular 360):
  - Atraso no envio: R$ 50.000 (primária), R$ 100.000 (reincidente)
  - Dados incorretos: R$ 75.000 a R$ 150.000
  - Omissão de informações: R$ 100.000 a R$ 200.000
- **Processo**: Notificação → Defesa → Julgamento → Pagamento ou recurso

### R

**Regime de Competência**
- **Definição**: Critério contábil onde receita é reconhecida na data de emissão (não no pagamento)
- **SUSEP**: Exige reportar prêmios pelo regime de competência
- **Exemplo**: Apólice emitida em 15/10/2025, paga em 05/11/2025 → reportar em outubro

---

## Acrônimos e Siglas

### A-E

- **API**: Application Programming Interface (Interface de Programação de Aplicações)
- **ASCII**: American Standard Code for Information Interchange
- **CNPJ**: Cadastro Nacional da Pessoa Jurídica
- **COMP-3**: Computational-3 (packed decimal no COBOL)
- **COBOL**: COmmon Business-Oriented Language
- **CORS**: Cross-Origin Resource Sharing
- **CPF**: Cadastro de Pessoas Físicas
- **DB2**: Database 2 (IBM)
- **DCB**: Data Control Block (JCL)
- **DI**: Dependency Injection (Injeção de Dependência)
- **DTO**: Data Transfer Object
- **EBCDIC**: Extended Binary Coded Decimal Interchange Code
- **EF Core**: Entity Framework Core

### F-J

- **FD**: File Description (COBOL)
- **FP**: Function Points (Pontos de Função)
- **FTP**: File Transfer Protocol
- **HTTP**: HyperText Transfer Protocol
- **HTTPS**: HTTP Secure
- **IIB**: IBM Integration Bus
- **IOF**: Imposto sobre Operações Financeiras
- **I/O**: Input/Output (Entrada/Saída)
- **IoC**: Inversion of Control
- **JCL**: Job Control Language (linguagem de controle de jobs IBM)
- **JES**: Job Entry Subsystem (z/OS)
- **JSON**: JavaScript Object Notation

### L-R

- **LINQ**: Language Integrated Query
- **LRECL**: Logical RECord Length (tamanho do registro)
- **LTS**: Long Term Support (suporte de longo prazo)
- **MVC**: Model-View-Controller
- **ORM**: Object-Relational Mapper
- **REST**: Representational State Transfer
- **RECFM**: RECord ForMat (formato do registro no JCL)

### S-Z

- **SLA**: Service Level Agreement (acordo de nível de serviço)
- **SOAP**: Simple Object Access Protocol
- **SQL**: Structured Query Language
- **SQLCA**: SQL Communication Area (DB2)
- **SUSEP**: Superintendência de Seguros Privados
- **TWS**: Tivoli Workload Scheduler (IBM)
- **URI**: Uniform Resource Identifier
- **URL**: Uniform Resource Locator
- **XML**: eXtensible Markup Language
- **z/OS**: Sistema operacional IBM para mainframes

---

## Mapeamento COBOL → .NET

Esta seção mapeia conceitos COBOL para seus equivalentes .NET no contexto deste projeto.

| Conceito COBOL | Equivalente .NET | Notas |
|---|---|---|
| **PROGRAM-ID** | `class Program` | Nome do programa → Nome da classe |
| **WORKING-STORAGE SECTION** | Campos/propriedades privadas | Variáveis internas |
| **LINKAGE SECTION** | Parâmetros de método/construtor | Parâmetros recebidos |
| **PROCEDURE DIVISION** | Métodos públicos/privados | Lógica executável |
| **PERFORM** | Chamada de método | `PerformCalculation()` |
| **GO TO** | `goto` (evitar), `return`, `break` | Desvio de fluxo |
| **IF/ELSE** | `if/else` ou `switch` | Estruturas condicionais |
| **EVALUATE** | `switch` expression (C# 8+) | Múltiplas condições |
| **PERFORM UNTIL** | `while` loop | Loop condicional |
| **PERFORM VARYING** | `for` loop | Loop indexado |
| **MOVE** | Operador `=` | Atribuição |
| **COMPUTE** | Expressões aritméticas | `var result = a + b * c` |
| **STRING** | `string.Concat()` ou interpolação | Concatenação |
| **UNSTRING** | `string.Split()` | Parsing de strings |
| **DISPLAY** | `Console.WriteLine()` ou `ILogger` | Output de mensagens |
| **ACCEPT** | `Console.ReadLine()` ou config | Input de dados |
| **STOP RUN** | `return` ou `Environment.Exit()` | Término do programa |
| **OPEN** | `File.OpenRead()` ou `FileStream` | Abrir arquivo |
| **READ** | `StreamReader.ReadLine()` | Ler registro |
| **WRITE** | `StreamWriter.WriteLine()` | Escrever registro |
| **CLOSE** | `stream.Close()` ou `Dispose()` | Fechar arquivo |
| **CALL 'subprogram'** | Chamada de método/serviço | Invocação de módulo |
| **EXEC SQL** | LINQ to Entities ou EF Core | Queries SQL |
| **CURSOR** | `IAsyncEnumerable<T>` | Streaming de dados |
| **FETCH** | `yield return` em async enumerable | Próximo item |
| **SQLCODE** | `DbException` ou try/catch | Tratamento de erros SQL |
| **COMP-3** | `decimal` (tipo C#) | Precisão exata |
| **PIC 9(n)** | `int`, `long` (dependendo de n) | Numérico inteiro |
| **PIC X(n)** | `string` | Alfanumérico |
| **PIC 9(n)V99** | `decimal` | Numérico com decimais |
| **PIC S9(n)** | `int`, `long` (com sinal) | Numérico com sinal |
| **OCCURS n TIMES** | `T[]` ou `List<T>` | Arrays |
| **REDEFINES** | `StructLayout` ou pattern matching | União (mesma memória) |
| **88 level (condition)** | `const bool` ou property | Valor nomeado |
| **JCL JOB** | Docker Compose ou script bash | Orquestração |
| **TWS scheduling** | Hangfire ou Quartz.NET | Agendamento de jobs |
| **Mainframe batch** | Console Application (.NET) | Processamento em lote |

---

## Tipos de Dados

### Comparação COBOL ↔ C#

| COBOL PIC Clause | Tipo C# | Tamanho | Exemplo Valor | Notas |
|---|---|---|---|---|
| `PIC 9(3)` | `short` | 2 bytes | 123 | 0-999 |
| `PIC 9(5)` | `int` | 4 bytes | 12345 | 0-99999 |
| `PIC 9(9)` | `int` | 4 bytes | 123456789 | 0-999999999 |
| `PIC 9(10)` | `long` | 8 bytes | 1234567890 | Acima de 2 bilhões |
| `PIC S9(5)` | `int` | 4 bytes | -12345 | Com sinal |
| `PIC 9(5) COMP` | `int` | 4 bytes | 12345 | Binário |
| `PIC 9(5)V99` | `decimal` | 16 bytes | 123.45 | 5 dígitos + 2 decimais |
| `PIC 9(13)V99` | `decimal` | 16 bytes | 1234567890123.45 | Prêmio/capital |
| `PIC 9(5)V99 COMP-3` | `decimal` | 16 bytes | 123.45 | Packed decimal |
| `PIC X(10)` | `string` | Variável | "ABCDE     " | Fixed-width 10 |
| `PIC X(100)` | `string` | Variável | "Razão Social..." | Texto longo |
| `PIC 9(8)` (data) | `DateTime` | 8 bytes | 20251027 | YYYYMMDD |
| `PIC X(1)` (flag) | `bool` | 1 byte | 'S' ou 'N' | Booleano |

### Regras de Conversão

1. **Numérico COBOL → C#**:
   - `PIC 9(n)` onde n ≤ 4 → `short`
   - `PIC 9(n)` onde 5 ≤ n ≤ 9 → `int`
   - `PIC 9(n)` onde n ≥ 10 → `long`
   - `PIC 9(n)Vdd` (com decimais) → **SEMPRE** `decimal`
   - `PIC 9(n) COMP-3` → **SEMPRE** `decimal`

2. **Alfanumérico COBOL → C#**:
   - `PIC X(n)` → `string` (usar `[MaxLength(n)]` em entidade)
   - Remover espaços à direita: `.TrimEnd()`
   - Adicionar espaços à direita: `.PadRight(n)`

3. **Data COBOL → C#**:
   - `PIC 9(8)` (YYYYMMDD) → `DateTime`
   - Conversão: `DateTime.ParseExact(dateString, "yyyyMMdd", CultureInfo.InvariantCulture)`
   - **Atenção**: COBOL armazena como inteiro (20251027), não string

4. **Flags COBOL → C#**:
   - `PIC X(1)` com valores 'S'/'N' → `bool`
   - Conversão: `flag == 'S'` ou `flag == '1'`

---

## Conceitos de Arquitetura

### Padrões de Design Usados Neste Projeto

**1. Clean Architecture**
- **Objetivo**: Separar responsabilidades, independência de frameworks
- **Camadas**:
  - **Core**: Regras de negócio puras (sem dependências externas)
  - **Infrastructure**: Acesso a dados, APIs, file I/O
  - **API**: Apresentação, controllers HTTP
- **Regra de ouro**: Dependências apontam para dentro (API → Core ← Infrastructure)

**2. Repository Pattern**
- **Objetivo**: Abstrair acesso a dados
- **Implementação**: Interface no Core, implementação na Infrastructure
- **Benefício**: Trocar banco de dados sem alterar lógica de negócio

**3. Dependency Injection**
- **Objetivo**: Desacoplar dependências
- **Implementação**: Constructor injection + IoC container (.NET)
- **Benefício**: Testabilidade (mock de repositórios/serviços)

**4. DTO Pattern**
- **Objetivo**: Separar modelos de domínio de representação de API
- **Implementação**: Classes DTO + AutoMapper
- **Benefício**: Evolução independente de API e domínio

**5. Unit of Work**
- **Objetivo**: Agrupar múltiplas operações em uma transação
- **Implementação**: `DbContext.SaveChangesAsync()` (EF Core)
- **Benefício**: Atomicidade (tudo ou nada)

**6. CQRS (Command Query Responsibility Segregation) - Simplificado**
- **Objetivo**: Separar operações de leitura (queries) de escrita (commands)
- **Implementação**: Métodos de serviço distintos (`GetAsync` vs `AddAsync`)
- **Benefício**: Otimização independente (queries read-only com AsNoTracking)

**7. Async/Await Pattern**
- **Objetivo**: I/O não bloqueante
- **Implementação**: Todos os métodos de repositório/serviço são async
- **Benefício**: Escalabilidade (threads liberadas durante I/O)

### Princípios SOLID

**S - Single Responsibility Principle**
- Cada classe tem uma única responsabilidade
- Exemplo: `PremiumController` só trata HTTP, `PremiumService` só tem lógica de negócio

**O - Open/Closed Principle**
- Aberto para extensão, fechado para modificação
- Exemplo: Novos repositórios implementam `IRepository<T>`, sem alterar código existente

**L - Liskov Substitution Principle**
- Subtipos devem ser substituíveis por seus tipos base
- Exemplo: Qualquer `IPremiumRepository` pode ser injetado sem quebrar código

**I - Interface Segregation Principle**
- Interfaces específicas > interfaces gordas
- Exemplo: `IPremiumRepository`, `IPolicyRepository` (não um único `IRepository` com 50 métodos)

**D - Dependency Inversion Principle**
- Depender de abstrações (interfaces), não de implementações concretas
- Exemplo: Controller depende de `IPremiumService`, não de `PremiumService` diretamente

---

## Glossário de Campos do Sistema

### Campos Comuns em Múltiplas Tabelas

| Campo | Tipo COBOL | Tipo C# | Descrição |
|---|---|---|---|
| `COD_EMPRESA` | `PIC 9(3)` | `short` | Código da seguradora (Caixa = 185) |
| `NUM_APOLICE` | `PIC 9(10)` | `long` | Número único da apólice |
| `NUM_ENDOSSO` | `PIC 9(5)` | `int` | Número sequencial do endosso (0 = apólice original) |
| `COD_PRODUTO` | `PIC 9(5)` | `int` | Código do produto de seguro |
| `COD_SUSEP` | `PIC X(10)` | `string` | Código do produto registrado na SUSEP |
| `DTA_EMISSAO` | `PIC 9(8)` | `DateTime` | Data de emissão (YYYYMMDD) |
| `DTA_INICIO_VIGENCIA` | `PIC 9(8)` | `DateTime` | Início da vigência |
| `DTA_FIM_VIGENCIA` | `PIC 9(8)` | `DateTime` | Fim da vigência |
| `VLR_PREMIO_LIQUIDO` | `PIC 9(13)V99 COMP-3` | `decimal` | Prêmio líquido (sem IOF) |
| `VLR_IOF` | `PIC 9(11)V99 COMP-3` | `decimal` | Imposto IOF |
| `VLR_PREMIO_TOTAL` | `PIC 9(13)V99 COMP-3` | `decimal` | Prêmio total (líquido + IOF + adic.) |
| `COD_MOVIMENTO` | `PIC 9(1)` | `byte` | Tipo de movimento (1=emissão, 2=endosso, 3=cancel.) |
| `CPF_CNPJ_SEGURADO` | `PIC X(14)` | `string` | CPF (11 dígitos) ou CNPJ (14 dígitos) do segurado |
| `NOM_SEGURADO` | `PIC X(100)` | `string` | Nome/razão social do segurado |

### Campos Específicos do Relatório SUSEP (PREMIT.TXT)

- **Posições 1-1200**: Layout completo definido pela Circular 360
- **687 campos** no total (conforme análise COBOL)
- Ver documentação completa em `03-data-structures.md`

---

## Referências Cruzadas

### Para Mais Informações

- **Estruturas de dados detalhadas**: Ver `03-data-structures.md`
- **Modelo de banco de dados**: Ver `04-database-model.md`
- **Regras de negócio**: Ver `05-business-logic.md`
- **Módulos externos**: Ver `06-external-modules.md`
- **Operações mainframe**: Ver `07-operations-guide.md`
- **Histórico de manutenção**: Ver `08-maintenance-history.md`
- **Guia de migração**: Ver `09-migration-guide.md`
- **Plano do projeto**: Ver `11-migration-project-plan.md`

### Documentação Externa

- **COBOL Language Reference**: IBM Enterprise COBOL for z/OS
- **DB2 SQL Reference**: IBM DB2 for z/OS
- **.NET Documentation**: https://learn.microsoft.com/dotnet
- **Entity Framework Core**: https://learn.microsoft.com/ef/core
- **ASP.NET Core**: https://learn.microsoft.com/aspnet/core
- **SUSEP Normativas**: https://www.gov.br/susep

---

**Fim do Glossário**

**Total de Termos**: 150+ termos técnicos e de negócio
**Última Atualização**: 27 de outubro de 2025
**Versão**: 1.0.0

---

## Controle de Alterações

| Versão | Data | Autor | Descrição |
|---|---|---|---|
| 1.0.0 | 27/10/2025 | Claude Code | Versão inicial completa do glossário |