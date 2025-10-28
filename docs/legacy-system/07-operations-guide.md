# 07 - Operations Guide

[← Voltar ao Índice](README.md)

## Índice

- [Visão Geral](#visão-geral)
- [Agendamento e Execução](#agendamento-e-execução)
- [JCL (Job Control Language)](#jcl-job-control-language)
- [Procedimentos de Operação](#procedimentos-de-operação)
- [Monitoramento e SLA](#monitoramento-e-sla)
- [Tratamento de Erros](#tratamento-de-erros)
- [Contingência e Recuperação](#contingência-e-recuperação)
- [Migração para .NET](#migração-para-net)

---

## Visão Geral

O programa RG1866B é executado **mensalmente** no mainframe IBM z/OS através do sistema de agendamento TWS (Tivoli Workload Scheduler). A execução ocorre sempre no **1º dia útil do mês** às **03:00 AM**, processando os dados do mês anterior.

### Características Operacionais

| Característica | Valor |
|----------------|-------|
| **Frequência** | Mensal (1º dia útil) |
| **Horário** | 03:00 AM |
| **Duração Típica** | 45-60 minutos |
| **Volume de Dados** | ~10.000 registros |
| **Arquivos de Saída** | 2 (PREMIT.TXT, PREMCED.TXT) |
| **Prioridade** | ALTA (regulatório) |
| **Job Class** | A (produção crítica) |
| **Retenção de Logs** | 90 dias |

### Fluxo Operacional

```text
TWS Scheduler
    ↓
RG1866B.JCL (Job iniciado)
    ↓
Step 1: CLEANUP (limpar arquivos anteriores)
    ↓
Step 2: RG1866B (executar programa COBOL)
    ↓
Step 3: VALIDATE (validar arquivos gerados)
    ↓
Step 4: FTP (transferir para SUSEP)
    ↓
Step 5: BACKUP (arquivar em tape)
    ↓
TWS (notificação de sucesso/falha)
```

---

## Agendamento e Execução

### TWS (Tivoli Workload Scheduler)

**Job Name**: `RG1866B_MENSAL`

**Definição TWS**:

```text
JOBD RG1866B_MENSAL
  DESCRIPTION 'RELATORIO MENSAL PREMIOS SUSEP CIRC 360'
  SCHEDULE MONTHLY FIRSTWORKDAY AT 0300
  PRIORITY HIGH
  FOLLOWS JOB RG1865B_MENSAL
  DEADLINE 0600
  RECOVERY AUTO
  NOTIFY ON(ERROR) TO(OPS_SUSEP@CAIXASEGURADORA.COM.BR)
END
```

**Dependências**:
- **Predecessor**: RG1865B_MENSAL (processamento de coberturas)
- **Sucessor**: RG1867B_MENSAL (relatório de sinistros)

### Calendário de Execução

| Mês | Data Prevista | Deadline | Observações |
|-----|---------------|----------|-------------|
| Janeiro | 02/01 (1º útil) | 06/01 | Feriado 01/01 |
| Fevereiro | 01/02 | 05/02 | - |
| Março | 01/03 | 05/03 | - |
| Abril | 01/04 | 05/04 | - |
| Maio | 02/05 (1º útil) | 06/05 | Feriado 01/05 |
| Junho | 01/06 | 05/06 | - |
| Julho | 01/07 | 05/07 | - |
| Agosto | 01/08 | 05/08 | - |
| Setembro | 02/09 (1º útil) | 06/09 | Feriado 07/09 |
| Outubro | 01/10 | 05/10 | - |
| Novembro | 03/11 (1º útil) | 07/11 | Feriados 02/11, 15/11 |
| Dezembro | 01/12 | 05/12 | - |

### Execução Manual (Contingência)

**Comando MVS**:

```jcl
//EXECJOB  JOB (ACCT),'RG1866B MANUAL',
//         CLASS=A,MSGCLASS=X,MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID
//STEP1    EXEC PGM=RG1866B,
//         PARM='202510'          ← Data processamento YYYYMM
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
//PREMIT   DD DSN=PROD.PREMIT.TXT,DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(10,5),RLSE)
//PREMCED  DD DSN=PROD.PREMCED.TXT,DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,2),RLSE)
//SYSIN    DD *
202510  ← Data processamento
1       ← Código da companhia
/*
```

---

## JCL (Job Control Language)

### RG1866B.JCL (Completo)

```jcl
//RG1866BM JOB (PROD1866),'PREMIOS SUSEP 360',
//         CLASS=A,
//         MSGCLASS=X,
//         MSGLEVEL=(1,1),
//         NOTIFY=&SYSUID,
//         REGION=128M,
//         TIME=(0,30)
//*
//*********************************************************************
//* JOB NAME   : RG1866BM                                            *
//* DESCRIPTION: RELATORIO MENSAL PREMIOS EMITIDOS SUSEP CIRC 360   *
//* FREQUENCY  : MENSAL (1º DIA UTIL)                               *
//* AUTHOR     : OPERACOES TI CAIXA SEGURADORA                      *
//* CREATED    : 2014-03-15                                          *
//* UPDATED    : 2022-09-30                                          *
//*********************************************************************
//*
//*====================================================================
//* STEP 1: CLEANUP - LIMPAR ARQUIVOS ANTERIORES
//*====================================================================
//CLEANUP  EXEC PGM=IEFBR14
//DELETE1  DD DSN=PROD.PREMIT.TXT,DISP=(MOD,DELETE,DELETE)
//DELETE2  DD DSN=PROD.PREMCED.TXT,DISP=(MOD,DELETE,DELETE)
//DELETE3  DD DSN=PROD.RG1866B.LOG,DISP=(MOD,DELETE,DELETE)
//*
//*====================================================================
//* STEP 2: RG1866B - EXECUTAR PROGRAMA PRINCIPAL
//*====================================================================
//RG1866B  EXEC PGM=RG1866B,
//         COND=(0,NE,CLEANUP),
//         PARM='&YYYYMM,1'
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//         DD DSN=SYS1.COBLIB,DISP=SHR
//*
//* ARQUIVOS DE SAIDA
//PREMIT   DD DSN=PROD.PREMIT.TXT,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(10,5),RLSE),
//            DCB=(RECFM=FB,LRECL=1200,BLKSIZE=12000)
//PREMCED  DD DSN=PROD.PREMCED.TXT,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(CYL,(5,2),RLSE),
//            DCB=(RECFM=FB,LRECL=800,BLKSIZE=8000)
//*
//* LOG DE EXECUCAO
//SYSOUT   DD DSN=PROD.RG1866B.LOG,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(1,1),RLSE),
//            DCB=(RECFM=VBA,LRECL=125,BLKSIZE=1250)
//*
//* ENTRADA PARAMETROS
//SYSIN    DD *
&YYYYMM   ← Substituido por TWS (ex: 202510)
1         ← Codigo companhia
/*
//*
//* ACESSO DATABASE DB2
//DSNPLAN  DD DSN=DB2PROD.PLAN.RG1866B,DISP=SHR
//*
//*====================================================================
//* STEP 3: VALIDATE - VALIDAR ARQUIVOS GERADOS
//*====================================================================
//VALIDATE EXEC PGM=RG1866BV,
//         COND=(0,NE,RG1866B)
//STEPLIB  DD DSN=PROD.LOADLIB,DISP=SHR
//INPUT1   DD DSN=PROD.PREMIT.TXT,DISP=SHR
//INPUT2   DD DSN=PROD.PREMCED.TXT,DISP=SHR
//REPORT   DD SYSOUT=*
//*
//*====================================================================
//* STEP 4: FTP - TRANSFERIR PARA SUSEP
//*====================================================================
//FTPSUSEP EXEC PGM=FTP,
//         COND=(0,NE,VALIDATE)
//INPUT    DD *
OPEN SUSEP.GOV.BR
USER CAIXASEG PASSWORD
CD /CIRC360/UPLOAD
LCD PROD
PUT PREMIT.TXT PREMIT_&YYYYMM..TXT
PUT PREMCED.TXT PREMCED_&YYYYMM..TXT
QUIT
/*
//OUTPUT   DD SYSOUT=*
//*
//*====================================================================
//* STEP 5: BACKUP - ARQUIVAR EM TAPE
//*====================================================================
//BACKUP   EXEC PGM=IEBGENER,
//         COND=(0,NE,FTPSUSEP)
//SYSUT1   DD DSN=PROD.PREMIT.TXT,DISP=SHR
//SYSUT2   DD DSN=TAPE.BACKUP.PREMIT.&YYYYMM,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=TAPE,
//            LABEL=(,SL),
//            DCB=(RECFM=FB,LRECL=1200,BLKSIZE=12000)
//SYSPRINT DD SYSOUT=*
//*
//
```

### Explicação dos Parâmetros JCL

**Job Card**:
- `CLASS=A`: Classe de alta prioridade
- `REGION=128M`: Memória alocada (128 MB)
- `TIME=(0,30)`: Timeout de 30 minutos

**DCB (Data Control Block)**:
- `RECFM=FB`: Fixed Block (registros de tamanho fixo)
- `LRECL=1200`: Logical Record Length (PREMIT)
- `BLKSIZE=12000`: Block size (10 registros por bloco)

**SPACE**:
- `CYL,(10,5)`: Alocação primária de 10 cilindros, secundária de 5
- `RLSE`: Liberar espaço não utilizado

---

## Procedimentos de Operação

### Procedimento 1: Execução Normal

**Responsável**: Operador de Turno (NOC)

**Passo a Passo**:

1. **Verificar Pré-requisitos** (03:00 AM)
   ```text
   - Job RG1865B_MENSAL completou com sucesso (RC=0000)
   - Database DB2 disponível (status: ACTIVE)
   - Espaço em disco suficiente (mínimo 50 MB livres)
   ```

2. **Acompanhar Execução** (03:00 - 04:00 AM)
   ```text
   - Verificar console TWS: status = RUNNING
   - Monitorar SYSLOG: sem mensagens de erro
   - Verificar CPU usage: < 80%
   ```

3. **Validar Conclusão** (04:00 AM)
   ```text
   - Job status: COMPLETED
   - Return code: RC=0000
   - Arquivos gerados:
     * PROD.PREMIT.TXT (existe, tamanho > 0)
     * PROD.PREMCED.TXT (existe, tamanho > 0)
   ```

4. **Verificar Logs** (04:00 AM)
   ```text
   - Abrir PROD.RG1866B.LOG
   - Procurar por: "PROCESSAMENTO CONCLUIDO COM SUCESSO"
   - Validar totalizadores:
     * Total registros PREMIT: ~10.000
     * Total registros PREMCED: ~500
     * Total prêmios: R$ XX.XXX.XXX,XX
   ```

5. **Confirmar FTP** (04:00 AM)
   ```text
   - Verificar step FTPSUSEP: RC=0000
   - Validar no servidor SUSEP (via navegador):
     https://susep.gov.br/upload/status
   - Status esperado: "Recebido com sucesso"
   ```

### Procedimento 2: Reprocessamento

**Quando Usar**: Após correção de dados ou erros detectados

**Comando**:

```text
//RERUN JOB ...
//STEP1 EXEC PGM=RG1866B,PARM='202510,1,RERUN'
                                    └────┘
                                    Modo reprocessamento
```

**Atenção**:
- ⚠️ Reprocessamento sobrescreve arquivos no SUSEP
- ⚠️ Requer aprovação do supervisor
- ⚠️ Enviar email para SUSEP informando reprocessamento

### Procedimento 3: Consulta de Status

**Via TSO/ISPF**:

```text
TSO SDSF
-> DA (Display Active jobs)
-> Filter: RG1866*
-> S (Select job) para ver steps
```

**Via TWS Web Console**:

```text
https://tws.caixaseguradora.com.br
-> Jobs > Active Jobs
-> Filtro: RG1866B_MENSAL
-> Status detalhado
```

---

## Monitoramento e SLA

### Métricas de SLA

| Métrica | Target | Limite Aceitável |
|---------|--------|------------------|
| **Duração** | 45 min | 60 min |
| **Taxa de Sucesso** | 100% | 95% (mensal) |
| **Disponibilidade** | 99.5% | 98% |
| **Tempo de Resposta a Incidentes** | 15 min | 30 min |
| **Reprocessamentos/Mês** | 0 | 1 |

### Dashboards de Monitoramento

**BMC Control-M Dashboard**:

```text
┌─────────────────────────────────────────────┐
│ RG1866B - SUSEP Circular 360               │
├─────────────────────────────────────────────┤
│ Status: RUNNING ●                           │
│ Início: 03:00:15                            │
│ Duração: 00:42:33                           │
│ CPU: 45%                                    │
│ I/O: 1.2 GB/s                               │
├─────────────────────────────────────────────┤
│ Steps Completados:                          │
│ ✓ CLEANUP                                   │
│ ✓ RG1866B                                   │
│ ▶ VALIDATE (running)                        │
│   FTPSUSEP (waiting)                        │
│   BACKUP (waiting)                          │
└─────────────────────────────────────────────┘
```

### Alertas Configurados

**Splunk Alert Rules**:

```text
1. Job Failed:
   - Trigger: RC != 0000
   - Severity: CRITICAL
   - Notify: OPS_SUSEP + Manager
   - Action: Auto-create incident

2. Job Timeout:
   - Trigger: Duration > 60 min
   - Severity: WARNING
   - Notify: OPS_SUSEP
   - Action: Send SMS

3. File Size Anomaly:
   - Trigger: File size < 1 MB ou > 100 MB
   - Severity: WARNING
   - Notify: OPS_SUSEP
   - Action: Email notification

4. FTP Failure:
   - Trigger: FTPSUSEP RC != 0
   - Severity: HIGH
   - Notify: OPS_SUSEP + Network Team
   - Action: Retry 3x com intervalo 5 min
```

---

## Tratamento de Erros

### Códigos de Retorno (Return Codes)

| RC | Descrição | Ação Operacional |
|----|-----------|------------------|
| **0000** | Sucesso completo | Nenhuma ação necessária |
| **0004** | Warning (dados processados, avisos menores) | Verificar log, processar normalmente |
| **0008** | Erro de validação | Analisar log, contactar suporte aplicação |
| **0012** | Erro de database (DB2) | Verificar disponibilidade DB2, reprocessar |
| **0016** | Erro de I/O (arquivos) | Verificar espaço em disco, reprocessar |
| **0020** | Erro de lógica de negócio | Contactar suporte aplicação urgente |
| **0322** | Abend U0322 (timeout SQL) | Verificar performance DB2 |
| **0806** | Abend S0C7 (data exception) | Dados corrompidos, investigar origem |
| **0C4** | Abend S0C4 (protection exception) | Erro crítico, contactar desenvolvimento |

### Mensagens de Erro Comuns

**1. SQL Error -911 (Deadlock)**

```text
DSNT408I SQLCODE = -911, ERROR:  DEADLOCK OR TIMEOUT
```

**Ação**:
1. Verificar se outras aplicações estão acessando V0PREMIOS
2. Aguardar 5 minutos
3. Reprocessar job
4. Se persistir, contactar DBA

**2. File Open Error**

```text
IGD17101I PREMIT DD STATEMENT MISSING
```

**Ação**:
1. Verificar JCL: DD PREMIT presente
2. Verificar permissões: RACF authorized
3. Verificar espaço: SPACE allocation sufficient

**3. Validation Error**

```text
RG1866B-E001: TOTAL REGISTROS DIFERENTE DO ESPERADO
ESPERADO: 10500
ENCONTRADO: 9876
```

**Ação**:
1. Analisar dados de entrada (V0PREMIOS)
2. Verificar filtros (data processamento)
3. Contactar área de negócio para confirmar volume

---

## Contingência e Recuperação

### Plano de Contingência

**Cenário 1: Job Falha no Deadline**

```text
Situação: Job não completou até 06:00 AM (deadline)
Impacto: Alto (regulatório - penalidades SUSEP)

Procedimento:
1. [06:00] Escalar para Gerente de Operações
2. [06:15] Avaliar causa raiz (logs, monitoring)
3. [06:30] Decisão:
   a) Se problema técnico resolvível: reprocessar
   b) Se dados corrompidos: contactar TI Desenvolvimento
4. [08:00] Notificar SUSEP sobre atraso (email oficial)
5. [12:00] Deadline crítico - enviar dados parciais se necessário
```

**Cenário 2: FTP para SUSEP Falha**

```text
Situação: FTPSUSEP step falhou (RC=0016)

Procedimento:
1. Verificar conectividade (ping susep.gov.br)
2. Tentar FTP manual:
   ftp susep.gov.br
   user: CAIXASEG
   put PREMIT.TXT
3. Se FTP manual falha:
   - Contactar Network Team
   - Usar portal web SUSEP como alternativa:
     https://susep.gov.br/upload
4. Confirmar recebimento via email SUSEP
```

**Cenário 3: Dados Corrompidos Detectados**

```text
Situação: VALIDATE step detectou inconsistências

Procedimento:
1. NÃO prosseguir com FTP
2. Analisar relatório de validação (REPORT DD)
3. Identificar registros com problema
4. Opções:
   a) Se < 10 registros: excluir e reprocessar
   b) Se > 10 registros: investigar origem (DB2)
5. Contactar DBA e Desenvolvimento
6. Após correção: rerun completo
```

### Backup e Restore

**Localização Backups**:

```text
TAPE: /PROD/BACKUP/TAPE001
  - PREMIT_202501.TXT
  - PREMCED_202501.TXT
  - RG1866B_202501.LOG

HSM (Hierarchical Storage):
  - Retenção automática: 12 meses
  - Após 12 meses: migrado para tape offsite
```

**Restore de Backup**:

```jcl
//RESTORE  EXEC PGM=IEBGENER
//SYSUT1   DD DSN=TAPE.BACKUP.PREMIT.202510,
//            DISP=OLD,
//            UNIT=TAPE,
//            LABEL=(,SL)
//SYSUT2   DD DSN=PROD.PREMIT.RESTORE,
//            DISP=(NEW,CATLG,DELETE)
//SYSPRINT DD SYSOUT=*
```

---

## Migração para .NET

### Arquitetura de Deployment

**Ambiente On-Premises** (Opção 1):

```yaml
# docker-compose.yml
version: '3.8'
services:
  api:
    image: caixa-seguradora/rg1866b-api:latest
    environment:
      - ASPNETCORE_ENVIRONMENT=Production
      - ConnectionStrings__Default=Server=sql-server;Database=PremiumReporting
    ports:
      - "5000:80"
    volumes:
      - /data/reports:/app/output
    restart: unless-stopped

  sql-server:
    image: mcr.microsoft.com/mssql/server:2022-latest
    environment:
      - ACCEPT_EULA=Y
      - SA_PASSWORD=YourStrong!Passw0rd
    volumes:
      - sqldata:/var/opt/mssql
    restart: unless-stopped

  scheduler:
    image: caixa-seguradora/rg1866b-scheduler:latest
    environment:
      - ApiBaseUrl=http://api
      - Schedule__Cron=0 3 1 * *  # 03:00 AM, 1st day of month
    depends_on:
      - api
    restart: unless-stopped
```

**Agendamento com Hangfire**:

```csharp
public class ReportScheduler
{
    public void ConfigureSchedules()
    {
        // Execução mensal - 1º dia útil às 03:00 AM
        RecurringJob.AddOrUpdate<PremiumReportService>(
            "rg1866b-monthly",
            service => service.GenerateMonthlyReportAsync(),
            Cron.Monthly(1, 3), // Dia 1, hora 3
            new RecurringJobOptions
            {
                TimeZone = TimeZoneInfo.FindSystemTimeZoneById("E. South America Standard Time")
            });
    }
}
```

### Monitoramento .NET

**Application Insights**:

```csharp
public class ReportTelemetry
{
    private readonly TelemetryClient _telemetry;

    public async Task TrackReportGenerationAsync(Func<Task> reportGeneration)
    {
        var operation = _telemetry.StartOperation<RequestTelemetry>("GenerateReport");
        var sw = Stopwatch.StartNew();

        try
        {
            await reportGeneration();

            _telemetry.TrackMetric("ReportDuration", sw.ElapsedMilliseconds);
            _telemetry.TrackMetric("ReportSuccess", 1);

            operation.Telemetry.Success = true;
        }
        catch (Exception ex)
        {
            _telemetry.TrackException(ex);
            _telemetry.TrackMetric("ReportFailure", 1);

            operation.Telemetry.Success = false;
            throw;
        }
        finally
        {
            _telemetry.StopOperation(operation);
        }
    }
}
```

### Comparação Operacional

| Aspecto | Mainframe (COBOL) | .NET (Migrado) |
|---------|-------------------|----------------|
| **Agendamento** | TWS (Tivoli) | Hangfire / Cron Jobs |
| **Logs** | SYSLOG / SDSF | Serilog / Application Insights |
| **Monitoramento** | BMC Control-M | Azure Monitor / Prometheus |
| **Alertas** | Splunk | Application Insights Alerts |
| **Deployment** | JCL Submit | Docker / Kubernetes |
| **Backup** | Tape Library | Azure Blob Storage / S3 |
| **Retenção** | 90 dias (tape) | Configurável (cloud storage) |

---

## Referências

- **Lógica de Negócio**: `docs/legacy-system/05-business-logic.md`
- **Módulos Externos**: `docs/legacy-system/06-external-modules.md`
- **IBM z/OS JCL Reference**: SC33-7988
- **TWS User Guide**: SC23-9843

---

**Documento criado em**: 2025-10-27
**Última atualização**: 2025-10-27
**Versão**: 1.0
