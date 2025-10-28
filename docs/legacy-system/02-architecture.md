# 02 - Arquitetura do Sistema COBOL RG1866B

[← Voltar ao Índice](README.md) | [← Anterior](01-executive-summary.md) | [Próximo →](03-data-structures.md)

---

## Visão Geral da Arquitetura

O programa RG1866B segue o padrão clássico de batch processing mainframe com estrutura COBOL modular.

### Stack Tecnológico

```
┌─────────────────────────────────────────────────────────────┐
│                    CAMADA DE APRESENTAÇÃO                    │
│  (Não existe - Sistema Batch sem interface)                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                    CAMADA DE CONTROLE                        │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  JCL (Job Control Language)                         │   │
│  │  • Define parâmetros (PARM='202510')                │   │
│  │  • Aloca arquivos (PREMIT, PREMCED)                 │   │
│  │  • Configura ambiente DB2                           │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                   CAMADA DE APLICAÇÃO                        │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  RG1866B.cbl (COBOL ANSI 85)                        │   │
│  │  • IDENTIFICATION DIVISION                          │   │
│  │  • ENVIRONMENT DIVISION                             │   │
│  │  • DATA DIVISION                                    │   │
│  │    ├─ FILE SECTION (PREMIT, PREMCED)               │   │
│  │    └─ WORKING-STORAGE SECTION (687 vars)           │   │
│  │  • PROCEDURE DIVISION                               │   │
│  │    ├─ 63 seções de processamento                   │   │
│  │    └─ 65 parágrafos                                │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                CAMADA DE INTEGRAÇÃO                          │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Módulos Externos (Binários Compilados)            │   │
│  │  • RE0001S - Cálculos de resseguro                 │   │
│  │  • GE0009S - Formatações especiais                 │   │
│  │  • GE0010S - Validações auxiliares                 │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                    CAMADA DE DADOS                           │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  IBM DB2 for z/OS                                   │   │
│  │  • 26+ tabelas/views                                │   │
│  │  • 4 cursores ativos                                │   │
│  │  • SQL embarcado (EXEC SQL ... END-EXEC)           │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                  CAMADA DE PERSISTÊNCIA                      │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Arquivos Sequenciais (DASD)                       │   │
│  │  • PREMIT.TXT (fixed-width, 1200 bytes/rec)        │   │
│  │  • PREMCED.TXT (fixed-width, 800 bytes/rec)        │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

[Restante do conteúdo com seções detalhadas sobre:
- Fluxo de Execução Completo
- Mapeamento de Seções COBOL
- Diagrama de Sequência
- Padrões Arquiteturais
- etc.]

---

**Documento**: 02-architecture.md
**Versão**: 1.0
**Próximo**: [03-data-structures.md](03-data-structures.md) →
