# Documentação do Sistema Legado COBOL RG1866B

## Índice de Documentos

Esta documentação está organizada em módulos especializados para facilitar navegação e manutenção.

### 📋 Documentos Principais

1. **[01-executive-summary.md](01-executive-summary.md)**
   - Visão executiva do sistema
   - Identificação e propósito
   - Métricas chave
   - Contexto regulatório SUSEP

2. **[02-architecture.md](02-architecture.md)**
   - Arquitetura técnica
   - Fluxo de execução
   - Componentes do sistema
   - Diagramas de arquitetura

3. **[03-data-structures.md](03-data-structures.md)**
   - Working Storage Section (687 variáveis)
   - File Section (PREMIT, PREMCED)
   - Estruturas de dados principais
   - Layouts de arquivo

4. **[04-database-model.md](04-database-model.md)**
   - 26+ tabelas/views DB2
   - Modelo de relacionamentos
   - 4 cursores ativos
   - Queries SQL principais

5. **[05-business-logic.md](05-business-logic.md)**
   - Regras de negócio
   - Tipos de movimento (101-106)
   - Cálculos financeiros
   - Validações críticas
   - Cosseguro e resseguro

6. **[06-external-modules.md](06-external-modules.md)**
   - RE0001S (Resseguro)
   - GE0009S (Formatações)
   - GE0010S (Validações)
   - Interfaces CALL

7. **[07-operations-guide.md](07-operations-guide.md)**
   - JCL de execução
   - Parâmetros de entrada
   - Métricas de performance
   - Códigos de retorno
   - Agendamento mensal

8. **[08-maintenance-history.md](08-maintenance-history.md)**
   - Histórico de alterações (2014-2022)
   - 35+ manutenções
   - Desenvolvedores principais
   - Projetos CADMUS

9. **[09-migration-guide.md](09-migration-guide.md)**
   - Complexidades técnicas
   - Riscos regulatórios
   - Recomendações
   - Checklist de migração

10. **[10-glossary.md](10-glossary.md)**
    - Termos técnicos
    - Jargões SUSEP
    - Siglas e abreviações

---

## 🎯 Quick Start

**Novo no projeto?** Comece por:
1. [01-executive-summary.md](01-executive-summary.md) - Entenda o que o sistema faz
2. [02-architecture.md](02-architecture.md) - Veja como funciona
3. [05-business-logic.md](05-business-logic.md) - Aprenda as regras de negócio

**Migrando o sistema?** Foque em:
1. [09-migration-guide.md](09-migration-guide.md) - Pontos de atenção
2. [03-data-structures.md](03-data-structures.md) - Mapeamento de dados
3. [04-database-model.md](04-database-model.md) - Estrutura DB2

**Operando o sistema?** Consulte:
1. [07-operations-guide.md](07-operations-guide.md) - Guia operacional
2. [08-maintenance-history.md](08-maintenance-history.md) - Histórico

---

## 📊 Visão Geral em Números

| Métrica | Valor |
|---------|-------|
| **Linhas de Código** | 5.046 |
| **Variáveis (WORKING-STORAGE)** | 687 |
| **Tabelas DB2 Acessadas** | 26+ |
| **Cursores Ativos** | 4 |
| **Seções de Procedimento** | 63 |
| **Parágrafos** | 65 |
| **Módulos Externos (CALL)** | 3 |
| **Anos em Produção** | 8+ (desde 2014) |
| **Manutenções Acumuladas** | 35+ |
| **Tempo Execução Médio** | 45-60 min |
| **Registros Processados/Mês** | 10.000-12.000 |

---

## 🚨 Informações Críticas

### Compliance Regulatório
- **SUSEP Circular 360/2017**: Formato de arquivo obrigatório
- **Penalidades**: Até R$ 1.000.000 por não-conformidade
- **Prazo**: 15º dia útil do mês subsequente
- **Validação**: Byte-for-byte match obrigatório na migração

### Criticidades Técnicas
- ⚠️ **ALTA**: Aritmética financeira (COMP-3) e fixed-width format
- ⚠️ **MÉDIA**: Cursores DB2 aninhados e módulos externos
- ✅ **BAIXA**: Validações específicas por ramo

---

## 📞 Contatos

| Papel | Responsabilidade |
|-------|------------------|
| **Product Owner** | Aprovação mudanças regulatórias |
| **Analista SUSEP** | Validação Circular 360 |
| **DBA DB2** | Performance e otimização |
| **Especialista Cosseguro** | Validação cálculos |
| **Operações Mainframe** | Execução e monitoramento |

---

## 📚 Documentos Relacionados Externos

1. **Circular SUSEP 360/2017** - Norma oficial
2. **Layout PREMIT v2.3** - Especificação SUSEP
3. **Manual de Produtos** - Catálogo de ramos
4. **Acordo de Cosseguro** - Contratos
5. **SLA Mainframe** - Acordo de serviço

---

**Última Atualização**: Outubro 2025
**Versão da Documentação**: 1.0
**Status**: ✅ Completo
