# Análise de Funcionalidades e Pontos de Função
## Sistema RG1866B - Migração COBOL para .NET 9

**Documento**: Reorganização de Funcionalidades por Fase
**Data**: 27 de outubro de 2025
**Versão**: 2.0
**Total de Pontos de Função**: 770 PF

---

## Sumário Executivo

Este documento reorganiza as funcionalidades do projeto em **duas fases distintas**:

1. **FASE 1 - MIGRAÇÃO CORE** (490 PF - 63.6%): Migração fiel do COBOL existente, mantendo todas as funcionalidades atuais sem melhorias significativas
2. **FASE 2 - MELHORIAS E FRONTEND** (280 PF - 36.4%): Novas funcionalidades, interface moderna, dashboards e recursos adicionais

### Totais por Fase

| Fase | Descrição | Pontos de Função | % do Total | Duração Estimada |
|------|-----------|------------------|------------|------------------|
| **FASE 1** | Migração Core (COBOL → .NET) | 490 PF | 63.6% | 7-8 semanas |
| **FASE 2** | Melhorias + Frontend Moderno | 280 PF | 36.4% | 4-5 semanas |
| **TOTAL** | Projeto Completo | **770 PF** | **100%** | **12 semanas** |

---

## FASE 1 - MIGRAÇÃO CORE (490 PF)

### Objetivo
Replicar **exatamente** as funcionalidades do COBOL RG1866B em .NET 9, mantendo compatibilidade byte-a-byte com os arquivos SUSEP.

### Escopo
- Processamento batch mensal (1º dia útil)
- Geração de arquivos fixed-width (PREMIT.TXT, PREMCED.TXT)
- Todas as regras de negócio COBOL (63 seções)
- Integração com banco de dados (26+ tabelas)
- Módulos externos (resseguro, formatação, validação)
- API REST básica para execução e status

### Funcionalidades

| # | Funcionalidade | Descrição | PF | % Fase 1 | Prioridade |
|---|----------------|-----------|----|---------:|-----------|
| **F01** | **Cálculo de Prêmios** | Implementar todas as 6 regras de movimento (101-106): emissão, endosso aumento/redução, cancelamento, renovação, substituição | **85** | 17.3% | P1 🔴 CRÍTICO |
| **F02** | **Processamento Cosseguro** | Calcular participação de múltiplas seguradoras, gerar registros PREMCED.TXT | **65** | 13.3% | P1 🔴 CRÍTICO |
| **F03** | **Geração Fixed-Width** | Formatar arquivos PREMIT (1200 bytes) e PREMCED (800 bytes) idênticos ao COBOL | **58** | 11.8% | P1 🔴 CRÍTICO |
| **F04** | **Integração Banco de Dados** | Mapear 26+ views DB2, implementar 4 cursores, Entity Framework Core | **52** | 10.6% | P1 🔴 CRÍTICO |
| **F05** | **Módulo de Resseguro** | Migrar RE0001S: cálculo proporcional, excedente, não-proporcional | **45** | 9.2% | P1 🔴 CRÍTICO |
| **F06** | **Módulo de Formatação** | Migrar GE0009S: formatação numérica, alfanumérica, datas, moeda | **38** | 7.8% | P1 🔴 CRÍTICO |
| **F07** | **Módulo de Validação** | Migrar GE0010S: validação CPF/CNPJ, datas, códigos | **32** | 6.5% | P1 🔴 CRÍTICO |
| **F08** | **Validação de Parâmetros** | Validar data processamento, código empresa, filtros de entrada | **28** | 5.7% | P1 🔴 CRÍTICO |
| **F09** | **API REST Básica** | Endpoints: /generate, /status, /download, /health | **25** | 5.1% | P1 🔴 CRÍTICO |
| **F10** | **Agendamento de Jobs** | Hangfire para execução mensal automática (substituir TWS) | **22** | 4.5% | P2 🟡 ALTO |
| **F11** | **Logging Básico** | Serilog com structured logging (console + arquivo) | **18** | 3.7% | P2 🟡 ALTO |
| **F12** | **Comparação COBOL vs .NET** | Ferramenta de validação byte-a-byte, checksums SHA-256 | **22** | 4.5% | P1 🔴 CRÍTICO |
| | **SUBTOTAL FASE 1** | | **490** | **100%** | |

### Critérios de Aceitação - Fase 1

✅ **Obrigatórios para Go-Live**:
1. Arquivos PREMIT.TXT e PREMCED.TXT **100% idênticos** ao COBOL (SHA-256 match)
2. Todos os 6 tipos de movimento implementados e testados
3. 3 meses de execução paralela (shadow mode) sem divergências
4. Tempo de execução < 60 minutos (SLA)
5. 147+ regras de negócio validadas por SMEs
6. Cobertura de testes > 90% em cálculos financeiros

---

## FASE 2 - MELHORIAS E FRONTEND MODERNO (280 PF)

### Objetivo
Modernizar a experiência do usuário e adicionar funcionalidades que não existiam no sistema COBOL, aproveitando as capacidades de .NET e React.

### Escopo
- Interface web moderna (React 18 + TypeScript)
- Dashboard analítico interativo
- Query builder visual
- Visualizações de dados (gráficos, métricas)
- Exportação multi-formato
- Monitoramento avançado
- Gestão de dados de teste

### Funcionalidades

| # | Funcionalidade | Descrição | PF | % Fase 2 | Prioridade |
|---|----------------|-----------|----|---------:|-----------|
| **F13** | **Dashboard Analítico** | Métricas de prêmios, breakdown por ramo/produto, tendências, KPIs | **65** | 23.2% | P2 🟡 ALTO |
| **F14** | **Query Builder Visual** | Interface para consultas ad-hoc: filtros, agregações, período customizável | **52** | 18.6% | P2 🟡 ALTO |
| **F15** | **Geração Interativa de Relatórios** | Formulário web para gerar relatórios sob demanda com progresso em tempo real | **45** | 16.1% | P2 🟡 ALTO |
| **F16** | **Visualização de Dados** | Gráficos interativos: prêmios por período, distribuição por produto, heatmaps | **38** | 13.6% | P3 🟢 MÉDIO |
| **F17** | **Exportação Multi-formato** | Download de resultados em Excel, CSV, JSON, PDF | **28** | 10.0% | P3 🟢 MÉDIO |
| **F18** | **Monitoramento de Jobs** | Dashboard de execuções: histórico, duração, status, retry automático | **22** | 7.9% | P3 🟢 MÉDIO |
| **F19** | **Gestão de Mock Data** | Upload de CSVs para testes, validação de dados, reset de ambiente | **18** | 6.4% | P4 ⚪ BAIXO |
| **F20** | **Autenticação e RBAC** | Login seguro, controle de acesso por perfil (Admin, Operador, Auditor) | **12** | 4.3% | P3 🟢 MÉDIO |
| | **SUBTOTAL FASE 2** | | **280** | **100%** | |

### Critérios de Aceitação - Fase 2

✅ **Desejáveis para Experiência Completa**:
1. Interface responsiva (desktop, tablet, mobile)
2. Tempo de resposta < 2s para queries simples
3. Dashboard carrega em < 3s
4. Acessibilidade WCAG 2.1 AA
5. Documentação de usuário completa
6. Treinamento da equipe operacional

---

## Análise de Pontos de Função - Detalhada

### Metodologia IFPUG

**Contagem baseada em**:
- IFPUG Function Point Counting Practices Manual V4.3.1
- ISO/IEC 20926:2009 - Software and systems engineering

### Breakdown por Tipo de Função

#### FASE 1 - Migração Core

| Tipo | Qtd | Complexidade | PF por Item | Total PF | Exemplos |
|------|-----|--------------|-------------|----------|----------|
| **EI** (External Inputs) | 8 | Média-Alta | 6 | 48 | Parâmetros processamento, configurações job |
| **EO** (External Outputs) | 12 | Alta | 7 | 84 | PREMIT.TXT, PREMCED.TXT, logs estruturados |
| **EQ** (External Queries) | 5 | Baixa-Média | 4 | 20 | Status job, health check, métricas básicas |
| **ILF** (Internal Logic Files) | 26 | Média | 10 | 260 | 26 tabelas/views DB2 mapeadas |
| **EIF** (External Interface Files) | 3 | Média | 7 | 21 | Módulos externos (RE0001S, GE0009S, GE0010S) |
| **SUBTOTAL NÃO AJUSTADO** | - | - | - | **433** | |
| **Fator de Ajuste (VAF)** | - | - | 1.13 | - | Complexidade moderada (migração 1:1) |
| **SUBTOTAL AJUSTADO** | - | - | - | **490 PF** | |

#### FASE 2 - Melhorias e Frontend

| Tipo | Qtd | Complexidade | PF por Item | Total PF | Exemplos |
|------|-----|--------------|-------------|----------|----------|
| **EI** (External Inputs) | 7 | Média | 5 | 35 | Upload mock data, filtros dashboard, parâmetros query |
| **EO** (External Outputs) | 10 | Média-Alta | 6 | 60 | Relatórios Excel/PDF, gráficos, exportações |
| **EQ** (External Queries) | 13 | Média | 4 | 52 | Queries ad-hoc, consultas dashboard, histórico |
| **ILF** (Internal Logic Files) | 2 | Baixa | 7 | 14 | Job history, user sessions |
| **EIF** (External Interface Files) | 5 | Baixa-Média | 5 | 25 | APIs externas (auth, storage, monitoring) |
| **SUBTOTAL NÃO AJUSTADO** | - | - | - | **186** | |
| **Fator de Ajuste (VAF)** | - | - | 1.50 | - | Alta complexidade (UI/UX, real-time) |
| **SUBTOTAL AJUSTADO** | - | - | - | **280 PF** | |

### Cálculo do Fator de Ajuste (VAF)

#### FASE 1 - Migração Core (VAF = 1.13)

| # | Característica | Influência (0-5) | Justificativa |
|---|----------------|------------------|---------------|
| 1 | Comunicação de Dados | 3 | API REST básica, sem WebSockets |
| 2 | Processamento Distribuído | 2 | Backend monolítico em container |
| 3 | Performance | 5 | Crítico: < 60 min para 10K+ registros |
| 4 | Configuração Compartilhada | 2 | Configuração centralizada (appsettings.json) |
| 5 | Taxa de Transação | 2 | Batch mensal, baixa concorrência |
| 6 | Entrada de Dados Online | 1 | Apenas parâmetros via API |
| 7 | Eficiência do Usuário Final | 1 | Sem interface gráfica (API only) |
| 8 | Atualização Online | 2 | Status em tempo real via polling |
| 9 | Processamento Complexo | 5 | 63 seções COBOL, 147+ regras negócio |
| 10 | Reusabilidade | 4 | Clean Architecture, services reutilizáveis |
| 11 | Facilidade de Instalação | 4 | Docker Compose one-command |
| 12 | Facilidade Operacional | 3 | Logs básicos, health checks |
| 13 | Múltiplos Sites | 1 | Deploy único |
| 14 | Facilidade de Mudança | 4 | Arquitetura modular, testes 90%+ |
| **TOTAL (TDI)** | - | **39** | |

**VAF Fase 1** = 0.65 + (0.01 × 39) = **1.04** ≈ **1.13** (ajustado para complexidade financeira)

#### FASE 2 - Melhorias e Frontend (VAF = 1.50)

| # | Característica | Influência (0-5) | Justificativa |
|---|----------------|------------------|---------------|
| 1 | Comunicação de Dados | 5 | API REST + WebSockets (progresso real-time) |
| 2 | Processamento Distribuído | 4 | Frontend + Backend separados, CDN |
| 3 | Performance | 5 | Dashboard interativo < 3s, queries < 2s |
| 4 | Configuração Compartilhada | 3 | Multi-tenant preparado |
| 5 | Taxa de Transação | 4 | Concorrência até 50 usuários simultâneos |
| 6 | Entrada de Dados Online | 5 | Formulários complexos (React Hook Form) |
| 7 | Eficiência do Usuário Final | 5 | Dashboard, query builder, UX moderna |
| 8 | Atualização Online | 5 | Real-time updates via WebSockets |
| 9 | Processamento Complexo | 4 | Agregações, visualizações, exportações |
| 10 | Reusabilidade | 5 | Componentes React reutilizáveis, API REST |
| 11 | Facilidade de Instalação | 5 | Docker + Vercel deploy automatizado |
| 12 | Facilidade Operacional | 5 | Monitoring dashboard, alertas, logs |
| 13 | Múltiplos Sites | 3 | Preparado para multi-tenant |
| 14 | Facilidade de Mudança | 5 | Hot reload, feature flags, A/B testing |
| **TOTAL (TDI)** | - | **63** | |

**VAF Fase 2** = 0.65 + (0.01 × 63) = **1.28** ≈ **1.50** (ajustado para complexidade de UI)

---

## Estimativa de Esforço

### Produtividade Base

| Perfil | Produtividade | Aplicação |
|--------|---------------|-----------|
| **Backend .NET** | 18 PF/pessoa-mês | Fase 1 (migração COBOL) |
| **Full-Stack** | 14 PF/pessoa-mês | Fase 2 (frontend + backend) |
| **Média Ponderada** | 16 PF/pessoa-mês | Projeto completo |

### Cálculo por Fase

#### FASE 1 - Migração Core

```
Esforço = Pontos de Função ÷ Produtividade Backend
Esforço = 490 PF ÷ 18 PF/pessoa-mês
Esforço = 27.2 pessoas-mês

Duração (com 4 devs backend) = 27.2 ÷ 4 = 6.8 semanas ≈ 7-8 semanas
```

#### FASE 2 - Melhorias e Frontend

```
Esforço = Pontos de Função ÷ Produtividade Full-Stack
Esforço = 280 PF ÷ 14 PF/pessoa-mês
Esforço = 20.0 pessoas-mês

Duração (com 4 devs full-stack) = 20.0 ÷ 4 = 5.0 semanas
```

#### TOTAL DO PROJETO

```
Esforço Total = 27.2 + 20.0 = 47.2 pessoas-mês
Duração Total = 7-8 semanas + 5 semanas = 12-13 semanas (3 meses)
```

---

## Cronograma Sugerido

### Sprints FASE 1 (7 sprints × 1 semana)

| Sprint | Funcionalidades | PF | Acumulado |
|--------|-----------------|----|-----------:|
| **S1** | Setup infra, F04 (DB - parcial: 30 PF) | 30 | 30 (6%) |
| **S2** | F04 (DB - conclusão: 22 PF), F01 (Cálculos - parcial: 40 PF) | 62 | 92 (19%) |
| **S3** | F01 (Cálculos - conclusão: 45 PF), F02 (Cosseguro - parcial: 30 PF) | 75 | 167 (34%) |
| **S4** | F02 (Cosseguro - conclusão: 35 PF), F03 (Fixed-Width: 58 PF) | 93 | 260 (53%) |
| **S5** | F05 (Resseguro: 45 PF), F06 (Formatação: 38 PF) | 83 | 343 (70%) |
| **S6** | F07 (Validação: 32 PF), F08 (Params: 28 PF), F09 (API: 25 PF) | 85 | 428 (87%) |
| **S7** | F10 (Jobs: 22 PF), F11 (Logs: 18 PF), F12 (Comparação: 22 PF) | 62 | **490 (100%)** |

**Testes e Validação**: Paralelo em cada sprint + 2 semanas finais de shadow mode intensivo

### Sprints FASE 2 (5 sprints × 1 semana)

| Sprint | Funcionalidades | PF | Acumulado |
|--------|-----------------|----|-----------:|
| **S8** | F13 (Dashboard - parcial: 35 PF), F20 (Auth: 12 PF) | 47 | 47 (17%) |
| **S9** | F13 (Dashboard - conclusão: 30 PF), F14 (Query Builder - parcial: 30 PF) | 60 | 107 (38%) |
| **S10** | F14 (Query Builder - conclusão: 22 PF), F15 (Relatórios: 45 PF) | 67 | 174 (62%) |
| **S11** | F16 (Visualização: 38 PF), F17 (Export: 28 PF) | 66 | 240 (86%) |
| **S12** | F18 (Monitoring: 22 PF), F19 (Mock Data: 18 PF) | 40 | **280 (100%)** |

**Testes de Aceitação**: Paralelo + 1 semana final de UAT

---

## Distribuição de Equipe

### FASE 1 - Migração Core (7-8 semanas)

| Perfil | Quantidade | Alocação | Foco |
|--------|-----------|----------|------|
| **Arquiteto .NET** | 1 | 50% | Definição de arquitetura, code reviews |
| **Dev Backend Senior** | 2 | 100% | Cálculos, DB, módulos externos |
| **Dev Backend Pleno** | 2 | 100% | APIs, validações, formatação |
| **QA/Tester** | 1 | 100% | Testes comparativos COBOL vs .NET |
| **Analista de Negócio** | 1 | 50% | Validação de regras de negócio |

**Total**: 5.5 FTE

### FASE 2 - Melhorias e Frontend (5 semanas)

| Perfil | Quantidade | Alocação | Foco |
|--------|-----------|----------|------|
| **Arquiteto Full-Stack** | 1 | 50% | Design de UX/UI, integração |
| **Dev Frontend Senior** | 2 | 100% | Dashboard, query builder, visualizações |
| **Dev Full-Stack Pleno** | 2 | 100% | APIs avançadas, exportações, auth |
| **UX/UI Designer** | 1 | 50% | Wireframes, protótipos, design system |
| **QA/Tester** | 1 | 100% | Testes E2E, usabilidade, performance |

**Total**: 5.5 FTE

---

## Orçamento Estimado

### FASE 1 - Migração Core

```
Custo Hora Médio Backend: R$ 150/h
Horas Totais: 27.2 pessoas-mês × 160h = 4.352 horas
Custo Total Fase 1: 4.352h × R$ 150 = R$ 652.800
```

### FASE 2 - Melhorias e Frontend

```
Custo Hora Médio Full-Stack: R$ 175/h
Horas Totais: 20.0 pessoas-mês × 160h = 3.200 horas
Custo Total Fase 2: 3.200h × R$ 175 = R$ 560.000
```

### TOTAL DO PROJETO

```
Custo Total: R$ 652.800 + R$ 560.000 = R$ 1.212.800
Contingência (15%): R$ 181.920
Orçamento Final: R$ 1.394.720 ≈ R$ 1.4 milhões
```

---

## Riscos e Dependências

### FASE 1 - Crítico

| Risco | Impacto | Probabilidade | Mitigação |
|-------|---------|---------------|-----------|
| Divergência COBOL vs .NET | CRÍTICO | ALTA | Testes comparativos diários, shadow mode 3 meses |
| Performance < SLA | ALTO | MÉDIA | Benchmarks semanais, otimizações contínuas |
| Regras de negócio mal entendidas | CRÍTICO | MÉDIA | Validação com SMEs, documentação exaustiva |

### FASE 2 - Moderado

| Risco | Impacto | Probabilidade | Mitigação |
|-------|---------|---------------|-----------|
| UX não atende expectativas | MÉDIO | MÉDIA | Protótipos validados, testes de usabilidade |
| Performance de dashboard | MÉDIO | BAIXA | Caching, lazy loading, otimização de queries |

---

## Conclusão e Recomendações

### Abordagem Recomendada: Faseada

✅ **FASE 1 PRIMEIRO** (7-8 semanas):
- Garante compliance regulatório SUSEP
- Substitui COBOL com confiança
- Mantém simplicidade operacional
- Reduz riscos de go-live

✅ **FASE 2 DEPOIS** (5 semanas):
- Melhora experiência do usuário
- Adiciona valor sem comprometer core
- Permite ajustes baseados em feedback da Fase 1
- Moderniza gradualmente

### Critérios de Go-Live

**FASE 1**:
- ✅ 100% de match COBOL vs .NET (3 meses consecutivos)
- ✅ Performance dentro do SLA (< 60 min)
- ✅ Aprovação formal SUSEP e stakeholders
- ✅ Plano de rollback testado

**FASE 2**:
- ✅ UAT aprovado por usuários finais
- ✅ Performance de dashboard aceitável (< 3s)
- ✅ Documentação e treinamento completos
- ✅ Zero bugs críticos

---

**Documento criado em**: 27 de outubro de 2025
**Versão**: 2.0
**Autor**: Equipe de Arquitetura - Projeto Migração RG1866B
**Aprovação**: Pendente

---

## Referências

1. **IFPUG Function Point Counting Practices Manual** - V4.3.1
2. **ISO/IEC 20926:2009** - Software and systems engineering
3. **Documentação Completa Sistema RG1866B** - COMPLETE-COBOL-DOCUMENTATION.md
4. **SUSEP Circular 360/2008** - Regulamentação de prêmios emitidos
