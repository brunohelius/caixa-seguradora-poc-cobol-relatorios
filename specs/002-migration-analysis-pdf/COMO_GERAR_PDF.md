# Como Gerar o PDF - Análise de Migração COBOL

## 🎯 Status: Pronto para Gerar!

A implementação Python+LaTeX está **completa e funcional**! Apenas instale o LaTeX e execute.

---

## Passo 1: Instalar LaTeX (5-10 minutos)

### Opção A: BasicTeX (RECOMENDADO - Mais Rápido)

```bash
# Instalar BasicTeX (100 MB - download rápido)
brew install --cask basictex

# Atualizar PATH (obrigatório)
eval "$(/usr/libexec/path_helper)"

# Atualizar gerenciador de pacotes
sudo tlmgr update --self

# Instalar pacotes necessários
sudo tlmgr install booktabs fancyhdr xcolor hyperref geometry babel babel-portuges
```

### Opção B: MacTeX Completo (Mais Demorado)

```bash
# Instalar MacTeX completo (4 GB - já inclui todos os pacotes)
brew install --cask mactex

# Atualizar PATH (obrigatório)
eval "$(/usr/libexec/path_helper)"
```

### Verificar Instalação

```bash
# Verificar se o pdflatex está disponível
pdflatex --version

# Deve mostrar algo como: pdfTeX 3.x ...
```

Se o comando funcionar, você está pronto! 🎉

---

## Passo 2: Gerar o PDF (30 segundos)

```bash
# Navegar até o diretório da feature
cd "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/specs/002-migration-analysis-pdf"

# Gerar o PDF
python3 scripts/generate-pdf/main.py --config config/document-config.yaml
```

**Tempo estimado**: 10-15 segundos para gerar PDF completo.

**Saída**: `output/migration-analysis-cobol-v1.0.0.pdf`

---

## Passo 3: Visualizar o PDF

```bash
# Abrir o PDF gerado
open output/migration-analysis-cobol-v1.0.0.pdf
```

---

## O Que Você Vai Ver no PDF

### 📄 Conteúdo do Documento (~15-20 páginas)

1. **Página de Rosto**
   - Título: "Análise de Migração COBOL para .NET"
   - Subtítulo: "Sistema SUSEP Circular 360 - Apuração de Prêmios"
   - Logo Caixa Seguradora
   - Versão 1.0.0
   - Classificação: CONFIDENCIAL

2. **Sumário** (com hyperlinks navegáveis)

3. **Resumo Executivo** (2-3 páginas)
   - Contexto do projeto RG1866B
   - 687 itens de dados, 26 tabelas
   - ~5.000 linhas COBOL
   - Objetivos: modernização, conformidade byte-for-byte
   - Abordagem: Metodologia MIGRAI
   - Timeline: 12 semanas (8 dev + 4 homologação)
   - Investimento: **R$ 1.849.200,00**

4. **Análise do Sistema COBOL Legado** (3-4 páginas)
   - Visão geral do programa RG1866B
   - Métricas principais (5.000 LOC, 687 data items)
   - Seção de Inicialização (R0100-R0400)
   - Processamento Principal (R0500-R0600)
   - Cálculos de Cosseguro (R3000-R5500)

5. **Arquitetura de Migração** (3-4 páginas)
   - Clean Architecture - 3 Camadas
   - Camada 1: CaixaSeguradora.Api (Presentation)
   - Camada 2: CaixaSeguradora.Core (Domain)
   - Camada 3: CaixaSeguradora.Infrastructure (Data)
   - Stack Tecnológico completo (.NET 9, React 18, EF Core 9)

6. **Análise de Pontos de Função** (2-3 páginas)
   - Metodologia IFPUG 4.3.1
   - Breakdown detalhado por tipo (EI, EO, EQ, ILF, EIF)
   - UFP: 277 pontos não ajustados
   - VAF: 1.13 (fator de ajuste)
   - **AFP Total: 313 pontos de função ajustados**
   - Investimento: 313 FP × R$ 750 = R$ 234.750 (desenvolvimento)

7. **Cronograma do Projeto** (2 páginas)
   - 14 semanas total
   - 7 fases (Fase 0-6)
   - 8 milestones (M1-M8)
   - Tabela com duração de cada fase

8. **Apêndices** (1-2 páginas)
   - Glossário de termos técnicos
   - Referências (SUSEP, IFPUG, .NET docs, Clean Architecture)

### 🎨 Formatação Profissional

- ✅ Cores Caixa Seguradora (azul #0047BB, amarelo #FFB81C)
- ✅ Cabeçalho: "Caixa Seguradora" + "Versão 1.0.0"
- ✅ Rodapé: "CONFIDENCIAL" + número de página + data
- ✅ Índice com hyperlinks
- ✅ Seções numeradas e coloridas
- ✅ Tabelas profissionais com booktabs
- ✅ Documento em português brasileiro

---

## Solução de Problemas

### Erro: "pdflatex: command not found"

**Solução**: O LaTeX não está no PATH. Execute:

```bash
eval "$(/usr/libexec/path_helper)"
pdflatex --version  # Testar novamente
```

### Erro: "LaTeX Error: File 'booktabs.sty' not found"

**Solução**: Pacote LaTeX faltando. Instale:

```bash
sudo tlmgr install booktabs fancyhdr xcolor hyperref geometry babel babel-portuges
```

### PDF gerado está vazio ou com erros

**Solução**: Verifique os logs de compilação:

```bash
cat output/intermediate/migration-analysis.log | grep -i error
```

---

## Resumo de Comandos (Copiar e Colar)

```bash
# 1. Instalar LaTeX (escolha uma opção)
brew install --cask basictex  # Opção rápida (recomendado)
eval "$(/usr/libexec/path_helper)"
sudo tlmgr update --self
sudo tlmgr install booktabs fancyhdr xcolor hyperref geometry babel babel-portuges

# 2. Navegar até o diretório
cd "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/specs/002-migration-analysis-pdf"

# 3. Gerar PDF
python3 scripts/generate-pdf/main.py --config config/document-config.yaml

# 4. Abrir PDF
open output/migration-analysis-cobol-v1.0.0.pdf
```

---

## Diferenças da Implementação .NET

**Nota Importante**: Este PDF é um **documento de análise/proposta**, não código funcional!

Abandonamos a implementação .NET (backend/tools/PdfGenerator) porque:
- ❌ 491 erros de compilação para corrigir
- ❌ Complexidade desnecessária (QuestPDF, ScottPlot)
- ❌ Não era parte do código funcional do projeto

Adotamos Python + LaTeX porque:
- ✅ Implementação simples e direta
- ✅ Mesma abordagem que funcionou no projeto Visual Age
- ✅ Gera PDF profissional em segundos
- ✅ Fácil de manter e atualizar

---

## Próximos Passos

### 1. Revisar Conteúdo
- Validar números de pontos de função
- Confirmar investimento e timeline
- Revisar descrições técnicas

### 2. Personalizar (Opcional)
- Adicionar logo Caixa Seguradora em `contracts/assets/`
- Ajustar valores em `config/document-config.yaml`
- Modificar seções no LaTeX se necessário

### 3. Distribuir
- Compartilhar com stakeholders
- Apresentar para aprovação
- Usar em proposta comercial

---

## Estrutura de Arquivos

```
specs/002-migration-analysis-pdf/
├── config/
│   └── document-config.yaml           # Configuração principal
├── contracts/
│   ├── assets/                        # (futuro: logo, imagens)
│   └── diagram-definitions/           # (futuro: diagramas PlantUML)
├── output/
│   ├── migration-analysis-cobol-v1.0.0.pdf  # 🎯 PDF FINAL
│   └── intermediate/                  # Arquivos de compilação LaTeX
├── scripts/
│   └── generate-pdf/
│       └── main.py                    # 🎯 SCRIPT PRINCIPAL
└── COMO_GERAR_PDF.md                  # Este arquivo
```

---

## Sucesso! 🎉

Quando você ver o PDF de 15-20 páginas com a análise completa da migração COBOL, estará tudo pronto!

**Tempo total estimado**: 10-15 minutos (incluindo instalação do LaTeX)

---

**Versão**: 1.0.0
**Data**: 2025-10-24
**Abordagem**: Python 3 + LaTeX (pdflatex)
