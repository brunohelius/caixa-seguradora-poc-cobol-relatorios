# PDF Migration Analysis Document Generator

Gerador de documentos PDF para análise de migração COBOL para .NET 9 - Caixa Seguradora SUSEP Circular 360.

## Instalação

### Pré-requisitos

- .NET 9.0 SDK ou superior
- macOS, Windows ou Linux

### Setup

```bash
# Clone o repositório
git clone <repository-url>
cd backend/tools/PdfGenerator

# Restaurar pacotes
dotnet restore

# Compilar
dotnet build
```

## Uso Rápido

### Gerar PDF com configurações padrão

```bash
dotnet run --project src/PdfGenerator
```

Isso irá gerar `output/migration-analysis-v1.0.0.pdf` com todas as seções configuradas.

### Opções de linha de comando

```bash
# Especificar diretório de saída
dotnet run -- --output ~/Documents/reports

# Modo verbose (debug)
dotnet run -- --verbose

# Modo dry-run (validação sem gerar PDF)
dotnet run -- --dry-run

# Versão específica
dotnet run -- --version 2.0.0

# Idioma (padrão: pt-BR)
dotnet run -- --language en

# Saída JSON para CI/CD
dotnet run -- --format json
```

## Estrutura do Documento

O PDF gerado contém as seguintes seções:

1. **Capa**: Logo, título e informações do projeto
2. **Sumário**: Índice com hyperlinks navegáveis
3. **Resumo Executivo**: Visão geral em 2 páginas
4. **Análise COBOL**: 687 itens de dados, 26 tabelas, ~5.000 LOC
5. **Arquitetura de Migração**: Clean Architecture, 3 camadas
6. **Especificações de Componentes**: React, .NET 9, Entity Framework
7. **Pontos de Função**: Cálculo IFPUG 4.3.1 (~2.100 FP)
8. **Análise Financeira**: R$ 1.575.000 (FP × 750)
9. **Cronograma**: Gantt de 12 semanas
10. **Metodologia**: Processo MIGRAI

## Configuração

### appsettings.json

```json
{
  "DocumentSettings": {
    "CompanyName": "Caixa Seguradora",
    "OutputDirectory": "output",
    "SourceFilePaths": {
      "CobolAnalysis": "docs/parser/FINAL-ANALYSIS-REPORT.md",
      "Specification": "specs/001-vamos-migrar-sistema/spec.md"
    }
  },
  "Branding": {
    "PrimaryColor": "#0047BB",
    "AccentColor": "#FFB81C"
  }
}
```

## Dados de Entrada

### Function Points (data/function-points.json)

```json
{
  "functions": [
    {
      "type": "EI",
      "name": "Gerar Relatório PREMIT",
      "complexity": "High",
      "files": 26,
      "dataElements": 687
    }
  ]
}
```

### Cronograma (data/project-schedule.json)

```json
{
  "phases": [
    {
      "name": "Setup",
      "startWeek": 1,
      "duration": 2,
      "tasks": ["Ambiente", "Dependências"]
    }
  ]
}
```

## Testes

```bash
# Executar todos os testes
dotnet test

# Testes unitários apenas
dotnet test --filter Category=Unit

# Testes de integração
dotnet test --filter Category=Integration

# Com cobertura
dotnet test /p:CollectCoverage=true
```

## Performance

- **Tempo de geração**: < 60 segundos
- **Tamanho do PDF**: < 20MB com 50+ diagramas
- **Memória**: ~500MB máximo
- **CPU**: Paralelização de gráficos (4 threads)

## Validação PDF/A

O documento é gerado em conformidade com PDF/A-1b para arquivamento de longo prazo:

```bash
# Instalar VeraPDF (opcional)
brew install verapdf

# Validar conformidade
verapdf output/migration-analysis-v1.0.0.pdf
```

## CI/CD Integration

### GitHub Actions

```yaml
- name: Generate PDF Report
  run: |
    cd backend/tools/PdfGenerator
    dotnet run -- --format json > report.json

- name: Upload PDF
  uses: actions/upload-artifact@v3
  with:
    name: migration-analysis
    path: output/*.pdf
```

### Azure DevOps

```yaml
- task: DotNetCoreCLI@2
  inputs:
    command: 'run'
    projects: 'backend/tools/PdfGenerator/src/PdfGenerator/PdfGenerator.csproj'
    arguments: '--configuration Release'
```

## Troubleshooting

### Erro: "QuestPDF license required"

QuestPDF requer licença para uso comercial. Para desenvolvimento/teste:

```csharp
QuestPDF.Settings.License = LicenseType.Community;
```

### Erro: "Font not found"

Instalar fontes do sistema:

```bash
# macOS
brew install font-arial

# Linux
apt-get install fonts-liberation

# Windows
# Arial já incluído
```

### Erro: "Out of memory"

Aumentar limite de memória:

```bash
export DOTNET_GCHeapHardLimit=800000000
dotnet run
```

## Estrutura do Projeto

```
PdfGenerator/
├── src/
│   └── PdfGenerator/
│       ├── Models/           # Modelos de dados
│       ├── Services/         # Lógica de negócio
│       ├── PdfGeneration/    # Renderização PDF
│       │   ├── Sections/     # Seções do documento
│       │   └── Formatting/   # Estilos e formatação
│       ├── Formatting/       # Formatação BR
│       ├── Resources/        # Arquivos i18n
│       └── Program.cs        # Entry point CLI
└── tests/
    └── PdfGenerator.Tests/
        ├── Unit/            # Testes unitários
        └── Integration/     # Testes de integração
```

## Contribuindo

1. Criar branch: `git checkout -b feature/nova-secao`
2. Commit: `git commit -m "feat: adicionar seção X"`
3. Push: `git push origin feature/nova-secao`
4. Pull Request com descrição detalhada

## Licença

Propriedade de Caixa Seguradora. Uso interno apenas.

## Suporte

- **Email**: equipe-migracao@caixaseguradora.com.br
- **Teams**: Canal #migração-cobol
- **Wiki**: https://wiki.caixaseguradora.com.br/migracao

## Changelog

### v1.0.0 (2025-10-23)
- Versão inicial com todas as 12 seções
- Suporte para português brasileiro
- Cálculo de pontos de função IFPUG
- Gráficos com ScottPlot
- Conformidade PDF/A-1b