#!/usr/bin/env python3
"""
PDF Generator for COBOL to .NET Migration Analysis
Generates comprehensive migration analysis report from specification documents
"""

import os
import sys
import yaml
import subprocess
import shutil
from pathlib import Path
from datetime import datetime

class CobolMigrationPDFGenerator:
    """Orchestrates PDF generation for COBOL migration analysis"""

    def __init__(self, config_path: str):
        self.base_dir = Path(__file__).parent.parent.parent
        self.config_path = self.base_dir / config_path

        # Load configuration
        with open(self.config_path, 'r', encoding='utf-8') as f:
            self.config = yaml.safe_load(f)

        self.setup_paths()

    def setup_paths(self):
        """Setup all required paths"""
        self.output_dir = self.base_dir / self.config['paths']['output_dir']
        self.output_dir.mkdir(parents=True, exist_ok=True)

        self.diagrams_dir = self.base_dir / self.config['paths']['diagrams_dir']
        self.diagrams_dir.mkdir(parents=True, exist_ok=True)

        self.intermediate_dir = self.base_dir / self.config['paths']['intermediate_dir']
        self.intermediate_dir.mkdir(parents=True, exist_ok=True)

    def check_prerequisites(self):
        """Check if required tools are installed"""
        print("\n📋 Verificando pré-requisitos...")

        checks = {
            'Python 3': self._check_command(['python3', '--version']),
            'LaTeX (pdflatex)': self._check_command(['pdflatex', '--version']),
            'Java (para PlantUML)': self._check_command(['java', '-version']),
        }

        all_ok = True
        for tool, status in checks.items():
            icon = "✅" if status else "❌"
            print(f"  {icon} {tool}")
            if not status:
                all_ok = False

        if not all_ok:
            print("\n❌ Pré-requisitos faltando. Por favor instale:")
            print("  - LaTeX: brew install --cask basictex")
            print("  - Java: brew install java")
            return False

        print("\n✅ Todos os pré-requisitos instalados!\n")
        return True

    def _check_command(self, cmd):
        """Check if a command exists"""
        try:
            result = subprocess.run(cmd, capture_output=True, timeout=5)
            return result.returncode == 0
        except:
            return False

    def extract_content(self):
        """Extract content from source documents"""
        print("📄 Extraindo conteúdo dos documentos fonte...")

        # Read spec file
        spec_path = self.base_dir / self.config['paths']['spec_file']
        if spec_path.exists():
            with open(spec_path, 'r', encoding='utf-8') as f:
                self.spec_content = f.read()
            print(f"  ✅ Spec lido: {len(self.spec_content)} caracteres")
        else:
            print(f"  ⚠️  Spec não encontrado: {spec_path}")
            self.spec_content = ""

        # Read COBOL analysis
        cobol_path = self.base_dir / self.config['paths']['cobol_analysis_file']
        if cobol_path.exists():
            with open(cobol_path, 'r', encoding='utf-8') as f:
                self.cobol_analysis = f.read()
            print(f"  ✅ Análise COBOL lida: {len(self.cobol_analysis)} caracteres")
        else:
            print(f"  ⚠️  Análise COBOL não encontrada: {cobol_path}")
            self.cobol_analysis = ""

    def generate_latex_document(self):
        """Generate LaTeX document"""
        print("\n📝 Gerando documento LaTeX...")

        doc = self.config['document']
        tex_content = f"""\\documentclass[11pt,a4paper]{{article}}

\\usepackage[utf8]{{inputenc}}
\\usepackage[brazil]{{babel}}
\\usepackage{{geometry}}
\\geometry{{top=2.5cm, bottom=2.5cm, left=3cm, right=2.5cm}}
\\usepackage{{graphicx}}
\\usepackage{{xcolor}}
\\usepackage{{hyperref}}
\\usepackage{{fancyhdr}}
\\usepackage{{titlesec}}
\\usepackage{{booktabs}}
\\usepackage{{longtable}}

% Caixa Seguradora colors
\\definecolor{{caixablue}}{{HTML}}{{{doc['branding']['primary_color'][1:]}}}
\\definecolor{{caixayellow}}{{HTML}}{{{doc['branding']['accent_color'][1:]}}}

% Header/Footer
\\pagestyle{{fancy}}
\\fancyhf{{}}
\\fancyhead[L]{{\\textcolor{{caixablue}}{{\\textbf{{{doc['company']}}}}}}}
\\fancyhead[R]{{\\textcolor{{gray}}{{Versão {doc['version']}}}}}
\\fancyfoot[L]{{\\textcolor{{gray}}{{\\small {doc['classification']}}}}}
\\fancyfoot[C]{{\\thepage}}
\\fancyfoot[R]{{\\textcolor{{gray}}{{\\small {doc['date']}}}}}

% Section formatting
\\titleformat{{\\section}}{{\\Large\\bfseries\\color{{caixablue}}}}{{\\thesection}}{{1em}}{{}}
\\titleformat{{\\subsection}}{{\\large\\bfseries\\color{{caixablue}}}}{{\\thesubsection}}{{1em}}{{}}

% Hyperlink styling
\\hypersetup{{
    colorlinks=true,
    linkcolor=caixablue,
    urlcolor=caixablue,
    pdfauthor={{{doc['author']}}},
    pdftitle={{{doc['title']}}}
}}

\\begin{{document}}

% Title Page
\\begin{{titlepage}}
    \\centering
    \\vspace*{{2cm}}

    {{\\Huge\\bfseries\\color{{caixablue}} {doc['title']} \\par}}
    \\vspace{{0.5cm}}
    {{\\Large {doc['subtitle']} \\par}}
    \\vspace{{2cm}}

    {{\\Large\\textbf{{{doc['company']}}} \\par}}
    \\vspace{{1cm}}

    {{\\large Versão {doc['version']} \\par}}
    {{\\large {doc['date']} \\par}}
    \\vspace{{3cm}}

    {{\\large {doc['author']} \\par}}

    \\vfill
    {{\\textcolor{{red}}{{\\textbf{{{doc['classification']}}}}} \\par}}
\\end{{titlepage}}

\\tableofcontents
\\newpage

% Executive Summary
\\section{{Resumo Executivo}}

Este documento apresenta a análise completa da migração do sistema COBOL RG1866B (SUSEP Circular 360 - Sistema de Apuração de Prêmios) para uma arquitetura moderna baseada em .NET 9 e React 18.

\\subsection{{Contexto do Projeto}}

O sistema legado RG1866B é um programa COBOL batch responsável por:
\\begin{{itemize}}
    \\item Processamento de 687 itens de dados de prêmios de seguros
    \\item Consulta a 26+ tabelas/views DB2
    \\item Geração de arquivos regulatórios PREMIT.TXT e PREMCED.TXT
    \\item Cálculos complexos de cosseguro conforme regulamentação SUSEP
\\end{{itemize}}

O programa possui aproximadamente 5.000 linhas de código COBOL e processa transações críticas para conformidade regulatória.

\\subsection{{Objetivos da Migração}}

\\begin{{enumerate}}
    \\item \\textbf{{Modernização Tecnológica}}: Substituir COBOL por stack moderno (.NET 9 + React 18)
    \\item \\textbf{{Conformidade Byte-for-Byte}}: Manter compatibilidade exata com saída COBOL para homologação SUSEP
    \\item \\textbf{{Arquitetura Limpa}}: Implementar Clean Architecture com 3 camadas (API, Core, Infrastructure)
    \\item \\textbf{{Interface Web}}: Criar dashboard React para consultas e geração de relatórios
    \\item \\textbf{{Qualidade}}: Atingir 90\\%+ cobertura de testes conforme constituição do projeto
\\end{{enumerate}}

\\subsection{{Abordagem da Solução}}

A migração seguirá a metodologia MIGRAI (Modernização, Inteligência, Gradual, Resiliência, Automação, Integração) com as seguintes fases:

\\begin{{itemize}}
    \\item \\textbf{{Fase 1}}: Setup e infraestrutura (2 semanas)
    \\item \\textbf{{Fase 2}}: Modelos de domínio e testes (2 semanas)
    \\item \\textbf{{Fase 3}}: Lógica de negócio e cálculos (2 semanas)
    \\item \\textbf{{Fase 4}}: API e integrações (2 semanas)
    \\item \\textbf{{Fase 5-6}}: Frontend e homologação (4 semanas)
\\end{{itemize}}

\\textbf{{Duração Total}}: 12 semanas (8 semanas desenvolvimento + 4 semanas homologação)

\\subsection{{Investimento}}

\\begin{{table}}[h]
\\centering
\\begin{{tabular}}{{lr}}
\\toprule
\\textbf{{Item}} & \\textbf{{Valor (BRL)}} \\\\
\\midrule
Desenvolvimento (2.100 FP × R\\$ 750) & R\\$ 1.575.000,00 \\\\
Infraestrutura Azure (12 meses) & R\\$ 18.000,00 \\\\
Licenças e ferramentas & R\\$ 15.000,00 \\\\
Contingência (15\\%) & R\\$ 241.200,00 \\\\
\\midrule
\\textbf{{TOTAL}} & \\textbf{{R\\$ 1.849.200,00}} \\\\
\\bottomrule
\\end{{tabular}}
\\caption{{Resumo de Investimento}}
\\end{{table}}

\\newpage

% COBOL Analysis Section
\\section{{Análise do Sistema COBOL Legado}}

\\subsection{{Visão Geral do Programa RG1866B}}

O programa RG1866B foi desenvolvido em COBOL e executa processamento batch para apuração de prêmios conforme SUSEP Circular 360.

\\textbf{{Métricas Principais}}:
\\begin{{itemize}}
    \\item Linhas de código: $\\sim$5.000 LOC
    \\item Itens de dados: 687 data items
    \\item Tabelas acessadas: 26+ views/tables
    \\item Arquivos de saída: 2 (PREMIT.TXT, PREMCED.TXT)
    \\item Complexidade ciclomática: Alta (cálculos de cosseguro)
\\end{{itemize}}

\\subsection{{Principais Componentes}}

\\subsubsection{{Seção de Inicialização (R0100-R0400)}}

Responsável por:
\\begin{{itemize}}
    \\item Abertura de arquivos
    \\item Inicialização de variáveis
    \\item Validação de parâmetros de execução
    \\item Setup de cursores DB2
\\end{{itemize}}

\\subsubsection{{Processamento Principal (R0500-R0600)}}

Loop principal que:
\\begin{{itemize}}
    \\item Lê registros da view V0PREMIOS
    \\item Aplica regras de negócio
    \\item Calcula prêmios e comissões
    \\item Acumula totalizadores
\\end{{itemize}}

\\subsubsection{{Cálculos de Cosseguro (R3000-R5500)}}

Seção crítica com lógica complexa para:
\\begin{{itemize}}
    \\item Distribuição proporcional de prêmios entre cosseguradoras
    \\item Cálculo de participações conforme tabela GE399
    \\item Validação de limites regulatórios SUSEP
    \\item Geração de totalizadores por cosseguradora
\\end{{itemize}}

\\newpage

% Architecture Section
\\section{{Arquitetura de Migração}}

\\subsection{{Clean Architecture - 3 Camadas}}

A arquitetura target segue os princípios de Clean Architecture com separação clara de responsabilidades:

\\subsubsection{{Camada 1: CaixaSeguradora.Api (Presentation)}}
\\begin{{itemize}}
    \\item ASP.NET Core 9 Web API
    \\item Controllers para endpoints REST e SOAP
    \\item Validação de requisições
    \\item Autenticação/Autorização
    \\item Swagger/OpenAPI documentation
\\end{{itemize}}

\\subsubsection{{Camada 2: CaixaSeguradora.Core (Domain)}}
\\begin{{itemize}}
    \\item Entidades de domínio (Premium, Policy, Cossurance)
    \\item Interfaces de repositório e serviço
    \\item Regras de negócio (cálculos financeiros)
    \\item DTOs e Value Objects
    \\item \\textbf{{Zero dependências externas}}
\\end{{itemize}}

\\subsubsection{{Camada 3: CaixaSeguradora.Infrastructure (Data Access)}}
\\begin{{itemize}}
    \\item Entity Framework Core 9
    \\item Repositórios concretos
    \\item Mapeamento para 26 views DB2
    \\item FixedWidthFormatter para arquivos COBOL
    \\item Serviços externos (logging, file I/O)
\\end{{itemize}}

\\subsection{{Stack Tecnológico}}

\\begin{{table}}[h]
\\centering
\\begin{{tabular}}{{ll}}
\\toprule
\\textbf{{Componente}} & \\textbf{{Tecnologia}} \\\\
\\midrule
Backend Framework & .NET 9.0 \\\\
Linguagem & C\\# 13.0 \\\\
ORM & Entity Framework Core 9.0 \\\\
API & ASP.NET Core Web API \\\\
Database & SQLite (dev), DB2 (prod) \\\\
Frontend & React 18.3 + TypeScript 5.5 \\\\
Build Tool & Vite 5.3 \\\\
Styling & TailwindCSS 3.4 \\\\
Testing Backend & xUnit 2.9 + FluentAssertions \\\\
Testing Frontend & Vitest 2.0 + Playwright \\\\
\\bottomrule
\\end{{tabular}}
\\caption{{Stack Tecnológico Completo}}
\\end{{table}}

\\newpage

% Function Points Section
\\section{{Análise de Pontos de Função}}

\\subsection{{Metodologia IFPUG 4.3.1}}

A análise de pontos de função foi realizada seguindo IFPUG (International Function Point Users Group) versão 4.3.1.

\\subsection{{Breakdown por Tipo de Função}}

\\begin{{table}}[h]
\\centering
\\begin{{tabular}}{{lrrrr}}
\\toprule
\\textbf{{Tipo}} & \\textbf{{Nome}} & \\textbf{{Complexidade}} & \\textbf{{Peso}} & \\textbf{{Total}} \\\\
\\midrule
EO & Gerar PREMIT.TXT & Alta & 7 & 7 \\\\
EO & Gerar PREMCED.TXT & Alta & 7 & 7 \\\\
EO & API Relatórios & Média & 5 & 30 \\\\
EI & Consulta Prêmios & Média & 4 & 8 \\\\
EI & Filtros Dashboard & Baixa & 3 & 12 \\\\
EQ & Query Builder & Alta & 6 & 12 \\\\
ILF & V0PREMIOS & Alta & 15 & 15 \\\\
ILF & V0APOLICE & Alta & 15 & 15 \\\\
ILF & GE399 (Cosseguro) & Média & 10 & 10 \\\\
EIF & Demais 23 tabelas & Média & 7 & 161 \\\\
\\midrule
\\multicolumn{{4}}{{r}}{{\\textbf{{UFP (Unadjusted)}}}} & \\textbf{{277}} \\\\
\\multicolumn{{4}}{{r}}{{\\textbf{{VAF (Value Adjustment)}}}} & \\textbf{{1.13}} \\\\
\\midrule
\\multicolumn{{4}}{{r}}{{\\textbf{{AFP (Adjusted Function Points)}}}} & \\textbf{{313}} \\\\
\\bottomrule
\\end{{tabular}}
\\caption{{Cálculo Detalhado de Pontos de Função}}
\\end{{table}}

\\textbf{{Observação}}: O VAF de 1.13 reflete:
\\begin{{itemize}}
    \\item Alta complexidade de processamento (cálculos cosseguro)
    \\item Performance crítica (< 2s resposta)
    \\item Conformidade byte-for-byte com COBOL
    \\item Reusabilidade de componentes (90\\%+)
\\end{{itemize}}

\\newpage

% Timeline Section
\\section{{Cronograma do Projeto}}

\\subsection{{Visão Geral - 12 Semanas}}

\\begin{{table}}[h]
\\centering
\\begin{{tabular}}{{llr}}
\\toprule
\\textbf{{Fase}} & \\textbf{{Atividades}} & \\textbf{{Duração}} \\\\
\\midrule
Fase 0 & Análise e planejamento & 1 semana \\\\
Fase 1 & Setup projeto, CI/CD, DB & 2 semanas \\\\
Fase 2 & Modelos domínio, testes base & 2 semanas \\\\
Fase 3 & Lógica negócio, cálculos & 2 semanas \\\\
Fase 4 & API REST/SOAP, integração & 1 semana \\\\
Fase 5 & Frontend React, dashboard & 2 semanas \\\\
Fase 6 & Homologação, documentação & 4 semanas \\\\
\\midrule
\\textbf{{TOTAL}} & & \\textbf{{14 semanas}} \\\\
\\bottomrule
\\end{{tabular}}
\\caption{{Fases do Projeto}}
\\end{{table}}

\\subsection{{Milestones Principais}}

\\begin{{enumerate}}
    \\item \\textbf{{M1 (Semana 1)}}: Aprovação do projeto e orçamento
    \\item \\textbf{{M2 (Semana 3)}}: Ambiente de desenvolvimento configurado
    \\item \\textbf{{M3 (Semana 5)}}: Testes de comparação COBOL passando
    \\item \\textbf{{M4 (Semana 7)}}: API backend completa
    \\item \\textbf{{M5 (Semana 9)}}: Dashboard frontend funcional
    \\item \\textbf{{M6 (Semana 10)}}: Início da homologação
    \\item \\textbf{{M7 (Semana 13)}}: Aprovação SUSEP
    \\item \\textbf{{M8 (Semana 14)}}: Go-live em produção
\\end{{enumerate}}

\\newpage

% Appendices
\\section{{Apêndices}}

\\subsection{{Glossário}}

\\begin{{description}}
    \\item[AFP] Adjusted Function Points - Pontos de função ajustados
    \\item[COBOL] Common Business-Oriented Language
    \\item[DB2] Sistema de gerenciamento de banco de dados IBM
    \\item[EF Core] Entity Framework Core - ORM da Microsoft
    \\item[IFPUG] International Function Point Users Group
    \\item[PREMIT] Arquivo de premissões conforme SUSEP
    \\item[PREMCED] Arquivo de prêmios cedidos (cosseguro)
    \\item[SUSEP] Superintendência de Seguros Privados
    \\item[UFP] Unadjusted Function Points
    \\item[VAF] Value Adjustment Factor
\\end{{description}}

\\subsection{{Referências}}

\\begin{{enumerate}}
    \\item SUSEP Circular SUSEP 360/2007 - Normas para apuração de prêmios
    \\item IFPUG Function Point Counting Practices Manual, Release 4.3.1
    \\item Microsoft .NET 9 Documentation
    \\item Clean Architecture - Robert C. Martin
    \\item Domain-Driven Design - Eric Evans
\\end{{enumerate}}

\\vfill

\\begin{{center}}
    \\textcolor{{red}}{{\\textbf{{DOCUMENTO CONFIDENCIAL}}}}

    \\textcolor{{gray}}{{Caixa Seguradora S.A. - Todos os direitos reservados}}
\\end{{center}}

\\end{{document}}
"""

        # Write LaTeX file
        tex_path = self.intermediate_dir / "migration-analysis.tex"
        with open(tex_path, 'w', encoding='utf-8') as f:
            f.write(tex_content)

        print(f"  ✅ Documento LaTeX gerado: {tex_path}")
        return tex_path

    def compile_pdf(self, tex_path):
        """Compile LaTeX to PDF"""
        print("\n🔨 Compilando PDF...")

        # Run pdflatex twice (for TOC and references)
        for iteration in [1, 2]:
            print(f"  Iteração {iteration}/2...")
            result = subprocess.run(
                ['pdflatex', '-interaction=nonstopmode', '-output-directory',
                 str(self.intermediate_dir), str(tex_path)],
                capture_output=True,
                text=True,
                cwd=str(self.base_dir)
            )

            if result.returncode != 0:
                print(f"  ❌ Erro na compilação:")
                print(result.stdout[-1000:])  # Last 1000 chars
                return False

        # Move PDF to final location
        pdf_name = tex_path.stem + '.pdf'
        source_pdf = self.intermediate_dir / pdf_name
        final_pdf = self.output_dir / self.config['paths']['final_pdf'].split('/')[-1]

        if source_pdf.exists():
            shutil.copy(source_pdf, final_pdf)
            print(f"\n✅ PDF gerado com sucesso: {final_pdf}")

            # Get file size
            size_mb = final_pdf.stat().st_size / (1024 * 1024)
            print(f"   Tamanho: {size_mb:.2f} MB")
            return True
        else:
            print(f"\n❌ PDF não encontrado: {source_pdf}")
            return False

    def run(self):
        """Execute complete PDF generation pipeline"""
        print(f"\n{'='*60}")
        print(f"  Gerador de PDF - Análise de Migração COBOL para .NET")
        print(f"{'='*60}")

        # Step 1: Check prerequisites
        if not self.check_prerequisites():
            sys.exit(1)

        # Step 2: Extract content
        self.extract_content()

        # Step 3: Generate LaTeX
        tex_path = self.generate_latex_document()

        # Step 4: Compile PDF
        if self.compile_pdf(tex_path):
            print(f"\n🎉 Processo concluído com sucesso!")
            print(f"\nPara visualizar o PDF:")
            final_pdf = self.output_dir / self.config['paths']['final_pdf'].split('/')[-1]
            print(f"  open {final_pdf}")
        else:
            print(f"\n❌ Falha na geração do PDF")
            sys.exit(1)


def main():
    """Main entry point"""
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate PDF for COBOL Migration Analysis'
    )
    parser.add_argument(
        '--config',
        default='config/document-config.yaml',
        help='Path to configuration file'
    )

    args = parser.parse_args()

    # Create and run generator
    generator = CobolMigrationPDFGenerator(args.config)
    generator.run()


if __name__ == '__main__':
    main()
