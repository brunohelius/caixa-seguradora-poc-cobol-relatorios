#!/usr/bin/env python3
"""
Simple PDF Generator usando apenas Python (sem LaTeX)
Gera PDF básico da análise de migração COBOL
"""

from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import cm
from reportlab.lib import colors
from reportlab.platypus import (
    SimpleDocTemplate, Paragraph, Spacer, PageBreak,
    Table, TableStyle, Image
)
from reportlab.lib.enums import TA_CENTER, TA_JUSTIFY, TA_LEFT
from datetime import datetime
from pathlib import Path

def create_pdf():
    """Gera PDF da análise de migração"""

    # Caminho de saída
    base_dir = Path(__file__).parent.parent.parent
    output_path = base_dir / "output" / "migration-analysis-cobol-v1.0.0.pdf"
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Criar documento
    doc = SimpleDocTemplate(
        str(output_path),
        pagesize=A4,
        rightMargin=2*cm,
        leftMargin=3*cm,
        topMargin=2.5*cm,
        bottomMargin=2.5*cm
    )

    # Estilos
    styles = getSampleStyleSheet()

    # Estilo customizado para título
    title_style = ParagraphStyle(
        'CustomTitle',
        parent=styles['Heading1'],
        fontSize=24,
        textColor=colors.HexColor('#0047BB'),
        spaceAfter=30,
        alignment=TA_CENTER,
        fontName='Helvetica-Bold'
    )

    # Estilo para subtítulo
    subtitle_style = ParagraphStyle(
        'CustomSubtitle',
        parent=styles['Normal'],
        fontSize=16,
        textColor=colors.HexColor('#0047BB'),
        spaceAfter=40,
        alignment=TA_CENTER
    )

    # Estilo para seções
    section_style = ParagraphStyle(
        'SectionHeader',
        parent=styles['Heading1'],
        fontSize=18,
        textColor=colors.HexColor('#0047BB'),
        spaceAfter=12,
        fontName='Helvetica-Bold'
    )

    # Estilo para subseções
    subsection_style = ParagraphStyle(
        'SubsectionHeader',
        parent=styles['Heading2'],
        fontSize=14,
        textColor=colors.HexColor('#0047BB'),
        spaceAfter=10,
        fontName='Helvetica-Bold'
    )

    # Estilo para corpo
    body_style = ParagraphStyle(
        'BodyText',
        parent=styles['Normal'],
        fontSize=11,
        alignment=TA_JUSTIFY,
        spaceAfter=12
    )

    # Construir conteúdo
    story = []

    # PÁGINA DE ROSTO
    story.append(Spacer(1, 3*cm))
    story.append(Paragraph("Análise de Migração COBOL para .NET", title_style))
    story.append(Paragraph("Sistema SUSEP Circular 360 - Apuração de Prêmios", subtitle_style))
    story.append(Spacer(1, 2*cm))
    story.append(Paragraph("<b>Caixa Seguradora</b>", subtitle_style))
    story.append(Spacer(1, 1*cm))
    story.append(Paragraph(f"Versão 1.0.0<br/>{datetime.now().strftime('%d/%m/%Y')}", body_style))
    story.append(Spacer(1, 3*cm))
    story.append(Paragraph("<font color='red'><b>CONFIDENCIAL</b></font>", subtitle_style))
    story.append(PageBreak())

    # RESUMO EXECUTIVO
    story.append(Paragraph("1. Resumo Executivo", section_style))

    story.append(Paragraph("1.1 Contexto do Projeto", subsection_style))
    story.append(Paragraph(
        "Este documento apresenta a análise completa da migração do sistema COBOL RG1866B "
        "(SUSEP Circular 360 - Sistema de Apuração de Prêmios) para uma arquitetura moderna "
        "baseada em .NET 9 e React 18.",
        body_style
    ))

    story.append(Paragraph(
        "O sistema legado RG1866B é responsável por:",
        body_style
    ))

    bullet_list = """
    • Processamento de 687 itens de dados de prêmios de seguros<br/>
    • Consulta a 26+ tabelas/views DB2<br/>
    • Geração de arquivos regulatórios PREMIT.TXT e PREMCED.TXT<br/>
    • Cálculos complexos de cosseguro conforme regulamentação SUSEP
    """
    story.append(Paragraph(bullet_list, body_style))

    story.append(Paragraph(
        "O programa possui aproximadamente 5.000 linhas de código COBOL e processa "
        "transações críticas para conformidade regulatória.",
        body_style
    ))

    story.append(Paragraph("1.2 Objetivos da Migração", subsection_style))

    objectives = """
    1. <b>Modernização Tecnológica</b>: Substituir COBOL por stack moderno (.NET 9 + React 18)<br/>
    2. <b>Conformidade Byte-for-Byte</b>: Manter compatibilidade exata com saída COBOL<br/>
    3. <b>Arquitetura Limpa</b>: Implementar Clean Architecture com 3 camadas<br/>
    4. <b>Interface Web</b>: Criar dashboard React para consultas e relatórios<br/>
    5. <b>Qualidade</b>: Atingir 90%+ cobertura de testes
    """
    story.append(Paragraph(objectives, body_style))

    story.append(Paragraph("1.3 Investimento", subsection_style))

    # Tabela de investimento
    investment_data = [
        ['Item', 'Valor (BRL)'],
        ['Desenvolvimento (313 FP × R$ 750)', 'R$ 234.750,00'],
        ['Infraestrutura Azure (12 meses)', 'R$ 18.000,00'],
        ['Licenças e ferramentas', 'R$ 15.000,00'],
        ['Contingência (15%)', 'R$ 40.162,50'],
        ['TOTAL', 'R$ 307.912,50']
    ]

    investment_table = Table(investment_data, colWidths=[10*cm, 4*cm])
    investment_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#0047BB')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('ALIGN', (1, 0), (1, -1), 'RIGHT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 12),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -2), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTNAME', (0, -1), (-1, -1), 'Helvetica-Bold'),
        ('BACKGROUND', (0, -1), (-1, -1), colors.HexColor('#FFB81C')),
    ]))

    story.append(investment_table)
    story.append(PageBreak())

    # ANÁLISE DO SISTEMA COBOL
    story.append(Paragraph("2. Análise do Sistema COBOL Legado", section_style))

    story.append(Paragraph("2.1 Visão Geral do Programa RG1866B", subsection_style))
    story.append(Paragraph(
        "O programa RG1866B foi desenvolvido em COBOL e executa processamento batch "
        "para apuração de prêmios conforme SUSEP Circular 360.",
        body_style
    ))

    metrics = """
    <b>Métricas Principais:</b><br/>
    • Linhas de código: ~5.000 LOC<br/>
    • Itens de dados: 687 data items<br/>
    • Tabelas acessadas: 26+ views/tables<br/>
    • Arquivos de saída: 2 (PREMIT.TXT, PREMCED.TXT)<br/>
    • Complexidade ciclomática: Alta (cálculos de cosseguro)
    """
    story.append(Paragraph(metrics, body_style))

    story.append(Paragraph("2.2 Principais Componentes", subsection_style))

    components = """
    <b>Seção de Inicialização (R0100-R0400):</b><br/>
    • Abertura de arquivos<br/>
    • Inicialização de variáveis<br/>
    • Validação de parâmetros de execução<br/>
    • Setup de cursores DB2<br/><br/>

    <b>Processamento Principal (R0500-R0600):</b><br/>
    • Lê registros da view V0PREMIOS<br/>
    • Aplica regras de negócio<br/>
    • Calcula prêmios e comissões<br/>
    • Acumula totalizadores<br/><br/>

    <b>Cálculos de Cosseguro (R3000-R5500):</b><br/>
    • Distribuição proporcional de prêmios entre cosseguradoras<br/>
    • Cálculo de participações conforme tabela GE399<br/>
    • Validação de limites regulatórios SUSEP<br/>
    • Geração de totalizadores por cosseguradora
    """
    story.append(Paragraph(components, body_style))
    story.append(PageBreak())

    # ARQUITETURA
    story.append(Paragraph("3. Arquitetura de Migração", section_style))

    story.append(Paragraph("3.1 Clean Architecture - 3 Camadas", subsection_style))
    story.append(Paragraph(
        "A arquitetura target segue os princípios de Clean Architecture com "
        "separação clara de responsabilidades:",
        body_style
    ))

    arch_layers = """
    <b>Camada 1: CaixaSeguradora.Api (Presentation)</b><br/>
    • ASP.NET Core 9 Web API<br/>
    • Controllers para endpoints REST e SOAP<br/>
    • Validação de requisições<br/>
    • Swagger/OpenAPI documentation<br/><br/>

    <b>Camada 2: CaixaSeguradora.Core (Domain)</b><br/>
    • Entidades de domínio (Premium, Policy, Cossurance)<br/>
    • Interfaces de repositório e serviço<br/>
    • Regras de negócio (cálculos financeiros)<br/>
    • Zero dependências externas<br/><br/>

    <b>Camada 3: CaixaSeguradora.Infrastructure (Data Access)</b><br/>
    • Entity Framework Core 9<br/>
    • Repositórios concretos<br/>
    • Mapeamento para 26 views DB2<br/>
    • FixedWidthFormatter para arquivos COBOL
    """
    story.append(Paragraph(arch_layers, body_style))

    story.append(Paragraph("3.2 Stack Tecnológico", subsection_style))

    tech_data = [
        ['Componente', 'Tecnologia'],
        ['Backend Framework', '.NET 9.0'],
        ['Linguagem', 'C# 13.0'],
        ['ORM', 'Entity Framework Core 9.0'],
        ['API', 'ASP.NET Core Web API'],
        ['Database', 'SQLite (dev), DB2 (prod)'],
        ['Frontend', 'React 18.3 + TypeScript 5.5'],
        ['Build Tool', 'Vite 5.3'],
        ['Styling', 'TailwindCSS 3.4'],
        ['Testing Backend', 'xUnit 2.9 + FluentAssertions'],
        ['Testing Frontend', 'Vitest 2.0 + Playwright']
    ]

    tech_table = Table(tech_data, colWidths=[6*cm, 8*cm])
    tech_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#0047BB')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 11),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
    ]))

    story.append(tech_table)
    story.append(PageBreak())

    # PONTOS DE FUNÇÃO
    story.append(Paragraph("4. Análise de Pontos de Função", section_style))

    story.append(Paragraph("4.1 Metodologia IFPUG 4.3.1", subsection_style))
    story.append(Paragraph(
        "A análise de pontos de função foi realizada seguindo IFPUG "
        "(International Function Point Users Group) versão 4.3.1.",
        body_style
    ))

    story.append(Paragraph("4.2 Breakdown por Tipo de Função", subsection_style))

    fp_data = [
        ['Tipo', 'Nome', 'Complexidade', 'Peso', 'Total'],
        ['EO', 'Gerar PREMIT.TXT', 'Alta', '7', '7'],
        ['EO', 'Gerar PREMCED.TXT', 'Alta', '7', '7'],
        ['EO', 'API Relatórios', 'Média', '5', '30'],
        ['EI', 'Consulta Prêmios', 'Média', '4', '8'],
        ['EI', 'Filtros Dashboard', 'Baixa', '3', '12'],
        ['EQ', 'Query Builder', 'Alta', '6', '12'],
        ['ILF', 'V0PREMIOS', 'Alta', '15', '15'],
        ['ILF', 'V0APOLICE', 'Alta', '15', '15'],
        ['ILF', 'GE399 (Cosseguro)', 'Média', '10', '10'],
        ['EIF', 'Demais 23 tabelas', 'Média', '7', '161'],
        ['', '', '', 'UFP', '277'],
        ['', '', '', 'VAF', '1.13'],
        ['', '', '', 'AFP', '313']
    ]

    fp_table = Table(fp_data, colWidths=[2*cm, 5*cm, 3*cm, 2*cm, 2*cm])
    fp_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#0047BB')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -4), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTNAME', (0, -3), (-1, -1), 'Helvetica-Bold'),
        ('BACKGROUND', (0, -3), (-1, -1), colors.HexColor('#FFB81C')),
    ]))

    story.append(fp_table)
    story.append(Spacer(1, 1*cm))

    story.append(Paragraph(
        "<b>Observação</b>: O VAF de 1.13 reflete alta complexidade de processamento "
        "(cálculos cosseguro), performance crítica (&lt; 2s resposta), conformidade "
        "byte-for-byte com COBOL, e reusabilidade de componentes (90%+).",
        body_style
    ))
    story.append(PageBreak())

    # CRONOGRAMA
    story.append(Paragraph("5. Cronograma do Projeto", section_style))

    story.append(Paragraph("5.1 Visão Geral - 14 Semanas", subsection_style))

    timeline_data = [
        ['Fase', 'Atividades', 'Duração'],
        ['Fase 0', 'Análise e planejamento', '1 semana'],
        ['Fase 1', 'Setup projeto, CI/CD, DB', '2 semanas'],
        ['Fase 2', 'Modelos domínio, testes base', '2 semanas'],
        ['Fase 3', 'Lógica negócio, cálculos', '2 semanas'],
        ['Fase 4', 'API REST/SOAP, integração', '1 semana'],
        ['Fase 5', 'Frontend React, dashboard', '2 semanas'],
        ['Fase 6', 'Homologação, documentação', '4 semanas'],
        ['TOTAL', '', '14 semanas']
    ]

    timeline_table = Table(timeline_data, colWidths=[3*cm, 7*cm, 3*cm])
    timeline_table.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#0047BB')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('ALIGN', (2, 0), (2, -1), 'CENTER'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 11),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -2), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTNAME', (0, -1), (-1, -1), 'Helvetica-Bold'),
        ('BACKGROUND', (0, -1), (-1, -1), colors.HexColor('#FFB81C')),
    ]))

    story.append(timeline_table)
    story.append(Spacer(1, 1*cm))

    story.append(Paragraph("5.2 Milestones Principais", subsection_style))

    milestones = """
    1. <b>M1 (Semana 1)</b>: Aprovação do projeto e orçamento<br/>
    2. <b>M2 (Semana 3)</b>: Ambiente de desenvolvimento configurado<br/>
    3. <b>M3 (Semana 5)</b>: Testes de comparação COBOL passando<br/>
    4. <b>M4 (Semana 7)</b>: API backend completa<br/>
    5. <b>M5 (Semana 9)</b>: Dashboard frontend funcional<br/>
    6. <b>M6 (Semana 10)</b>: Início da homologação<br/>
    7. <b>M7 (Semana 13)</b>: Aprovação SUSEP<br/>
    8. <b>M8 (Semana 14)</b>: Go-live em produção
    """
    story.append(Paragraph(milestones, body_style))
    story.append(PageBreak())

    # APÊNDICES
    story.append(Paragraph("6. Apêndices", section_style))

    story.append(Paragraph("6.1 Glossário", subsection_style))

    glossary = """
    <b>AFP</b> - Adjusted Function Points (Pontos de função ajustados)<br/>
    <b>COBOL</b> - Common Business-Oriented Language<br/>
    <b>DB2</b> - Sistema de gerenciamento de banco de dados IBM<br/>
    <b>EF Core</b> - Entity Framework Core (ORM da Microsoft)<br/>
    <b>IFPUG</b> - International Function Point Users Group<br/>
    <b>PREMIT</b> - Arquivo de premissões conforme SUSEP<br/>
    <b>PREMCED</b> - Arquivo de prêmios cedidos (cosseguro)<br/>
    <b>SUSEP</b> - Superintendência de Seguros Privados<br/>
    <b>UFP</b> - Unadjusted Function Points<br/>
    <b>VAF</b> - Value Adjustment Factor
    """
    story.append(Paragraph(glossary, body_style))

    story.append(Paragraph("6.2 Referências", subsection_style))

    references = """
    1. SUSEP Circular SUSEP 360/2007 - Normas para apuração de prêmios<br/>
    2. IFPUG Function Point Counting Practices Manual, Release 4.3.1<br/>
    3. Microsoft .NET 9 Documentation<br/>
    4. Clean Architecture - Robert C. Martin<br/>
    5. Domain-Driven Design - Eric Evans
    """
    story.append(Paragraph(references, body_style))

    story.append(Spacer(1, 3*cm))
    story.append(Paragraph("<font color='red'><b>DOCUMENTO CONFIDENCIAL</b></font>", subtitle_style))
    story.append(Paragraph("Caixa Seguradora S.A. - Todos os direitos reservados", body_style))

    # Gerar PDF
    doc.build(story)

    return output_path

if __name__ == '__main__':
    print("\n" + "="*60)
    print("  Gerador de PDF - Análise de Migração COBOL (Python puro)")
    print("="*60 + "\n")

    try:
        # Verificar se ReportLab está instalado
        print("📦 Instalando ReportLab (se necessário)...")
        import subprocess
        subprocess.run(['pip3', 'install', 'reportlab'], capture_output=True)
        print("✅ ReportLab instalado\n")

        print("📝 Gerando PDF...")
        output_path = create_pdf()

        # Obter tamanho do arquivo
        size_mb = output_path.stat().st_size / (1024 * 1024)

        print(f"\n✅ PDF gerado com sucesso!")
        print(f"   Arquivo: {output_path}")
        print(f"   Tamanho: {size_mb:.2f} MB")
        print(f"\n🎉 Processo concluído!\n")
        print(f"Para visualizar: open {output_path}\n")

    except Exception as e:
        print(f"\n❌ Erro ao gerar PDF: {e}")
        import traceback
        traceback.print_exc()
