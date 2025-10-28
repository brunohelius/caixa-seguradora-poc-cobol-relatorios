#!/usr/bin/env python3
"""
Seção 02: Resumo Executivo
Visão geral do projeto de migração para stakeholders
"""

import sys
import yaml
from pathlib import Path
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.lib import colors
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.enums import TA_LEFT, TA_CENTER

CAIXA_BLUE = colors.HexColor('#0047BB')
CAIXA_YELLOW = colors.HexColor('#FFB81C')


def create_styles():
    styles = getSampleStyleSheet()

    styles.add(ParagraphStyle(
        name='SectionTitle',
        parent=styles['Heading1'],
        fontSize=18,
        textColor=CAIXA_BLUE,
        spaceAfter=15,
        fontName='Helvetica-Bold'
    ))

    styles.add(ParagraphStyle(
        name='Subsection',
        parent=styles['Heading2'],
        fontSize=14,
        textColor=CAIXA_BLUE,
        spaceAfter=10,
        fontName='Helvetica-Bold'
    ))

    styles.add(ParagraphStyle(
        name='Body',
        parent=styles['Normal'],
        fontSize=11,
        spaceAfter=12,
        alignment=TA_LEFT
    ))

    styles.add(ParagraphStyle(
        name='Bullet',
        parent=styles['Normal'],
        fontSize=11,
        leftIndent=20,
        spaceAfter=8,
        bulletIndent=10
    ))

    return styles


def table_style():
    return TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), CAIXA_BLUE),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.white),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 11),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.white),
        ('GRID', (0, 0), (-1, -1), 0.5, colors.grey),
        ('FONTNAME', (0, 1), (-1, -1), 'Helvetica'),
        ('FONTSIZE', (0, 1), (-1, -1), 10),
        ('TOPPADDING', (0, 1), (-1, -1), 8),
        ('BOTTOMPADDING', (0, 1), (-1, -1), 8),
    ])


def generate_content(story, styles, config):
    # Título da seção
    story.append(Paragraph("1. Resumo Executivo", styles['SectionTitle']))
    story.append(Spacer(1, 0.3*cm))

    # 1.1 Contexto
    story.append(Paragraph("1.1. Contexto do Projeto", styles['Subsection']))

    context = """O sistema <b>RG1866B</b> é um programa COBOL batch crítico responsável pelo cálculo e
apuração de prêmios de seguros conforme <b>SUSEP Circular 360</b>. Desenvolvido originalmente para mainframe IBM,
o sistema processa mensalmente milhares de registros de prêmios emitidos e cedidos, gerando relatórios regulatórios
obrigatórios (PREMIT.TXT e PREMCED.TXT)."""

    story.append(Paragraph(context, styles['Body']))

    metrics = """O programa possui aproximadamente <b>5.000 linhas de código COBOL</b>, contendo <b>687 data items</b>
na Working-Storage Section e integrando com <b>26+ tabelas e views DB2</b>. A complexidade reside não apenas
no volume de código, mas na lógica de negócio intrincada distribuída em <b>63 seções PROCEDURE DIVISION</b>
(R0100 a R5500), incluindo cálculos de cosseguro, validações regulatórias e processamento batch com múltiplos cursores."""

    story.append(Paragraph(metrics, styles['Body']))
    story.append(Spacer(1, 0.4*cm))

    # 1.2 Objetivos
    story.append(Paragraph("1.2. Objetivos da Migração", styles['Subsection']))

    objectives = [
        "<b>Modernização Tecnológica</b>: Migrar de mainframe COBOL/DB2 para stack moderna .NET 9/React 18",
        "<b>Conformidade Regulatória</b>: Manter 100% de compatibilidade byte-for-byte com saída COBOL",
        "<b>Redução de Custos</b>: Eliminar licenças mainframe e reduzir custos operacionais em 60%",
        "<b>Manutenibilidade</b>: Código modular em C# com Clean Architecture, reduzindo tempo de manutenção",
        "<b>Novos Recursos</b>: Habilitar APIs RESTful, dashboards web e consultas interativas",
        "<b>Escalabilidade</b>: Arquitetura cloud-ready preparada para crescimento futuro"
    ]

    for obj in objectives:
        story.append(Paragraph(f"• {obj}", styles['Bullet']))

    story.append(Spacer(1, 0.4*cm))

    # 1.3 Abordagem Técnica
    story.append(Paragraph("1.3. Abordagem Técnica", styles['Subsection']))

    approach_data = [
        ['Aspecto', 'Decisão Técnica', 'Justificativa'],
        ['Metodologia', 'MIGRAI (Migration Guide for AI)', 'Abordagem estruturada com IA para migração COBOL'],
        ['Arquitetura', 'Clean Architecture 3 Camadas', 'Separação clara: API, Core (Domain), Infrastructure'],
        ['Backend', '.NET 9 + EF Core 9', 'Performance, type safety, ecossistema maduro'],
        ['Frontend', 'React 18 + TailwindCSS', 'SPA moderna com componentes reutilizáveis'],
        ['Banco de Dados', 'SQLite (dev) / PostgreSQL (prod)', 'EF Core migrations, portabilidade'],
        ['Validação', 'Comparison Tests', 'Comparação byte-for-byte .NET vs COBOL']
    ]

    approach_table = Table(approach_data, colWidths=[4*cm, 5*cm, 6*cm])
    approach_table.setStyle(table_style())
    story.append(approach_table)
    story.append(Spacer(1, 0.4*cm))

    # 1.4 Timeline e Investimento
    story.append(Paragraph("1.4. Timeline e Investimento", styles['Subsection']))

    investment_data = [
        ['Métrica', 'Valor'],
        ['Duração Total', '14 semanas'],
        ['Desenvolvimento', '8 semanas'],
        ['Homologação', '4 semanas'],
        ['Pontos de Função (AFP)', '313 pontos'],
        ['Custo por FP', 'R$ 750,00'],
        ['Desenvolvimento', 'R$ 234.750,00'],
        ['Contingência (15%)', 'R$ 35.212,50'],
        ['Infraestrutura', 'R$ 53.762,50'],
        ['<b>Investimento Total</b>', '<b>R$ 323.725,00</b>'],
        ['<b>Payback Estimado</b>', '<b>4 anos</b>']
    ]

    investment_table = Table(investment_data, colWidths=[7*cm, 5*cm])
    investment_table.setStyle(table_style())
    story.append(investment_table)
    story.append(Spacer(1, 0.3*cm))

    summary_text = """<b>Resumo</b>: A migração do RG1866B representa uma oportunidade estratégica de modernização
com investimento de <b>R$ 323.725,00</b> e retorno em <b>4 anos</b>. A abordagem técnica rigorosa garante
conformidade regulatória mantendo compatibilidade byte-for-byte, enquanto a nova arquitetura habilita
funcionalidades modernas (APIs, dashboards) impossíveis no mainframe."""

    story.append(Paragraph(summary_text, styles['Body']))


def main():
    if len(sys.argv) < 3:
        print("Uso: python section_02_executive_summary.py <config.yaml> <output.pdf>")
        sys.exit(1)

    config_path = sys.argv[1]
    output_path = sys.argv[2]

    with open(config_path, 'r', encoding='utf-8') as f:
        config = yaml.safe_load(f)

    doc = SimpleDocTemplate(
        output_path,
        pagesize=A4,
        leftMargin=2.5*cm,
        rightMargin=2.5*cm,
        topMargin=2.5*cm,
        bottomMargin=2.5*cm
    )

    story = []
    styles = create_styles()
    generate_content(story, styles, config)
    doc.build(story)

    print(f"✅ Seção 02 (Resumo Executivo) gerada: {output_path}")


if __name__ == "__main__":
    main()
