#!/usr/bin/env python3
"""
Section 01: Cover Page and Table of Contents
For COBOL RG1866B Migration Analysis PDF

This script generates the cover page and table of contents for the migration
analysis document following Caixa Seguradora branding standards.

Usage:
    python section_01_cover.py <config_yaml_path> <output_pdf_path>

Requirements:
    - ReportLab library
    - PyYAML library
    - document-config.yaml configuration file
"""

import sys
import os
from datetime import datetime
from pathlib import Path

try:
    import yaml
    from reportlab.lib import colors
    from reportlab.lib.pagesizes import A4
    from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
    from reportlab.lib.units import cm
    from reportlab.platypus import (
        SimpleDocTemplate, Paragraph, Spacer, PageBreak,
        Table, TableStyle, Image
    )
    from reportlab.lib.enums import TA_CENTER, TA_LEFT, TA_JUSTIFY
    from reportlab.pdfgen import canvas
except ImportError as e:
    print(f"Error: Missing required library - {e}")
    print("Install with: pip install reportlab pyyaml")
    sys.exit(1)


# Caixa Seguradora Brand Colors
CAIXA_BLUE = colors.HexColor("#0047BB")
CAIXA_YELLOW = colors.HexColor("#FFB81C")
DARK_GRAY = colors.HexColor("#333333")
LIGHT_GRAY = colors.HexColor("#F5F5F5")
MEDIUM_GRAY = colors.HexColor("#999999")


class CoverPageGenerator:
    """Generates professional cover page following Caixa Seguradora branding"""

    def __init__(self, config_data):
        """
        Initialize cover page generator

        Args:
            config_data: Dictionary containing document configuration
        """
        self.config = config_data
        self.doc_info = config_data.get('document', {})
        self.branding = config_data.get('branding', {})

    def create_cover_elements(self):
        """
        Create all elements for the cover page

        Returns:
            List of ReportLab flowable elements
        """
        elements = []
        styles = getSampleStyleSheet()

        # Custom styles for cover page
        title_style = ParagraphStyle(
            'CoverTitle',
            parent=styles['Heading1'],
            fontSize=28,
            textColor=CAIXA_BLUE,
            spaceAfter=12,
            alignment=TA_CENTER,
            fontName='Helvetica-Bold',
            leading=34
        )

        subtitle_style = ParagraphStyle(
            'CoverSubtitle',
            parent=styles['Heading2'],
            fontSize=18,
            textColor=DARK_GRAY,
            spaceAfter=30,
            alignment=TA_CENTER,
            fontName='Helvetica',
            leading=22
        )

        program_style = ParagraphStyle(
            'ProgramName',
            parent=styles['Normal'],
            fontSize=16,
            textColor=CAIXA_BLUE,
            spaceAfter=8,
            alignment=TA_CENTER,
            fontName='Helvetica-Bold'
        )

        meta_style = ParagraphStyle(
            'MetaInfo',
            parent=styles['Normal'],
            fontSize=12,
            textColor=DARK_GRAY,
            spaceAfter=6,
            alignment=TA_CENTER,
            fontName='Helvetica'
        )

        confidential_style = ParagraphStyle(
            'Confidential',
            parent=styles['Normal'],
            fontSize=14,
            textColor=colors.red,
            spaceAfter=20,
            alignment=TA_CENTER,
            fontName='Helvetica-Bold'
        )

        # Add top spacing
        elements.append(Spacer(1, 3*cm))

        # Company name
        company_name = Paragraph(
            f"<b>{self.doc_info.get('company', 'Caixa Seguradora')}</b>",
            subtitle_style
        )
        elements.append(company_name)
        elements.append(Spacer(1, 1*cm))

        # Main title
        title = Paragraph(
            self.doc_info.get('title', 'Análise de Migração COBOL para .NET'),
            title_style
        )
        elements.append(title)
        elements.append(Spacer(1, 0.5*cm))

        # Subtitle
        subtitle = Paragraph(
            self.doc_info.get('subtitle', 'Sistema SUSEP Circular 360'),
            subtitle_style
        )
        elements.append(subtitle)
        elements.append(Spacer(1, 1.5*cm))

        # Program name in box
        program_box = Table(
            [[Paragraph('<b>Programa COBOL</b>', program_style)],
             [Paragraph('<b>RG1866B</b>', program_style)]],
            colWidths=[8*cm]
        )
        program_box.setStyle(TableStyle([
            ('BACKGROUND', (0, 0), (-1, -1), CAIXA_YELLOW),
            ('BOX', (0, 0), (-1, -1), 2, CAIXA_BLUE),
            ('VALIGN', (0, 0), (-1, -1), 'MIDDLE'),
            ('TOPPADDING', (0, 0), (-1, -1), 12),
            ('BOTTOMPADDING', (0, 0), (-1, -1), 12),
        ]))
        elements.append(program_box)
        elements.append(Spacer(1, 2*cm))

        # Document metadata
        version = self.doc_info.get('version', '1.0.0')
        author = self.doc_info.get('author', 'Equipe de Migração')
        doc_date = self.doc_info.get('date', datetime.now().strftime('%Y-%m-%d'))

        elements.append(Paragraph(f"<b>Versão:</b> {version}", meta_style))
        elements.append(Paragraph(f"<b>Data:</b> {doc_date}", meta_style))
        elements.append(Paragraph(f"<b>Elaborado por:</b> {author}", meta_style))

        elements.append(Spacer(1, 2*cm))

        # Confidential marking
        classification = self.doc_info.get('classification', 'CONFIDENCIAL')
        confidential = Paragraph(f"<b>{classification}</b>", confidential_style)
        elements.append(confidential)

        # Add decorative line
        line_table = Table([['']], colWidths=[15*cm])
        line_table.setStyle(TableStyle([
            ('LINEABOVE', (0, 0), (-1, -1), 3, CAIXA_BLUE),
        ]))
        elements.append(line_table)

        return elements


class TableOfContentsGenerator:
    """Generates table of contents with section navigation"""

    def __init__(self, config_data):
        """
        Initialize TOC generator

        Args:
            config_data: Dictionary containing document configuration
        """
        self.config = config_data

    def create_toc_elements(self):
        """
        Create table of contents elements

        Returns:
            List of ReportLab flowable elements
        """
        elements = []
        styles = getSampleStyleSheet()

        # Custom TOC styles
        toc_title_style = ParagraphStyle(
            'TOCTitle',
            parent=styles['Heading1'],
            fontSize=24,
            textColor=CAIXA_BLUE,
            spaceAfter=20,
            alignment=TA_CENTER,
            fontName='Helvetica-Bold'
        )

        toc_entry_style = ParagraphStyle(
            'TOCEntry',
            parent=styles['Normal'],
            fontSize=12,
            textColor=DARK_GRAY,
            spaceAfter=8,
            alignment=TA_LEFT,
            fontName='Helvetica',
            leftIndent=0
        )

        toc_subsection_style = ParagraphStyle(
            'TOCSubsection',
            parent=styles['Normal'],
            fontSize=10,
            textColor=MEDIUM_GRAY,
            spaceAfter=6,
            alignment=TA_LEFT,
            fontName='Helvetica',
            leftIndent=20
        )

        # Title
        elements.append(Spacer(1, 1*cm))
        title = Paragraph("<b>Sumário</b>", toc_title_style)
        elements.append(title)
        elements.append(Spacer(1, 1*cm))

        # TOC entries - comprehensive section list
        toc_sections = [
            {
                'number': '1',
                'title': 'Resumo Executivo',
                'subsections': [
                    'Visão Geral do Projeto',
                    'Principais Métricas',
                    'Recomendações'
                ]
            },
            {
                'number': '2',
                'title': 'Análise do Sistema COBOL Legado',
                'subsections': [
                    'Análise Estrutural do Programa RG1866B',
                    'Fluxo de Processamento',
                    'Complexidade Ciclomática'
                ]
            },
            {
                'number': '3',
                'title': 'Estruturas de Dados Completas',
                'subsections': [
                    'Áreas de Working Storage',
                    'File Descriptions',
                    'Copybooks Utilizados'
                ]
            },
            {
                'number': '4',
                'title': 'Tabelas de Banco de Dados',
                'subsections': [
                    'Views DB2 (26 tabelas)',
                    'Relacionamentos',
                    'Modelo de Dados .NET'
                ]
            },
            {
                'number': '5',
                'title': 'Regras de Negócio',
                'subsections': [
                    'Cálculos de Prêmios',
                    'Processamento de Cosseguro',
                    'Validações e Controles'
                ]
            },
            {
                'number': '6',
                'title': 'Arquitetura de Migração',
                'subsections': [
                    'Arquitetura Clean Architecture',
                    'Componentes da Solução .NET',
                    'Diagrama de Componentes'
                ]
            },
            {
                'number': '7',
                'title': 'Análise de Pontos de Função',
                'subsections': [
                    'Metodologia IFPUG 4.3.1',
                    'Detalhamento por Componente',
                    'Fatores de Ajuste'
                ]
            },
            {
                'number': '8',
                'title': 'Cronograma do Projeto',
                'subsections': [
                    'Fases de Implementação',
                    'Marcos e Entregas',
                    'Diagrama de Gantt'
                ]
            },
            {
                'number': '9',
                'title': 'Orçamento e Investimento',
                'subsections': [
                    'Custos de Desenvolvimento',
                    'Custos de Infraestrutura',
                    'Distribuição de Recursos'
                ]
            },
            {
                'number': '10',
                'title': 'Análise de ROI',
                'subsections': [
                    'Benefícios Esperados',
                    'Redução de Custos',
                    'Payback Period'
                ]
            },
            {
                'number': '11',
                'title': 'Apêndices',
                'subsections': [
                    'A - Código COBOL Completo',
                    'B - Especificações Técnicas',
                    'C - Contratos OpenAPI'
                ]
            }
        ]

        # Create TOC table
        toc_data = []

        for section in toc_sections:
            # Main section
            section_text = f"<b>{section['number']}. {section['title']}</b>"
            toc_data.append([
                Paragraph(section_text, toc_entry_style),
                Paragraph(f"<b>{section['number']}</b>", toc_entry_style)
            ])

            # Subsections
            for subsection in section.get('subsections', []):
                sub_text = f"• {subsection}"
                toc_data.append([
                    Paragraph(sub_text, toc_subsection_style),
                    Paragraph('', toc_subsection_style)
                ])

        # Create table
        toc_table = Table(toc_data, colWidths=[14*cm, 2*cm])
        toc_table.setStyle(TableStyle([
            ('VALIGN', (0, 0), (-1, -1), 'TOP'),
            ('ALIGN', (1, 0), (1, -1), 'RIGHT'),
            ('TOPPADDING', (0, 0), (-1, -1), 4),
            ('BOTTOMPADDING', (0, 0), (-1, -1), 4),
            ('TEXTCOLOR', (0, 0), (-1, -1), DARK_GRAY),
        ]))

        elements.append(toc_table)

        return elements


def load_config(config_path):
    """
    Load YAML configuration file

    Args:
        config_path: Path to document-config.yaml

    Returns:
        Dictionary with configuration data
    """
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            return yaml.safe_load(f)
    except FileNotFoundError:
        print(f"Error: Configuration file not found: {config_path}")
        sys.exit(1)
    except yaml.YAMLError as e:
        print(f"Error parsing YAML configuration: {e}")
        sys.exit(1)


def create_footer_canvas(canvas_obj, doc):
    """
    Add footer with page numbers and document info

    Args:
        canvas_obj: ReportLab canvas object
        doc: Document object
    """
    canvas_obj.saveState()

    # Footer line
    canvas_obj.setStrokeColor(CAIXA_BLUE)
    canvas_obj.setLineWidth(1)
    canvas_obj.line(2.5*cm, 2*cm, A4[0] - 2.5*cm, 2*cm)

    # Page number
    canvas_obj.setFont('Helvetica', 9)
    canvas_obj.setFillColor(MEDIUM_GRAY)
    page_num = canvas_obj.getPageNumber()
    text = f"Página {page_num}"
    canvas_obj.drawRightString(A4[0] - 2.5*cm, 1.5*cm, text)

    # Document classification
    canvas_obj.setFillColor(colors.red)
    canvas_obj.setFont('Helvetica-Bold', 8)
    canvas_obj.drawString(2.5*cm, 1.5*cm, "CONFIDENCIAL")

    canvas_obj.restoreState()


def generate_section_01(config_path, output_path):
    """
    Main function to generate Section 01 PDF

    Args:
        config_path: Path to document-config.yaml
        output_path: Path for output PDF file
    """
    print("=" * 60)
    print("SECTION 01: Cover Page and Table of Contents Generator")
    print("=" * 60)

    # Load configuration
    print(f"\n1. Loading configuration from: {config_path}")
    config = load_config(config_path)

    # Ensure output directory exists
    output_dir = Path(output_path).parent
    output_dir.mkdir(parents=True, exist_ok=True)

    # Create PDF document
    print(f"\n2. Creating PDF document: {output_path}")
    doc = SimpleDocTemplate(
        output_path,
        pagesize=A4,
        rightMargin=2.5*cm,
        leftMargin=2.5*cm,
        topMargin=2.5*cm,
        bottomMargin=2.5*cm,
        title=config['document']['title'],
        author=config['document']['author']
    )

    # Generate content elements
    print("\n3. Generating cover page...")
    cover_gen = CoverPageGenerator(config)
    elements = cover_gen.create_cover_elements()

    # Add page break
    elements.append(PageBreak())

    # Generate table of contents
    print("4. Generating table of contents...")
    toc_gen = TableOfContentsGenerator(config)
    elements.extend(toc_gen.create_toc_elements())

    # Build PDF with custom footer
    print("\n5. Building PDF with styling...")
    doc.build(elements, onFirstPage=create_footer_canvas,
              onLaterPages=create_footer_canvas)

    # Verify output
    if os.path.exists(output_path):
        file_size = os.path.getsize(output_path)
        print(f"\n✓ PDF generated successfully!")
        print(f"  Location: {output_path}")
        print(f"  Size: {file_size:,} bytes ({file_size/1024:.1f} KB)")
    else:
        print("\n✗ Error: PDF file was not created")
        sys.exit(1)

    print("\n" + "=" * 60)
    print("Section 01 generation complete!")
    print("=" * 60)


def main():
    """Main entry point"""
    if len(sys.argv) != 3:
        print("Usage: python section_01_cover.py <config_yaml_path> <output_pdf_path>")
        print("\nExample:")
        print("  python section_01_cover.py config/document-config.yaml output/section_01.pdf")
        sys.exit(1)

    config_path = sys.argv[1]
    output_path = sys.argv[2]

    # Validate input
    if not os.path.exists(config_path):
        print(f"Error: Configuration file not found: {config_path}")
        sys.exit(1)

    # Generate section
    generate_section_01(config_path, output_path)


if __name__ == "__main__":
    main()
