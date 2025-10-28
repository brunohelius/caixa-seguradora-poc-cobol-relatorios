#!/usr/bin/env python3
"""
PDF Orchestrator - Coordena a geração modular de PDFs por seções
Cada seção é gerada por um script especializado, depois são mescladas
"""

import os
import sys
import subprocess
from pathlib import Path
from datetime import datetime
import yaml

# Para merge de PDFs
try:
    from PyPDF2 import PdfMerger
except ImportError:
    print("⚠️  PyPDF2 não instalado. Instalando...")
    subprocess.run([sys.executable, "-m", "pip", "install", "--break-system-packages", "PyPDF2"], check=True)
    from PyPDF2 import PdfMerger


class PDFOrchestrator:
    """Orquestrador de geração modular de PDFs"""

    def __init__(self, config_path: str):
        self.config_path = Path(config_path)
        self.base_dir = self.config_path.parent.parent
        self.scripts_dir = self.base_dir / "scripts" / "generate-pdf"
        self.sections_dir = self.scripts_dir / "sections"
        self.temp_dir = self.scripts_dir / "temp_pdfs"
        self.output_dir = self.base_dir / "output"

        # Criar diretórios se não existirem
        self.temp_dir.mkdir(parents=True, exist_ok=True)
        self.output_dir.mkdir(parents=True, exist_ok=True)

        # Carregar configuração
        with open(self.config_path, 'r', encoding='utf-8') as f:
            self.config = yaml.safe_load(f)

    def print_header(self, text: str):
        """Imprime cabeçalho formatado"""
        print("\n" + "="*70)
        print(f"  {text}")
        print("="*70 + "\n")

    def generate_section(self, section_name: str, script_name: str) -> Path:
        """Gera uma seção específica do PDF usando script dedicado"""
        print(f"📝 Gerando seção: {section_name}...")

        script_path = self.sections_dir / script_name
        output_path = self.temp_dir / f"{script_name.replace('.py', '')}.pdf"

        if not script_path.exists():
            print(f"⚠️  Script não encontrado: {script_path}")
            print(f"   Pulando seção: {section_name}")
            return None

        try:
            # Executar script da seção
            result = subprocess.run(
                [sys.executable, str(script_path), str(self.config_path), str(output_path)],
                capture_output=True,
                text=True,
                check=True
            )

            if output_path.exists():
                size_kb = output_path.stat().st_size / 1024
                print(f"✅ Seção gerada: {section_name} ({size_kb:.1f} KB)")
                return output_path
            else:
                print(f"❌ Erro: PDF não gerado para {section_name}")
                return None

        except subprocess.CalledProcessError as e:
            print(f"❌ Erro ao gerar {section_name}:")
            print(f"   {e.stderr}")
            return None

    def merge_pdfs(self, pdf_files: list, output_name: str) -> Path:
        """Mescla múltiplos PDFs em um único arquivo"""
        print(f"\n📚 Mesclando {len(pdf_files)} seções em PDF final...")

        merger = PdfMerger()

        for pdf_path in pdf_files:
            if pdf_path and pdf_path.exists():
                print(f"   + Adicionando: {pdf_path.name}")
                merger.append(str(pdf_path))

        output_path = self.output_dir / output_name
        merger.write(str(output_path))
        merger.close()

        size_mb = output_path.stat().st_size / (1024 * 1024)
        print(f"\n✅ PDF final gerado!")
        print(f"   Arquivo: {output_path}")
        print(f"   Tamanho: {size_mb:.2f} MB")

        return output_path

    def generate_ultra_complete_pdf(self):
        """Gera o PDF ULTRA COMPLETO com todas as seções detalhadas"""
        self.print_header("GERANDO PDF ULTRA COMPLETO - Análise de Migração COBOL")

        # Lista de seções na ordem correta
        sections = [
            ("Capa e Sumário", "section_01_cover.py"),
            ("Resumo Executivo", "section_02_executive_summary.py"),
            ("Análise COBOL Detalhada", "section_03_cobol_analysis.py"),
            ("Estruturas de Dados", "section_04_data_structures.py"),
            ("Tabelas de Banco de Dados", "section_05_database_tables.py"),
            ("Regras de Negócio", "section_06_business_rules.py"),
            ("Arquitetura Alvo", "section_07_architecture.py"),
            ("Pontos de Função", "section_08_function_points.py"),
            ("Cronograma", "section_09_timeline.py"),
            ("Orçamento e ROI", "section_10_budget.py"),
            ("Apêndices", "section_11_appendices.py"),
        ]

        # Gerar cada seção
        pdf_files = []
        for section_name, script_name in sections:
            pdf_path = self.generate_section(section_name, script_name)
            if pdf_path:
                pdf_files.append(pdf_path)

        if not pdf_files:
            print("\n❌ Nenhuma seção foi gerada. Abortando merge.")
            return None

        # Mesclar todos os PDFs
        version = self.config.get('document', {}).get('version', '1.0.0')
        output_name = f"migration-analysis-ULTRA-COMPLETE-v{version}.pdf"
        final_pdf = self.merge_pdfs(pdf_files, output_name)

        return final_pdf

    def generate_migration_plan_pdf(self):
        """Gera o PDF PLANO DE MIGRAÇÃO focado em FP, cronograma e custos"""
        self.print_header("GERANDO PDF PLANO DE MIGRAÇÃO - Cronograma, Custos e FP")

        sections = [
            ("Capa", "section_01_cover.py"),
            ("Pontos de Função", "section_08_function_points.py"),
            ("Cronograma", "section_09_timeline.py"),
            ("Orçamento e ROI", "section_10_budget.py"),
        ]

        pdf_files = []
        for section_name, script_name in sections:
            pdf_path = self.generate_section(section_name, script_name)
            if pdf_path:
                pdf_files.append(pdf_path)

        if not pdf_files:
            print("\n❌ Nenhuma seção foi gerada. Abortando merge.")
            return None

        version = self.config.get('document', {}).get('version', '1.0.0')
        output_name = f"migration-analysis-plan-COMPLETE-v{version}.pdf"
        final_pdf = self.merge_pdfs(pdf_files, output_name)

        return final_pdf

    def cleanup_temp_files(self):
        """Remove arquivos temporários após merge"""
        print("\n🧹 Limpando arquivos temporários...")
        for pdf_file in self.temp_dir.glob("*.pdf"):
            pdf_file.unlink()
        print("✅ Limpeza concluída")

    def run(self):
        """Executa orquestração completa"""
        self.print_header("PDF ORCHESTRATOR - Geração Modular de Documentação")

        print(f"📁 Diretório base: {self.base_dir}")
        print(f"📁 Scripts: {self.sections_dir}")
        print(f"📁 Temporários: {self.temp_dir}")
        print(f"📁 Output: {self.output_dir}")

        # Gerar PDF ULTRA COMPLETO
        ultra_pdf = self.generate_ultra_complete_pdf()

        # Gerar PDF PLANO DE MIGRAÇÃO
        plan_pdf = self.generate_migration_plan_pdf()

        # Limpar arquivos temporários
        self.cleanup_temp_files()

        # Resumo final
        self.print_header("✅ GERAÇÃO CONCLUÍDA!")

        if ultra_pdf:
            print(f"📄 PDF ULTRA COMPLETO: {ultra_pdf}")
            print(f"   Abrir: open \"{ultra_pdf}\"")

        if plan_pdf:
            print(f"📄 PDF PLANO MIGRAÇÃO: {plan_pdf}")
            print(f"   Abrir: open \"{plan_pdf}\"")

        print()


def main():
    """Função principal"""
    if len(sys.argv) < 2:
        config_path = "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/specs/002-migration-analysis-pdf/config/document-config.yaml"
    else:
        config_path = sys.argv[1]

    orchestrator = PDFOrchestrator(config_path)
    orchestrator.run()


if __name__ == "__main__":
    main()
