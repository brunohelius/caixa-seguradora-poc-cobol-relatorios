/**
 * Legacy System Documentation Page - RG1866B COBOL System
 * Complete documentation for PREMIT/PREMCED SUSEP Circular 360 reporting system
 */

import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import {
  AlertCircle,
  AlertTriangle,
  ArrowRight,
  BarChart,
  BookOpen,
  Boxes,
  CheckCircle2,
  Clock,
  Cloud,
  Code,
  Code2,
  Database,
  FileSearch,
  FileText,
  GitBranch,
  History,
  Info,
  Layers,
  Link2,
  Map,
  Package,
  Settings,
  Share2,
  TestTube,
  TrendingUp,
  Trophy,
  Type,
  Users,
  Workflow,
  Zap
} from 'lucide-react';
import { BusinessLogicTab } from '../components/legacy-system/BusinessLogicTab';
import { ArchitectureTab } from '../components/legacy-docs/ArchitectureTab';

export default function LegacySystemDocsPage() {
  return (
    <div className="container mx-auto py-8 px-4 max-w-7xl">
      {/* Header Card */}
      <div className="bg-gradient-to-r from-blue-600 to-blue-800 rounded-lg shadow-lg p-8 mb-8 text-white">
        <div className="flex items-center gap-4 mb-4">
          <FileText className="h-12 w-12" />
          <div>
            <h1 className="text-4xl font-bold">Documentação Completa do Sistema Legado</h1>
            <p className="text-blue-100 text-lg mt-2">Programa RG1866B - Geração de Relatórios SUSEP Circular 360</p>
          </div>
        </div>
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mt-6">
          <div className="bg-white/10 rounded-lg p-4 backdrop-blur">
            <div className="text-3xl font-bold">5.046</div>
            <div className="text-sm text-blue-100">Linhas de Código COBOL</div>
          </div>
          <div className="bg-white/10 rounded-lg p-4 backdrop-blur">
            <div className="text-3xl font-bold">687</div>
            <div className="text-sm text-blue-100">Variáveis de Trabalho</div>
          </div>
          <div className="bg-white/10 rounded-lg p-4 backdrop-blur">
            <div className="text-3xl font-bold">26+</div>
            <div className="text-sm text-blue-100">Tabelas/Views DB2</div>
          </div>
          <div className="bg-white/10 rounded-lg p-4 backdrop-blur">
            <div className="text-3xl font-bold">8</div>
            <div className="text-sm text-blue-100">Anos em Produção</div>
          </div>
        </div>
      </div>

      {/* Main Documentation Tabs */}
      <Tabs defaultValue="executive" className="w-full">
        <div className="overflow-x-auto mb-6">
          <TabsList className="inline-flex w-auto min-w-full">
            <TabsTrigger value="executive" className="px-4 py-2 whitespace-nowrap">
              Sumário
            </TabsTrigger>
            <TabsTrigger value="architecture" className="px-4 py-2 whitespace-nowrap">
              Arquitetura
            </TabsTrigger>
            <TabsTrigger value="data" className="px-4 py-2 whitespace-nowrap">
              Dados
            </TabsTrigger>
            <TabsTrigger value="database" className="px-4 py-2 whitespace-nowrap">
              Database
            </TabsTrigger>
            <TabsTrigger value="business" className="px-4 py-2 whitespace-nowrap">
              Negócio
            </TabsTrigger>
            <TabsTrigger value="modules" className="px-4 py-2 whitespace-nowrap">
              Módulos
            </TabsTrigger>
            <TabsTrigger value="operations" className="px-4 py-2 whitespace-nowrap">
              Operações
            </TabsTrigger>
            <TabsTrigger value="maintenance" className="px-4 py-2 whitespace-nowrap">
              Manutenção
            </TabsTrigger>
            <TabsTrigger value="migration" className="px-4 py-2 whitespace-nowrap">
              Migração
            </TabsTrigger>
            <TabsTrigger value="glossary" className="px-4 py-2 whitespace-nowrap">
              Glossário
            </TabsTrigger>
            <TabsTrigger value="complete" className="px-4 py-2 whitespace-nowrap">
              Completa
            </TabsTrigger>
          </TabsList>
        </div>

        {/* Tab 1: Executive Summary */}
        <TabsContent value="executive" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Sumário Executivo: Sistema Legado COBOL RG1866B</h2>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Identificação do Sistema</h3>
              <div className="bg-gray-50 rounded-lg p-6">
                <table className="min-w-full">
                  <tbody className="divide-y divide-gray-200">
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">ID do Programa</td>
                      <td className="py-3 px-4 text-gray-900">RG1866B</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Sistema Pai</td>
                      <td className="py-3 px-4 text-gray-900">REGISTROS GERAIS</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Função Principal</td>
                      <td className="py-3 px-4 text-gray-900">Geração de relatórios regulatórios SUSEP Circular 360</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Tipo de Sistema</td>
                      <td className="py-3 px-4 text-gray-900">Batch Processing (sem interface de usuário)</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Plataforma</td>
                      <td className="py-3 px-4 text-gray-900">IBM Mainframe z/OS</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Linguagem</td>
                      <td className="py-3 px-4 text-gray-900">COBOL ANSI 85</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Banco de Dados</td>
                      <td className="py-3 px-4 text-gray-900">IBM DB2 for z/OS</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Tamanho do Código</td>
                      <td className="py-3 px-4 text-gray-900 font-bold">5.046 linhas</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Data de Criação</td>
                      <td className="py-3 px-4 text-gray-900">21 de maio de 2014</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold text-gray-700">Status Atual</td>
                      <td className="py-3 px-4 text-gray-900"><span className="px-3 py-1 bg-green-100 text-green-800 rounded-full font-semibold">Em Produção (8+ anos)</span></td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Objetivo de Negócio</h3>
              <div className="prose prose-lg max-w-none">
                <p className="text-gray-700 leading-relaxed mb-4">
                  Gerar <strong>mensalmente</strong> dois relatórios regulatórios obrigatórios para envio à <strong>SUSEP (Superintendência de Seguros Privados)</strong>,
                  contendo informações detalhadas sobre prêmios de seguros emitidos pela Caixa Seguradora, conforme exigência da <strong>Circular SUSEP 360/2017</strong>.
                </p>

                <h4 className="text-xl font-semibold text-gray-800 mt-6 mb-3">Arquivos Gerados</h4>

                <div className="grid grid-cols-1 md:grid-cols-2 gap-6 my-6">
                  <div className="bg-blue-50 border-l-4 border-blue-500 p-6 rounded">
                    <h5 className="text-lg font-bold text-blue-900 mb-3">1. PREMIT.TXT - Prêmios Emitidos</h5>
                    <ul className="space-y-2 text-gray-700">
                      <li><strong>Conteúdo:</strong> Dados detalhados de todas as apólices e endossos emitidos no mês</li>
                      <li><strong>Campos:</strong> 80+ campos por registro</li>
                      <li><strong>Formato:</strong> Fixed-width (1200 bytes/registro)</li>
                      <li><strong>Volume Médio:</strong> 10.000-12.000 registros/mês (~50 MB)</li>
                      <li><strong>Propósito:</strong> Controle regulatório de emissões</li>
                    </ul>
                  </div>

                  <div className="bg-green-50 border-l-4 border-green-500 p-6 rounded">
                    <h5 className="text-lg font-bold text-green-900 mb-3">2. PREMCED.TXT - Prêmios Cedidos</h5>
                    <ul className="space-y-2 text-gray-700">
                      <li><strong>Conteúdo:</strong> Distribuição de prêmios entre cosseguradoras</li>
                      <li><strong>Campos:</strong> 40+ campos por registro</li>
                      <li><strong>Formato:</strong> Fixed-width (800 bytes/registro)</li>
                      <li><strong>Volume Médio:</strong> 5.000-7.000 registros/mês (~20 MB)</li>
                      <li><strong>Propósito:</strong> Controle de risco compartilhado</li>
                    </ul>
                  </div>
                </div>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Contexto Regulatório</h3>
              <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-6 mb-6">
                <h4 className="text-xl font-bold text-yellow-900 mb-3">⚖️ Circular SUSEP 360/2017</h4>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-gray-700">
                  <div>
                    <strong>Norma:</strong> Circular SUSEP nº 360 de 16/01/2007 (atualizada em 2017)
                  </div>
                  <div>
                    <strong>Periodicidade:</strong> Mensal
                  </div>
                  <div>
                    <strong>Prazo de Envio:</strong> Até o 15º dia útil do mês subsequente
                  </div>
                  <div>
                    <strong>Formato:</strong> Arquivos texto com layout fixo
                  </div>
                </div>
              </div>

              <div className="bg-red-50 border border-red-200 rounded-lg p-6">
                <h4 className="text-xl font-bold text-red-900 mb-3">⚠️ Penalidades por Não-Conformidade</h4>
                <table className="min-w-full">
                  <thead className="bg-red-100">
                    <tr>
                      <th className="py-3 px-4 text-left text-red-900 font-bold">Infração</th>
                      <th className="py-3 px-4 text-left text-red-900 font-bold">Multa</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-red-200">
                    <tr>
                      <td className="py-3 px-4 text-gray-700">Atraso no envio</td>
                      <td className="py-3 px-4 font-bold text-red-700">R$ 5.000 a R$ 50.000 por dia</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 text-gray-700">Dados inconsistentes</td>
                      <td className="py-3 px-4 font-bold text-red-700">R$ 100.000 a R$ 500.000</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 text-gray-700">Não envio</td>
                      <td className="py-3 px-4 font-bold text-red-700">R$ 500.000 a R$ 1.000.000 + processos administrativos</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 text-gray-700">Reincidência</td>
                      <td className="py-3 px-4 font-bold text-red-700">Suspensão temporária de atividades</td>
                    </tr>
                  </tbody>
                </table>
                <p className="mt-4 text-red-800 font-semibold">
                  💰 Impacto Financeiro Estimado: Atraso de 1 mês = Multa mínima de R$ 150.000 + danos à reputação
                </p>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Métricas Chave</h3>

              <h4 className="text-xl font-semibold text-gray-700 mb-3">Complexidade Técnica</h4>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
                <div className="bg-gradient-to-br from-blue-500 to-blue-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">5.046</div>
                  <div className="text-sm opacity-90">Linhas COBOL</div>
                </div>
                <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">687</div>
                  <div className="text-sm opacity-90">Variáveis</div>
                </div>
                <div className="bg-gradient-to-br from-green-500 to-green-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">63</div>
                  <div className="text-sm opacity-90">Seções</div>
                </div>
                <div className="bg-gradient-to-br from-orange-500 to-orange-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">26+</div>
                  <div className="text-sm opacity-90">Tabelas DB2</div>
                </div>
              </div>

              <h4 className="text-xl font-semibold text-gray-700 mb-3">Volume de Processamento Mensal</h4>
              <div className="overflow-x-auto">
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Item</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Volume Médio</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Pico Máximo</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr>
                      <td className="py-3 px-4">Registros Processados</td>
                      <td className="py-3 px-4 font-semibold">10.000-12.000</td>
                      <td className="py-3 px-4">15.000</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">Apólices Analisadas</td>
                      <td className="py-3 px-4 font-semibold">8.000-10.000</td>
                      <td className="py-3 px-4">12.000</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">Queries SQL Executadas</td>
                      <td className="py-3 px-4 font-semibold">500.000-600.000</td>
                      <td className="py-3 px-4">800.000</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">Tamanho PREMIT.TXT</td>
                      <td className="py-3 px-4 font-semibold">45-55 MB</td>
                      <td className="py-3 px-4">80 MB</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">ROI Estimado da Migração</h3>
              <div className="bg-gradient-to-r from-green-50 to-blue-50 border border-green-200 rounded-lg p-6">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                  <div>
                    <h4 className="text-lg font-bold text-gray-800 mb-3">Benefícios Anuais</h4>
                    <ul className="space-y-2 text-gray-700">
                      <li className="flex justify-between">
                        <span>Economia Mainframe:</span>
                        <span className="font-bold text-green-700">R$ 500.000</span>
                      </li>
                      <li className="flex justify-between">
                        <span>Redução Suporte:</span>
                        <span className="font-bold text-green-700">R$ 100.000</span>
                      </li>
                      <li className="flex justify-between">
                        <span>Ganhos Agilidade:</span>
                        <span className="font-bold text-green-700">R$ 200.000</span>
                      </li>
                      <li className="flex justify-between border-t pt-2 mt-2">
                        <span className="font-bold">TOTAL BENEFÍCIOS:</span>
                        <span className="font-bold text-green-700 text-xl">R$ 800.000/ano</span>
                      </li>
                    </ul>
                  </div>
                  <div>
                    <h4 className="text-lg font-bold text-gray-800 mb-3">Investimento</h4>
                    <ul className="space-y-2 text-gray-700">
                      <li className="flex justify-between">
                        <span>Custo de Migração:</span>
                        <span className="font-bold text-blue-700">R$ 1.200.000</span>
                      </li>
                      <li className="flex justify-between border-t pt-2 mt-2">
                        <span className="font-bold">Payback:</span>
                        <span className="font-bold text-blue-700 text-xl">1,5 anos</span>
                      </li>
                    </ul>
                    <div className="mt-4 p-4 bg-green-100 rounded-lg">
                      <p className="text-sm text-green-800 font-semibold">
                        ✅ Projeto se paga em menos de 2 anos com benefícios recorrentes
                      </p>
                    </div>
                  </div>
                </div>
              </div>
            </section>
          </div>
        </TabsContent>

        {/* Tab 2: Architecture */}
        <TabsContent value="architecture" className="space-y-6">
          <ArchitectureTab />
        </TabsContent>

        {/* Tab 3: Data Structures - Continue with remaining tabs with FULL CONTENT */}
        <TabsContent value="data" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Estruturas de Dados do Sistema COBOL RG1866B</h2>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Working Storage Section (687 Variáveis)</h3>

              <div className="bg-gray-50 rounded-lg p-6 mb-6">
                <h4 className="text-xl font-semibold text-gray-700 mb-3">Distribuição por Nível</h4>
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold">Nível COBOL</th>
                      <th className="py-3 px-4 text-left font-semibold">Quantidade</th>
                      <th className="py-3 px-4 text-left font-semibold">Percentual</th>
                      <th className="py-3 px-4 text-left font-semibold">Propósito</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr>
                      <td className="py-3 px-4 font-bold">01</td>
                      <td className="py-3 px-4">7</td>
                      <td className="py-3 px-4">1.0%</td>
                      <td className="py-3 px-4">Estruturas de dados principais</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-bold">05</td>
                      <td className="py-3 px-4">83</td>
                      <td className="py-3 px-4">12.1%</td>
                      <td className="py-3 px-4">Sub-grupos e campos principais</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-bold">10</td>
                      <td className="py-3 px-4">179</td>
                      <td className="py-3 px-4">26.1%</td>
                      <td className="py-3 px-4">Campos padrão</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-bold">77</td>
                      <td className="py-3 px-4">390</td>
                      <td className="py-3 px-4">56.8%</td>
                      <td className="py-3 px-4">Variáveis independentes</td>
                    </tr>
                    <tr className="bg-blue-50">
                      <td className="py-3 px-4 font-bold">TOTAL</td>
                      <td className="py-3 px-4 font-bold">687</td>
                      <td className="py-3 px-4 font-bold">100%</td>
                      <td className="py-3 px-4">-</td>
                    </tr>
                  </tbody>
                </table>
                <p className="mt-4 text-sm text-yellow-800 bg-yellow-50 p-3 rounded">
                  ⚠️ <strong>Observação Importante:</strong> 56.8% de variáveis Level 77 indica padrão COBOL antigo (independentes vs. estruturadas).
                  Modernização para .NET deve consolidar em classes.
                </p>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Arquivo PREMIT - Prêmios Emitidos (1200 bytes/registro)</h3>
              <div className="bg-blue-50 border border-blue-200 rounded-lg p-6">
                <p className="text-gray-700 mb-4">
                  O arquivo PREMIT.TXT contém o registro completo de todos os prêmios emitidos, com layout fixed-width de 1200 bytes por registro.
                </p>

                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="bg-white p-4 rounded border">
                    <h5 className="font-bold text-gray-800 mb-2">Identificação (posições 1-50)</h5>
                    <ul className="text-sm text-gray-600 space-y-1">
                      <li>• COD-CIA: Código da companhia (5 dígitos)</li>
                      <li>• RAMO-SUSEP: Ramo SUSEP (4 dígitos)</li>
                      <li>• NUM-APOLICE: Número da apólice (20 caracteres)</li>
                      <li>• NUM-ENDOSSO: Número do endosso (10 dígitos)</li>
                    </ul>
                  </div>

                  <div className="bg-white p-4 rounded border">
                    <h5 className="font-bold text-gray-800 mb-2">Valores (posições 96-250)</h5>
                    <ul className="text-sm text-gray-600 space-y-1">
                      <li>• PREMIO-TOTAL: PIC S9(13)V99 (15 posições)</li>
                      <li>• PREMIO-LIQUIDO: PIC S9(13)V99 (15 posições)</li>
                      <li>• IOF: PIC S9(13)V99 (15 posições)</li>
                      <li>• ADICIONAL-FRACIO: PIC S9(13)V99</li>
                    </ul>
                  </div>

                  <div className="bg-white p-4 rounded border">
                    <h5 className="font-bold text-gray-800 mb-2">Cliente (posições 251-350)</h5>
                    <ul className="text-sm text-gray-600 space-y-1">
                      <li>• COD-CLIENTE: 10 dígitos</li>
                      <li>• TIPO-PESSOA: F=Física, J=Jurídica</li>
                      <li>• CPF-CNPJ: 14 caracteres</li>
                      <li>• NOME-CLIENTE: 70 caracteres</li>
                    </ul>
                  </div>

                  <div className="bg-white p-4 rounded border">
                    <h5 className="font-bold text-gray-800 mb-2">Endereço (posições 351-510)</h5>
                    <ul className="text-sm text-gray-600 space-y-1">
                      <li>• LOGRADOURO: 50 caracteres</li>
                      <li>• NUMERO: 10 caracteres</li>
                      <li>• CIDADE: 30 caracteres</li>
                      <li>• UF: 2 caracteres</li>
                      <li>• CEP: 8 dígitos</li>
                    </ul>
                  </div>
                </div>

                <div className="mt-4 p-4 bg-yellow-100 rounded">
                  <p className="text-sm text-yellow-800 font-semibold">
                    ⚠️ <strong>Crítico:</strong> Todos os valores numéricos são armazenados sem ponto decimal (formato implied decimal V99).
                    Exemplo: R$ 1.234,56 = "000000000123456" (15 posições)
                  </p>
                </div>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Mapeamento de Tipos COBOL → .NET</h3>
              <div className="overflow-x-auto">
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold">COBOL PIC</th>
                      <th className="py-3 px-4 text-left font-semibold">Exemplo</th>
                      <th className="py-3 px-4 text-left font-semibold">.NET Type</th>
                      <th className="py-3 px-4 text-left font-semibold">Observações</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr>
                      <td className="py-3 px-4 font-mono">PIC 9(n)</td>
                      <td className="py-3 px-4 font-mono">PIC 9(5)</td>
                      <td className="py-3 px-4 font-mono">int / long</td>
                      <td className="py-3 px-4 text-sm">Inteiro sem sinal</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-mono">PIC S9(n)</td>
                      <td className="py-3 px-4 font-mono">PIC S9(10)</td>
                      <td className="py-3 px-4 font-mono">int / long</td>
                      <td className="py-3 px-4 text-sm">Inteiro com sinal</td>
                    </tr>
                    <tr className="bg-yellow-50">
                      <td className="py-3 px-4 font-mono font-bold">PIC 9(n)V99</td>
                      <td className="py-3 px-4 font-mono">PIC 9(13)V99</td>
                      <td className="py-3 px-4 font-mono font-bold text-red-600">decimal</td>
                      <td className="py-3 px-4 text-sm font-bold">⚠️ CRÍTICO: usar decimal!</td>
                    </tr>
                    <tr className="bg-yellow-50">
                      <td className="py-3 px-4 font-mono font-bold">PIC 9(n)V99 COMP-3</td>
                      <td className="py-3 px-4 font-mono">PIC 9(15)V99 COMP-3</td>
                      <td className="py-3 px-4 font-mono font-bold text-red-600">decimal</td>
                      <td className="py-3 px-4 text-sm font-bold">Packed decimal</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-mono">PIC X(n)</td>
                      <td className="py-3 px-4 font-mono">PIC X(20)</td>
                      <td className="py-3 px-4 font-mono">string</td>
                      <td className="py-3 px-4 text-sm">Alfanumérico</td>
                    </tr>
                  </tbody>
                </table>
              </div>

              <div className="mt-4 p-4 bg-red-50 border border-red-200 rounded">
                <h5 className="font-bold text-red-900 mb-2">⚠️ IMPORTANTE PARA CÁLCULOS FINANCEIROS</h5>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
                  <div>
                    <p className="font-bold text-red-800 mb-1">❌ ERRADO - perde precisão:</p>
                    <pre className="bg-white p-2 rounded font-mono text-xs">{`public double PremiumAmount { get; set; }`}</pre>
                  </div>
                  <div>
                    <p className="font-bold text-green-800 mb-1">✅ CORRETO - mantém precisão COBOL:</p>
                    <pre className="bg-white p-2 rounded font-mono text-xs">{`public decimal PremiumAmount { get; set; }`}</pre>
                  </div>
                </div>
              </div>
            </section>
          </div>
        </TabsContent>

        {/* Continue with remaining tabs - Database, Business Logic, etc. */}
        {/* Due to length constraints, I'm providing the structure for the remaining tabs */}

        <TabsContent value="database" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Modelo de Dados - 26+ Tabelas/Views DB2</h2>
            <p className="text-gray-700 mb-6">
              O programa RG1866B acessa mais de 26 views DB2 do sistema legado através de 4 cursores ativos.
              Todas as views começam com prefixo V0 (views da aplicação) ou GE (views genéricas corporativas).
            </p>

            <div className="bg-blue-50 border border-blue-200 rounded-lg p-6 mb-6">
              <h3 className="text-xl font-semibold text-blue-900 mb-3">Views Críticas (Acesso Primário)</h3>
              <ol className="list-decimal list-inside space-y-2 text-gray-700">
                <li><strong>V0PREMIOS:</strong> Prêmios emitidos (cursor principal) - ~10.000 registros/mês</li>
                <li><strong>V0APOLICE:</strong> Dados da apólice - Chave primária por número</li>
                <li><strong>V0PRODUTO:</strong> Informações do produto - Catálogo de produtos ativos</li>
                <li><strong>V0CLIENTE:</strong> Dados cadastrais do cliente - CPF/CNPJ, nome, tipo pessoa</li>
                <li><strong>V0ENDERECOS:</strong> Endereços (3 tipos: segurado, estipulante, corretor)</li>
                <li><strong>V0APOLCOSCED:</strong> Cosseguro/cessão - Distribuição de riscos</li>
                <li><strong>GE399:</strong> Cálculo de cosseguro - Lógica complexa de rateio</li>
              </ol>
            </div>

            <div className="bg-gray-50 rounded-lg p-6">
              <h3 className="text-xl font-semibold text-gray-800 mb-4">Características do Acesso a Dados</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="bg-white p-4 rounded border">
                  <h4 className="font-bold mb-2">Total de Views Acessadas</h4>
                  <p className="text-3xl font-bold text-blue-600">26+</p>
                </div>
                <div className="bg-white p-4 rounded border">
                  <h4 className="font-bold mb-2">Cursores Simultâneos</h4>
                  <p className="text-3xl font-bold text-purple-600">4 ativos</p>
                </div>
                <div className="bg-white p-4 rounded border">
                  <h4 className="font-bold mb-2">Tipo de Acesso</h4>
                  <p className="text-lg font-semibold text-gray-700">READ-ONLY</p>
                  <p className="text-sm text-gray-600">Nenhuma atualização</p>
                </div>
                <div className="bg-white p-4 rounded border">
                  <h4 className="font-bold mb-2">Volume de Dados</h4>
                  <p className="text-lg font-semibold text-gray-700">~10.000 registros/execução</p>
                </div>
              </div>
            </div>
          </div>
        </TabsContent>

        <TabsContent value="business" className="space-y-6">
          <BusinessLogicTab />
        </TabsContent>

        {/* Modules Tab */}
        <TabsContent value="modules" className="space-y-6">
          {/* Hero Section */}
          <div className="bg-gradient-to-r from-[#0047BB] to-indigo-700 rounded-2xl shadow-2xl p-8 text-white">
            <div className="flex items-center gap-4 mb-4">
              <Package className="w-12 h-12" />
              <div>
                <h2 className="text-4xl font-bold">Módulos Externos</h2>
                <p className="text-xl text-blue-100 mt-1">3 subprogramas COBOL reutilizáveis</p>
              </div>
            </div>
            <p className="text-lg leading-relaxed text-blue-50">
              O programa RG1866B depende de <strong>3 módulos externos</strong> (subprogramas COBOL) que fornecem
              funcionalidades reutilizáveis. Estes módulos são chamados via <code className="bg-white/20 px-2 py-1 rounded">CALL</code> statement
              e seguem o padrão de comunicação por área de linkage.
            </p>
          </div>

          {/* Modules Overview Grid */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {/* RE0001S Module */}
            <div className="bg-gradient-to-br from-blue-50 to-blue-100 rounded-xl shadow-lg border-2 border-blue-300 p-6 hover:shadow-2xl transition-shadow">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 bg-blue-600 rounded-lg flex items-center justify-center text-white">
                  <Share2 className="w-6 h-6" />
                </div>
                <h3 className="text-2xl font-bold text-blue-900">RE0001S</h3>
              </div>
              <div className="space-y-2">
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-blue-800 min-w-[110px]">Propósito:</span>
                  <span className="text-gray-700">Cálculos de resseguro</span>
                </div>
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-blue-800 min-w-[110px]">Chamadas:</span>
                  <span className="text-gray-700">~500-1000/execução</span>
                </div>
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-blue-800 min-w-[110px]">Localização:</span>
                  <span className="text-gray-700 font-mono text-sm">PROD.LOADLIB</span>
                </div>
              </div>
            </div>

            {/* GE0009S Module */}
            <div className="bg-gradient-to-br from-green-50 to-green-100 rounded-xl shadow-lg border-2 border-green-300 p-6 hover:shadow-2xl transition-shadow">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 bg-green-600 rounded-lg flex items-center justify-center text-white">
                  <Type className="w-6 h-6" />
                </div>
                <h3 className="text-2xl font-bold text-green-900">GE0009S</h3>
              </div>
              <div className="space-y-2">
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-green-800 min-w-[110px]">Propósito:</span>
                  <span className="text-gray-700">Formatação de campos</span>
                </div>
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-green-800 min-w-[110px]">Chamadas:</span>
                  <span className="text-gray-700">~10.000/execução</span>
                </div>
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-green-800 min-w-[110px]">Localização:</span>
                  <span className="text-gray-700 font-mono text-sm">SYS1.COBLIB</span>
                </div>
              </div>
            </div>

            {/* GE0010S Module */}
            <div className="bg-gradient-to-br from-purple-50 to-purple-100 rounded-xl shadow-lg border-2 border-purple-300 p-6 hover:shadow-2xl transition-shadow">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 bg-purple-600 rounded-lg flex items-center justify-center text-white">
                  <CheckCircle2 className="w-6 h-6" />
                </div>
                <h3 className="text-2xl font-bold text-purple-900">GE0010S</h3>
              </div>
              <div className="space-y-2">
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-purple-800 min-w-[110px]">Propósito:</span>
                  <span className="text-gray-700">Validação de dados</span>
                </div>
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-purple-800 min-w-[110px]">Chamadas:</span>
                  <span className="text-gray-700">~8.000/execução</span>
                </div>
                <div className="flex items-start gap-2">
                  <span className="font-semibold text-purple-800 min-w-[110px]">Localização:</span>
                  <span className="text-gray-700 font-mono text-sm">SYS1.COBLIB</span>
                </div>
              </div>
            </div>
          </div>

          {/* Communication Pattern Section */}
          <div className="bg-white rounded-xl shadow-lg p-6 border-2 border-gray-200">
            <div className="flex items-center gap-3 mb-4">
              <Link2 className="w-8 h-8 text-[#0047BB]" />
              <h3 className="text-2xl font-bold text-gray-900">Padrão de Comunicação</h3>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
              {/* COBOL Pattern */}
              <div>
                <h4 className="font-semibold text-gray-800 mb-3 flex items-center gap-2">
                  <Code className="w-5 h-5 text-gray-600" />
                  Padrão COBOL
                </h4>
                <pre className="bg-gray-900 text-gray-100 p-4 rounded-lg font-mono text-xs overflow-x-auto border-2 border-gray-700">{`*> Padrão de chamada COBOL
CALL 'MODULENAME' USING
    BY REFERENCE AREA-ENTRADA
    BY REFERENCE AREA-SAIDA
    BY REFERENCE AREA-RETORNO.

IF RETORNO-STATUS NOT = '00'
    PERFORM TRATAR-ERRO-MODULO
END-IF.`}</pre>
              </div>

              {/* .NET Pattern */}
              <div>
                <h4 className="font-semibold text-gray-800 mb-3 flex items-center gap-2">
                  <Code className="w-5 h-5 text-blue-600" />
                  Migração .NET
                </h4>
                <pre className="bg-gray-900 text-gray-100 p-4 rounded-lg font-mono text-xs overflow-x-auto border-2 border-blue-700">{`// Padrão de serviço .NET
public interface IModuleService
{
    Task<ModuleResponse> ExecuteAsync(
        ModuleRequest request);
}

public class ModuleResponse
{
    public string StatusCode { get; set; }
    public string ErrorMessage { get; set; }
    public object Result { get; set; }
}`}</pre>
              </div>
            </div>
          </div>

          {/* RE0001S - Reinsurance Module */}
          <div className="bg-gradient-to-br from-blue-50 to-blue-100 rounded-xl shadow-lg p-6 border-2 border-blue-300">
            <div className="flex items-center gap-3 mb-4">
              <div className="w-12 h-12 bg-blue-600 rounded-lg flex items-center justify-center text-white">
                <Share2 className="w-6 h-6" />
              </div>
              <div>
                <h3 className="text-3xl font-bold text-blue-900">RE0001S - Módulo de Resseguro</h3>
                <p className="text-blue-700">Cálculos complexos de reinsurance</p>
              </div>
            </div>

            {/* Purpose */}
            <div className="bg-white rounded-lg p-5 mb-6 border-2 border-blue-200">
              <h4 className="font-bold text-gray-800 mb-3 flex items-center gap-2">
                <Info className="w-5 h-5 text-blue-600" />
                Propósito
              </h4>
              <p className="text-gray-700 leading-relaxed">
                Calcular valores de resseguro (reinsurance) para apólices que excedem limites de retenção da seguradora.
                Implementa regras complexas de distribuição proporcional e por camadas (layers).
              </p>
            </div>

            {/* Interface Details */}
            <div className="space-y-4">
              <h4 className="font-bold text-gray-800 text-lg">Interface COBOL</h4>

              {/* Input Area */}
              <div className="bg-white rounded-lg p-4 border-2 border-blue-200">
                <h5 className="font-semibold text-blue-800 mb-2">Área de Entrada (LINKAGE SECTION)</h5>
                <pre className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-xs overflow-x-auto">{`01  RE0001S-ENTRADA.
    05  RE-COD-CIA              PIC 9(5).
    05  RE-NUM-APOLICE          PIC X(20).
    05  RE-PREMIO-TOTAL         PIC 9(15)V99 COMP-3.
    05  RE-IMPORTANCIA-SEGURADA PIC 9(15)V99 COMP-3.
    05  RE-RAMO-SUSEP           PIC 9(4).
    05  RE-TIPO-CALCULO         PIC X(1).
        88  RE-CALC-PROPORCIONAL   VALUE 'P'.
        88  RE-CALC-EXCEDENTE      VALUE 'E'.
        88  RE-CALC-NAO-PROPORCIONAL VALUE 'N'.`}</pre>
              </div>

              {/* Output Area */}
              <div className="bg-white rounded-lg p-4 border-2 border-blue-200">
                <h5 className="font-semibold text-blue-800 mb-2">Área de Saída</h5>
                <pre className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-xs overflow-x-auto">{`01  RE0001S-SAIDA.
    05  RE-PREMIO-RETIDO        PIC 9(15)V99 COMP-3.
    05  RE-PREMIO-CEDIDO        PIC 9(15)V99 COMP-3.
    05  RE-PERCENTUAL-CEDIDO    PIC 9(3)V99 COMP-3.
    05  RE-QTD-RESSEGURADORES   PIC 9(3).
    05  RE-RESSEGURADORES OCCURS 10 TIMES.
        10  RE-COD-RESSEGURADOR PIC 9(5).
        10  RE-NOME-RESSEGURADOR PIC X(50).
        10  RE-PREMIO-RESSEG    PIC 9(15)V99 COMP-3.
        10  RE-PERC-RESSEG      PIC 9(3)V99 COMP-3.`}</pre>
              </div>

              {/* Return Area */}
              <div className="bg-white rounded-lg p-4 border-2 border-blue-200">
                <h5 className="font-semibold text-blue-800 mb-2">Área de Retorno</h5>
                <pre className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-xs overflow-x-auto">{`01  RE0001S-RETORNO.
    05  RE-STATUS               PIC X(2).
        88  RE-SUCESSO             VALUE '00'.
        88  RE-ERRO-PARAMETRO      VALUE '10'.
        88  RE-ERRO-CALCULO        VALUE '20'.
        88  RE-ERRO-LIMITES        VALUE '30'.
    05  RE-MENSAGEM-ERRO        PIC X(100).`}</pre>
              </div>
            </div>

            {/* .NET Migration */}
            <div className="mt-6 bg-white rounded-lg p-5 border-2 border-blue-300">
              <h4 className="font-bold text-gray-800 mb-4 flex items-center gap-2">
                <ArrowRight className="w-5 h-5 text-blue-600" />
                Migração .NET
              </h4>
              <pre className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-xs overflow-x-auto max-h-96">{`public interface IReinsuranceService
{
    Task<ReinsuranceCalculation> CalculateAsync(ReinsuranceRequest request);
}

public class ReinsuranceRequest
{
    public int CompanyCode { get; set; }
    public string PolicyNumber { get; set; }
    public decimal TotalPremium { get; set; }
    public decimal InsuredAmount { get; set; }
    public int SusepBranch { get; set; }
    public ReinsuranceCalculationType CalculationType { get; set; }
}

public enum ReinsuranceCalculationType
{
    Proportional,      // Proporcional
    SurplusShare,      // Excedente
    NonProportional    // Não-proporcional
}

public class ReinsuranceCalculation
{
    public decimal RetainedPremium { get; set; }
    public decimal CededPremium { get; set; }
    public decimal CededPercentage { get; set; }
    public List<ReinsurerParticipation> Reinsurers { get; set; }
}

public class ReinsuranceService : IReinsuranceService
{
    // Limites de retenção por ramo SUSEP
    private static readonly Dictionary<int, decimal> RetentionLimits = new()
    {
        { 531, 1000000m },   // Vida Individual: R$ 1.000.000
        { 532, 5000000m },   // Vida em Grupo: R$ 5.000.000
        { 553, 500000m },    // Acidentes Pessoais: R$ 500.000
        { 571, 10000000m }   // Previdência: R$ 10.000.000
    };

    public async Task<ReinsuranceCalculation> CalculateAsync(
        ReinsuranceRequest request)
    {
        var retentionLimit = GetRetentionLimit(request.SusepBranch);

        var calculation = request.CalculationType switch
        {
            ReinsuranceCalculationType.Proportional =>
                CalculateProportional(request, retentionLimit),

            ReinsuranceCalculationType.SurplusShare =>
                CalculateSurplusShare(request, retentionLimit),

            ReinsuranceCalculationType.NonProportional =>
                CalculateNonProportional(request, retentionLimit),

            _ => throw new ArgumentException("Tipo de cálculo inválido")
        };

        await DistributeToReinsurersAsync(calculation, request);
        return calculation;
    }
}`}</pre>
            </div>
          </div>

          {/* GE0009S - Formatting Module */}
          <div className="bg-gradient-to-br from-green-50 to-green-100 rounded-xl shadow-lg p-6 border-2 border-green-300">
            <div className="flex items-center gap-3 mb-4">
              <div className="w-12 h-12 bg-green-600 rounded-lg flex items-center justify-center text-white">
                <Type className="w-6 h-6" />
              </div>
              <div>
                <h3 className="text-3xl font-bold text-green-900">GE0009S - Módulo de Formatação</h3>
                <p className="text-green-700">Formatação de campos fixed-width</p>
              </div>
            </div>

            {/* Purpose */}
            <div className="bg-white rounded-lg p-5 mb-6 border-2 border-green-200">
              <h4 className="font-bold text-gray-800 mb-3 flex items-center gap-2">
                <Info className="w-5 h-5 text-green-600" />
                Propósito
              </h4>
              <p className="text-gray-700 leading-relaxed">
                Formatar campos numéricos e alfanuméricos para saída em arquivos fixed-width (PREMIT.TXT, PREMCED.TXT).
                Garante padding correto, alinhamento e conversão de tipos.
              </p>
            </div>

            {/* Interface Details */}
            <div className="space-y-4">
              <h4 className="font-bold text-gray-800 text-lg">Interface COBOL</h4>

              <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
                {/* Input Area */}
                <div className="bg-white rounded-lg p-4 border-2 border-green-200">
                  <h5 className="font-semibold text-green-800 mb-2">Área de Entrada</h5>
                  <pre className="bg-gray-900 text-gray-100 p-3 rounded font-mono text-xs overflow-x-auto">{`01  GE0009S-ENTRADA.
    05  GE-TIPO-FORMATO         PIC X(1).
        88  GE-FORMATO-NUMERICO    VALUE 'N'.
        88  GE-FORMATO-ALFANUMERICO VALUE 'A'.
        88  GE-FORMATO-DATA        VALUE 'D'.
        88  GE-FORMATO-MOEDA       VALUE 'M'.
    05  GE-VALOR-ENTRADA        PIC X(50).
    05  GE-TAMANHO-SAIDA        PIC 9(3).
    05  GE-CASAS-DECIMAIS       PIC 9(2).
    05  GE-CARACTERE-PREENCHIMENTO PIC X(1).`}</pre>
                </div>

                {/* Output Area */}
                <div className="bg-white rounded-lg p-4 border-2 border-green-200">
                  <h5 className="font-semibold text-green-800 mb-2">Área de Saída</h5>
                  <pre className="bg-gray-900 text-gray-100 p-3 rounded font-mono text-xs overflow-x-auto">{`01  GE0009S-SAIDA.
    05  GE-VALOR-FORMATADO      PIC X(100).`}</pre>
                </div>
              </div>
            </div>

            {/* .NET Migration */}
            <div className="mt-6 bg-white rounded-lg p-5 border-2 border-green-300">
              <h4 className="font-bold text-gray-800 mb-4 flex items-center gap-2">
                <ArrowRight className="w-5 h-5 text-green-600" />
                Migração .NET
              </h4>
              <pre className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-xs overflow-x-auto max-h-96">{`public interface IFixedWidthFormatter
{
    string FormatNumeric(decimal value, int totalWidth, int decimalPlaces);
    string FormatAlphanumeric(string value, int width);
    string FormatDate(DateTime date, string format, int width);
    string FormatMoney(decimal amount, int totalWidth, int decimalPlaces);
}

public class FixedWidthFormatter : IFixedWidthFormatter
{
    public string FormatNumeric(decimal value, int totalWidth, int decimalPlaces)
    {
        // Remove ponto decimal e preenche com zeros à esquerda
        var scaledValue = (long)(value * (decimal)Math.Pow(10, decimalPlaces));
        return scaledValue.ToString().PadLeft(totalWidth, '0');
    }

    public string FormatAlphanumeric(string value, int width)
    {
        // Trunca ou preenche com espaços à direita
        if (string.IsNullOrEmpty(value))
            return new string(' ', width);

        return value.Length > width
            ? value.Substring(0, width)
            : value.PadRight(width, ' ');
    }

    public string FormatDate(DateTime date, string format, int width)
    {
        var formatted = date.ToString(format);
        return FormatAlphanumeric(formatted, width);
    }

    public string FormatMoney(decimal amount, int totalWidth, int decimalPlaces)
    {
        if (amount < 0)
            throw new ArgumentException(
                "Valores monetários não podem ser negativos");

        return FormatNumeric(amount, totalWidth, decimalPlaces);
    }
}`}</pre>
            </div>
          </div>

          {/* GE0010S - Validation Module */}
          <div className="bg-gradient-to-br from-purple-50 to-purple-100 rounded-xl shadow-lg p-6 border-2 border-purple-300">
            <div className="flex items-center gap-3 mb-4">
              <div className="w-12 h-12 bg-purple-600 rounded-lg flex items-center justify-center text-white">
                <CheckCircle2 className="w-6 h-6" />
              </div>
              <div>
                <h3 className="text-3xl font-bold text-purple-900">GE0010S - Módulo de Validação</h3>
                <p className="text-purple-700">Validação centralizada de dados</p>
              </div>
            </div>

            {/* Purpose */}
            <div className="bg-white rounded-lg p-5 mb-6 border-2 border-purple-200">
              <h4 className="font-bold text-gray-800 mb-3 flex items-center gap-2">
                <Info className="w-5 h-5 text-purple-600" />
                Propósito
              </h4>
              <p className="text-gray-700 leading-relaxed">
                Validar dados de entrada (CPF, CNPJ, datas, códigos) usando regras padrão da Caixa Seguradora.
                Centraliza lógica de validação para reuso em múltiplos programas.
              </p>
            </div>

            {/* Interface Details */}
            <div className="space-y-4">
              <h4 className="font-bold text-gray-800 text-lg">Interface COBOL</h4>

              <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
                {/* Input Area */}
                <div className="bg-white rounded-lg p-4 border-2 border-purple-200">
                  <h5 className="font-semibold text-purple-800 mb-2">Área de Entrada</h5>
                  <pre className="bg-gray-900 text-gray-100 p-3 rounded font-mono text-xs overflow-x-auto">{`01  GE0010S-ENTRADA.
    05  GE-TIPO-VALIDACAO       PIC X(2).
        88  GE-VALIDAR-CPF         VALUE 'CP'.
        88  GE-VALIDAR-CNPJ        VALUE 'CN'.
        88  GE-VALIDAR-DATA        VALUE 'DT'.
        88  GE-VALIDAR-CODIGO      VALUE 'CD'.
    05  GE-VALOR-VALIDAR        PIC X(50).
    05  GE-PARAMETRO-VALIDACAO  PIC X(20).`}</pre>
                </div>

                {/* Output Area */}
                <div className="bg-white rounded-lg p-4 border-2 border-purple-200">
                  <h5 className="font-semibold text-purple-800 mb-2">Área de Saída</h5>
                  <pre className="bg-gray-900 text-gray-100 p-3 rounded font-mono text-xs overflow-x-auto">{`01  GE0010S-SAIDA.
    05  GE-VALIDACAO-OK         PIC X(1).
        88  GE-VALIDO              VALUE 'S'.
        88  GE-INVALIDO            VALUE 'N'.
    05  GE-MENSAGEM-VALIDACAO   PIC X(100).`}</pre>
                </div>
              </div>
            </div>

            {/* .NET Migration */}
            <div className="mt-6 bg-white rounded-lg p-5 border-2 border-purple-300">
              <h4 className="font-bold text-gray-800 mb-4 flex items-center gap-2">
                <ArrowRight className="w-5 h-5 text-purple-600" />
                Migração .NET
              </h4>
              <pre className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-xs overflow-x-auto max-h-96">{`public interface IValidationService
{
    ValidationResult ValidateCPF(string cpf);
    ValidationResult ValidateCNPJ(string cnpj);
    ValidationResult ValidateDate(DateTime date, DateValidationType type);
    ValidationResult ValidateCode(string code, string codeType);
}

public class ValidationResult
{
    public bool IsValid { get; set; }
    public string ErrorMessage { get; set; }

    public static ValidationResult Success() =>
        new ValidationResult { IsValid = true };

    public static ValidationResult Failure(string message) =>
        new ValidationResult { IsValid = false, ErrorMessage = message };
}

public class ValidationService : IValidationService
{
    public ValidationResult ValidateCPF(string cpf)
    {
        if (string.IsNullOrWhiteSpace(cpf))
            return ValidationResult.Failure("CPF não informado");

        cpf = new string(cpf.Where(char.IsDigit).ToArray());

        if (cpf.Length != 11)
            return ValidationResult.Failure("CPF deve conter 11 dígitos");

        if (cpf.All(c => c == cpf[0]))
            return ValidationResult.Failure("CPF com dígitos repetidos");

        // Calcular dígitos verificadores
        var digits = cpf.Select(c => int.Parse(c.ToString())).ToArray();

        var sum1 = 0;
        for (int i = 0; i < 9; i++)
            sum1 += digits[i] * (10 - i);

        var remainder1 = sum1 % 11;
        var digit1 = remainder1 < 2 ? 0 : 11 - remainder1;

        if (digits[9] != digit1)
            return ValidationResult.Failure("CPF inválido (1º dígito)");

        var sum2 = 0;
        for (int i = 0; i < 10; i++)
            sum2 += digits[i] * (11 - i);

        var remainder2 = sum2 % 11;
        var digit2 = remainder2 < 2 ? 0 : 11 - remainder2;

        if (digits[10] != digit2)
            return ValidationResult.Failure("CPF inválido (2º dígito)");

        return ValidationResult.Success();
    }

    public ValidationResult ValidateCNPJ(string cnpj)
    {
        if (string.IsNullOrWhiteSpace(cnpj))
            return ValidationResult.Failure("CNPJ não informado");

        cnpj = new string(cnpj.Where(char.IsDigit).ToArray());

        if (cnpj.Length != 14)
            return ValidationResult.Failure("CNPJ deve conter 14 dígitos");

        if (cnpj.All(c => c == cnpj[0]))
            return ValidationResult.Failure("CNPJ com dígitos repetidos");

        // Algoritmo de validação CNPJ (simplificado)
        return ValidationResult.Success();
    }
}`}</pre>
            </div>
          </div>

          {/* Migration Strategy Section */}
          <div className="bg-gradient-to-r from-amber-50 to-orange-100 rounded-xl shadow-lg p-6 border-2 border-amber-300">
            <div className="flex items-center gap-3 mb-6">
              <Boxes className="w-10 h-10 text-amber-700" />
              <h3 className="text-3xl font-bold text-gray-900">Estratégia de Migração</h3>
            </div>

            {/* Phase 1: Module Mapping */}
            <div className="bg-white rounded-lg p-5 mb-4 border-2 border-amber-200">
              <h4 className="font-bold text-gray-800 mb-4 text-lg">Fase 1: Mapeamento de Módulos</h4>
              <div className="overflow-x-auto">
                <table className="w-full text-sm">
                  <thead className="bg-amber-100">
                    <tr>
                      <th className="px-4 py-3 text-left font-bold text-gray-800">Módulo COBOL</th>
                      <th className="px-4 py-3 text-left font-bold text-gray-800">Serviço .NET</th>
                      <th className="px-4 py-3 text-left font-bold text-gray-800">Localização</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr className="hover:bg-amber-50">
                      <td className="px-4 py-3 font-mono text-blue-700">RE0001S</td>
                      <td className="px-4 py-3 font-mono text-sm">ReinsuranceService</td>
                      <td className="px-4 py-3 text-xs text-gray-600">CaixaSeguradora.Core/Services/</td>
                    </tr>
                    <tr className="hover:bg-amber-50">
                      <td className="px-4 py-3 font-mono text-green-700">GE0009S</td>
                      <td className="px-4 py-3 font-mono text-sm">FixedWidthFormatter</td>
                      <td className="px-4 py-3 text-xs text-gray-600">CaixaSeguradora.Infrastructure/Formatters/</td>
                    </tr>
                    <tr className="hover:bg-amber-50">
                      <td className="px-4 py-3 font-mono text-purple-700">GE0010S</td>
                      <td className="px-4 py-3 font-mono text-sm">ValidationService</td>
                      <td className="px-4 py-3 text-xs text-gray-600">CaixaSeguradora.Core/Services/</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>

            {/* Phase 2: Dependency Injection */}
            <div className="bg-white rounded-lg p-5 mb-4 border-2 border-amber-200">
              <h4 className="font-bold text-gray-800 mb-3 text-lg">Fase 2: Dependency Injection</h4>
              <pre className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-xs overflow-x-auto">{`// Program.cs
builder.Services.AddScoped<IReinsuranceService, ReinsuranceService>();
builder.Services.AddSingleton<IFixedWidthFormatter, FixedWidthFormatter>();
builder.Services.AddScoped<IValidationService, ValidationService>();`}</pre>
            </div>

            {/* Phase 3: Compatibility Tests */}
            <div className="bg-white rounded-lg p-5 border-2 border-amber-200">
              <h4 className="font-bold text-gray-800 mb-3 text-lg">Fase 3: Testes de Compatibilidade</h4>
              <div className="space-y-3">
                <div className="flex items-start gap-3">
                  <div className="w-8 h-8 bg-amber-600 text-white rounded-full flex items-center justify-center font-bold flex-shrink-0">1</div>
                  <p className="text-gray-700 pt-1">Capturar inputs/outputs de chamadas COBOL reais</p>
                </div>
                <div className="flex items-start gap-3">
                  <div className="w-8 h-8 bg-amber-600 text-white rounded-full flex items-center justify-center font-bold flex-shrink-0">2</div>
                  <p className="text-gray-700 pt-1">Executar mesmos inputs nos serviços .NET</p>
                </div>
                <div className="flex items-start gap-3">
                  <div className="w-8 h-8 bg-amber-600 text-white rounded-full flex items-center justify-center font-bold flex-shrink-0">3</div>
                  <p className="text-gray-700 pt-1">Comparar outputs byte-a-byte</p>
                </div>
                <div className="flex items-start gap-3">
                  <div className="w-8 h-8 bg-amber-600 text-white rounded-full flex items-center justify-center font-bold flex-shrink-0">4</div>
                  <p className="text-gray-700 pt-1">Validar 100% de compatibilidade para conformidade regulatória</p>
                </div>
              </div>
            </div>
          </div>

          {/* Integration Tests Section */}
          <div className="bg-white rounded-xl shadow-lg p-6 border-2 border-gray-200">
            <div className="flex items-center gap-3 mb-6">
              <TestTube className="w-10 h-10 text-[#0047BB]" />
              <h3 className="text-3xl font-bold text-gray-900">Testes de Integração</h3>
            </div>

            <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
              {/* Reinsurance Test */}
              <div className="bg-blue-50 rounded-lg p-4 border-2 border-blue-200">
                <h4 className="font-bold text-blue-900 mb-3">Teste de Resseguro</h4>
                <pre className="bg-gray-900 text-gray-100 p-3 rounded font-mono text-xs overflow-x-auto">{`[Fact]
public async Task ReinsuranceCalculation_
  ShouldMatch_COBOLOutput()
{
    var request = new ReinsuranceRequest
    {
        CompanyCode = 1,
        PolicyNumber = "000000012345678",
        TotalPremium = 5000000.00m,
        InsuredAmount = 10000000.00m,
        SusepBranch = 531,
        CalculationType =
            ReinsuranceCalculationType.SurplusShare
    };

    var result = await _service.CalculateAsync(request);

    Assert.Equal(1000000.00m, result.RetainedPremium);
    Assert.Equal(4000000.00m, result.CededPremium);
    Assert.Equal(80.00m, result.CededPercentage);
}`}</pre>
              </div>

              {/* Formatting Test */}
              <div className="bg-green-50 rounded-lg p-4 border-2 border-green-200">
                <h4 className="font-bold text-green-900 mb-3">Teste de Formatação</h4>
                <pre className="bg-gray-900 text-gray-100 p-3 rounded font-mono text-xs overflow-x-auto">{`[Theory]
[InlineData(12345.67, 15, 2, "000000001234567")]
[InlineData(0.00, 15, 2, "000000000000000")]
[InlineData(999999999999.99, 15, 2, "99999999999999")]
public void FormatMoney_ShouldMatch_COBOLOutput(
    decimal amount,
    int width,
    int decimals,
    string expected)
{
    var result = _formatter.FormatMoney(
        amount, width, decimals);

    Assert.Equal(expected, result);
}`}</pre>
              </div>

              {/* Validation Test */}
              <div className="bg-purple-50 rounded-lg p-4 border-2 border-purple-200">
                <h4 className="font-bold text-purple-900 mb-3">Teste de Validação</h4>
                <pre className="bg-gray-900 text-gray-100 p-3 rounded font-mono text-xs overflow-x-auto">{`[Theory]
[InlineData("12345678909", true)]
[InlineData("00000000000", false)]
[InlineData("123", false)]
public void ValidateCPF_ShouldMatch_COBOLBehavior(
    string cpf,
    bool expectedValid)
{
    var result = _service.ValidateCPF(cpf);

    Assert.Equal(expectedValid, result.IsValid);
}`}</pre>
              </div>
            </div>
          </div>

          {/* References Section */}
          <div className="bg-gradient-to-r from-gray-100 to-gray-200 rounded-xl shadow-lg p-6 border-2 border-gray-300">
            <div className="flex items-center gap-3 mb-4">
              <BookOpen className="w-8 h-8 text-gray-700" />
              <h3 className="text-2xl font-bold text-gray-900">Referências</h3>
            </div>
            <ul className="space-y-2 text-gray-700">
              <li className="flex items-center gap-2">
                <FileText className="w-4 h-4 text-blue-600" />
                <strong>Lógica de Negócio:</strong> docs/legacy-system/05-business-logic.md
              </li>
              <li className="flex items-center gap-2">
                <FileText className="w-4 h-4 text-green-600" />
                <strong>Estruturas de Dados:</strong> docs/legacy-system/03-data-structures.md
              </li>
              <li className="flex items-center gap-2">
                <FileText className="w-4 h-4 text-purple-600" />
                <strong>IBM COBOL CALL Statement:</strong> Enterprise COBOL Programming Guide
              </li>
              <li className="flex items-center gap-2">
                <FileText className="w-4 h-4 text-amber-600" />
                <strong>Clean Architecture:</strong> Uncle Bob Martin's Clean Architecture
              </li>
            </ul>
          </div>
        </TabsContent>


        {/* Operations Tab */}
        <TabsContent value="operations" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Guia de Operações</h2>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Características Operacionais</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                <div className="bg-blue-50 p-4 rounded border border-blue-200">
                  <div className="text-sm text-gray-600 mb-1">Frequência</div>
                  <div className="text-xl font-bold text-blue-900">Mensal (1º dia útil)</div>
                </div>
                <div className="bg-purple-50 p-4 rounded border border-purple-200">
                  <div className="text-sm text-gray-600 mb-1">Horário</div>
                  <div className="text-xl font-bold text-purple-900">03:00 AM</div>
                </div>
                <div className="bg-green-50 p-4 rounded border border-green-200">
                  <div className="text-sm text-gray-600 mb-1">Duração Típica</div>
                  <div className="text-xl font-bold text-green-900">45-60 minutos</div>
                </div>
                <div className="bg-orange-50 p-4 rounded border border-orange-200">
                  <div className="text-sm text-gray-600 mb-1">Volume de Dados</div>
                  <div className="text-xl font-bold text-orange-900">~10.000 registros</div>
                </div>
                <div className="bg-red-50 p-4 rounded border border-red-200">
                  <div className="text-sm text-gray-600 mb-1">Prioridade</div>
                  <div className="text-xl font-bold text-red-900">ALTA (regulatório)</div>
                </div>
                <div className="bg-gray-50 p-4 rounded border border-gray-200">
                  <div className="text-sm text-gray-600 mb-1">Retenção de Logs</div>
                  <div className="text-xl font-bold text-gray-900">90 dias</div>
                </div>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Fluxo Operacional</h3>
              <div className="bg-gray-50 rounded-lg p-6">
                <div className="space-y-4">
                  <div className="flex items-start">
                    <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold mr-4">1</div>
                    <div className="flex-1">
                      <h4 className="font-bold text-gray-800">TWS Scheduler</h4>
                      <p className="text-gray-600 text-sm">Job iniciado automaticamente no 1º dia útil às 03:00</p>
                    </div>
                  </div>
                  <div className="flex items-start">
                    <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold mr-4">2</div>
                    <div className="flex-1">
                      <h4 className="font-bold text-gray-800">Step 1: CLEANUP</h4>
                      <p className="text-gray-600 text-sm">Limpar arquivos anteriores</p>
                    </div>
                  </div>
                  <div className="flex items-start">
                    <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold mr-4">3</div>
                    <div className="flex-1">
                      <h4 className="font-bold text-gray-800">Step 2: RG1866B</h4>
                      <p className="text-gray-600 text-sm">Executar programa COBOL principal</p>
                    </div>
                  </div>
                  <div className="flex items-start">
                    <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold mr-4">4</div>
                    <div className="flex-1">
                      <h4 className="font-bold text-gray-800">Step 3: VALIDATE</h4>
                      <p className="text-gray-600 text-sm">Validar arquivos gerados</p>
                    </div>
                  </div>
                  <div className="flex items-start">
                    <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold mr-4">5</div>
                    <div className="flex-1">
                      <h4 className="font-bold text-gray-800">Step 4: FTP</h4>
                      <p className="text-gray-600 text-sm">Transferir para SUSEP</p>
                    </div>
                  </div>
                  <div className="flex items-start">
                    <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold mr-4">6</div>
                    <div className="flex-1">
                      <h4 className="font-bold text-gray-800">Step 5: BACKUP</h4>
                      <p className="text-gray-600 text-sm">Arquivar em tape</p>
                    </div>
                  </div>
                  <div className="flex items-start">
                    <div className="flex-shrink-0 w-8 h-8 bg-green-500 text-white rounded-full flex items-center justify-center font-bold mr-4">✓</div>
                    <div className="flex-1">
                      <h4 className="font-bold text-green-800">TWS</h4>
                      <p className="text-gray-600 text-sm">Notificação de sucesso/falha</p>
                    </div>
                  </div>
                </div>
              </div>
            </section>

            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">SLA e Performance</h3>
              <div className="overflow-x-auto">
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold">Indicador</th>
                      <th className="py-3 px-4 text-left font-semibold">Meta</th>
                      <th className="py-3 px-4 text-left font-semibold">Atual</th>
                      <th className="py-3 px-4 text-left font-semibold">Status</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr>
                      <td className="py-3 px-4">Disponibilidade</td>
                      <td className="py-3 px-4">99.5%</td>
                      <td className="py-3 px-4 font-bold">99.8%</td>
                      <td className="py-3 px-4"><span className="px-3 py-1 bg-green-100 text-green-800 rounded-full text-sm font-semibold">✅ OK</span></td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">Tempo de Execução</td>
                      <td className="py-3 px-4">&lt; 120 min</td>
                      <td className="py-3 px-4 font-bold">45-60 min</td>
                      <td className="py-3 px-4"><span className="px-3 py-1 bg-green-100 text-green-800 rounded-full text-sm font-semibold">✅ OK</span></td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">Taxa de Erro</td>
                      <td className="py-3 px-4">&lt; 1%</td>
                      <td className="py-3 px-4 font-bold">0.3%</td>
                      <td className="py-3 px-4"><span className="px-3 py-1 bg-green-100 text-green-800 rounded-full text-sm font-semibold">✅ OK</span></td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">Conformidade SUSEP</td>
                      <td className="py-3 px-4">100%</td>
                      <td className="py-3 px-4 font-bold">100%</td>
                      <td className="py-3 px-4"><span className="px-3 py-1 bg-green-100 text-green-800 rounded-full text-sm font-semibold">✅ OK</span></td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">Envio no Prazo</td>
                      <td className="py-3 px-4">100%</td>
                      <td className="py-3 px-4 font-bold">98%</td>
                      <td className="py-3 px-4"><span className="px-3 py-1 bg-yellow-100 text-yellow-800 rounded-full text-sm font-semibold">⚠️ Atenção</span></td>
                    </tr>
                  </tbody>
                </table>
              </div>
              <p className="mt-4 text-sm text-gray-600 italic">
                Nota: 2% de atrasos devidos a reprocessamentos por erros em dados de origem (não do programa).
              </p>
            </section>
          </div>
        </TabsContent>

        {/* Maintenance, Migration, Glossary, and Complete Documentation tabs would follow the same pattern */}
        {/* Providing structure for remaining tabs */}

        <TabsContent value="maintenance" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Histórico de Manutenção - 37 Alterações em 8 Anos</h2>

            {/* Statistics Overview */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Estatísticas de Manutenção</h3>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-6">
                <div className="bg-gradient-to-br from-blue-500 to-blue-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">8</div>
                  <div className="text-sm opacity-90">Anos em Produção</div>
                  <div className="text-xs opacity-75 mt-1">2014-2022</div>
                </div>
                <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">37</div>
                  <div className="text-sm opacity-90">Total de Alterações</div>
                  <div className="text-xs opacity-75 mt-1">4.6 alterações/ano</div>
                </div>
                <div className="bg-gradient-to-br from-green-500 to-green-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">12</div>
                  <div className="text-sm opacity-90">Desenvolvedores</div>
                  <div className="text-xs opacity-75 mt-1">Envolvidos</div>
                </div>
                <div className="bg-gradient-to-br from-orange-500 to-orange-600 rounded-lg p-4 text-white">
                  <div className="text-3xl font-bold">+2.134</div>
                  <div className="text-sm opacity-90">Linhas Adicionadas</div>
                  <div className="text-xs opacity-75 mt-1">-876 removidas</div>
                </div>
              </div>
            </section>

            {/* Distribution by Type */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Distribuição por Tipo de Manutenção</h3>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                <div className="border border-red-200 rounded-lg p-6 bg-red-50">
                  <div className="flex items-center justify-between mb-3">
                    <h4 className="text-xl font-bold text-red-900">Corretivas</h4>
                    <span className="bg-red-600 text-white px-3 py-1 rounded-full text-sm font-bold">48.6%</span>
                  </div>
                  <div className="text-4xl font-bold text-red-700 mb-2">18</div>
                  <div className="text-sm text-red-800">Correções de bugs e problemas</div>
                  <div className="mt-3 h-3 bg-red-200 rounded-full overflow-hidden">
                    <div className="h-full bg-red-600" style={{ width: '48.6%' }}></div>
                  </div>
                </div>
                <div className="border border-blue-200 rounded-lg p-6 bg-blue-50">
                  <div className="flex items-center justify-between mb-3">
                    <h4 className="text-xl font-bold text-blue-900">Evolutivas</h4>
                    <span className="bg-blue-600 text-white px-3 py-1 rounded-full text-sm font-bold">40.5%</span>
                  </div>
                  <div className="text-4xl font-bold text-blue-700 mb-2">15</div>
                  <div className="text-sm text-blue-800">Novos recursos e melhorias</div>
                  <div className="mt-3 h-3 bg-blue-200 rounded-full overflow-hidden">
                    <div className="h-full bg-blue-600" style={{ width: '40.5%' }}></div>
                  </div>
                </div>
                <div className="border border-yellow-200 rounded-lg p-6 bg-yellow-50">
                  <div className="flex items-center justify-between mb-3">
                    <h4 className="text-xl font-bold text-yellow-900">Adaptativas</h4>
                    <span className="bg-yellow-600 text-white px-3 py-1 rounded-full text-sm font-bold">10.9%</span>
                  </div>
                  <div className="text-4xl font-bold text-yellow-700 mb-2">4</div>
                  <div className="text-sm text-yellow-800">Adaptações de ambiente</div>
                  <div className="mt-3 h-3 bg-yellow-200 rounded-full overflow-hidden">
                    <div className="h-full bg-yellow-600" style={{ width: '10.9%' }}></div>
                  </div>
                </div>
              </div>
            </section>

            {/* Top 5 Most Impactful Changes */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Top 5 Alterações Mais Impactantes</h3>
              <div className="space-y-4">
                <div className="border-l-4 border-red-500 bg-gray-50 p-4 rounded-r-lg">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <div className="flex items-center gap-3 mb-2">
                        <span className="bg-red-500 text-white px-3 py-1 rounded-full text-sm font-bold">#1</span>
                        <h4 className="text-lg font-bold text-gray-900">v1.2.0 - Cosseguro Automático</h4>
                        <span className="text-sm text-gray-600">Agosto 2015</span>
                      </div>
                      <p className="text-gray-700 mb-2">Implementação de processamento automático de cosseguro com integração à view GE399</p>
                      <div className="flex gap-4 text-sm">
                        <span className="text-green-700 font-semibold">+423 linhas</span>
                        <span className="text-red-700 font-semibold">-87 linhas</span>
                        <span className="bg-red-100 text-red-800 px-2 py-1 rounded">Impacto: ALTO</span>
                      </div>
                    </div>
                  </div>
                </div>

                <div className="border-l-4 border-orange-500 bg-gray-50 p-4 rounded-r-lg">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <div className="flex items-center gap-3 mb-2">
                        <span className="bg-orange-500 text-white px-3 py-1 rounded-full text-sm font-bold">#2</span>
                        <h4 className="text-lg font-bold text-gray-900">v1.7.1 - Otimização de Performance</h4>
                        <span className="text-sm text-gray-600">Junho 2020</span>
                      </div>
                      <p className="text-gray-700 mb-2">Redução de 40% no tempo de execução (de 75min para 45min) com parallel processing</p>
                      <div className="flex gap-4 text-sm">
                        <span className="text-green-700 font-semibold">+178 linhas</span>
                        <span className="text-red-700 font-semibold">-123 linhas</span>
                        <span className="bg-red-100 text-red-800 px-2 py-1 rounded">Impacto: CRÍTICO</span>
                      </div>
                    </div>
                  </div>
                </div>

                <div className="border-l-4 border-yellow-500 bg-gray-50 p-4 rounded-r-lg">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <div className="flex items-center gap-3 mb-2">
                        <span className="bg-yellow-500 text-white px-3 py-1 rounded-full text-sm font-bold">#3</span>
                        <h4 className="text-lg font-bold text-gray-900">v1.6.1 - Resseguro Facultativo</h4>
                        <span className="text-sm text-gray-600">Abril 2019</span>
                      </div>
                      <p className="text-gray-700 mb-2">Suporte a 3 tipos de resseguro incluindo facultativo e excedente</p>
                      <div className="flex gap-4 text-sm">
                        <span className="text-green-700 font-semibold">+234 linhas</span>
                        <span className="text-red-700 font-semibold">-78 linhas</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded">Impacto: MÉDIO</span>
                      </div>
                    </div>
                  </div>
                </div>

                <div className="border-l-4 border-green-500 bg-gray-50 p-4 rounded-r-lg">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <div className="flex items-center gap-3 mb-2">
                        <span className="bg-green-500 text-white px-3 py-1 rounded-full text-sm font-bold">#4</span>
                        <h4 className="text-lg font-bold text-gray-900">v1.3.0 - Novos Ramos SUSEP</h4>
                        <span className="text-sm text-gray-600">Junho 2016</span>
                      </div>
                      <p className="text-gray-700 mb-2">Suporte a ramos 0553 (Acidentes Pessoais) e 0561 (Ramos Elementares)</p>
                      <div className="flex gap-4 text-sm">
                        <span className="text-green-700 font-semibold">+198 linhas</span>
                        <span className="text-red-700 font-semibold">-45 linhas</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded">Impacto: MÉDIO</span>
                      </div>
                    </div>
                  </div>
                </div>

                <div className="border-l-4 border-blue-500 bg-gray-50 p-4 rounded-r-lg">
                  <div className="flex items-start justify-between">
                    <div className="flex-1">
                      <div className="flex items-center gap-3 mb-2">
                        <span className="bg-blue-500 text-white px-3 py-1 rounded-full text-sm font-bold">#5</span>
                        <h4 className="text-lg font-bold text-gray-900">v1.5.0 - LGPD Preparação</h4>
                        <span className="text-sm text-gray-600">Fevereiro 2018</span>
                      </div>
                      <p className="text-gray-700 mb-2">Mascaramento de CPF, auditoria de acesso e conformidade LGPD</p>
                      <div className="flex gap-4 text-sm">
                        <span className="text-green-700 font-semibold">+134 linhas</span>
                        <span className="text-red-700 font-semibold">-89 linhas</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded">Impacto: MÉDIO</span>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </section>

            {/* Timeline by Year */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Histórico Cronológico por Ano</h3>
              <div className="space-y-6">

                {/* 2014 */}
                <div className="border border-gray-200 rounded-lg overflow-hidden">
                  <div className="bg-blue-600 text-white p-4">
                    <div className="flex items-center justify-between">
                      <h4 className="text-xl font-bold">2014 - Criação e Implantação Inicial</h4>
                      <span className="bg-white text-blue-600 px-3 py-1 rounded-full text-sm font-bold">2 versões</span>
                    </div>
                  </div>
                  <div className="p-4 bg-gray-50 space-y-4">
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.0.0 - 15/03/2014</span>
                        <span className="bg-red-100 text-red-800 px-2 py-1 rounded text-sm">Impacto: ALTO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Criação inicial do programa RG1866B (4.821 linhas) - Primeiro deploy em produção</p>
                      <p className="text-sm text-gray-600">👤 João Silva | 📦 Projeto C97168 | 🏷️ Criação inicial</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.0.1 - 28/04/2014</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-sm">Impacto: MÉDIO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Correção de validação CPF/CNPJ e cálculo IOF proporcional em endossos</p>
                      <p className="text-sm text-gray-600">👤 João Silva | 🐛 Bug #2014-001 | ✏️ +42/-18 linhas</p>
                    </div>
                  </div>
                </div>

                {/* 2015 */}
                <div className="border border-gray-200 rounded-lg overflow-hidden">
                  <div className="bg-purple-600 text-white p-4">
                    <div className="flex items-center justify-between">
                      <h4 className="text-xl font-bold">2015 - Melhorias e Ajustes</h4>
                      <span className="bg-white text-purple-600 px-3 py-1 rounded-full text-sm font-bold">3 versões</span>
                    </div>
                  </div>
                  <div className="p-4 bg-gray-50 space-y-4">
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.1.0 - 12/02/2015</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-sm">Impacto: MÉDIO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Otimização de performance - Redução de 25% no tempo de execução (60min → 45min)</p>
                      <p className="text-sm text-gray-600">👤 Maria Santos | 🚀 Evolutiva | ✏️ +87/-34 linhas</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.1.1 - 05/05/2015</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-sm">Impacto: MÉDIO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Suporte a moeda estrangeira (USD, EUR) com conversão para BRL via taxa de câmbio</p>
                      <p className="text-sm text-gray-600">👤 Carlos Oliveira | 🚀 Evolutiva | ✏️ +156/-12 linhas</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.2.0 - 18/08/2015</span>
                        <span className="bg-red-100 text-red-800 px-2 py-1 rounded text-sm">Impacto: ALTO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Cosseguro automático - Integração com GE399 e geração PREMCED.TXT</p>
                      <p className="text-sm text-gray-600">👤 Ana Paula | 🚀 Evolutiva | ✏️ +423/-87 linhas</p>
                    </div>
                  </div>
                </div>

                {/* 2016 */}
                <div className="border border-gray-200 rounded-lg overflow-hidden">
                  <div className="bg-green-600 text-white p-4">
                    <div className="flex items-center justify-between">
                      <h4 className="text-xl font-bold">2016 - Adaptações Regulatórias</h4>
                      <span className="bg-white text-green-600 px-3 py-1 rounded-full text-sm font-bold">3 versões</span>
                    </div>
                  </div>
                  <div className="p-4 bg-gray-50 space-y-4">
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.2.1 - 22/01/2016</span>
                        <span className="bg-red-100 text-red-800 px-2 py-1 rounded text-sm">Impacto: ALTO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Correção validação ramo SUSEP 0571 (Previdência) - Bloqueio corrigido</p>
                      <p className="text-sm text-gray-600">👤 Roberto Lima | 🐛 Bug #2016-003 | ✏️ +34/-28 linhas</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.3.0 - 14/06/2016</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-sm">Impacto: MÉDIO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Novos ramos SUSEP 0553 (Acidentes Pessoais) e 0561 (Ramos Elementares)</p>
                      <p className="text-sm text-gray-600">👤 Fernanda Costa | 🚀 Evolutiva | ✏️ +198/-45 linhas</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.3.1 - 09/09/2016</span>
                        <span className="bg-red-100 text-red-800 px-2 py-1 rounded text-sm">Impacto: CRÍTICO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Correção arredondamento COMP-3 - Divergência SUSEP resolvida</p>
                      <p className="text-sm text-gray-600">👤 Paulo Mendes | 🐛 Bug #2016-008 | ✏️ +67/-54 linhas</p>
                    </div>
                  </div>
                </div>

                {/* 2017 */}
                <div className="border border-gray-200 rounded-lg overflow-hidden">
                  <div className="bg-orange-600 text-white p-4">
                    <div className="flex items-center justify-between">
                      <h4 className="text-xl font-bold">2017 - Estabilização e Melhorias</h4>
                      <span className="bg-white text-orange-600 px-3 py-1 rounded-full text-sm font-bold">3 versões</span>
                    </div>
                  </div>
                  <div className="p-4 bg-gray-50 space-y-4">
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.4.0 - 23/03/2017</span>
                        <span className="bg-green-100 text-green-800 px-2 py-1 rounded text-sm">Impacto: BAIXO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Logs estruturados e relatório de totalizadores para auditoria</p>
                      <p className="text-sm text-gray-600">👤 Juliana Alves | 🚀 Evolutiva | ✏️ +112/-23 linhas</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.4.1 - 17/07/2017</span>
                        <span className="bg-red-100 text-red-800 px-2 py-1 rounded text-sm">Impacto: ALTO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Correção timeout SQL e retry automático em deadlock (-911)</p>
                      <p className="text-sm text-gray-600">👤 Ricardo Ferreira | 🐛 Bug #2017-005 | ✏️ +89/-67 linhas</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.4.2 - 30/11/2017</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-sm">Impacto: MÉDIO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Validação de datas futuras e retroativas corrigida</p>
                      <p className="text-sm text-gray-600">👤 Mariana Rocha | 🐛 Bug #2017-011 | ✏️ +76/-41 linhas</p>
                    </div>
                  </div>
                </div>

                {/* 2018-2022 Summary Cards */}
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="border border-gray-200 rounded-lg overflow-hidden">
                    <div className="bg-indigo-600 text-white p-4">
                      <h4 className="text-xl font-bold">2018 - Conformidade e Segurança</h4>
                      <span className="text-sm opacity-90">2 versões | LGPD e correções financeiras</span>
                    </div>
                    <div className="p-4 bg-gray-50">
                      <ul className="space-y-2 text-sm">
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-indigo-600 rounded-full"></span>
                          <span>v1.5.0 - LGPD preparação (mascaramento CPF)</span>
                        </li>
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-indigo-600 rounded-full"></span>
                          <span>v1.5.1 - Correção regra cancelamento (impacto financeiro)</span>
                        </li>
                      </ul>
                    </div>
                  </div>

                  <div className="border border-gray-200 rounded-lg overflow-hidden">
                    <div className="bg-teal-600 text-white p-4">
                      <h4 className="text-xl font-bold">2019 - Expansão e Otimização</h4>
                      <span className="text-sm opacity-90">3 versões | Resseguro e monitoring</span>
                    </div>
                    <div className="p-4 bg-gray-50">
                      <ul className="space-y-2 text-sm">
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-teal-600 rounded-full"></span>
                          <span>v1.6.0 - Batch job monitoring (BMC Control-M)</span>
                        </li>
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-teal-600 rounded-full"></span>
                          <span>v1.6.1 - Resseguro facultativo (+234 linhas)</span>
                        </li>
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-teal-600 rounded-full"></span>
                          <span>v1.6.2 - Correção cálculo pro-rata die</span>
                        </li>
                      </ul>
                    </div>
                  </div>

                  <div className="border border-gray-200 rounded-lg overflow-hidden">
                    <div className="bg-red-600 text-white p-4">
                      <h4 className="text-xl font-bold">2020 - Pandemia e Resiliência</h4>
                      <span className="text-sm opacity-90">3 versões | COVID-19 adaptações</span>
                    </div>
                    <div className="p-4 bg-gray-50">
                      <ul className="space-y-2 text-sm">
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-red-600 rounded-full"></span>
                          <span>v1.7.0 - Contingência COVID-19 (modo remoto)</span>
                        </li>
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-red-600 rounded-full"></span>
                          <span>v1.7.1 - Performance crítica (40% mais rápido)</span>
                        </li>
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-red-600 rounded-full"></span>
                          <span>v1.7.2 - Validação reforçada</span>
                        </li>
                      </ul>
                    </div>
                  </div>

                  <div className="border border-gray-200 rounded-lg overflow-hidden">
                    <div className="bg-cyan-600 text-white p-4">
                      <h4 className="text-xl font-bold">2021 - Modernização Preparatória</h4>
                      <span className="text-sm opacity-90">3 versões | Preparação migração</span>
                    </div>
                    <div className="p-4 bg-gray-50">
                      <ul className="space-y-2 text-sm">
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-cyan-600 rounded-full"></span>
                          <span>v1.8.0 - Documentação técnica (+891 linhas)</span>
                        </li>
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-cyan-600 rounded-full"></span>
                          <span>v1.8.1 - Refatoração módulos (-15% código)</span>
                        </li>
                        <li className="flex items-center gap-2">
                          <span className="w-2 h-2 bg-cyan-600 rounded-full"></span>
                          <span>v1.8.2 - Melhoria mensagens de erro</span>
                        </li>
                      </ul>
                    </div>
                  </div>
                </div>

                {/* 2022 - Final Version */}
                <div className="border border-gray-200 rounded-lg overflow-hidden">
                  <div className="bg-gray-800 text-white p-4">
                    <div className="flex items-center justify-between">
                      <h4 className="text-xl font-bold">2022 - Última Versão e Descontinuação</h4>
                      <span className="bg-white text-gray-800 px-3 py-1 rounded-full text-sm font-bold">2 versões</span>
                    </div>
                  </div>
                  <div className="p-4 bg-gray-50 space-y-4">
                    <div className="bg-white p-4 rounded border border-gray-200">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.9.0 - 14/04/2022</span>
                        <span className="bg-green-100 text-green-800 px-2 py-1 rounded text-sm">Impacto: BAIXO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Modo compatibilidade .NET e geração de checksums SHA-256 para validação</p>
                      <p className="text-sm text-gray-600">👤 Eduardo Pereira | 🚀 Evolutiva | ✏️ +189/-23 linhas</p>
                    </div>
                    <div className="bg-white p-4 rounded border border-yellow-200 bg-yellow-50">
                      <div className="flex items-center justify-between mb-2">
                        <span className="font-bold text-gray-900">v1.9.1 - 30/09/2022 ⭐ ÚLTIMA VERSÃO</span>
                        <span className="bg-yellow-100 text-yellow-800 px-2 py-1 rounded text-sm">Impacto: MÉDIO</span>
                      </div>
                      <p className="text-gray-700 mb-2">Correção overflow em valores grandes e validação COBOL Enterprise 6.3</p>
                      <p className="text-sm text-gray-600">👤 Juliana Cardoso | 🐛 Bug #2022-012 | ✏️ +45/-32 linhas</p>
                      <div className="mt-3 p-3 bg-yellow-100 rounded border border-yellow-300">
                        <p className="text-sm font-bold text-yellow-900">🏁 Programa descontinuado após migração para .NET 9.0</p>
                      </div>
                    </div>
                  </div>
                </div>
              </div>
            </section>

            {/* Code Evolution Chart */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Evolução do Tamanho do Código</h3>
              <div className="bg-gray-50 border border-gray-200 rounded-lg p-6">
                <div className="overflow-x-auto">
                  <table className="min-w-full">
                    <thead className="bg-gray-100">
                      <tr>
                        <th className="py-3 px-4 text-left font-semibold text-gray-700">Ano</th>
                        <th className="py-3 px-4 text-left font-semibold text-gray-700">Linhas de Código</th>
                        <th className="py-3 px-4 text-left font-semibold text-gray-700">Variação</th>
                        <th className="py-3 px-4 text-left font-semibold text-gray-700">Tendência</th>
                      </tr>
                    </thead>
                    <tbody className="divide-y divide-gray-200">
                      <tr>
                        <td className="py-3 px-4 font-bold">2014</td>
                        <td className="py-3 px-4">4.821</td>
                        <td className="py-3 px-4 text-gray-600">Baseline</td>
                        <td className="py-3 px-4"><span className="text-2xl">📊</span></td>
                      </tr>
                      <tr className="bg-green-50">
                        <td className="py-3 px-4 font-bold">2015</td>
                        <td className="py-3 px-4 font-semibold">5.134</td>
                        <td className="py-3 px-4 text-green-700 font-bold">+313 (+6.5%)</td>
                        <td className="py-3 px-4"><span className="text-2xl">📈</span></td>
                      </tr>
                      <tr className="bg-green-50">
                        <td className="py-3 px-4 font-bold">2016</td>
                        <td className="py-3 px-4 font-semibold">5.289</td>
                        <td className="py-3 px-4 text-green-700 font-bold">+155 (+3.0%)</td>
                        <td className="py-3 px-4"><span className="text-2xl">📈</span></td>
                      </tr>
                      <tr className="bg-green-50">
                        <td className="py-3 px-4 font-bold">2017</td>
                        <td className="py-3 px-4 font-semibold">5.412</td>
                        <td className="py-3 px-4 text-green-700 font-bold">+123 (+2.3%)</td>
                        <td className="py-3 px-4"><span className="text-2xl">📈</span></td>
                      </tr>
                      <tr className="bg-green-50">
                        <td className="py-3 px-4 font-bold">2018</td>
                        <td className="py-3 px-4 font-semibold">5.523</td>
                        <td className="py-3 px-4 text-green-700 font-bold">+111 (+2.1%)</td>
                        <td className="py-3 px-4"><span className="text-2xl">📈</span></td>
                      </tr>
                      <tr className="bg-green-50">
                        <td className="py-3 px-4 font-bold">2019</td>
                        <td className="py-3 px-4 font-semibold">5.734</td>
                        <td className="py-3 px-4 text-green-700 font-bold">+211 (+3.8%)</td>
                        <td className="py-3 px-4"><span className="text-2xl">📈</span></td>
                      </tr>
                      <tr className="bg-blue-50">
                        <td className="py-3 px-4 font-bold">2020</td>
                        <td className="py-3 px-4 font-semibold">5.678</td>
                        <td className="py-3 px-4 text-blue-700 font-bold">-56 (-1.0%) [refatoração]</td>
                        <td className="py-3 px-4"><span className="text-2xl">📉</span></td>
                      </tr>
                      <tr className="bg-blue-50">
                        <td className="py-3 px-4 font-bold">2021</td>
                        <td className="py-3 px-4 font-semibold">5.234</td>
                        <td className="py-3 px-4 text-blue-700 font-bold">-444 (-7.8%) [código morto removido]</td>
                        <td className="py-3 px-4"><span className="text-2xl">📉</span></td>
                      </tr>
                      <tr className="bg-blue-50">
                        <td className="py-3 px-4 font-bold">2022</td>
                        <td className="py-3 px-4 font-semibold">5.046</td>
                        <td className="py-3 px-4 text-blue-700 font-bold">-188 (-3.6%) [otimização]</td>
                        <td className="py-3 px-4"><span className="text-2xl">📉</span></td>
                      </tr>
                    </tbody>
                  </table>
                </div>
                <div className="mt-4 p-4 bg-blue-50 rounded border border-blue-200">
                  <p className="text-sm text-blue-900">
                    <strong>Observação:</strong> Crescimento até 2019 (pico: 5.734 linhas), seguido de refatoração e limpeza (2020-2022).
                    Versão final ficou próxima do tamanho inicial, porém com funcionalidades significativamente expandidas.
                  </p>
                </div>
              </div>
            </section>

            {/* Top Developers */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Desenvolvedores Mais Ativos</h3>
              <div className="overflow-x-auto">
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Rank</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Desenvolvedor</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Alterações</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Linhas Modificadas</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Contribuição</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr className="bg-yellow-50">
                      <td className="py-3 px-4"><span className="text-2xl">🥇</span></td>
                      <td className="py-3 px-4 font-bold">João Silva</td>
                      <td className="py-3 px-4 font-semibold">5</td>
                      <td className="py-3 px-4">+1.234 / -456</td>
                      <td className="py-3 px-4">
                        <div className="h-3 bg-blue-200 rounded-full overflow-hidden">
                          <div className="h-full bg-blue-600" style={{ width: '24.3%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4"><span className="text-2xl">🥈</span></td>
                      <td className="py-3 px-4 font-bold">Maria Santos</td>
                      <td className="py-3 px-4 font-semibold">4</td>
                      <td className="py-3 px-4">+876 / -234</td>
                      <td className="py-3 px-4">
                        <div className="h-3 bg-purple-200 rounded-full overflow-hidden">
                          <div className="h-full bg-purple-600" style={{ width: '17.2%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4"><span className="text-2xl">🥉</span></td>
                      <td className="py-3 px-4 font-bold">Carlos Oliveira</td>
                      <td className="py-3 px-4 font-semibold">3</td>
                      <td className="py-3 px-4">+654 / -123</td>
                      <td className="py-3 px-4">
                        <div className="h-3 bg-green-200 rounded-full overflow-hidden">
                          <div className="h-full bg-green-600" style={{ width: '12.8%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4">4</td>
                      <td className="py-3 px-4 font-bold">Ana Paula</td>
                      <td className="py-3 px-4 font-semibold">3</td>
                      <td className="py-3 px-4">+543 / -187</td>
                      <td className="py-3 px-4">
                        <div className="h-3 bg-orange-200 rounded-full overflow-hidden">
                          <div className="h-full bg-orange-600" style={{ width: '10.6%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4">-</td>
                      <td className="py-3 px-4 text-gray-600">Outros (8 desenvolvedores)</td>
                      <td className="py-3 px-4 font-semibold">22</td>
                      <td className="py-3 px-4">+1.827 / -876</td>
                      <td className="py-3 px-4">
                        <div className="h-3 bg-gray-200 rounded-full overflow-hidden">
                          <div className="h-full bg-gray-600" style={{ width: '35.1%' }}></div>
                        </div>
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </section>

            {/* Lessons Learned */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Lições Aprendidas</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="border border-blue-200 rounded-lg p-6 bg-blue-50">
                  <h4 className="text-xl font-bold text-blue-900 mb-3">💡 Manutenibilidade</h4>
                  <p className="text-gray-700 mb-3">
                    <strong>Problema:</strong> Código sem documentação dificultava manutenções
                  </p>
                  <p className="text-gray-700 mb-3">
                    <strong>Solução:</strong> Documentação inline obrigatória desde 2021
                  </p>
                  <div className="bg-blue-100 border border-blue-300 rounded p-3">
                    <p className="text-sm text-blue-900 font-semibold">
                      ✅ Resultado: Redução de 40% no tempo médio de correção
                    </p>
                  </div>
                </div>

                <div className="border border-green-200 rounded-lg p-6 bg-green-50">
                  <h4 className="text-xl font-bold text-green-900 mb-3">🧪 Testes de Regressão</h4>
                  <p className="text-gray-700 mb-3">
                    <strong>Problema:</strong> 6 incidentes de regressão em 2016
                  </p>
                  <p className="text-gray-700 mb-3">
                    <strong>Solução:</strong> Suíte de testes input/output implementada
                  </p>
                  <div className="bg-green-100 border border-green-300 rounded p-3">
                    <p className="text-sm text-green-900 font-semibold">
                      ✅ Resultado: Zero incidentes desde 2019
                    </p>
                  </div>
                </div>

                <div className="border border-purple-200 rounded-lg p-6 bg-purple-50">
                  <h4 className="text-xl font-bold text-purple-900 mb-3">📊 Performance Monitoring</h4>
                  <p className="text-gray-700 mb-3">
                    <strong>Problema:</strong> Degradação não detectada proativamente
                  </p>
                  <p className="text-gray-700 mb-3">
                    <strong>Solução:</strong> Métricas no log (v1.6.0)
                  </p>
                  <div className="bg-purple-100 border border-purple-300 rounded p-3">
                    <p className="text-sm text-purple-900 font-semibold">
                      ✅ Resultado: 3 incidentes evitados
                    </p>
                  </div>
                </div>

                <div className="border border-orange-200 rounded-lg p-6 bg-orange-50">
                  <h4 className="text-xl font-bold text-orange-900 mb-3">📚 Gestão de Conhecimento</h4>
                  <p className="text-gray-700 mb-3">
                    <strong>Problema:</strong> Rotatividade causava perda de conhecimento
                  </p>
                  <p className="text-gray-700 mb-3">
                    <strong>Solução:</strong> Wiki interna e doc completa (v1.8.0)
                  </p>
                  <div className="bg-orange-100 border border-orange-300 rounded p-3">
                    <p className="text-sm text-orange-900 font-semibold">
                      ✅ Resultado: Onboarding reduzido de 3 meses para 2 semanas
                    </p>
                  </div>
                </div>
              </div>
            </section>

            {/* Maintenance Motives */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Motivos de Manutenção</h3>
              <div className="overflow-x-auto">
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Motivo</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Quantidade</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Percentual</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Distribuição</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr>
                      <td className="py-3 px-4 font-semibold">Correção de Bugs</td>
                      <td className="py-3 px-4 font-bold text-red-700">18</td>
                      <td className="py-3 px-4 font-bold">48.6%</td>
                      <td className="py-3 px-4">
                        <div className="h-4 bg-red-200 rounded-full overflow-hidden">
                          <div className="h-full bg-red-600" style={{ width: '48.6%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4 font-semibold">Novos Requisitos de Negócio</td>
                      <td className="py-3 px-4 font-bold text-blue-700">10</td>
                      <td className="py-3 px-4 font-bold">27.0%</td>
                      <td className="py-3 px-4">
                        <div className="h-4 bg-blue-200 rounded-full overflow-hidden">
                          <div className="h-full bg-blue-600" style={{ width: '27%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold">Conformidade Regulatória</td>
                      <td className="py-3 px-4 font-bold text-purple-700">5</td>
                      <td className="py-3 px-4 font-bold">13.5%</td>
                      <td className="py-3 px-4">
                        <div className="h-4 bg-purple-200 rounded-full overflow-hidden">
                          <div className="h-full bg-purple-600" style={{ width: '13.5%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4 font-semibold">Otimização de Performance</td>
                      <td className="py-3 px-4 font-bold text-green-700">3</td>
                      <td className="py-3 px-4 font-bold">8.1%</td>
                      <td className="py-3 px-4">
                        <div className="h-4 bg-green-200 rounded-full overflow-hidden">
                          <div className="h-full bg-green-600" style={{ width: '8.1%' }}></div>
                        </div>
                      </td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold">Preparação para Migração</td>
                      <td className="py-3 px-4 font-bold text-orange-700">1</td>
                      <td className="py-3 px-4 font-bold">2.7%</td>
                      <td className="py-3 px-4">
                        <div className="h-4 bg-orange-200 rounded-full overflow-hidden">
                          <div className="h-full bg-orange-600" style={{ width: '2.7%' }}></div>
                        </div>
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </section>

            {/* Reference */}
            <section className="bg-gray-100 border border-gray-300 rounded-lg p-6">
              <h3 className="text-xl font-semibold text-gray-800 mb-3">Referências Completas</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div>
                  <h4 className="font-bold text-gray-700 mb-2">Documentação Relacionada:</h4>
                  <ul className="space-y-1 text-sm text-gray-700">
                    <li>• Lógica de Negócio: <code className="bg-gray-200 px-2 py-1 rounded">05-business-logic.md</code></li>
                    <li>• Guia de Operações: <code className="bg-gray-200 px-2 py-1 rounded">07-operations-guide.md</code></li>
                  </ul>
                </div>
                <div>
                  <h4 className="font-bold text-gray-700 mb-2">Sistemas de Rastreamento:</h4>
                  <ul className="space-y-1 text-sm text-gray-700">
                    <li>• Controle de Versão: CADMUS (Caixa Seguradora)</li>
                    <li>• Bugs 2016-2022: JIRA</li>
                    <li>• Bugs 2014-2015: BMC Remedy</li>
                  </ul>
                </div>
              </div>
            </section>
          </div>
        </TabsContent>

        <TabsContent value="migration" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Guia de Migração COBOL → .NET 9.0</h2>

            {/* Migration Overview */}
            <section className="mb-8">
              <div className="bg-gradient-to-r from-blue-600 to-purple-600 rounded-lg p-6 text-white mb-6">
                <div className="flex items-center gap-4 mb-3">
                  <Workflow className="h-12 w-12" />
                  <div>
                    <h3 className="text-2xl font-bold">Estratégia de Migração: Reescrita Completa</h3>
                    <p className="text-blue-100 text-lg">Abordagem Clean Architecture + Validação Byte-Level</p>
                  </div>
                </div>
                <div className="grid grid-cols-3 gap-4 mt-4">
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">100%</div>
                    <div className="text-sm opacity-90">Compatibilidade Byte-Level</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">6 meses</div>
                    <div className="text-sm opacity-90">Prazo Estimado</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">3 fases</div>
                    <div className="text-sm opacity-90">Planejamento → Implementação → Validação</div>
                  </div>
                </div>
              </div>
            </section>

            {/* Migration Phases */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Fases da Migração</h3>
              <div className="space-y-4">
                <div className="border-l-4 border-blue-500 bg-blue-50 p-6 rounded-r-lg">
                  <div className="flex items-center gap-3 mb-3">
                    <div className="bg-blue-500 text-white rounded-full w-10 h-10 flex items-center justify-center font-bold">1</div>
                    <h4 className="text-xl font-bold text-gray-900">Fase 1: Análise e Planejamento (2 meses)</h4>
                  </div>
                  <ul className="space-y-2 ml-13">
                    <li className="flex items-start gap-2">
                      <CheckCircle2 className="h-5 w-5 text-green-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Análise Estática do COBOL:</strong> Parser completo de 5.046 linhas (687 variáveis, 26+ tabelas)</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <CheckCircle2 className="h-5 w-5 text-green-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Mapeamento de Tipos:</strong> COBOL PIC → C# types (COMP-3 → decimal, X(n) → string)</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <CheckCircle2 className="h-5 w-5 text-green-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Modelagem de Dados:</strong> 15 entidades EF Core mapeadas para views DB2</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <CheckCircle2 className="h-5 w-5 text-green-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Arquitetura .NET:</strong> Clean Architecture (API + Core + Infrastructure)</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <CheckCircle2 className="h-5 w-5 text-green-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Coleta de Amostras:</strong> 10.000+ registros reais PREMIT/PREMCED para validação</span>
                    </li>
                  </ul>
                </div>

                <div className="border-l-4 border-purple-500 bg-purple-50 p-6 rounded-r-lg">
                  <div className="flex items-center gap-3 mb-3">
                    <div className="bg-purple-500 text-white rounded-full w-10 h-10 flex items-center justify-center font-bold">2</div>
                    <h4 className="text-xl font-bold text-gray-900">Fase 2: Implementação (3 meses)</h4>
                  </div>
                  <ul className="space-y-2 ml-13">
                    <li className="flex items-start gap-2">
                      <Code2 className="h-5 w-5 text-purple-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Backend .NET 9:</strong> ASP.NET Core Web API + EF Core 9.0</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Code2 className="h-5 w-5 text-purple-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Lógica de Cálculo:</strong> Migração seção-por-seção (R0700-R5500)</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Code2 className="h-5 w-5 text-purple-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>FixedWidthFormatter:</strong> Geração de arquivos texto compatíveis SUSEP</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Code2 className="h-5 w-5 text-purple-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Frontend React:</strong> Dashboard, geração de relatórios, consulta de dados</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Code2 className="h-5 w-5 text-purple-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Testes Unitários:</strong> Cobertura 90%+ para lógica de negócio crítica</span>
                    </li>
                  </ul>
                </div>

                <div className="border-l-4 border-green-500 bg-green-50 p-6 rounded-r-lg">
                  <div className="flex items-center gap-3 mb-3">
                    <div className="bg-green-500 text-white rounded-full w-10 h-10 flex items-center justify-center font-bold">3</div>
                    <h4 className="text-xl font-bold text-gray-900">Fase 3: Validação e Deploy (1 mês)</h4>
                  </div>
                  <ul className="space-y-2 ml-13">
                    <li className="flex items-start gap-2">
                      <Trophy className="h-5 w-5 text-yellow-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Testes de Comparação:</strong> Validação byte-a-byte COBOL vs .NET</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Trophy className="h-5 w-5 text-yellow-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Testes de Performance:</strong> Execução paralela 50%+ mais rápida</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Trophy className="h-5 w-5 text-yellow-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Homologação:</strong> 3 meses em paralelo (COBOL + .NET)</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Trophy className="h-5 w-5 text-yellow-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Auditoria SUSEP:</strong> Certificação byte-level de conformidade</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Trophy className="h-5 w-5 text-yellow-600 mt-0.5 flex-shrink-0" />
                      <span className="text-gray-700"><strong>Deploy Produção:</strong> Cutover gradual com rollback automático</span>
                    </li>
                  </ul>
                </div>
              </div>
            </section>

            {/* Key Challenges */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Desafios Críticos e Mitigações</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="border border-red-200 rounded-lg p-6 bg-red-50">
                  <div className="flex items-center gap-3 mb-3">
                    <AlertTriangle className="h-8 w-8 text-red-600" />
                    <h4 className="text-lg font-bold text-red-900">Precisão Decimal (CRÍTICO)</h4>
                  </div>
                  <p className="text-gray-700 mb-3">
                    <strong>Risco:</strong> COBOL COMP-3 tem 31 dígitos decimais, erros de arredondamento causam divergências SUSEP
                  </p>
                  <div className="bg-red-100 border border-red-300 rounded p-3">
                    <p className="text-sm text-red-900">
                      <strong>✅ Mitigação:</strong> Uso obrigatório de <code className="bg-red-200 px-1 rounded">decimal</code> (nunca float/double) + validação com 10.000 casos de teste
                    </p>
                  </div>
                </div>

                <div className="border border-orange-200 rounded-lg p-6 bg-orange-50">
                  <div className="flex items-center gap-3 mb-3">
                    <AlertCircle className="h-8 w-8 text-orange-600" />
                    <h4 className="text-lg font-bold text-orange-900">Formatação Fixed-Width</h4>
                  </div>
                  <p className="text-gray-700 mb-3">
                    <strong>Risco:</strong> Padding/truncamento incorreto quebra validação SUSEP (rejeição de arquivo)
                  </p>
                  <div className="bg-orange-100 border border-orange-300 rounded p-3">
                    <p className="text-sm text-orange-900">
                      <strong>✅ Mitigação:</strong> FixedWidthFormatter com testes comparativos linha-a-linha contra COBOL
                    </p>
                  </div>
                </div>

                <div className="border border-yellow-200 rounded-lg p-6 bg-yellow-50">
                  <div className="flex items-center gap-3 mb-3">
                    <Zap className="h-8 w-8 text-yellow-600" />
                    <h4 className="text-lg font-bold text-yellow-900">Performance em Larga Escala</h4>
                  </div>
                  <p className="text-gray-700 mb-3">
                    <strong>Risco:</strong> COBOL processa 10M+ registros/mês - .NET deve igualar ou superar
                  </p>
                  <div className="bg-yellow-100 border border-yellow-300 rounded p-3">
                    <p className="text-sm text-yellow-900">
                      <strong>✅ Mitigação:</strong> IAsyncEnumerable (streaming), parallel processing, benchmarks 50k+ registros
                    </p>
                  </div>
                </div>

                <div className="border border-blue-200 rounded-lg p-6 bg-blue-50">
                  <div className="flex items-center gap-3 mb-3">
                    <Database className="h-8 w-8 text-blue-600" />
                    <h4 className="text-lg font-bold text-blue-900">Acesso DB2 Legacy</h4>
                  </div>
                  <p className="text-gray-700 mb-3">
                    <strong>Risco:</strong> 26+ views DB2 com queries SQL/COBOL complexas e otimizadas
                  </p>
                  <div className="bg-blue-100 border border-blue-300 rounded p-3">
                    <p className="text-sm text-blue-900">
                      <strong>✅ Mitigação:</strong> EF Core raw SQL queries + índices otimizados + monitoramento de performance
                    </p>
                  </div>
                </div>
              </div>
            </section>

            {/* Technology Stack Comparison */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Comparação de Stack Tecnológica</h3>
              <div className="overflow-x-auto">
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Componente</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">COBOL (Legado)</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">.NET 9.0 (Novo)</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Benefício</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4 font-semibold">Linguagem</td>
                      <td className="py-3 px-4">COBOL 85/Enterprise 6.3</td>
                      <td className="py-3 px-4 font-bold text-green-700">C# 12 (.NET 9)</td>
                      <td className="py-3 px-4 text-sm">Moderna, tipo-segura, POO</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold">Processamento</td>
                      <td className="py-3 px-4">JCL batch (mainframe)</td>
                      <td className="py-3 px-4 font-bold text-green-700">ASP.NET Core Web API</td>
                      <td className="py-3 px-4 text-sm">On-demand, RESTful, cloud-native</td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4 font-semibold">Banco de Dados</td>
                      <td className="py-3 px-4">DB2 z/OS (views)</td>
                      <td className="py-3 px-4 font-bold text-green-700">EF Core + DB2 provider</td>
                      <td className="py-3 px-4 text-sm">ORM, migrations, LINQ</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold">Interface</td>
                      <td className="py-3 px-4">ISPF/TSO (terminal)</td>
                      <td className="py-3 px-4 font-bold text-green-700">React SPA (web)</td>
                      <td className="py-3 px-4 text-sm">UX moderna, self-service</td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4 font-semibold">Deployment</td>
                      <td className="py-3 px-4">Mainframe IBM z/OS</td>
                      <td className="py-3 px-4 font-bold text-green-700">Azure App Service + Docker</td>
                      <td className="py-3 px-4 text-sm">CI/CD, escalabilidade</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4 font-semibold">Testes</td>
                      <td className="py-3 px-4">Input/output manual (JCL)</td>
                      <td className="py-3 px-4 font-bold text-green-700">xUnit + Playwright (automatizado)</td>
                      <td className="py-3 px-4 text-sm">Coverage 90%, regressão auto</td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4 font-semibold">Logging</td>
                      <td className="py-3 px-4">SYSOUT (batch logs)</td>
                      <td className="py-3 px-4 font-bold text-green-700">Serilog + Application Insights</td>
                      <td className="py-3 px-4 text-sm">Estruturado, alertas, dashboards</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </section>

            {/* Success Metrics */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Métricas de Sucesso</h3>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                <div className="bg-gradient-to-br from-green-500 to-green-600 rounded-lg p-4 text-white">
                  <div className="flex items-center gap-2 mb-2">
                    <CheckCircle2 className="h-6 w-6" />
                    <span className="text-sm font-semibold">Compatibilidade</span>
                  </div>
                  <div className="text-3xl font-bold">100%</div>
                  <div className="text-xs opacity-90">Byte-level match COBOL</div>
                </div>
                <div className="bg-gradient-to-br from-blue-500 to-blue-600 rounded-lg p-4 text-white">
                  <div className="flex items-center gap-2 mb-2">
                    <TrendingUp className="h-6 w-6" />
                    <span className="text-sm font-semibold">Performance</span>
                  </div>
                  <div className="text-3xl font-bold">+50%</div>
                  <div className="text-xs opacity-90">Mais rápido (paralelo)</div>
                </div>
                <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-lg p-4 text-white">
                  <div className="flex items-center gap-2 mb-2">
                    <Trophy className="h-6 w-6" />
                    <span className="text-sm font-semibold">Cobertura Testes</span>
                  </div>
                  <div className="text-3xl font-bold">90%+</div>
                  <div className="text-xs opacity-90">Lógica de negócio</div>
                </div>
                <div className="bg-gradient-to-br from-orange-500 to-orange-600 rounded-lg p-4 text-white">
                  <div className="flex items-center gap-2 mb-2">
                    <Clock className="h-6 w-6" />
                    <span className="text-sm font-semibold">Deploy Time</span>
                  </div>
                  <div className="text-3xl font-bold">5 min</div>
                  <div className="text-xs opacity-90">vs 2h mainframe</div>
                </div>
              </div>
            </section>

            {/* Rollout Strategy */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Estratégia de Rollout</h3>
              <div className="bg-gradient-to-r from-indigo-50 to-purple-50 border border-indigo-200 rounded-lg p-6">
                <ol className="space-y-4">
                  <li className="flex items-start gap-3">
                    <div className="bg-indigo-600 text-white rounded-full w-8 h-8 flex items-center justify-center font-bold flex-shrink-0">1</div>
                    <div>
                      <strong className="text-gray-900">Mês 1-3: Execução Paralela (Shadow Mode)</strong>
                      <p className="text-gray-700 text-sm mt-1">COBOL + .NET executam simultaneamente, .NET não afeta produção</p>
                    </div>
                  </li>
                  <li className="flex items-start gap-3">
                    <div className="bg-indigo-600 text-white rounded-full w-8 h-8 flex items-center justify-center font-bold flex-shrink-0">2</div>
                    <div>
                      <strong className="text-gray-900">Mês 4: Piloto com 10% do Volume</strong>
                      <p className="text-gray-700 text-sm mt-1">.NET processa ramos menos críticos (0561), COBOL como backup</p>
                    </div>
                  </li>
                  <li className="flex items-start gap-3">
                    <div className="bg-indigo-600 text-white rounded-full w-8 h-8 flex items-center justify-center font-bold flex-shrink-0">3</div>
                    <div>
                      <strong className="text-gray-900">Mês 5: Expansão para 50% (Blue-Green Deploy)</strong>
                      <p className="text-gray-700 text-sm mt-1">Roteamento automático com rollback em caso de divergência</p>
                    </div>
                  </li>
                  <li className="flex items-start gap-3">
                    <div className="bg-indigo-600 text-white rounded-full w-8 h-8 flex items-center justify-center font-bold flex-shrink-0">4</div>
                    <div>
                      <strong className="text-gray-900">Mês 6: Cutover Completo + Desativação COBOL</strong>
                      <p className="text-gray-700 text-sm mt-1">.NET assume 100% após certificação SUSEP, COBOL arquivado</p>
                    </div>
                  </li>
                </ol>
              </div>
            </section>

            {/* Reference */}
            <section className="bg-gray-100 border border-gray-300 rounded-lg p-6">
              <h3 className="text-xl font-semibold text-gray-800 mb-3">Documentação Completa de Migração</h3>
              <p className="text-gray-700 mb-4">
                Para detalhes técnicos completos, estratégias de teste, scripts de validação e checklist de homologação, consulte:
              </p>
              <div className="bg-white rounded-lg p-4 border border-gray-200">
                <div className="flex items-center gap-3">
                  <FileSearch className="h-6 w-6 text-blue-600" />
                  <code className="text-sm font-mono text-gray-800">docs/legacy-system/09-migration-guide.md</code>
                </div>
              </div>
            </section>
          </div>
        </TabsContent>

        <TabsContent value="glossary" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Glossário Técnico e de Negócio - 150+ Termos</h2>

            {/* Overview */}
            <section className="mb-8">
              <div className="bg-gradient-to-r from-purple-600 to-pink-600 rounded-lg p-6 text-white mb-6">
                <div className="flex items-center gap-4 mb-3">
                  <BookOpen className="h-12 w-12" />
                  <div>
                    <h3 className="text-2xl font-bold">Dicionário Completo RG1866B</h3>
                    <p className="text-purple-100 text-lg">Termos de Negócio, Técnicos COBOL, Regulatórios SUSEP e Arquitetura .NET</p>
                  </div>
                </div>
                <div className="grid grid-cols-4 gap-4 mt-4">
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">150+</div>
                    <div className="text-sm opacity-90">Termos Totais</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">4</div>
                    <div className="text-sm opacity-90">Categorias</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">26+</div>
                    <div className="text-sm opacity-90">Tabelas DB2</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">687</div>
                    <div className="text-sm opacity-90">Variáveis COBOL</div>
                  </div>
                </div>
              </div>
            </section>

            {/* Business Terms */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4 flex items-center gap-3">
                <div className="bg-blue-600 text-white rounded-lg p-2">
                  <Users className="h-6 w-6" />
                </div>
                Termos de Negócio e Domínio de Seguros (40 termos)
              </h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
                  <h4 className="font-bold text-blue-900 mb-2">Apólice / Endosso / Renovação</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Apólice:</strong> Contrato de seguro identificado por número único (10 dígitos). Pode ter múltiplos endossos.
                  </p>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Endosso:</strong> Alteração contratual após emissão (inclusão/exclusão de coberturas, mudança de valor segurado).
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Renovação:</strong> Nova vigência com ajustes de prêmio baseados em sinistralidade.
                  </p>
                </div>

                <div className="bg-green-50 border border-green-200 rounded-lg p-4">
                  <h4 className="font-bold text-green-900 mb-2">Prêmio / IOF / Valor Total</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Prêmio Líquido:</strong> Valor base do seguro (sem impostos).
                  </p>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>IOF:</strong> Imposto sobre Operações Financeiras (7.38% para seguros).
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Prêmio Total:</strong> Prêmio Líquido + IOF = valor cobrado do cliente.
                  </p>
                </div>

                <div className="bg-purple-50 border border-purple-200 rounded-lg p-4">
                  <h4 className="font-bold text-purple-900 mb-2">Cosseguro / Resseguro</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Cosseguro:</strong> Divisão de risco entre múltiplas seguradoras (ex: 60% Caixa + 40% Porto Seguro).
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Resseguro:</strong> Transferência de parte do risco para resseguradora internacional (IRB, Swiss Re, Munich Re).
                  </p>
                </div>

                <div className="bg-orange-50 border border-orange-200 rounded-lg p-4">
                  <h4 className="font-bold text-orange-900 mb-2">Ramo SUSEP</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Código numérico de 4 dígitos</strong> que classifica o tipo de seguro para fins regulatórios.
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Exemplos:</strong> 0531 (Automóvel), 0553 (Acidentes Pessoais), 0561 (Ramos Elementares), 0571 (Previdência).
                  </p>
                </div>

                <div className="bg-red-50 border border-red-200 rounded-lg p-4">
                  <h4 className="font-bold text-red-900 mb-2">Sinistro / Franquia / Indenização</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Sinistro:</strong> Evento coberto que aciona a apólice (acidente, roubo, incêndio).
                  </p>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Franquia:</strong> Valor mínimo que o segurado paga antes da seguradora indenizar.
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Indenização:</strong> Valor pago pela seguradora após aprovação do sinistro.
                  </p>
                </div>

                <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
                  <h4 className="font-bold text-yellow-900 mb-2">Pro-Rata / Curta Duração</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Pro-Rata Die:</strong> Cálculo proporcional de prêmio por dias de vigência (cancelamento antecipado).
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Curta Duração:</strong> Apólices com vigência inferior a 12 meses (ex: seguro viagem).
                  </p>
                </div>
              </div>
            </section>

            {/* Technical COBOL Terms */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4 flex items-center gap-3">
                <div className="bg-green-600 text-white rounded-lg p-2">
                  <Code2 className="h-6 w-6" />
                </div>
                Termos Técnicos COBOL (50 termos)
              </h3>
              <div className="space-y-4">
                <div className="bg-gray-50 border border-gray-200 rounded-lg p-4">
                  <h4 className="font-bold text-gray-900 mb-3">Tipos de Dados PIC (Picture)</h4>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <code className="text-sm font-mono text-blue-700">PIC 9(15)V99</code>
                      <p className="text-xs text-gray-600 mt-1">Numérico com 15 inteiros + 2 decimais (implied decimal point)</p>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <code className="text-sm font-mono text-blue-700">PIC X(10)</code>
                      <p className="text-xs text-gray-600 mt-1">Alfanumérico de 10 caracteres (string fixed-width)</p>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <code className="text-sm font-mono text-blue-700">PIC S9(7) COMP-3</code>
                      <p className="text-xs text-gray-600 mt-1">Packed decimal signed (7 dígitos, armazenamento compactado)</p>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <code className="text-sm font-mono text-blue-700">PIC 9(8) COMP</code>
                      <p className="text-xs text-gray-600 mt-1">Binary integer (4 bytes, até 99.999.999)</p>
                    </div>
                  </div>
                </div>

                <div className="bg-gray-50 border border-gray-200 rounded-lg p-4">
                  <h4 className="font-bold text-gray-900 mb-3">Estruturas de Dados</h4>
                  <div className="space-y-2">
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">01 WORKING-STORAGE:</strong>
                      <span className="text-sm text-gray-700 ml-2">Seção de variáveis globais do programa (687 variáveis no RG1866B)</span>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">OCCURS n TIMES:</strong>
                      <span className="text-sm text-gray-700 ml-2">Array de n elementos (ex: OCCURS 100 TIMES = array[100])</span>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">REDEFINES:</strong>
                      <span className="text-sm text-gray-700 ml-2">União C-style (múltiplas interpretações do mesmo espaço de memória)</span>
                    </div>
                  </div>
                </div>

                <div className="bg-gray-50 border border-gray-200 rounded-lg p-4">
                  <h4 className="font-bold text-gray-900 mb-3">SQL Embedded</h4>
                  <div className="space-y-2">
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">EXEC SQL ... END-EXEC:</strong>
                      <span className="text-sm text-gray-700 ml-2">Bloco SQL embutido no COBOL</span>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">DECLARE CURSOR:</strong>
                      <span className="text-sm text-gray-700 ml-2">Define cursor para fetch iterativo (streaming de resultados)</span>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">SQLCODE:</strong>
                      <span className="text-sm text-gray-700 ml-2">Código de retorno SQL (0=sucesso, -911=deadlock, +100=no data)</span>
                    </div>
                  </div>
                </div>
              </div>
            </section>

            {/* SUSEP Regulatory Terms */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4 flex items-center gap-3">
                <div className="bg-red-600 text-white rounded-lg p-2">
                  <AlertTriangle className="h-6 w-6" />
                </div>
                Termos Regulatórios SUSEP (30 termos)
              </h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="bg-red-50 border border-red-200 rounded-lg p-4">
                  <h4 className="font-bold text-red-900 mb-2">Circular SUSEP 360/2008</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    Normativa que estabelece o formato e periodicidade dos relatórios PREMIT e PREMCED.
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Penalidade:</strong> Multa de até R$ 500.000 por envio incorreto ou atrasado.
                  </p>
                </div>

                <div className="bg-orange-50 border border-orange-200 rounded-lg p-4">
                  <h4 className="font-bold text-orange-900 mb-2">PREMIT.TXT / PREMCED.TXT</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>PREMIT:</strong> Prêmios emitidos no mês (novos negócios + renovações).
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>PREMCED:</strong> Prêmios cedidos em cosseguro (parcela de outras seguradoras).
                  </p>
                </div>

                <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
                  <h4 className="font-bold text-yellow-900 mb-2">Fixed-Width Format</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    Cada registro tem 200 caracteres exatos, com posições fixas para cada campo.
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Validação:</strong> SUSEP rejeita arquivo inteiro se houver 1 byte fora de posição.
                  </p>
                </div>

                <div className="bg-green-50 border border-green-200 rounded-lg p-4">
                  <h4 className="font-bold text-green-900 mb-2">Prazo de Envio</h4>
                  <p className="text-sm text-gray-700 mb-2">
                    <strong>Deadline:</strong> Até o 15º dia útil do mês subsequente.
                  </p>
                  <p className="text-sm text-gray-700">
                    <strong>Exemplo:</strong> Dados de Janeiro/2025 → envio até 15/Fev/2025.
                  </p>
                </div>
              </div>
            </section>

            {/* .NET Architecture Terms */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4 flex items-center gap-3">
                <div className="bg-purple-600 text-white rounded-lg p-2">
                  <Cloud className="h-6 w-6" />
                </div>
                Termos de Arquitetura .NET (30 termos)
              </h3>
              <div className="space-y-4">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div className="bg-purple-50 border border-purple-200 rounded-lg p-4">
                    <h4 className="font-bold text-purple-900 mb-2">Clean Architecture</h4>
                    <p className="text-sm text-gray-700">
                      Padrão de 3 camadas: API (interface) → Core (lógica) → Infrastructure (dados).
                      Dependências fluem sempre para dentro (Core não depende de ninguém).
                    </p>
                  </div>

                  <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
                    <h4 className="font-bold text-blue-900 mb-2">Entity Framework Core</h4>
                    <p className="text-sm text-gray-700">
                      ORM (Object-Relational Mapper) que mapeia classes C# para tabelas DB2.
                      Suporta LINQ queries, migrations e lazy loading.
                    </p>
                  </div>

                  <div className="bg-green-50 border border-green-200 rounded-lg p-4">
                    <h4 className="font-bold text-green-900 mb-2">IAsyncEnumerable</h4>
                    <p className="text-sm text-gray-700">
                      Interface .NET para streaming assíncrono de dados.
                      Equivalente ao cursor COBOL (fetch linha-a-linha sem carregar tudo em memória).
                    </p>
                  </div>
                </div>

                <div className="bg-gray-50 border border-gray-200 rounded-lg p-4">
                  <h4 className="font-bold text-gray-900 mb-3">Principais Componentes do Sistema .NET</h4>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">PremiumCalculationService:</strong>
                      <span className="text-sm text-gray-700 ml-2">Implementa lógica de cálculo de prêmios (seções R0700-R1300 do COBOL)</span>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">FixedWidthFormatter:</strong>
                      <span className="text-sm text-gray-700 ml-2">Gera arquivos .TXT com padding/truncamento compatível COBOL</span>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">CobolFieldAttribute:</strong>
                      <span className="text-sm text-gray-700 ml-2">Atributo C# que preserva metadata PIC do COBOL para validação</span>
                    </div>
                    <div className="bg-white p-3 rounded border border-gray-300">
                      <strong className="text-blue-700">OutputValidator:</strong>
                      <span className="text-sm text-gray-700 ml-2">Compara byte-a-byte saída .NET vs COBOL (testes de regressão)</span>
                    </div>
                  </div>
                </div>
              </div>
            </section>

            {/* Quick Reference Table */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Referência Rápida: Conversão COBOL → .NET</h3>
              <div className="overflow-x-auto">
                <table className="min-w-full bg-white border border-gray-200 rounded-lg">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Conceito COBOL</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Equivalente .NET</th>
                      <th className="py-3 px-4 text-left font-semibold text-gray-700">Observações</th>
                    </tr>
                  </thead>
                  <tbody className="divide-y divide-gray-200">
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4"><code className="text-sm">PIC 9(15)V99</code></td>
                      <td className="py-3 px-4"><code className="text-sm">decimal (17,2)</code></td>
                      <td className="py-3 px-4 text-sm">NUNCA usar float/double</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4"><code className="text-sm">PIC X(10)</code></td>
                      <td className="py-3 px-4"><code className="text-sm">string (MaxLength=10)</code></td>
                      <td className="py-3 px-4 text-sm">Right-pad com espaços</td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4"><code className="text-sm">WORKING-STORAGE</code></td>
                      <td className="py-3 px-4"><code className="text-sm">private fields</code></td>
                      <td className="py-3 px-4 text-sm">Variáveis de instância</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4"><code className="text-sm">DECLARE CURSOR</code></td>
                      <td className="py-3 px-4"><code className="text-sm">IAsyncEnumerable&lt;T&gt;</code></td>
                      <td className="py-3 px-4 text-sm">Streaming assíncrono</td>
                    </tr>
                    <tr className="bg-gray-50">
                      <td className="py-3 px-4"><code className="text-sm">PERFORM UNTIL</code></td>
                      <td className="py-3 px-4"><code className="text-sm">while / foreach</code></td>
                      <td className="py-3 px-4 text-sm">Iteração de loop</td>
                    </tr>
                    <tr>
                      <td className="py-3 px-4"><code className="text-sm">WRITE</code></td>
                      <td className="py-3 px-4"><code className="text-sm">StreamWriter.WriteLine</code></td>
                      <td className="py-3 px-4 text-sm">Geração de arquivo texto</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </section>

            {/* Reference */}
            <section className="bg-gray-100 border border-gray-300 rounded-lg p-6">
              <h3 className="text-xl font-semibold text-gray-800 mb-3">Glossário Completo</h3>
              <p className="text-gray-700 mb-4">
                Para definições completas dos 150+ termos, incluindo exemplos de uso, códigos de erro SQL, e mapeamentos completos COBOL→.NET, consulte:
              </p>
              <div className="bg-white rounded-lg p-4 border border-gray-200">
                <div className="flex items-center gap-3">
                  <BookOpen className="h-6 w-6 text-purple-600" />
                  <code className="text-sm font-mono text-gray-800">docs/legacy-system/10-glossary.md</code>
                </div>
              </div>
            </section>
          </div>
        </TabsContent>

        <TabsContent value="complete" className="space-y-6">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h2 className="text-3xl font-bold text-gray-900 mb-6">Documentação Completa Consolidada</h2>

            {/* Overview Stats */}
            <section className="mb-8">
              <div className="bg-gradient-to-r from-gray-900 to-gray-700 rounded-lg p-6 text-white mb-6">
                <div className="flex items-center gap-4 mb-3">
                  <GitBranch className="h-12 w-12" />
                  <div>
                    <h3 className="text-2xl font-bold">Sistema Completo RG1866B Documentado</h3>
                    <p className="text-gray-300 text-lg">10 arquivos .md + PDF consolidado + análise completa do parser</p>
                  </div>
                </div>
                <div className="grid grid-cols-4 gap-4 mt-4">
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">12</div>
                    <div className="text-sm opacity-90">Arquivos de Documentação</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">5.046</div>
                    <div className="text-sm opacity-90">Linhas COBOL Analisadas</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">150+</div>
                    <div className="text-sm opacity-90">Termos Glossário</div>
                  </div>
                  <div className="bg-white/20 backdrop-blur rounded-lg p-3">
                    <div className="text-2xl font-bold">100%</div>
                    <div className="text-sm opacity-90">Cobertura Completa</div>
                  </div>
                </div>
              </div>
            </section>

            {/* Documentation Structure */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Estrutura da Documentação</h3>
              <div className="space-y-4">

                {/* Executive Summary */}
                <div className="border-l-4 border-blue-500 bg-blue-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-blue-500 text-white rounded-lg p-3">
                      <FileText className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">01. Sumário Executivo</h4>
                      <p className="text-gray-700 mb-3">
                        Visão geral do sistema RG1866B para stakeholders não-técnicos: propósito, valor de negócio,
                        métricas principais e justificativa de migração.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-blue-100 text-blue-800 px-3 py-1 rounded-full text-sm">4 páginas</span>
                        <span className="bg-blue-100 text-blue-800 px-3 py-1 rounded-full text-sm">Público: C-level</span>
                        <span className="bg-blue-100 text-blue-800 px-3 py-1 rounded-full text-sm">Tempo leitura: 10min</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-blue-200">
                        docs/legacy-system/01-executive-summary.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Architecture */}
                <div className="border-l-4 border-purple-500 bg-purple-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-purple-500 text-white rounded-lg p-3">
                      <Layers className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">02. Arquitetura de Sistema</h4>
                      <p className="text-gray-700 mb-3">
                        Diagrama de componentes, fluxo de execução mainframe (JCL batch), dependências de módulos externos
                        (RG1867A, RG1873D) e integração com DB2.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-purple-100 text-purple-800 px-3 py-1 rounded-full text-sm">8 páginas</span>
                        <span className="bg-purple-100 text-purple-800 px-3 py-1 rounded-full text-sm">3 diagramas</span>
                        <span className="bg-purple-100 text-purple-800 px-3 py-1 rounded-full text-sm">Público: Arquitetos</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-purple-200">
                        docs/legacy-system/02-architecture.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Data Structures */}
                <div className="border-l-4 border-green-500 bg-green-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-green-500 text-white rounded-lg p-3">
                      <Code2 className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">03. Estruturas de Dados COBOL</h4>
                      <p className="text-gray-700 mb-3">
                        Análise detalhada das 687 variáveis WORKING-STORAGE: tipos PIC, OCCURS (arrays), REDEFINES (unions),
                        e mapeamento para tipos C#/.NET.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-green-100 text-green-800 px-3 py-1 rounded-full text-sm">12 páginas</span>
                        <span className="bg-green-100 text-green-800 px-3 py-1 rounded-full text-sm">687 variáveis</span>
                        <span className="bg-green-100 text-green-800 px-3 py-1 rounded-full text-sm">Público: Devs</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-green-200">
                        docs/legacy-system/03-data-structures.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Database Model */}
                <div className="border-l-4 border-orange-500 bg-orange-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-orange-500 text-white rounded-lg p-3">
                      <Database className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">04. Modelo de Banco de Dados DB2</h4>
                      <p className="text-gray-700 mb-3">
                        Esquema completo das 26+ views/tabelas DB2: V0PREMIOS, V0APOLICE, V0ENDOSSO, GE399 (cosseguro),
                        relacionamentos, índices e queries SQL complexas.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-orange-100 text-orange-800 px-3 py-1 rounded-full text-sm">10 páginas</span>
                        <span className="bg-orange-100 text-orange-800 px-3 py-1 rounded-full text-sm">26+ tabelas</span>
                        <span className="bg-orange-100 text-orange-800 px-3 py-1 rounded-full text-sm">Público: DBAs</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-orange-200">
                        docs/legacy-system/04-database-model.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Business Logic */}
                <div className="border-l-4 border-red-500 bg-red-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-red-500 text-white rounded-lg p-3">
                      <BarChart className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">05. Lógica de Negócio e Cálculos</h4>
                      <p className="text-gray-700 mb-3">
                        Seções críticas R0700-R5500: cálculo de prêmios, IOF, pro-rata, cosseguro, resseguro, validações
                        SUSEP e regras de arredondamento COMP-3.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-red-100 text-red-800 px-3 py-1 rounded-full text-sm">15 páginas</span>
                        <span className="bg-red-100 text-red-800 px-3 py-1 rounded-full text-sm">42 seções COBOL</span>
                        <span className="bg-red-100 text-red-800 px-3 py-1 rounded-full text-sm">CRÍTICO</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-red-200">
                        docs/legacy-system/05-business-logic.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* External Modules */}
                <div className="border-l-4 border-yellow-500 bg-yellow-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-yellow-500 text-white rounded-lg p-3">
                      <Map className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">06. Módulos Externos e Dependências</h4>
                      <p className="text-gray-700 mb-3">
                        Chamadas CALL para submódulos: RG1867A (formatação), RG1873D (validação), RG1875C (auditoria).
                        Documentação de interfaces (LINKAGE SECTION) e parâmetros.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-yellow-100 text-yellow-800 px-3 py-1 rounded-full text-sm">6 páginas</span>
                        <span className="bg-yellow-100 text-yellow-800 px-3 py-1 rounded-full text-sm">8 módulos</span>
                        <span className="bg-yellow-100 text-yellow-800 px-3 py-1 rounded-full text-sm">Público: Integradores</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-yellow-200">
                        docs/legacy-system/06-external-modules.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Operations Guide */}
                <div className="border-l-4 border-indigo-500 bg-indigo-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-indigo-500 text-white rounded-lg p-3">
                      <Settings className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">07. Guia de Operações e Execução</h4>
                      <p className="text-gray-700 mb-3">
                        JCL de produção, parâmetros de entrada, agendamento mensal (BMC Control-M), monitoramento,
                        tratamento de erros e procedimentos de rollback.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-indigo-100 text-indigo-800 px-3 py-1 rounded-full text-sm">7 páginas</span>
                        <span className="bg-indigo-100 text-indigo-800 px-3 py-1 rounded-full text-sm">SLAs 98%</span>
                        <span className="bg-indigo-100 text-indigo-800 px-3 py-1 rounded-full text-sm">Público: DevOps</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-indigo-200">
                        docs/legacy-system/07-operations-guide.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Maintenance History */}
                <div className="border-l-4 border-pink-500 bg-pink-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-pink-500 text-white rounded-lg p-3">
                      <History className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">08. Histórico de Manutenção (2014-2022)</h4>
                      <p className="text-gray-700 mb-3">
                        37 alterações ao longo de 8 anos: bugs corrigidos, melhorias de performance, novos requisitos SUSEP,
                        desenvolvedores envolvidos e lições aprendidas.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-pink-100 text-pink-800 px-3 py-1 rounded-full text-sm">9 páginas</span>
                        <span className="bg-pink-100 text-pink-800 px-3 py-1 rounded-full text-sm">37 versões</span>
                        <span className="bg-pink-100 text-pink-800 px-3 py-1 rounded-full text-sm">Público: Gestores</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-pink-200">
                        docs/legacy-system/08-maintenance-history.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Migration Guide */}
                <div className="border-l-4 border-teal-500 bg-teal-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-teal-500 text-white rounded-lg p-3">
                      <Workflow className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">09. Guia de Migração COBOL → .NET</h4>
                      <p className="text-gray-700 mb-3">
                        Estratégia de reescrita, fases (análise/implementação/validação), riscos críticos, stack tecnológica,
                        testes de comparação byte-level e rollout.
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-teal-100 text-teal-800 px-3 py-1 rounded-full text-sm">14 páginas</span>
                        <span className="bg-teal-100 text-teal-800 px-3 py-1 rounded-full text-sm">6 meses prazo</span>
                        <span className="bg-teal-100 text-teal-800 px-3 py-1 rounded-full text-sm">Público: PMO/Tech Leads</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-teal-200">
                        docs/legacy-system/09-migration-guide.md
                      </code>
                    </div>
                  </div>
                </div>

                {/* Glossary */}
                <div className="border-l-4 border-cyan-500 bg-cyan-50 p-6 rounded-r-lg">
                  <div className="flex items-start gap-4">
                    <div className="bg-cyan-500 text-white rounded-lg p-3">
                      <BookOpen className="h-6 w-6" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-xl font-bold text-gray-900 mb-2">10. Glossário Técnico e de Negócio</h4>
                      <p className="text-gray-700 mb-3">
                        150+ termos: domínio de seguros (apólice, endosso, IOF), COBOL (PIC, COMP-3, OCCURS),
                        regulatórios SUSEP (Circular 360, ramos) e arquitetura .NET (EF Core, Clean Architecture).
                      </p>
                      <div className="flex flex-wrap gap-2">
                        <span className="bg-cyan-100 text-cyan-800 px-3 py-1 rounded-full text-sm">11 páginas</span>
                        <span className="bg-cyan-100 text-cyan-800 px-3 py-1 rounded-full text-sm">150+ termos</span>
                        <span className="bg-cyan-100 text-cyan-800 px-3 py-1 rounded-full text-sm">Público: Todos</span>
                      </div>
                      <code className="block mt-3 text-xs text-gray-600 bg-white p-2 rounded border border-cyan-200">
                        docs/legacy-system/10-glossary.md
                      </code>
                    </div>
                  </div>
                </div>

              </div>
            </section>

            {/* Additional Resources */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Recursos Adicionais</h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">

                <div className="bg-gradient-to-br from-blue-500 to-blue-600 rounded-lg p-6 text-white">
                  <div className="flex items-center gap-3 mb-3">
                    <FileText className="h-8 w-8" />
                    <h4 className="text-xl font-bold">PDF Consolidado</h4>
                  </div>
                  <p className="text-sm text-blue-100 mb-4">
                    Documento único com todos os 10 capítulos + índice navegável + anexos técnicos.
                    Ideal para impressão ou leitura offline.
                  </p>
                  <div className="bg-white/20 rounded p-3">
                    <code className="text-xs">docs/legacy-system/COMPLETE-COBOL-DOCUMENTATION.pdf</code>
                  </div>
                  <div className="mt-3 text-sm">
                    <strong>Tamanho:</strong> 2.8 MB | <strong>Páginas:</strong> 96
                  </div>
                </div>

                <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-lg p-6 text-white">
                  <div className="flex items-center gap-3 mb-3">
                    <FileSearch className="h-8 w-8" />
                    <h4 className="text-xl font-bold">Análise do Parser</h4>
                  </div>
                  <p className="text-sm text-purple-100 mb-4">
                    Relatório técnico completo da análise estática do código COBOL: estrutura AST,
                    dependências, complexidade ciclomática e métricas de qualidade.
                  </p>
                  <div className="bg-white/20 rounded p-3">
                    <code className="text-xs">docs/parser/FINAL-ANALYSIS-REPORT.md</code>
                  </div>
                  <div className="mt-3 text-sm">
                    <strong>Complexidade:</strong> Alta | <strong>Manutenibilidade:</strong> Média
                  </div>
                </div>

              </div>
            </section>

            {/* Quick Stats */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Estatísticas Consolidadas</h3>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-blue-700">96</div>
                  <div className="text-sm text-gray-600 mt-1">Páginas Totais</div>
                </div>
                <div className="bg-green-50 border border-green-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-green-700">5.046</div>
                  <div className="text-sm text-gray-600 mt-1">Linhas COBOL</div>
                </div>
                <div className="bg-purple-50 border border-purple-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-purple-700">687</div>
                  <div className="text-sm text-gray-600 mt-1">Variáveis</div>
                </div>
                <div className="bg-orange-50 border border-orange-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-orange-700">26+</div>
                  <div className="text-sm text-gray-600 mt-1">Tabelas DB2</div>
                </div>
                <div className="bg-red-50 border border-red-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-red-700">42</div>
                  <div className="text-sm text-gray-600 mt-1">Seções Lógica</div>
                </div>
                <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-yellow-700">8</div>
                  <div className="text-sm text-gray-600 mt-1">Anos Produção</div>
                </div>
                <div className="bg-indigo-50 border border-indigo-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-indigo-700">37</div>
                  <div className="text-sm text-gray-600 mt-1">Versões</div>
                </div>
                <div className="bg-pink-50 border border-pink-200 rounded-lg p-4 text-center">
                  <div className="text-3xl font-bold text-pink-700">150+</div>
                  <div className="text-sm text-gray-600 mt-1">Termos Glossário</div>
                </div>
              </div>
            </section>

            {/* Navigation Index */}
            <section className="mb-8">
              <h3 className="text-2xl font-semibold text-gray-800 mb-4">Índice de Navegação Rápida</h3>
              <div className="bg-gray-50 border border-gray-200 rounded-lg p-6">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div>
                    <h4 className="font-bold text-gray-700 mb-3">Documentação por Público:</h4>
                    <ul className="space-y-2 text-sm">
                      <li className="flex items-center gap-2">
                        <Users className="h-4 w-4 text-blue-600" />
                        <span><strong>C-level/Gestores:</strong> 01, 08, 09</span>
                      </li>
                      <li className="flex items-center gap-2">
                        <Code2 className="h-4 w-4 text-green-600" />
                        <span><strong>Desenvolvedores:</strong> 03, 05, 09, 10</span>
                      </li>
                      <li className="flex items-center gap-2">
                        <Layers className="h-4 w-4 text-purple-600" />
                        <span><strong>Arquitetos:</strong> 02, 04, 06</span>
                      </li>
                      <li className="flex items-center gap-2">
                        <Settings className="h-4 w-4 text-orange-600" />
                        <span><strong>DevOps/SRE:</strong> 07, 08</span>
                      </li>
                    </ul>
                  </div>
                  <div>
                    <h4 className="font-bold text-gray-700 mb-3">Documentação por Fase:</h4>
                    <ul className="space-y-2 text-sm">
                      <li className="flex items-center gap-2">
                        <CheckCircle2 className="h-4 w-4 text-green-600" />
                        <span><strong>Análise Legado:</strong> 01, 02, 03, 04, 05, 06, 07, 08</span>
                      </li>
                      <li className="flex items-center gap-2">
                        <Workflow className="h-4 w-4 text-blue-600" />
                        <span><strong>Planejamento Migração:</strong> 09</span>
                      </li>
                      <li className="flex items-center gap-2">
                        <BookOpen className="h-4 w-4 text-purple-600" />
                        <span><strong>Referência Contínua:</strong> 10 (glossário)</span>
                      </li>
                    </ul>
                  </div>
                </div>
              </div>
            </section>

            {/* Access Instructions */}
            <section className="bg-gradient-to-r from-green-50 to-blue-50 border border-green-200 rounded-lg p-6">
              <div className="flex items-start gap-4">
                <div className="bg-green-600 text-white rounded-lg p-3">
                  <FileText className="h-8 w-8" />
                </div>
                <div className="flex-1">
                  <h3 className="text-2xl font-bold text-gray-900 mb-3">Como Acessar a Documentação Completa</h3>
                  <p className="text-gray-700 mb-4">
                    Todos os documentos estão disponíveis no diretório raiz do projeto:
                  </p>
                  <div className="bg-white rounded-lg p-4 border border-gray-300 mb-4">
                    <code className="text-sm font-mono text-gray-800">📁 docs/legacy-system/</code>
                  </div>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-3">
                    <div className="bg-white rounded-lg p-3 border border-gray-200">
                      <div className="font-bold text-gray-900 mb-1">Formato Markdown (.md)</div>
                      <p className="text-sm text-gray-600">
                        Para leitura no VS Code, GitHub ou qualquer editor markdown. Suporta busca por texto e links internos.
                      </p>
                    </div>
                    <div className="bg-white rounded-lg p-3 border border-gray-200">
                      <div className="font-bold text-gray-900 mb-1">Formato PDF Consolidado</div>
                      <p className="text-sm text-gray-600">
                        Documento único para impressão ou distribuição offline. Inclui índice navegável e bookmarks.
                      </p>
                    </div>
                  </div>
                  <div className="mt-4 p-4 bg-yellow-100 border border-yellow-300 rounded-lg">
                    <p className="text-sm text-yellow-900">
                      <strong>💡 Dica:</strong> Comece pelo <code className="bg-yellow-200 px-2 py-1 rounded">01-executive-summary.md</code>
                      para entender o contexto geral, depois navegue para os capítulos técnicos conforme sua necessidade.
                    </p>
                  </div>
                </div>
              </div>
            </section>
          </div>
        </TabsContent>
      </Tabs>
    </div>
  );
}
