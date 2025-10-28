/**
 * Legacy System Documentation Page
 * Displays comprehensive documentation for RG1866B COBOL System (PREMIT/PREMCED SUSEP Reports)
 * Documentation source: /docs/legacy-system/
 */

import { useState } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '../components/ui/tabs';
import { FileText, Database, Cog, BookOpen, History, Map, AlertTriangle, Code, List, FileCode, Target } from 'lucide-react';

const LegacySystemDocsPage = () => {
  const [activeTab, setActiveTab] = useState('executive');

  const tabs = [
    {
      id: 'executive',
      label: 'Visão Executiva',
      icon: <Target className="w-4 h-4" />,
      description: 'Sumário executivo do sistema RG1866B'
    },
    {
      id: 'architecture',
      label: 'Arquitetura',
      icon: <Map className="w-4 h-4" />,
      description: 'Arquitetura técnica e fluxo de execução'
    },
    {
      id: 'data-structures',
      label: 'Estruturas de Dados',
      icon: <Code className="w-4 h-4" />,
      description: '687 variáveis COBOL e layouts de arquivo'
    },
    {
      id: 'database',
      label: 'Modelo de Banco',
      icon: <Database className="w-4 h-4" />,
      description: '26+ tabelas DB2 e cursores'
    },
    {
      id: 'business-logic',
      label: 'Lógica de Negócio',
      icon: <Cog className="w-4 h-4" />,
      description: 'Regras de negócio e cálculos financeiros'
    },
    {
      id: 'external-modules',
      label: 'Módulos Externos',
      icon: <FileCode className="w-4 h-4" />,
      description: 'RE0001S, GE0009S, GE0010S'
    },
    {
      id: 'operations',
      label: 'Guia Operacional',
      icon: <BookOpen className="w-4 h-4" />,
      description: 'JCL e procedimentos de execução'
    },
    {
      id: 'maintenance',
      label: 'Histórico de Manutenção',
      icon: <History className="w-4 h-4" />,
      description: '35+ manutenções desde 2014'
    },
    {
      id: 'migration',
      label: 'Guia de Migração',
      icon: <AlertTriangle className="w-4 h-4" />,
      description: 'Complexidades e recomendações'
    },
    {
      id: 'glossary',
      label: 'Glossário',
      icon: <List className="w-4 h-4" />,
      description: 'Termos técnicos e jargões SUSEP'
    },
    {
      id: 'complete',
      label: 'Documentação Completa',
      icon: <FileText className="w-4 h-4" />,
      description: 'Documento consolidado completo'
    },
  ];

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-gradient-caixa text-white">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
          <div className="text-center">
            <h1 className="text-4xl font-bold mb-4">
              Sistema Legado COBOL RG1866B
            </h1>
            <p className="text-xl text-blue-100 mb-6">
              Geração de Relatórios Regulatórios SUSEP Circular 360
            </p>

            {/* Key Metrics Cards */}
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mt-8">
              <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4">
                <div className="text-3xl font-bold">5.046</div>
                <div className="text-sm text-blue-100">Linhas de Código</div>
              </div>
              <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4">
                <div className="text-3xl font-bold">687</div>
                <div className="text-sm text-blue-100">Variáveis COBOL</div>
              </div>
              <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4">
                <div className="text-3xl font-bold">26+</div>
                <div className="text-sm text-blue-100">Tabelas DB2</div>
              </div>
              <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4">
                <div className="text-3xl font-bold">8+</div>
                <div className="text-sm text-blue-100">Anos em Produção</div>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* System Overview Card */}
        <div className="card-modern mb-6">
          <div className="p-6">
            <h2 className="text-2xl font-bold text-gray-900 mb-4">Sobre o Sistema</h2>
            <div className="grid md:grid-cols-2 gap-6">
              <div>
                <h3 className="font-semibold text-gray-900 mb-2">Propósito Principal</h3>
                <p className="text-gray-700">
                  Gerar mensalmente dois relatórios regulatórios obrigatórios (PREMIT.TXT e PREMCED.TXT)
                  para envio à SUSEP (Superintendência de Seguros Privados), contendo informações
                  detalhadas sobre prêmios de seguros emitidos pela Caixa Seguradora.
                </p>
              </div>
              <div>
                <h3 className="font-semibold text-gray-900 mb-2">Criticidade Regulatória</h3>
                <div className="space-y-2">
                  <div className="flex items-start">
                    <span className="inline-block w-2 h-2 bg-red-500 rounded-full mt-2 mr-2"></span>
                    <span className="text-gray-700">Circular SUSEP 360/2017: Formato obrigatório</span>
                  </div>
                  <div className="flex items-start">
                    <span className="inline-block w-2 h-2 bg-red-500 rounded-full mt-2 mr-2"></span>
                    <span className="text-gray-700">Penalidades: Até R$ 1.000.000 por não-conformidade</span>
                  </div>
                  <div className="flex items-start">
                    <span className="inline-block w-2 h-2 bg-red-500 rounded-full mt-2 mr-2"></span>
                    <span className="text-gray-700">Prazo: 15º dia útil do mês subsequente</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Documentation Tabs */}
        <div className="card-modern">
          <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
            <div className="border-b border-gray-200 px-6 pt-4">
              <TabsList className="w-full flex flex-wrap gap-2 bg-transparent">
                {tabs.map((tab) => (
                  <TabsTrigger
                    key={tab.id}
                    value={tab.id}
                    className="flex items-center gap-2 data-[state=active]:bg-caixa-blue-100 data-[state=active]:text-caixa-blue-700"
                  >
                    {tab.icon}
                    <span className="hidden md:inline">{tab.label}</span>
                    <span className="md:hidden">{tab.label.split(' ')[0]}</span>
                  </TabsTrigger>
                ))}
              </TabsList>
            </div>

            {/* Executive Summary */}
            <TabsContent value="executive" className="p-6">
              <div className="prose max-w-none">
                <h2 className="text-3xl font-bold text-gray-900 mb-6">Sumário Executivo</h2>

                <div className="grid md:grid-cols-2 gap-6 mb-8">
                  <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
                    <h3 className="text-lg font-semibold text-blue-900 mb-3">Informações Básicas</h3>
                    <dl className="space-y-2">
                      <div>
                        <dt className="text-sm font-medium text-blue-700">ID do Programa</dt>
                        <dd className="text-gray-900">RG1866B</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-blue-700">Sistema Pai</dt>
                        <dd className="text-gray-900">REGISTROS GERAIS</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-blue-700">Plataforma</dt>
                        <dd className="text-gray-900">IBM Mainframe z/OS</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-blue-700">Linguagem</dt>
                        <dd className="text-gray-900">COBOL ANSI 85</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-blue-700">Banco de Dados</dt>
                        <dd className="text-gray-900">IBM DB2 for z/OS</dd>
                      </div>
                      <div>
                        <dt className="text-sm font-medium text-blue-700">Data de Criação</dt>
                        <dd className="text-gray-900">21 de maio de 2014</dd>
                      </div>
                    </dl>
                  </div>

                  <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
                    <h3 className="text-lg font-semibold text-yellow-900 mb-3">Arquivos Gerados</h3>
                    <div className="space-y-4">
                      <div>
                        <h4 className="font-semibold text-gray-900">PREMIT.TXT - Prêmios Emitidos</h4>
                        <ul className="text-sm text-gray-700 mt-2 space-y-1">
                          <li>• 80+ campos por registro</li>
                          <li>• Formato: Fixed-width (1200 bytes/registro)</li>
                          <li>• Volume: 10.000-12.000 registros/mês (~50 MB)</li>
                          <li>• Propósito: Controle regulatório de emissões</li>
                        </ul>
                      </div>
                      <div>
                        <h4 className="font-semibold text-gray-900">PREMCED.TXT - Prêmios Cedidos</h4>
                        <ul className="text-sm text-gray-700 mt-2 space-y-1">
                          <li>• 40+ campos por registro</li>
                          <li>• Formato: Fixed-width (800 bytes/registro)</li>
                          <li>• Volume: 5.000-7.000 registros/mês (~20 MB)</li>
                          <li>• Propósito: Controle de cosseguro/resseguro</li>
                        </ul>
                      </div>
                    </div>
                  </div>
                </div>

                <div className="bg-red-50 border-l-4 border-red-500 p-4 mb-6">
                  <div className="flex">
                    <AlertTriangle className="h-5 w-5 text-red-500 mr-3 flex-shrink-0" />
                    <div>
                      <h3 className="text-lg font-semibold text-red-900 mb-2">Penalidades por Não-Conformidade</h3>
                      <ul className="space-y-1 text-red-800">
                        <li>• <strong>Atraso no envio:</strong> R$ 5.000 a R$ 50.000 por dia</li>
                        <li>• <strong>Dados inconsistentes:</strong> R$ 100.000 a R$ 500.000</li>
                        <li>• <strong>Não envio:</strong> R$ 500.000 a R$ 1.000.000 + processos</li>
                        <li>• <strong>Reincidência:</strong> Suspensão temporária de atividades</li>
                      </ul>
                    </div>
                  </div>
                </div>

                <div className="grid md:grid-cols-3 gap-4 mb-6">
                  <div className="bg-green-50 border border-green-200 rounded-lg p-4">
                    <h4 className="font-semibold text-green-900 mb-2">Performance</h4>
                    <ul className="text-sm text-green-800 space-y-1">
                      <li>• Duração: 45-60 minutos</li>
                      <li>• Disponibilidade: 99.8%</li>
                      <li>• Taxa de Erro: 0.3%</li>
                      <li>• Conformidade: 100%</li>
                    </ul>
                  </div>
                  <div className="bg-purple-50 border border-purple-200 rounded-lg p-4">
                    <h4 className="font-semibold text-purple-900 mb-2">Volume Mensal</h4>
                    <ul className="text-sm text-purple-800 space-y-1">
                      <li>• 10K-12K registros</li>
                      <li>• 8K-10K apólices</li>
                      <li>• 5K-7K endossos</li>
                      <li>• 500K-600K queries SQL</li>
                    </ul>
                  </div>
                  <div className="bg-indigo-50 border border-indigo-200 rounded-lg p-4">
                    <h4 className="font-semibold text-indigo-900 mb-2">Manutenção</h4>
                    <ul className="text-sm text-indigo-800 space-y-1">
                      <li>• 35+ alterações em 8 anos</li>
                      <li>• Taxa: 4 alterações/ano</li>
                      <li>• Período ativo: 2016-2017</li>
                      <li>• Motivo: Mudanças SUSEP (60%)</li>
                    </ul>
                  </div>
                </div>

                <div className="bg-blue-50 border border-blue-200 rounded-lg p-6">
                  <h3 className="text-xl font-bold text-blue-900 mb-4">ROI Estimado da Migração para .NET</h3>
                  <div className="grid md:grid-cols-2 gap-6">
                    <div>
                      <h4 className="font-semibold text-blue-800 mb-2">Benefícios Anuais</h4>
                      <ul className="space-y-2 text-blue-900">
                        <li>• Economia Mainframe: <strong>R$ 500.000</strong></li>
                        <li>• Redução Suporte: <strong>R$ 100.000</strong></li>
                        <li>• Ganhos Agilidade: <strong>R$ 200.000</strong></li>
                        <li className="text-lg font-bold pt-2 border-t border-blue-300">
                          Total: R$ 800.000/ano
                        </li>
                      </ul>
                    </div>
                    <div>
                      <h4 className="font-semibold text-blue-800 mb-2">Investimento</h4>
                      <ul className="space-y-2 text-blue-900">
                        <li>• Custo de Migração: <strong>R$ 1.200.000</strong> (one-time)</li>
                        <li className="text-lg font-bold pt-2 border-t border-blue-300">
                          Payback: 1,5 anos
                        </li>
                      </ul>
                    </div>
                  </div>
                </div>
              </div>
            </TabsContent>

            {/* Other tabs content placeholders - user can expand later */}
            <TabsContent value="architecture" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Arquitetura Técnica</h2>
                <p className="text-gray-600">Consulte o arquivo 02-architecture.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="data-structures" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Estruturas de Dados COBOL</h2>
                <p className="text-gray-600">Consulte o arquivo 03-data-structures.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="database" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Modelo de Banco de Dados DB2</h2>
                <p className="text-gray-600">Consulte o arquivo 04-database-model.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="business-logic" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Lógica de Negócio</h2>
                <p className="text-gray-600">Consulte o arquivo 05-business-logic.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="external-modules" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Módulos Externos</h2>
                <p className="text-gray-600">Consulte o arquivo 06-external-modules.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="operations" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Guia Operacional</h2>
                <p className="text-gray-600">Consulte o arquivo 07-operations-guide.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="maintenance" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Histórico de Manutenção</h2>
                <p className="text-gray-600">Consulte o arquivo 08-maintenance-history.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="migration" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Guia de Migração</h2>
                <p className="text-gray-600">Consulte o arquivo 09-migration-guide.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="glossary" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Glossário</h2>
                <p className="text-gray-600">Consulte o arquivo 10-glossary.md para detalhes completos.</p>
              </div>
            </TabsContent>

            <TabsContent value="complete" className="p-6">
              <div className="text-center py-12">
                <h2 className="text-2xl font-bold text-gray-900 mb-4">Documentação Completa Consolidada</h2>
                <p className="text-gray-600 mb-6">
                  Este documento consolida TODAS as informações técnicas do sistema RG1866B em um único arquivo.
                </p>
                <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4 max-w-2xl mx-auto">
                  <h4 className="font-semibold text-yellow-900 mb-3">Arquivos Disponíveis</h4>
                  <div className="space-y-2">
                    <div className="flex items-center justify-between p-2 bg-white border border-yellow-200 rounded">
                      <div className="flex items-center">
                        <FileText className="h-5 w-5 text-yellow-700 mr-2" />
                        <span className="text-gray-900 font-medium">COMPLETE-COBOL-DOCUMENTATION.md</span>
                      </div>
                      <span className="text-sm text-gray-600">~300 KB</span>
                    </div>
                    <div className="flex items-center justify-between p-2 bg-white border border-yellow-200 rounded">
                      <div className="flex items-center">
                        <FileText className="h-5 w-5 text-red-700 mr-2" />
                        <span className="text-gray-900 font-medium">COMPLETE-COBOL-DOCUMENTATION.pdf</span>
                      </div>
                      <span className="text-sm text-gray-600">~600 KB</span>
                    </div>
                  </div>
                </div>
              </div>
            </TabsContent>
          </Tabs>
        </div>
      </div>
    </div>
  );
};

export default LegacySystemDocsPage;
