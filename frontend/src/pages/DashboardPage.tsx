import { useState } from 'react';
import { Card, CardContent } from '../components/ui/card';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '../components/ui/tabs';
import { Badge } from '../components/ui/badge';

export const DashboardPage: React.FC = () => {
  const [activeTab, setActiveTab] = useState('overview');

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Hero Header */}
      <div className="bg-gradient-caixa text-white py-12 px-6 shadow-xl">
        <div className="container-modern">
          <div className="flex items-center gap-4 mb-4">
            <div className="w-16 h-16 bg-caixa-yellow-500 rounded-lg flex items-center justify-center shadow-lg">
              <span className="text-3xl">📊</span>
            </div>
            <div>
              <h1 className="text-4xl font-bold">Sistema RG1866B - Documentação Legacy</h1>
              <p className="text-xl text-caixa-blue-100 mt-1">
                Análise Completa do Sistema COBOL de Relatórios de Prêmios SUSEP Circular 360
              </p>
            </div>
          </div>
        </div>
      </div>

      <div className="container-modern py-8">
        {/* Tabs Navigation */}
        <Tabs value={activeTab} onValueChange={setActiveTab}>
          <TabsList className="bg-white mb-8 flex-wrap h-auto gap-2">
            <TabsTrigger value="overview" className="gap-2">
              📋 Visão Geral
            </TabsTrigger>
            <TabsTrigger value="architecture" className="gap-2">
              🏗️ Arquitetura
            </TabsTrigger>
            <TabsTrigger value="data" className="gap-2">
              💾 Estruturas de Dados
            </TabsTrigger>
            <TabsTrigger value="database" className="gap-2">
              🗄️ Modelo de Banco
            </TabsTrigger>
            <TabsTrigger value="business" className="gap-2">
              💼 Lógica de Negócio
            </TabsTrigger>
            <TabsTrigger value="external" className="gap-2">
              🔗 Módulos Externos
            </TabsTrigger>
            <TabsTrigger value="operations" className="gap-2">
              ⚙️ Operações
            </TabsTrigger>
            <TabsTrigger value="functions" className="gap-2">
              🎯 Pontos de Função
            </TabsTrigger>
            <TabsTrigger value="migration" className="gap-2">
              🚀 Plano de Migração
            </TabsTrigger>
          </TabsList>

          {/* Tab: Overview */}
          <TabsContent value="overview" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>📊</span> Sumário Executivo
                </h2>

                <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
                  <div className="stat-card">
                    <div className="text-3xl font-bold text-white">RG1866B</div>
                    <div className="text-sm text-caixa-blue-100 opacity-90">Identificação do Programa</div>
                  </div>
                  <div className="stat-card">
                    <div className="text-3xl font-bold text-white">5,046</div>
                    <div className="text-sm text-caixa-blue-100 opacity-90">Linhas de Código COBOL</div>
                  </div>
                  <div className="stat-card">
                    <div className="text-3xl font-bold text-white">770 FP</div>
                    <div className="text-sm text-caixa-blue-100 opacity-90">Pontos de Função (IFPUG)</div>
                  </div>
                </div>

                <div className="prose max-w-none">
                  <h3 className="text-xl font-semibold text-gray-900 mb-3">Propósito do Sistema</h3>
                  <p className="text-gray-700 mb-4">
                    O RG1866B é um programa batch COBOL responsável pela geração de relatórios de prêmios de seguros
                    para atender à <strong>SUSEP Circular 360/2017</strong>. O sistema processa dados de apólices,
                    endossos e movimentações de prêmios, gerando arquivos de saída em formato texto fixo para
                    submissão regulatória mensal.
                  </p>

                  <h3 className="text-xl font-semibold text-gray-900 mb-3">Funcionalidades Principais</h3>
                  <ul className="list-disc pl-6 text-gray-700 space-y-2">
                    <li><strong>Processamento de Prêmios:</strong> Leitura de dados de V0PREMIOS (cursor C1) com filtros de data e ramo SUSEP</li>
                    <li><strong>Cálculo de Cosseguro:</strong> Processamento complexo de múltiplas tabelas (GE399, GE066, GE062, GE082) para calcular percentuais de participação</li>
                    <li><strong>Geração de Relatórios:</strong> Criação de PREMIT.TXT (prêmios emitidos) e PREMCED.TXT (prêmios cedidos) com formato fixo de 687 posições</li>
                    <li><strong>Validação de Dados:</strong> 147+ regras de negócio para garantir integridade e conformidade SUSEP</li>
                    <li><strong>Auditoria:</strong> Registro de processamento, erros e estatísticas em logs estruturados</li>
                  </ul>

                  <h3 className="text-xl font-semibold text-gray-900 mb-3 mt-6">Execução Mensal</h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div className="bg-gray-50 p-4 rounded-lg">
                      <div className="font-semibold text-gray-900 mb-2">📅 Agendamento</div>
                      <p className="text-sm text-gray-700">
                        Execução mensal via JCL (Job Control Language) no mainframe.
                        Processamento noturno para minimizar impacto em sistemas online.
                      </p>
                    </div>
                    <div className="bg-gray-50 p-4 rounded-lg">
                      <div className="font-semibold text-gray-900 mb-2">⏱️ Performance</div>
                      <p className="text-sm text-gray-700">
                        Média de 2-4 horas para processar 10.000+ registros de prêmios.
                        Uso de cursores DB2 para otimização de memória.
                      </p>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>

            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>🎯</span> Requisitos Regulatórios
                </h2>
                <div className="bg-yellow-50 border-l-4 border-caixa-yellow-500 p-4 mb-4">
                  <div className="flex items-start gap-3">
                    <span className="text-2xl">⚖️</span>
                    <div>
                      <h3 className="font-semibold text-gray-900">SUSEP Circular 360/2017</h3>
                      <p className="text-sm text-gray-700 mt-1">
                        Estabelece normas e procedimentos para a remessa de informações contábeis,
                        financeiras e estatísticas das sociedades seguradoras ao Sistema de Estatísticas da SUSEP (SES).
                      </p>
                    </div>
                  </div>
                </div>

                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="bg-red-50 border border-red-200 p-4 rounded-lg">
                    <div className="font-semibold text-red-900 mb-2">⚠️ Penalidades por Não Conformidade</div>
                    <ul className="text-sm text-red-800 space-y-1">
                      <li>• Multas de até R$ 500.000,00</li>
                      <li>• Suspensão de autorização para operar</li>
                      <li>• Intervenção administrativa</li>
                    </ul>
                  </div>
                  <div className="bg-green-50 border border-green-200 p-4 rounded-lg">
                    <div className="font-semibold text-green-900 mb-2">✅ Requisitos de Conformidade</div>
                    <ul className="text-sm text-green-800 space-y-1">
                      <li>• Formato de arquivo exato (687 posições)</li>
                      <li>• Precisão decimal byte-a-byte</li>
                      <li>• Prazo de submissão (até dia 15)</li>
                      <li>• Auditoria completa de dados</li>
                    </ul>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: Architecture */}
          <TabsContent value="architecture" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>🏗️</span> Arquitetura do Sistema
                </h2>

                <div className="space-y-6">
                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Componentes Principais</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="component-card">
                        <div className="flex items-center gap-2 mb-2">
                          <Badge variant="default">COBOL</Badge>
                          <span className="font-semibold">RG1866B.cbl</span>
                        </div>
                        <p className="text-sm text-gray-600">
                          Programa principal batch com 5,046 linhas de código.
                          Processamento sequencial de cursores DB2.
                        </p>
                      </div>

                      <div className="component-card">
                        <div className="flex items-center gap-2 mb-2">
                          <Badge variant="secondary">DB2</Badge>
                          <span className="font-semibold">26+ Tabelas/Views</span>
                        </div>
                        <p className="text-sm text-gray-600">
                          Database relacional com views otimizadas (V0PREMIOS, V0APOLICE, V0ENDOSSO).
                        </p>
                      </div>

                      <div className="component-card">
                        <div className="flex items-center gap-2 mb-2">
                          <Badge variant="secondary">JCL</Badge>
                          <span className="font-semibold">Job Control</span>
                        </div>
                        <p className="text-sm text-gray-600">
                          Scripts de agendamento e execução batch mensal.
                        </p>
                      </div>

                      <div className="component-card">
                        <div className="flex items-center gap-2 mb-2">
                          <Badge variant="info">VSAM</Badge>
                          <span className="font-semibold">Arquivos de Saída</span>
                        </div>
                        <p className="text-sm text-gray-600">
                          PREMIT.TXT e PREMCED.TXT (formato fixo 687 posições).
                        </p>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Fluxo de Processamento</h3>
                    <div className="bg-gray-50 p-6 rounded-lg">
                      <div className="space-y-4">
                        <div className="flex items-start gap-4">
                          <div className="w-8 h-8 rounded-full bg-caixa-blue-700 text-white flex items-center justify-center font-bold flex-shrink-0">1</div>
                          <div>
                            <div className="font-semibold text-gray-900">Inicialização (R0100-R0400)</div>
                            <p className="text-sm text-gray-600">Abertura de cursores DB2, validação de parâmetros, inicialização de variáveis</p>
                          </div>
                        </div>
                        <div className="flex items-start gap-4">
                          <div className="w-8 h-8 rounded-full bg-caixa-blue-700 text-white flex items-center justify-center font-bold flex-shrink-0">2</div>
                          <div>
                            <div className="font-semibold text-gray-900">Processamento de Prêmios (R0500-R1300)</div>
                            <p className="text-sm text-gray-600">Loop principal: FETCH de V0PREMIOS, cálculos de valores, validações</p>
                          </div>
                        </div>
                        <div className="flex items-start gap-4">
                          <div className="w-8 h-8 rounded-full bg-caixa-blue-700 text-white flex items-center justify-center font-bold flex-shrink-0">3</div>
                          <div>
                            <div className="font-semibold text-gray-900">Cálculo de Cosseguro (R3000-R5500)</div>
                            <p className="text-sm text-gray-600">Processamento de tabelas GE399, GE066, GE062 para percentuais de participação</p>
                          </div>
                        </div>
                        <div className="flex items-start gap-4">
                          <div className="w-8 h-8 rounded-full bg-caixa-blue-700 text-white flex items-center justify-center font-bold flex-shrink-0">4</div>
                          <div>
                            <div className="font-semibold text-gray-900">Geração de Arquivos (R8000-R8200)</div>
                            <p className="text-sm text-gray-600">Escrita de registros em PREMIT.TXT e PREMCED.TXT com formatação fixa</p>
                          </div>
                        </div>
                        <div className="flex items-start gap-4">
                          <div className="w-8 h-8 rounded-full bg-caixa-blue-700 text-white flex items-center justify-center font-bold flex-shrink-0">5</div>
                          <div>
                            <div className="font-semibold text-gray-900">Finalização (R9000-R9900)</div>
                            <p className="text-sm text-gray-600">Fechamento de cursores, geração de logs, estatísticas de processamento</p>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Tecnologias Legacy</h3>
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                      <div className="tech-card">
                        <div className="font-semibold text-gray-900">COBOL 85</div>
                        <p className="text-xs text-gray-600">Linguagem de programação principal</p>
                      </div>
                      <div className="tech-card">
                        <div className="font-semibold text-gray-900">DB2 v11</div>
                        <p className="text-xs text-gray-600">Database relacional IBM</p>
                      </div>
                      <div className="tech-card">
                        <div className="font-semibold text-gray-900">z/OS</div>
                        <p className="text-xs text-gray-600">Sistema operacional mainframe</p>
                      </div>
                      <div className="tech-card">
                        <div className="font-semibold text-gray-900">JCL</div>
                        <p className="text-xs text-gray-600">Job Control Language</p>
                      </div>
                      <div className="tech-card">
                        <div className="font-semibold text-gray-900">VSAM</div>
                        <p className="text-xs text-gray-600">Virtual Storage Access Method</p>
                      </div>
                      <div className="tech-card">
                        <div className="font-semibold text-gray-900">CICS</div>
                        <p className="text-xs text-gray-600">Customer Information Control System</p>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: Data Structures */}
          <TabsContent value="data" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>💾</span> Estruturas de Dados
                </h2>

                <div className="space-y-6">
                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Working Storage (687 Data Items)</h3>
                    <p className="text-gray-700 mb-4">
                      O programa utiliza 687 variáveis na WORKING-STORAGE SECTION, organizadas em estruturas hierárquicas
                      para representar apólices, prêmios, endossos, cosseguros e dados de saída.
                    </p>

                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="data-structure-card">
                        <div className="font-semibold text-gray-900 mb-2">📄 WS-PREMIUM-RECORD</div>
                        <p className="text-sm text-gray-600 mb-2">Registro principal de prêmio (342 bytes)</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• WS-POLICY-NUMBER (PIC X(10))</li>
                          <li>• WS-ENDORSEMENT-NUMBER (PIC 9(8))</li>
                          <li>• WS-PREMIUM-AMOUNT (PIC 9(15)V99)</li>
                          <li>• WS-EFFECTIVE-DATE (PIC 9(8))</li>
                          <li>• WS-PRODUCT-CODE (PIC X(6))</li>
                          <li>+ 28 outros campos</li>
                        </ul>
                      </div>

                      <div className="data-structure-card">
                        <div className="font-semibold text-gray-900 mb-2">🔢 WS-COSSURANCE-DATA</div>
                        <p className="text-sm text-gray-600 mb-2">Dados de cosseguro (156 bytes)</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• WS-TOTAL-PERCENTAGE (PIC 9(3)V99)</li>
                          <li>• WS-CEDANT-PERCENTAGE (PIC 9(3)V99)</li>
                          <li>• WS-CEDENT-AMOUNT (PIC 9(15)V99)</li>
                          <li>• WS-REINSURER-COUNT (PIC 9(3))</li>
                          <li>+ 14 outros campos</li>
                        </ul>
                      </div>

                      <div className="data-structure-card">
                        <div className="font-semibold text-gray-900 mb-2">📊 WS-OUTPUT-RECORD</div>
                        <p className="text-sm text-gray-600 mb-2">Registro de saída PREMIT.TXT (687 bytes)</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• Layout fixo de 687 posições</li>
                          <li>• Campos numéricos sem decimal (implied V)</li>
                          <li>• Campos alfanuméricos com padding</li>
                          <li>• Totais de 42 campos definidos pela SUSEP</li>
                        </ul>
                      </div>

                      <div className="data-structure-card">
                        <div className="font-semibold text-gray-900 mb-2">🎯 WS-COUNTERS</div>
                        <p className="text-sm text-gray-600 mb-2">Contadores de processamento (48 bytes)</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• WS-TOTAL-RECORDS (PIC 9(9))</li>
                          <li>• WS-RECORDS-PROCESSED (PIC 9(9))</li>
                          <li>• WS-RECORDS-REJECTED (PIC 9(9))</li>
                          <li>• WS-TOTAL-AMOUNT (PIC 9(15)V99)</li>
                        </ul>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Tipos de Dados COBOL → .NET Mapping</h3>
                    <div className="overflow-x-auto">
                      <table className="table-modern">
                        <thead>
                          <tr>
                            <th>COBOL PIC</th>
                            <th>Exemplo</th>
                            <th>.NET Type</th>
                            <th>Observações</th>
                          </tr>
                        </thead>
                        <tbody>
                          <tr>
                            <td className="font-mono text-sm">PIC 9(15)V99</td>
                            <td className="font-mono text-xs">000000001234567</td>
                            <td className="font-mono text-sm">decimal(17,2)</td>
                            <td className="text-xs">Valores monetários (precisão exata)</td>
                          </tr>
                          <tr>
                            <td className="font-mono text-sm">PIC X(10)</td>
                            <td className="font-mono text-xs">"ABC&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"</td>
                            <td className="font-mono text-sm">string</td>
                            <td className="text-xs">Alfanumérico com padding à direita</td>
                          </tr>
                          <tr>
                            <td className="font-mono text-sm">PIC 9(8)</td>
                            <td className="font-mono text-xs">20251027</td>
                            <td className="font-mono text-sm">DateTime</td>
                            <td className="text-xs">Data YYYYMMDD (conversão necessária)</td>
                          </tr>
                          <tr>
                            <td className="font-mono text-sm">PIC 9(3)V99</td>
                            <td className="font-mono text-xs">12345</td>
                            <td className="font-mono text-sm">decimal(5,2)</td>
                            <td className="text-xs">Percentuais (123.45%)</td>
                          </tr>
                          <tr>
                            <td className="font-mono text-sm">PIC S9(9) COMP-3</td>
                            <td className="font-mono text-xs">Packed</td>
                            <td className="font-mono text-sm">int</td>
                            <td className="text-xs">Inteiro com sinal (packed decimal)</td>
                          </tr>
                        </tbody>
                      </table>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: Database */}
          <TabsContent value="database" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>🗄️</span> Modelo de Banco de Dados
                </h2>

                <div className="space-y-6">
                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Tabelas e Views Principais (26+)</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">V0PREMIOS</span>
                          <Badge variant="default">View</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Cursor principal (C1) - Prêmios e movimentações</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_APOLICE (PK)</li>
                          <li>• NUM_ENDOSSO</li>
                          <li>• VAL_PREMIO_TOTAL</li>
                          <li>• DAT_VIGENCIA</li>
                          <li>• COD_RAMO_SUSEP</li>
                        </ul>
                      </div>

                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">V0APOLICE</span>
                          <Badge variant="default">View</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Dados mestres de apólices</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_APOLICE (PK)</li>
                          <li>• COD_EMPRESA</li>
                          <li>• COD_PRODUTO</li>
                          <li>• STA_APOLICE</li>
                          <li>• NUM_PROPOSTA</li>
                        </ul>
                      </div>

                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">V0ENDOSSO</span>
                          <Badge variant="default">View</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Movimentações de endossos</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_APOLICE (FK)</li>
                          <li>• NUM_ENDOSSO (PK)</li>
                          <li>• TIP_MOVIMENTO</li>
                          <li>• DAT_MOVIMENTO</li>
                          <li>• VAL_PREMIO_LIQUIDO</li>
                        </ul>
                      </div>

                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">GE399</span>
                          <Badge variant="secondary">Table</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Cosseguro - Apólices participantes</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_APOLICE (FK)</li>
                          <li>• COD_COSSEGURO</li>
                          <li>• PER_PARTICIPACAO</li>
                          <li>• COD_SEGURADORA</li>
                        </ul>
                      </div>

                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">GE066</span>
                          <Badge variant="secondary">Table</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Participantes de cosseguro</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_COSSEGURO (PK)</li>
                          <li>• NOM_SEGURADORA</li>
                          <li>• NUM_SUSEP</li>
                          <li>• TIP_PARTICIPACAO</li>
                        </ul>
                      </div>

                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">V0PRODUTO</span>
                          <Badge variant="default">View</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Catálogo de produtos</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_PRODUTO (PK)</li>
                          <li>• NOM_PRODUTO</li>
                          <li>• COD_RAMO_SUSEP</li>
                          <li>• STA_ATIVO</li>
                        </ul>
                      </div>

                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">V0CLIENTE</span>
                          <Badge variant="default">View</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Dados de segurados</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_CLIENTE (PK)</li>
                          <li>• NUM_CPF_CNPJ</li>
                          <li>• NOM_CLIENTE</li>
                          <li>• TIP_PESSOA (PF/PJ)</li>
                        </ul>
                      </div>

                      <div className="db-table-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">GE062</span>
                          <Badge variant="secondary">Table</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Percentuais de resseguro</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• COD_APOLICE (FK)</li>
                          <li>• PER_CESSAO</li>
                          <li>• COD_RESSEGURADORA</li>
                          <li>• TIP_RESSEGURO</li>
                        </ul>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Cursores DB2</h3>
                    <div className="bg-gray-50 p-4 rounded-lg">
                      <div className="space-y-4">
                        <div>
                          <div className="font-semibold text-gray-900 mb-1">C1 - Cursor Principal (V0PREMIOS)</div>
                          <pre className="bg-gray-800 text-green-400 p-3 rounded text-xs overflow-x-auto">
{`DECLARE C1 CURSOR FOR
SELECT COD_APOLICE, NUM_ENDOSSO, VAL_PREMIO_TOTAL,
       DAT_VIGENCIA, COD_RAMO_SUSEP, TIP_MOVIMENTO
FROM V0PREMIOS
WHERE DAT_VIGENCIA BETWEEN :WS-START-DATE AND :WS-END-DATE
  AND COD_RAMO_SUSEP IN (SELECT COD_RAMO FROM WS-RAMO-TABLE)
ORDER BY COD_APOLICE, NUM_ENDOSSO`}
                          </pre>
                        </div>

                        <div>
                          <div className="font-semibold text-gray-900 mb-1">C2 - Cursor de Cosseguro (GE399)</div>
                          <pre className="bg-gray-800 text-green-400 p-3 rounded text-xs overflow-x-auto">
{`DECLARE C2 CURSOR FOR
SELECT COD_COSSEGURO, PER_PARTICIPACAO, COD_SEGURADORA
FROM GE399
WHERE COD_APOLICE = :WS-POLICY-NUMBER
ORDER BY PER_PARTICIPACAO DESC`}
                          </pre>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: Business Logic */}
          <TabsContent value="business" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>💼</span> Lógica de Negócio
                </h2>

                <div className="space-y-6">
                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Regras de Negócio (147+ regras)</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="business-rule-card">
                        <div className="font-semibold text-gray-900 mb-2">BR001 - Validação de Data</div>
                        <p className="text-sm text-gray-600">
                          Data de vigência deve estar no mês de processamento (DAT_VIGENCIA BETWEEN start AND end)
                        </p>
                      </div>

                      <div className="business-rule-card">
                        <div className="font-semibold text-gray-900 mb-2">BR015 - Cálculo de Prêmio Líquido</div>
                        <p className="text-sm text-gray-600">
                          PremioLiquido = PremioBruto - Descontos - IOF - CustoApolice
                        </p>
                      </div>

                      <div className="business-rule-card">
                        <div className="font-semibold text-gray-900 mb-2">BR032 - Percentual de Cosseguro</div>
                        <p className="text-sm text-gray-600">
                          Soma dos percentuais de participação deve totalizar 100.00% (validação com tolerância de 0.01)
                        </p>
                      </div>

                      <div className="business-rule-card">
                        <div className="font-semibold text-gray-900 mb-2">BR047 - Tipo de Movimento</div>
                        <p className="text-sm text-gray-600">
                          Valores permitidos: 1-Emissão, 2-Endosso, 3-Cancelamento, 4-Renovação, 5-Inclusão, 6-Exclusão
                        </p>
                      </div>

                      <div className="business-rule-card">
                        <div className="font-semibold text-gray-900 mb-2">BR056 - Ramo SUSEP Válido</div>
                        <p className="text-sm text-gray-600">
                          Código de ramo deve existir na tabela de ramos SUSEP (validação contra tabela regulatória)
                        </p>
                      </div>

                      <div className="business-rule-card">
                        <div className="font-semibold text-gray-900 mb-2">BR089 - Prêmio Cedido</div>
                        <p className="text-sm text-gray-600">
                          PremioCedido = PremioTotal × (PercentualCosseguro / 100) para cada participante
                        </p>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Tipos de Movimento</h3>
                    <div className="overflow-x-auto">
                      <table className="table-modern">
                        <thead>
                          <tr>
                            <th>Código</th>
                            <th>Descrição</th>
                            <th>Impacto no Prêmio</th>
                            <th>PREMIT</th>
                            <th>PREMCED</th>
                          </tr>
                        </thead>
                        <tbody>
                          <tr>
                            <td className="font-mono">1</td>
                            <td>Emissão Original</td>
                            <td className="text-green-600">+ Valor total</td>
                            <td>✅ Sim</td>
                            <td>✅ Se cosseguro</td>
                          </tr>
                          <tr>
                            <td className="font-mono">2</td>
                            <td>Endosso</td>
                            <td className="text-blue-600">± Diferença</td>
                            <td>✅ Sim</td>
                            <td>✅ Se cosseguro</td>
                          </tr>
                          <tr>
                            <td className="font-mono">3</td>
                            <td>Cancelamento</td>
                            <td className="text-red-600">- Valor total</td>
                            <td>✅ Sim</td>
                            <td>✅ Se cosseguro</td>
                          </tr>
                          <tr>
                            <td className="font-mono">4</td>
                            <td>Renovação</td>
                            <td className="text-green-600">+ Novo valor</td>
                            <td>✅ Sim</td>
                            <td>✅ Se cosseguro</td>
                          </tr>
                          <tr>
                            <td className="font-mono">5</td>
                            <td>Inclusão de Cobertura</td>
                            <td className="text-green-600">+ Valor cobertura</td>
                            <td>✅ Sim</td>
                            <td>❌ Não</td>
                          </tr>
                          <tr>
                            <td className="font-mono">6</td>
                            <td>Exclusão de Cobertura</td>
                            <td className="text-red-600">- Valor cobertura</td>
                            <td>✅ Sim</td>
                            <td>❌ Não</td>
                          </tr>
                        </tbody>
                      </table>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Cálculos Complexos</h3>
                    <div className="bg-blue-50 border-l-4 border-caixa-blue-700 p-4">
                      <div className="font-semibold text-gray-900 mb-2">Algoritmo de Cosseguro (R3000-R5500)</div>
                      <div className="space-y-2 text-sm text-gray-700">
                        <p>1. Buscar apólice na GE399 (COD_APOLICE)</p>
                        <p>2. Para cada participante:</p>
                        <div className="pl-6 space-y-1">
                          <p>a. Ler percentual de participação (PER_PARTICIPACAO)</p>
                          <p>b. Buscar dados da seguradora em GE066 (COD_SEGURADORA)</p>
                          <p>c. Calcular: PremioCedido = PremioTotal × (PER_PARTICIPACAO / 100)</p>
                          <p>d. Validar: Soma(PER_PARTICIPACAO) = 100.00 (tolerância ±0.01)</p>
                        </div>
                        <p>3. Gerar registro em PREMCED.TXT para cada cessão</p>
                        <p>4. Atualizar totalizadores e contadores</p>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: External Modules */}
          <TabsContent value="external" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>🔗</span> Módulos Externos e Integrações
                </h2>

                <div className="space-y-6">
                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Chamadas de Subprogramas</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="module-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">RLDATE</span>
                          <Badge variant="default">Utilitário</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Manipulação de datas</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• Conversão YYYYMMDD ↔ Julian</li>
                          <li>• Validação de datas</li>
                          <li>• Cálculo de diferenças</li>
                        </ul>
                      </div>

                      <div className="module-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">RLMATH</span>
                          <Badge variant="default">Utilitário</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Funções matemáticas</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• Arredondamento de decimais</li>
                          <li>• Cálculos de percentuais</li>
                          <li>• Validação numérica</li>
                        </ul>
                      </div>

                      <div className="module-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">RLLOG</span>
                          <Badge variant="secondary">Log</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Sistema de logging</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• Registro de eventos</li>
                          <li>• Logs de erro e warning</li>
                          <li>• Auditoria de processamento</li>
                        </ul>
                      </div>

                      <div className="module-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">RLDB2</span>
                          <Badge variant="secondary">Database</Badge>
                        </div>
                        <p className="text-sm text-gray-600 mb-2">Utilitários DB2</p>
                        <ul className="text-xs text-gray-600 space-y-1">
                          <li>• Tratamento de SQLCODE</li>
                          <li>• Formatação de queries</li>
                          <li>• Connection pooling</li>
                        </ul>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Arquivos de Saída</h3>
                    <div className="space-y-4">
                      <div className="file-card">
                        <div className="flex items-center justify-between mb-3">
                          <div>
                            <span className="font-semibold text-gray-900 text-lg">PREMIT.TXT</span>
                            <Badge variant="success" className="ml-2">Prêmios Emitidos</Badge>
                          </div>
                          <span className="text-sm text-gray-600">687 bytes/registro</span>
                        </div>
                        <div className="bg-gray-50 p-3 rounded">
                          <div className="text-sm text-gray-700 mb-2">
                            Contém todos os prêmios emitidos no período, incluindo emissões, endossos, cancelamentos e renovações.
                          </div>
                          <div className="text-xs text-gray-600">
                            <strong>Estrutura:</strong> 42 campos definidos pela SUSEP Circular 360, formato fixo sem delimitadores.
                          </div>
                        </div>
                      </div>

                      <div className="file-card">
                        <div className="flex items-center justify-between mb-3">
                          <div>
                            <span className="font-semibold text-gray-900 text-lg">PREMCED.TXT</span>
                            <Badge variant="warning" className="ml-2">Prêmios Cedidos</Badge>
                          </div>
                          <span className="text-sm text-gray-600">687 bytes/registro</span>
                        </div>
                        <div className="bg-gray-50 p-3 rounded">
                          <div className="text-sm text-gray-700 mb-2">
                            Contém cessões de cosseguro, com detalhamento de participação de cada seguradora participante.
                          </div>
                          <div className="text-xs text-gray-600">
                            <strong>Estrutura:</strong> Mesma estrutura do PREMIT.TXT, com campos adicionais de identificação de cessão.
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Dependências de Sistema</h3>
                    <div className="bg-yellow-50 border border-yellow-200 p-4 rounded-lg">
                      <div className="flex items-start gap-3">
                        <span className="text-2xl">⚠️</span>
                        <div className="flex-1">
                          <div className="font-semibold text-gray-900 mb-2">Crítico para Migração</div>
                          <ul className="text-sm text-gray-700 space-y-1">
                            <li>• <strong>DB2 v11:</strong> Todas as queries devem ser compatíveis ou migradas para SQL Server/PostgreSQL</li>
                            <li>• <strong>Módulos CALL:</strong> RLDATE, RLMATH, RLLOG devem ser reimplementados em .NET</li>
                            <li>• <strong>VSAM Files:</strong> Substituir por System.IO com formatação exata de campos</li>
                            <li>• <strong>JCL Scheduling:</strong> Migrar para Azure Functions ou Hangfire</li>
                          </ul>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: Operations */}
          <TabsContent value="operations" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>⚙️</span> Guia de Operações
                </h2>

                <div className="space-y-6">
                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Procedimentos de Execução</h3>
                    <div className="space-y-4">
                      <div className="procedure-card">
                        <div className="font-semibold text-gray-900 mb-2">1. Preparação (Pré-processamento)</div>
                        <ul className="text-sm text-gray-700 space-y-2">
                          <li>✓ Validar disponibilidade do banco DB2</li>
                          <li>✓ Verificar espaço em disco para arquivos de saída (mínimo 500MB)</li>
                          <li>✓ Confirmar parâmetros de data (início e fim do mês)</li>
                          <li>✓ Backup de execuções anteriores (PREMIT/PREMCED do mês anterior)</li>
                        </ul>
                      </div>

                      <div className="procedure-card">
                        <div className="font-semibold text-gray-900 mb-2">2. Execução (Processamento)</div>
                        <ul className="text-sm text-gray-700 space-y-2">
                          <li>✓ Submeter job JCL com parâmetros: START-DATE, END-DATE, RAMO-SUSEP</li>
                          <li>✓ Monitorar logs em tempo real (SYSOUT)</li>
                          <li>✓ Verificar contadores de processamento (WS-TOTAL-RECORDS)</li>
                          <li>✓ Aguardar finalização (média 2-4 horas)</li>
                        </ul>
                      </div>

                      <div className="procedure-card">
                        <div className="font-semibold text-gray-900 mb-2">3. Validação (Pós-processamento)</div>
                        <ul className="text-sm text-gray-700 space-y-2">
                          <li>✓ Verificar RC (Return Code) = 0</li>
                          <li>✓ Validar quantidade de registros gerados vs esperados</li>
                          <li>✓ Executar programa de validação SUSEP (RG1866V)</li>
                          <li>✓ Comparar totalizadores (soma de prêmios) com relatórios contábeis</li>
                        </ul>
                      </div>

                      <div className="procedure-card">
                        <div className="font-semibold text-gray-900 mb-2">4. Submissão à SUSEP</div>
                        <ul className="text-sm text-gray-700 space-y-2">
                          <li>✓ Fazer download de PREMIT.TXT e PREMCED.TXT</li>
                          <li>✓ Compactar arquivos (ZIP com senha)</li>
                          <li>✓ Enviar via portal SES (Sistema de Estatísticas da SUSEP)</li>
                          <li>✓ Aguardar confirmação de recebimento (protocolo)</li>
                        </ul>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Tratamento de Erros</h3>
                    <div className="overflow-x-auto">
                      <table className="table-modern">
                        <thead>
                          <tr>
                            <th>Código</th>
                            <th>Descrição</th>
                            <th>Ação Corretiva</th>
                          </tr>
                        </thead>
                        <tbody>
                          <tr>
                            <td className="font-mono">RC=04</td>
                            <td>Erro de leitura DB2 (SQLCODE -911)</td>
                            <td>Reexecutar job (deadlock temporário)</td>
                          </tr>
                          <tr>
                            <td className="font-mono">RC=08</td>
                            <td>Arquivo de saída não pôde ser aberto</td>
                            <td>Verificar permissões e espaço em disco</td>
                          </tr>
                          <tr>
                            <td className="font-mono">RC=12</td>
                            <td>Validação de negócio falhou (BR032)</td>
                            <td>Corrigir dados na origem e reprocessar</td>
                          </tr>
                          <tr>
                            <td className="font-mono">RC=16</td>
                            <td>Parâmetros de entrada inválidos</td>
                            <td>Revisar JCL (START-DATE/END-DATE)</td>
                          </tr>
                        </tbody>
                      </table>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Manutenção e Suporte</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="support-card">
                        <div className="font-semibold text-gray-900 mb-2">📞 Contatos de Suporte</div>
                        <ul className="text-sm text-gray-700 space-y-1">
                          <li>• Equipe Mainframe: mainframe@caixaseguradora.com.br</li>
                          <li>• DBA DB2: dba-db2@caixaseguradora.com.br</li>
                          <li>• Compliance SUSEP: compliance@caixaseguradora.com.br</li>
                        </ul>
                      </div>

                      <div className="support-card">
                        <div className="font-semibold text-gray-900 mb-2">📅 Cronograma Mensal</div>
                        <ul className="text-sm text-gray-700 space-y-1">
                          <li>• Dia 1-3: Validação de dados mestres</li>
                          <li>• Dia 5: Execução do job RG1866B</li>
                          <li>• Dia 6-7: Validação de arquivos</li>
                          <li>• Dia 10-15: Submissão à SUSEP (prazo legal)</li>
                        </ul>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: Function Points */}
          <TabsContent value="functions" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>🎯</span> Análise de Pontos de Função
                </h2>

                <div className="space-y-6">
                  <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <div className="stat-card-large">
                      <div className="text-4xl font-bold text-caixa-blue-700">770 FP</div>
                      <div className="text-sm text-gray-600 mt-1">Total Ajustado (AFP)</div>
                      <div className="text-xs text-gray-500 mt-1">619 UFP • VAF composto 1.24</div>
                    </div>
                    <div className="stat-card-large">
                      <div className="text-4xl font-bold text-green-600">490 FP</div>
                      <div className="text-sm text-gray-600 mt-1">Fase 1 - Migração Core</div>
                      <div className="text-xs text-gray-500 mt-1">63.6% do total • 433 UFP • VAF 1.13</div>
                    </div>
                    <div className="stat-card-large">
                      <div className="text-4xl font-bold text-blue-600">280 FP</div>
                      <div className="text-sm text-gray-600 mt-1">Fase 2 - Melhorias & Frontend</div>
                      <div className="text-xs text-gray-500 mt-1">36.4% do total • 186 UFP • VAF 1.50</div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Fase 1 - Migração Core (490 FP)</h3>
                    <p className="text-sm text-gray-600 mb-3">
                      Replica fiel do RG1866B em .NET 9, com geração de arquivos regulatórios e integrações DB2.
                    </p>
                    <div className="space-y-3">
                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F01 - Cálculo de Prêmios</span>
                          <Badge variant="default">85 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Implementação das seis regras de movimento (emissão, endosso, cancelamento, renovação e variações).
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F02 - Processamento Cosseguro</span>
                          <Badge variant="default">65 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Cálculo de participação de seguradoras e geração das informações PREMCED.TXT.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F03 - Geração Fixed-Width</span>
                          <Badge variant="default">58 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Formatação byte-a-byte dos arquivos PREMIT.TXT (1200 bytes) e PREMCED.TXT (800 bytes).
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F04 - Integração Banco de Dados</span>
                          <Badge variant="default">52 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Mapeamento de 26+ views DB2, cursores e repositórios utilizando Entity Framework Core.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F05 - Módulo de Resseguro</span>
                          <Badge variant="default">45 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Migração do RE0001S com cálculos proporcional, excedente e não-proporcional.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F06 - Módulo de Formatação</span>
                          <Badge variant="default">38 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Portabilidade do GE0009S para formatação numérica, alfanumérica, datas e moeda.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F07 - Módulo de Validação</span>
                          <Badge variant="default">32 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Migração do GE0010S com validações de CPF/CNPJ, datas e códigos SUSEP.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F08 - Validação de Parâmetros</span>
                          <Badge variant="default">28 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Conferência de datas, códigos de companhia e parâmetros obrigatórios para o batch.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F09 - API REST Básica</span>
                          <Badge variant="default">25 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Endpoints <code>/generate</code>, <code>/status</code>, <code>/download</code> e <code>/health</code> para orquestração.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F10 - Agendamento de Jobs</span>
                          <Badge variant="default">22 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Substituição do TWS por Hangfire com execução automática mensal e reprocessos.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F11 - Logging Básico</span>
                          <Badge variant="default">18 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Observabilidade com Serilog, correlação e trilhas de auditoria técnica.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F12 - Comparação COBOL vs .NET</span>
                          <Badge variant="default">22 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Ferramentas de validação byte-a-byte com checksums e relatório de divergências.
                        </p>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Fase 2 - Melhorias e Extensões (280 FP)</h3>
                    <p className="text-sm text-gray-600 mb-3">
                      Camada de experiência moderna (React + APIs) e operações avançadas para times de negócio.
                    </p>
                    <div className="space-y-3">
                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F13 - Dashboard Analítico</span>
                          <Badge variant="secondary">65 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          KPIs regulatórios, tendências e métricas por ramo/produto com visualizações interativas.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F14 - Query Builder Visual</span>
                          <Badge variant="secondary">52 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Composição de consultas ad-hoc com filtros, agregações e períodos customizados.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F15 - Geração Interativa de Relatórios</span>
                          <Badge variant="secondary">45 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Execução sob demanda com feedback em tempo real e histórico de execuções.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F16 - Visualização de Dados</span>
                          <Badge variant="secondary">40 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Gráficos interativos (heatmaps, séries temporais, comparativos) com drill-down.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F17 - Exportação Multi-formato</span>
                          <Badge variant="secondary">28 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Exportações em Excel, CSV, JSON e PDF alinhadas à política de compliance.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F18 - Monitoramento de Jobs</span>
                          <Badge variant="secondary">22 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Painel operacional com duração, status, tentativas e alertas de SLA.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F19 - Gestão de Mock Data</span>
                          <Badge variant="secondary">18 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Upload e validação de datasets de teste, com reset de ambiente automatizado.
                        </p>
                      </div>

                      <div className="fp-card">
                        <div className="flex items-center justify-between mb-2">
                          <span className="font-semibold text-gray-900">F20 - Autenticação e RBAC</span>
                          <Badge variant="secondary">35 FP</Badge>
                        </div>
                        <p className="text-sm text-gray-600">
                          Login seguro, controle de acesso por perfil (Admin, Operador, Auditor) e logs de auditoria.
                        </p>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Distribuição por Tipo (UFP)</h3>
                    <div className="bg-gray-50 p-6 rounded-lg">
                      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <div className="text-center">
                          <div className="text-3xl font-bold text-caixa-blue-700">227 UFP</div>
                          <div className="text-sm text-gray-600 mt-1">Transações (EI + EO)</div>
                          <div className="text-xs text-gray-500">132 UFP Fase 1 • 95 UFP Fase 2</div>
                        </div>
                        <div className="text-center">
                          <div className="text-3xl font-bold text-green-600">274 UFP</div>
                          <div className="text-sm text-gray-600 mt-1">Arquivos Internos (ILF)</div>
                          <div className="text-xs text-gray-500">260 UFP Fase 1 • 14 UFP Fase 2</div>
                        </div>
                        <div className="text-center">
                          <div className="text-3xl font-bold text-blue-600">118 UFP</div>
                          <div className="text-sm text-gray-600 mt-1">Consultas & Interfaces (EQ + EIF)</div>
                          <div className="text-xs text-gray-500">72 UFP Fase 1 • 46 UFP Fase 2</div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          {/* Tab: Migration Plan */}
          <TabsContent value="migration" className="space-y-6">
            <Card className="card-modern">
              <CardContent className="p-6">
                <h2 className="text-2xl font-bold text-gray-900 mb-4 flex items-center gap-2">
                  <span>🚀</span> Plano de Migração COBOL → .NET
                </h2>

                <div className="space-y-6">
                  <div className="bg-blue-50 border-l-4 border-caixa-blue-700 p-4">
                    <div className="flex items-start gap-3">
                      <span className="text-2xl">📋</span>
                      <div>
                        <h3 className="font-semibold text-caixa-blue-900">Cronograma: 3 Meses</h3>
                        <p className="text-sm text-caixa-blue-800 mt-1">
                          2 meses de implementação (8 semanas) + 1 mês de validação e deploy (4 semanas). Total de 12 semanas para migração completa do sistema legado.
                        </p>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Mês 1-2: Implementação (8 Semanas)</h3>
                    <div className="space-y-4">
                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">1</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semanas 1-2: Análise e Setup</div>
                              <div className="text-sm text-gray-600">Fundação do Projeto</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ Análise completa do código COBOL RG1866B (5,046 linhas)</li>
                          <li>□ Documentação de 26+ tabelas e views DB2</li>
                          <li>□ Setup de projeto .NET 9.0 com Clean Architecture</li>
                          <li>□ Configuração de pipeline CI/CD e ambiente de desenvolvimento</li>
                        </ul>
                      </div>

                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">2</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semanas 3-4: Camada de Dados</div>
                              <div className="text-sm text-gray-600">Entidades e Repositórios</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ Mapeamento de 15 entidades de domínio (Policy, Premium, Endorsement, etc.)</li>
                          <li>□ Configurações EF Core com Fluent API</li>
                          <li>□ Implementação de repositórios e interfaces</li>
                          <li>□ Migração de dados DB2 → Azure SQL</li>
                        </ul>
                      </div>

                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">3</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semanas 5-6: Lógica de Negócio</div>
                              <div className="text-sm text-gray-600">Regras e Cálculos</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ Implementação de 147+ regras de negócio</li>
                          <li>□ Algoritmo de cosseguro (GE399/GE066/GE062)</li>
                          <li>□ Cálculos de prêmios com precisão decimal</li>
                          <li>□ FixedWidthFormatter para saída PREMIT/PREMCED</li>
                        </ul>
                      </div>

                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">4</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semana 7: API REST</div>
                              <div className="text-sm text-gray-600">Endpoints e Integração</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ Desenvolvimento de 28 endpoints REST (9 categorias)</li>
                          <li>□ Documentação Swagger/OpenAPI</li>
                          <li>□ CORS, autenticação, logging e error handling</li>
                          <li>□ DTOs e AutoMapper para transformação de dados</li>
                        </ul>
                      </div>

                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">5</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semana 8: Frontend React</div>
                              <div className="text-sm text-gray-600">Interface do Usuário</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ Dashboard de documentação do sistema legacy</li>
                          <li>□ Interface de geração de relatórios PREMIT/PREMCED</li>
                          <li>□ Consulta de prêmios com filtros e visualizações</li>
                          <li>□ Design system com TailwindCSS e cores Caixa</li>
                        </ul>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3 mt-8">Mês 3: Validação e Deploy (4 Semanas)</h3>
                    <div className="space-y-4">
                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">6</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semanas 9-10: Testes e Validação</div>
                              <div className="text-sm text-gray-600">Garantia de Qualidade</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ Testes unitários e de integração (cobertura 90%+)</li>
                          <li>□ Testes de comparação byte-a-byte com COBOL</li>
                          <li>□ Testes E2E com Playwright</li>
                          <li>□ Validação de conformidade SUSEP Circular 360</li>
                        </ul>
                      </div>

                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">7</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semana 11: UAT e Refinamentos</div>
                              <div className="text-sm text-gray-600">Aceitação do Usuário</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ User Acceptance Testing com stakeholders</li>
                          <li>□ Validação paralela COBOL vs .NET (2 ciclos mensais)</li>
                          <li>□ Ajustes de performance e otimização</li>
                          <li>□ Documentação de operações e runbooks</li>
                        </ul>
                      </div>

                      <div className="phase-card phase-pending">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center gap-3">
                            <div className="w-10 h-10 rounded-full bg-gray-400 text-white flex items-center justify-center font-bold">8</div>
                            <div>
                              <div className="font-semibold text-gray-900">Semana 12: Deploy em Produção</div>
                              <div className="text-sm text-gray-600">Go-Live</div>
                            </div>
                          </div>
                          <Badge variant="outline">Planejado</Badge>
                        </div>
                        <ul className="text-sm text-gray-700 space-y-1 pl-13">
                          <li>□ Deploy de backend em Azure App Service</li>
                          <li>□ Deploy de frontend em Vercel/CDN</li>
                          <li>□ Cutover de COBOL para .NET (Big Bang)</li>
                          <li>□ Monitoramento 24/7 pós-deploy (primeira semana)</li>
                        </ul>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="text-xl font-semibold text-gray-900 mb-3">Estratégia de Migração</h3>
                    <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                      <div className="strategy-card">
                        <div className="font-semibold text-gray-900 mb-2">🔄 Big Bang vs Incremental</div>
                        <p className="text-sm text-gray-600">
                          <strong>Escolha: Big Bang</strong> - Migração completa em único cutover devido à criticidade
                          regulatória. Não é possível operar dois sistemas simultaneamente para SUSEP.
                        </p>
                      </div>

                      <div className="strategy-card">
                        <div className="font-semibold text-gray-900 mb-2">🔒 Rollback Plan</div>
                        <p className="text-sm text-gray-600">
                          Manter COBOL operacional por 3 meses pós-migração. Capacidade de rollback em 4 horas se
                          validação SUSEP falhar.
                        </p>
                      </div>

                      <div className="strategy-card">
                        <div className="font-semibold text-gray-900 mb-2">📊 Validação Paralela</div>
                        <p className="text-sm text-gray-600">
                          Executar COBOL e .NET em paralelo por 2 ciclos mensais (pre-prod), comparando saídas byte-a-byte
                          antes do cutover final.
                        </p>
                      </div>

                      <div className="strategy-card">
                        <div className="font-semibold text-gray-900 mb-2">⚡ Performance Target</div>
                        <p className="text-sm text-gray-600">
                          Reduzir tempo de processamento de 2-4h (COBOL) para &lt;30min (.NET) usando processamento
                          assíncrono e cache.
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

        </Tabs>
      </div>
    </div>
  );
};
