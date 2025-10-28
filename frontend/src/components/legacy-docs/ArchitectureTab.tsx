/**
 * Architecture Tab Component - Visual representation of RG1866B COBOL Architecture
 * Based on docs/legacy-system/02-architecture.md
 * Enhanced with interactive diagrams and visual flow representations
 */

import {
  Layers,
  Server,
  Database,
  FileText,
  Box,
  Code2,
  Workflow,
  ArrowDown,
  ArrowRight,
  Cpu,
  HardDrive,
  Zap,
  GitBranch,
  Activity
} from 'lucide-react';

export const ArchitectureTab = () => {
  return (
    <div className="space-y-8">
      {/* Header Card */}
      <div className="bg-gradient-to-r from-caixa-blue via-blue-600 to-indigo-700 rounded-xl shadow-2xl p-8 text-white relative overflow-hidden">
        <div className="absolute top-0 right-0 w-64 h-64 bg-white opacity-5 rounded-full -mr-32 -mt-32"></div>
        <div className="absolute bottom-0 left-0 w-48 h-48 bg-white opacity-5 rounded-full -ml-24 -mb-24"></div>
        <div className="relative flex items-center gap-4 mb-4">
          <div className="w-20 h-20 bg-caixa-yellow rounded-xl flex items-center justify-center shadow-xl transform hover:scale-110 transition-transform">
            <Layers className="w-12 h-12 text-caixa-blue" />
          </div>
          <div>
            <h2 className="text-4xl font-bold mb-2">Arquitetura do Sistema COBOL RG1866B</h2>
            <p className="text-xl text-blue-100">
              Padrão clássico de batch processing mainframe com estrutura COBOL modular
            </p>
          </div>
        </div>

        {/* Metrics Row */}
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mt-8">
          <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-white/20">
            <div className="text-3xl font-bold text-caixa-yellow">6</div>
            <div className="text-sm text-blue-100">Camadas</div>
          </div>
          <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-white/20">
            <div className="text-3xl font-bold text-caixa-yellow">5,046</div>
            <div className="text-sm text-blue-100">Linhas COBOL</div>
          </div>
          <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-white/20">
            <div className="text-3xl font-bold text-caixa-yellow">26+</div>
            <div className="text-sm text-blue-100">Tabelas DB2</div>
          </div>
          <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-white/20">
            <div className="text-3xl font-bold text-caixa-yellow">63</div>
            <div className="text-sm text-blue-100">Seções</div>
          </div>
        </div>
      </div>

      {/* Architecture Layers Visual Diagram */}
      <div className="bg-white rounded-xl shadow-xl p-8">
        <div className="flex items-center gap-3 mb-8">
          <GitBranch className="w-8 h-8 text-caixa-blue" />
          <h3 className="text-3xl font-bold text-gray-900">Diagrama de Camadas</h3>
        </div>

        <div className="relative">
          {/* Layer 1: Apresentação (Ghost layer) */}
          <div className="mb-4 opacity-50">
            <div className="border-2 border-dashed border-gray-300 rounded-xl p-6 bg-gray-50 relative group hover:opacity-100 transition-opacity">
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-3">
                  <div className="w-12 h-12 rounded-lg bg-gray-200 flex items-center justify-center">
                    <Box className="w-6 h-6 text-gray-400" />
                  </div>
                  <div>
                    <h4 className="text-xl font-bold text-gray-600">CAMADA 1: APRESENTAÇÃO</h4>
                    <p className="text-gray-500 mt-1 italic">Não existe - Sistema Batch sem interface</p>
                  </div>
                </div>
              </div>
            </div>
          </div>

          {/* Arrow Down */}
          <div className="flex justify-center mb-4">
            <div className="w-1 h-8 bg-gradient-to-b from-gray-300 to-caixa-blue"></div>
          </div>

          {/* Layer 2: Controle (JCL) */}
          <div className="mb-4">
            <div className="border-3 border-caixa-blue rounded-xl p-6 bg-gradient-to-br from-blue-50 to-indigo-50 shadow-lg hover:shadow-2xl transition-shadow group">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 rounded-lg bg-caixa-blue flex items-center justify-center shadow-md group-hover:scale-110 transition-transform">
                  <Workflow className="w-6 h-6 text-white" />
                </div>
                <div>
                  <h4 className="text-xl font-bold text-caixa-blue">CAMADA 2: CONTROLE (JCL)</h4>
                  <p className="text-sm text-gray-600">Job Control Language - Orquestração</p>
                </div>
              </div>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-3 mt-4">
                <div className="bg-white rounded-lg p-3 border-l-4 border-caixa-blue shadow-sm">
                  <div className="flex items-center gap-2">
                    <Zap className="w-4 h-4 text-caixa-blue" />
                    <span className="text-sm font-semibold text-gray-800">Parâmetros</span>
                  </div>
                  <p className="text-xs text-gray-600 mt-1">PARM='202510'</p>
                </div>
                <div className="bg-white rounded-lg p-3 border-l-4 border-caixa-blue shadow-sm">
                  <div className="flex items-center gap-2">
                    <FileText className="w-4 h-4 text-caixa-blue" />
                    <span className="text-sm font-semibold text-gray-800">Arquivos</span>
                  </div>
                  <p className="text-xs text-gray-600 mt-1">PREMIT, PREMCED</p>
                </div>
                <div className="bg-white rounded-lg p-3 border-l-4 border-caixa-blue shadow-sm">
                  <div className="flex items-center gap-2">
                    <Database className="w-4 h-4 text-caixa-blue" />
                    <span className="text-sm font-semibold text-gray-800">Ambiente</span>
                  </div>
                  <p className="text-xs text-gray-600 mt-1">Config DB2</p>
                </div>
              </div>
            </div>
          </div>

          {/* Arrow Down */}
          <div className="flex justify-center mb-4">
            <ArrowDown className="w-6 h-6 text-caixa-blue animate-bounce" />
          </div>

          {/* Layer 3: Aplicação (COBOL) */}
          <div className="mb-4">
            <div className="border-3 border-emerald-500 rounded-xl p-6 bg-gradient-to-br from-emerald-50 to-green-50 shadow-lg hover:shadow-2xl transition-shadow group">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 rounded-lg bg-emerald-600 flex items-center justify-center shadow-md group-hover:scale-110 transition-transform">
                  <Code2 className="w-6 h-6 text-white" />
                </div>
                <div>
                  <h4 className="text-xl font-bold text-emerald-700">CAMADA 3: APLICAÇÃO (RG1866B.cbl)</h4>
                  <p className="text-sm text-gray-600">COBOL ANSI 85 - Lógica de Negócio</p>
                </div>
              </div>

              {/* COBOL Structure Visualization */}
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mt-4">
                <div className="bg-white rounded-lg p-4 border-l-4 border-emerald-500 shadow-sm">
                  <h5 className="font-bold text-emerald-700 mb-3 flex items-center gap-2">
                    <Box className="w-4 h-4" />
                    Estrutura COBOL
                  </h5>
                  <div className="space-y-2 text-sm">
                    <div className="flex items-center gap-2">
                      <div className="w-2 h-2 bg-emerald-500 rounded-full"></div>
                      <span className="text-gray-700">IDENTIFICATION DIVISION</span>
                    </div>
                    <div className="flex items-center gap-2">
                      <div className="w-2 h-2 bg-emerald-500 rounded-full"></div>
                      <span className="text-gray-700">ENVIRONMENT DIVISION</span>
                    </div>
                    <div className="flex items-center gap-2">
                      <div className="w-2 h-2 bg-emerald-500 rounded-full"></div>
                      <span className="text-gray-700">DATA DIVISION</span>
                    </div>
                    <div className="flex items-center gap-2 ml-4">
                      <div className="w-1.5 h-1.5 bg-emerald-400 rounded-full"></div>
                      <span className="text-gray-600 text-xs">FILE SECTION (PREMIT, PREMCED)</span>
                    </div>
                    <div className="flex items-center gap-2 ml-4">
                      <div className="w-1.5 h-1.5 bg-emerald-400 rounded-full"></div>
                      <span className="text-gray-600 text-xs">WORKING-STORAGE (687 vars)</span>
                    </div>
                  </div>
                </div>

                <div className="bg-white rounded-lg p-4 border-l-4 border-emerald-500 shadow-sm">
                  <h5 className="font-bold text-emerald-700 mb-3 flex items-center gap-2">
                    <Activity className="w-4 h-4" />
                    PROCEDURE DIVISION
                  </h5>
                  <div className="grid grid-cols-2 gap-3">
                    <div className="text-center p-3 bg-emerald-100 rounded-lg">
                      <div className="text-2xl font-bold text-emerald-700">63</div>
                      <div className="text-xs text-gray-600 mt-1">Seções</div>
                    </div>
                    <div className="text-center p-3 bg-emerald-100 rounded-lg">
                      <div className="text-2xl font-bold text-emerald-700">65</div>
                      <div className="text-xs text-gray-600 mt-1">Parágrafos</div>
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>

          {/* Arrow Down */}
          <div className="flex justify-center mb-4">
            <ArrowDown className="w-6 h-6 text-caixa-blue animate-bounce" />
          </div>

          {/* Layer 4: Integração (Módulos Externos) */}
          <div className="mb-4">
            <div className="border-3 border-purple-500 rounded-xl p-6 bg-gradient-to-br from-purple-50 to-pink-50 shadow-lg hover:shadow-2xl transition-shadow group">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 rounded-lg bg-purple-600 flex items-center justify-center shadow-md group-hover:scale-110 transition-transform">
                  <Server className="w-6 h-6 text-white" />
                </div>
                <div>
                  <h4 className="text-xl font-bold text-purple-700">CAMADA 4: INTEGRAÇÃO</h4>
                  <p className="text-sm text-gray-600">Módulos Externos (Binários Compilados)</p>
                </div>
              </div>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-3 mt-4">
                <div className="bg-white rounded-lg p-4 border-2 border-purple-300 hover:border-purple-500 transition-colors shadow-sm">
                  <div className="font-mono text-lg font-bold text-purple-800 mb-1">RE0001S</div>
                  <div className="text-sm text-gray-700 mb-2">Cálculos de resseguro</div>
                  <div className="w-full bg-purple-100 rounded-full h-2">
                    <div className="bg-purple-600 h-2 rounded-full" style={{ width: '85%' }}></div>
                  </div>
                  <div className="text-xs text-gray-500 mt-1">Complexidade: Alta</div>
                </div>
                <div className="bg-white rounded-lg p-4 border-2 border-purple-300 hover:border-purple-500 transition-colors shadow-sm">
                  <div className="font-mono text-lg font-bold text-purple-800 mb-1">GE0009S</div>
                  <div className="text-sm text-gray-700 mb-2">Formatações especiais</div>
                  <div className="w-full bg-purple-100 rounded-full h-2">
                    <div className="bg-purple-600 h-2 rounded-full" style={{ width: '60%' }}></div>
                  </div>
                  <div className="text-xs text-gray-500 mt-1">Complexidade: Média</div>
                </div>
                <div className="bg-white rounded-lg p-4 border-2 border-purple-300 hover:border-purple-500 transition-colors shadow-sm">
                  <div className="font-mono text-lg font-bold text-purple-800 mb-1">GE0010S</div>
                  <div className="text-sm text-gray-700 mb-2">Validações auxiliares</div>
                  <div className="w-full bg-purple-100 rounded-full h-2">
                    <div className="bg-purple-600 h-2 rounded-full" style={{ width: '70%' }}></div>
                  </div>
                  <div className="text-xs text-gray-500 mt-1">Complexidade: Média</div>
                </div>
              </div>
            </div>
          </div>

          {/* Arrow Down */}
          <div className="flex justify-center mb-4">
            <ArrowDown className="w-6 h-6 text-caixa-blue animate-bounce" />
          </div>

          {/* Layer 5: Dados (DB2) */}
          <div className="mb-4">
            <div className="border-3 border-amber-500 rounded-xl p-6 bg-gradient-to-br from-amber-50 to-yellow-50 shadow-lg hover:shadow-2xl transition-shadow group">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 rounded-lg bg-amber-600 flex items-center justify-center shadow-md group-hover:scale-110 transition-transform">
                  <Database className="w-6 h-6 text-white" />
                </div>
                <div>
                  <h4 className="text-xl font-bold text-amber-700">CAMADA 5: DADOS (IBM DB2 for z/OS)</h4>
                  <p className="text-sm text-gray-600">Banco de Dados Relacional</p>
                </div>
              </div>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mt-4">
                <div className="bg-white rounded-lg p-4 text-center shadow-sm border-2 border-amber-200">
                  <Database className="w-10 h-10 text-amber-600 mx-auto mb-2" />
                  <div className="text-3xl font-bold text-amber-700">26+</div>
                  <div className="text-sm text-gray-600 mt-1">Tabelas/Views</div>
                  <div className="mt-3 text-xs text-gray-500">
                    V0PREMIOS, V0APOLICE, V0ENDOSSO...
                  </div>
                </div>
                <div className="bg-white rounded-lg p-4 text-center shadow-sm border-2 border-amber-200">
                  <Cpu className="w-10 h-10 text-amber-600 mx-auto mb-2" />
                  <div className="text-3xl font-bold text-amber-700">4</div>
                  <div className="text-sm text-gray-600 mt-1">Cursores Ativos</div>
                  <div className="mt-3 text-xs text-gray-500">
                    Processamento paralelo
                  </div>
                </div>
                <div className="bg-white rounded-lg p-4 text-center shadow-sm border-2 border-amber-200">
                  <Code2 className="w-10 h-10 text-amber-600 mx-auto mb-2" />
                  <div className="text-lg font-mono font-bold text-amber-700">EXEC SQL</div>
                  <div className="text-sm text-gray-600 mt-1">SQL Embarcado</div>
                  <div className="mt-3 text-xs text-gray-500">
                    END-EXEC
                  </div>
                </div>
              </div>
            </div>
          </div>

          {/* Arrow Down */}
          <div className="flex justify-center mb-4">
            <ArrowDown className="w-6 h-6 text-caixa-blue animate-bounce" />
          </div>

          {/* Layer 6: Persistência (Arquivos) */}
          <div>
            <div className="border-3 border-red-500 rounded-xl p-6 bg-gradient-to-br from-red-50 to-orange-50 shadow-lg hover:shadow-2xl transition-shadow group">
              <div className="flex items-center gap-3 mb-4">
                <div className="w-12 h-12 rounded-lg bg-red-600 flex items-center justify-center shadow-md group-hover:scale-110 transition-transform">
                  <HardDrive className="w-6 h-6 text-white" />
                </div>
                <div>
                  <h4 className="text-xl font-bold text-red-700">CAMADA 6: PERSISTÊNCIA (DASD)</h4>
                  <p className="text-sm text-gray-600">Arquivos Sequenciais Fixed-Width</p>
                </div>
              </div>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4 mt-4">
                <div className="bg-white rounded-lg p-5 border-3 border-red-300 shadow-md hover:shadow-lg transition-shadow">
                  <div className="flex items-center gap-3 mb-3">
                    <FileText className="w-8 h-8 text-red-600" />
                    <div className="font-mono text-xl font-bold text-red-800">PREMIT.TXT</div>
                  </div>
                  <div className="space-y-2">
                    <div className="flex justify-between text-sm">
                      <span className="text-gray-600">Formato:</span>
                      <span className="font-semibold text-gray-800">Fixed-Width</span>
                    </div>
                    <div className="flex justify-between text-sm">
                      <span className="text-gray-600">Tamanho Registro:</span>
                      <span className="font-semibold text-red-700">1200 bytes</span>
                    </div>
                    <div className="flex justify-between text-sm">
                      <span className="text-gray-600">Volume Médio:</span>
                      <span className="font-semibold text-gray-800">10-12k registros</span>
                    </div>
                    <div className="w-full bg-red-100 rounded-full h-3 mt-3">
                      <div className="bg-red-600 h-3 rounded-full" style={{ width: '75%' }}></div>
                    </div>
                    <div className="text-xs text-gray-500 text-right">~50 MB/mês</div>
                  </div>
                </div>
                <div className="bg-white rounded-lg p-5 border-3 border-red-300 shadow-md hover:shadow-lg transition-shadow">
                  <div className="flex items-center gap-3 mb-3">
                    <FileText className="w-8 h-8 text-red-600" />
                    <div className="font-mono text-xl font-bold text-red-800">PREMCED.TXT</div>
                  </div>
                  <div className="space-y-2">
                    <div className="flex justify-between text-sm">
                      <span className="text-gray-600">Formato:</span>
                      <span className="font-semibold text-gray-800">Fixed-Width</span>
                    </div>
                    <div className="flex justify-between text-sm">
                      <span className="text-gray-600">Tamanho Registro:</span>
                      <span className="font-semibold text-red-700">800 bytes</span>
                    </div>
                    <div className="flex justify-between text-sm">
                      <span className="text-gray-600">Volume Médio:</span>
                      <span className="font-semibold text-gray-800">5-7k registros</span>
                    </div>
                    <div className="w-full bg-red-100 rounded-full h-3 mt-3">
                      <div className="bg-red-600 h-3 rounded-full" style={{ width: '45%' }}></div>
                    </div>
                    <div className="text-xs text-gray-500 text-right">~20 MB/mês</div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Data Flow Pipeline */}
      <div className="bg-white rounded-xl shadow-xl p-8">
        <div className="flex items-center gap-3 mb-6">
          <Workflow className="w-8 h-8 text-caixa-blue" />
          <h3 className="text-3xl font-bold text-gray-900">Pipeline de Processamento</h3>
        </div>

        <div className="bg-gradient-to-r from-blue-50 via-purple-50 to-red-50 rounded-xl p-6 border-2 border-blue-200">
          <div className="flex flex-col md:flex-row items-center justify-between gap-4">
            {/* Step 1 */}
            <div className="flex-1 text-center">
              <div className="w-20 h-20 mx-auto bg-gradient-to-br from-amber-400 to-amber-600 rounded-xl flex items-center justify-center shadow-lg mb-3 transform hover:scale-110 transition-transform">
                <Database className="w-10 h-10 text-white" />
              </div>
              <div className="font-bold text-gray-800">DB2 Query</div>
              <div className="text-sm text-gray-600 mt-1">500K+ SELECTs</div>
            </div>

            <ArrowRight className="w-8 h-8 text-caixa-blue hidden md:block" />

            {/* Step 2 */}
            <div className="flex-1 text-center">
              <div className="w-20 h-20 mx-auto bg-gradient-to-br from-emerald-400 to-emerald-600 rounded-xl flex items-center justify-center shadow-lg mb-3 transform hover:scale-110 transition-transform">
                <Code2 className="w-10 h-10 text-white" />
              </div>
              <div className="font-bold text-gray-800">COBOL Process</div>
              <div className="text-sm text-gray-600 mt-1">63 Seções</div>
            </div>

            <ArrowRight className="w-8 h-8 text-caixa-blue hidden md:block" />

            {/* Step 3 */}
            <div className="flex-1 text-center">
              <div className="w-20 h-20 mx-auto bg-gradient-to-br from-purple-400 to-purple-600 rounded-xl flex items-center justify-center shadow-lg mb-3 transform hover:scale-110 transition-transform">
                <Server className="w-10 h-10 text-white" />
              </div>
              <div className="font-bold text-gray-800">External Modules</div>
              <div className="text-sm text-gray-600 mt-1">3 CALLs</div>
            </div>

            <ArrowRight className="w-8 h-8 text-caixa-blue hidden md:block" />

            {/* Step 4 */}
            <div className="flex-1 text-center">
              <div className="w-20 h-20 mx-auto bg-gradient-to-br from-red-400 to-red-600 rounded-xl flex items-center justify-center shadow-lg mb-3 transform hover:scale-110 transition-transform">
                <FileText className="w-10 h-10 text-white" />
              </div>
              <div className="font-bold text-gray-800">File Output</div>
              <div className="text-sm text-gray-600 mt-1">~70 MB</div>
            </div>
          </div>

          {/* Performance Metrics */}
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mt-8">
            <div className="bg-white rounded-lg p-4 text-center shadow-md">
              <Zap className="w-6 h-6 text-yellow-500 mx-auto mb-2" />
              <div className="text-2xl font-bold text-gray-800">45-60min</div>
              <div className="text-xs text-gray-600">Duração</div>
            </div>
            <div className="bg-white rounded-lg p-4 text-center shadow-md">
              <Cpu className="w-6 h-6 text-blue-500 mx-auto mb-2" />
              <div className="text-2xl font-bold text-gray-800">15-20min</div>
              <div className="text-xs text-gray-600">CPU Time</div>
            </div>
            <div className="bg-white rounded-lg p-4 text-center shadow-md">
              <Database className="w-6 h-6 text-green-500 mx-auto mb-2" />
              <div className="text-2xl font-bold text-gray-800">500K</div>
              <div className="text-xs text-gray-600">DB Reads</div>
            </div>
            <div className="bg-white rounded-lg p-4 text-center shadow-md">
              <Activity className="w-6 h-6 text-purple-500 mx-auto mb-2" />
              <div className="text-2xl font-bold text-gray-800">99.8%</div>
              <div className="text-xs text-gray-600">Disponibilidade</div>
            </div>
          </div>
        </div>
      </div>

      {/* Execution Flow */}
      <div className="bg-white rounded-xl shadow-xl p-8">
        <div className="flex items-center gap-3 mb-6">
          <Activity className="w-8 h-8 text-caixa-blue" />
          <h3 className="text-3xl font-bold text-gray-900">Fluxo de Execução (63 Seções)</h3>
        </div>

        <div className="bg-gradient-to-r from-blue-50 to-indigo-50 border-l-4 border-caixa-blue rounded-lg p-6">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-3">
            {[
              { section: 'R0000', name: 'INICIO', color: 'blue' },
              { section: 'R0100', name: 'INICIALIZACAO', color: 'blue' },
              { section: 'R0200', name: 'ABRIR-ARQUIVOS', color: 'green' },
              { section: 'R0300', name: 'LER-PARAMETROS', color: 'green' },
              { section: 'R0400', name: 'ABRIR-CURSORES', color: 'amber' },
              { section: 'R0500', name: 'PROCESSAR-LOTE', color: 'purple' },
              { section: 'R0600', name: 'PROCESSAR-PREMIO', color: 'purple' },
              { section: 'R0700-R1800', name: 'CALCULOS', color: 'red' },
              { section: 'R3000-R3900', name: 'COSSEGURO', color: 'orange' },
              { section: 'R4000', name: 'FORMATAR-PREMIT', color: 'pink' },
              { section: 'R5000', name: 'ESCREVER-REGISTRO', color: 'red' },
              { section: 'R8000', name: 'FECHAR-CURSORES', color: 'amber' },
              { section: 'R8100', name: 'FECHAR-ARQUIVOS', color: 'green' },
              { section: 'R8200', name: 'TOTALIZADORES', color: 'blue' },
              { section: 'R9999', name: 'FIM', color: 'gray' },
            ].map((step, idx) => (
              <div
                key={idx}
                className={`bg-white rounded-lg p-3 border-l-4 border-${step.color}-500 shadow-sm hover:shadow-md transition-shadow group cursor-pointer`}
              >
                <div className="flex items-center gap-2">
                  <div className={`w-8 h-8 bg-${step.color}-500 text-white rounded-full flex items-center justify-center text-xs font-bold group-hover:scale-110 transition-transform`}>
                    {idx + 1}
                  </div>
                  <div className="flex-1">
                    <div className="font-mono text-xs font-bold text-gray-800">{step.section}</div>
                    <div className="text-xs text-gray-600">{step.name}</div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Info Footer */}
      <div className="bg-gradient-to-r from-gray-100 to-gray-200 rounded-lg p-6 border-l-4 border-gray-400">
        <div className="flex items-start gap-3">
          <div className="text-2xl">📋</div>
          <div>
            <p className="text-sm font-semibold text-gray-700 mb-1">Documento de Referência</p>
            <p className="text-sm text-gray-600">
              <strong>Arquivo:</strong> 02-architecture.md • <strong>Versão:</strong> 1.0 • <strong>Última Atualização:</strong> Outubro 2025
            </p>
          </div>
        </div>
      </div>
    </div>
  );
};
