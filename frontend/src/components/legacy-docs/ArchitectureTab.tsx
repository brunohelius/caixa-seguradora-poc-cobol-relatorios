/**
 * Architecture Tab Component - Visual representation of RG1866B COBOL Architecture
 * Based on docs/legacy-system/02-architecture.md
 * Enhanced with 3D effects, animations and impressive visual graphics
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
  Activity,
  Terminal,
  Boxes,
  Settings
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

      {/* IMPRESSIVE 3D Architecture Layers Diagram */}
      <div className="bg-gradient-to-br from-gray-900 via-gray-800 to-gray-900 rounded-2xl shadow-2xl p-12 relative overflow-hidden">
        {/* Background decoration */}
        <div className="absolute inset-0 opacity-10">
          <div className="absolute top-0 left-0 w-full h-full" style={{
            backgroundImage: 'radial-gradient(circle at 2px 2px, white 1px, transparent 0)',
            backgroundSize: '40px 40px'
          }}></div>
        </div>

        <div className="relative">
          <div className="flex items-center gap-3 mb-12">
            <Boxes className="w-10 h-10 text-caixa-yellow" />
            <h3 className="text-4xl font-bold text-white">Arquitetura em Camadas - Vista 3D</h3>
          </div>

          {/* 3D Layered Stack with perspective */}
          <div className="relative" style={{ perspective: '1500px' }}>

            {/* Data Flow Indicator - Left Side */}
            <div className="absolute left-0 top-0 bottom-0 w-16 flex flex-col items-center justify-center">
              <div className="flex flex-col items-center gap-2">
                <div className="text-xs text-gray-400 rotate-[-90deg] whitespace-nowrap">Fluxo de Dados</div>
                <div className="h-full w-1 bg-gradient-to-b from-transparent via-caixa-yellow to-transparent"></div>
                <ArrowDown className="w-6 h-6 text-caixa-yellow animate-bounce" />
              </div>
            </div>

            {/* Layers Container with 3D effect */}
            <div className="ml-20 space-y-6">

              {/* Layer 0: Ghost Layer - Apresentação */}
              <div
                className="transform transition-all duration-500 hover:scale-105"
                style={{
                  transformStyle: 'preserve-3d',
                  animation: 'float 3s ease-in-out infinite'
                }}
              >
                <div className="relative border-4 border-dashed border-gray-600 rounded-2xl p-8 bg-gray-800/30 backdrop-blur-sm">
                  <div className="absolute -top-4 -left-4 w-12 h-12 bg-gray-700 rounded-full flex items-center justify-center text-white font-bold text-xl shadow-lg">
                    0
                  </div>
                  <div className="flex items-center gap-4">
                    <div className="w-20 h-20 bg-gray-700 rounded-xl flex items-center justify-center shadow-xl opacity-50">
                      <Box className="w-10 h-10 text-gray-500" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-2xl font-bold text-gray-500 mb-2">CAMADA DE APRESENTAÇÃO</h4>
                      <p className="text-gray-600 italic">(Não existe - Sistema Batch sem interface)</p>
                    </div>
                  </div>
                </div>
              </div>

              {/* Animated connector */}
              <div className="flex justify-center">
                <div className="w-2 h-12 bg-gradient-to-b from-gray-600 to-blue-500 rounded-full relative">
                  <div className="absolute top-0 left-1/2 -translate-x-1/2 w-3 h-3 bg-blue-400 rounded-full animate-ping"></div>
                </div>
              </div>

              {/* Layer 1: Controle (JCL) */}
              <div
                className="transform transition-all duration-500 hover:scale-105 hover:rotate-x-12"
                style={{
                  transformStyle: 'preserve-3d',
                  animation: 'float 3s ease-in-out 0.5s infinite'
                }}
              >
                <div className="relative border-4 border-blue-500 rounded-2xl p-8 bg-gradient-to-br from-blue-900/80 to-indigo-900/80 backdrop-blur-sm shadow-2xl">
                  <div className="absolute -top-6 -left-6 w-16 h-16 bg-gradient-to-br from-blue-500 to-blue-700 rounded-2xl flex items-center justify-center text-white font-bold text-3xl shadow-2xl transform rotate-12">
                    1
                  </div>
                  <div className="flex items-center gap-6">
                    <div className="w-24 h-24 bg-gradient-to-br from-blue-500 to-blue-700 rounded-2xl flex items-center justify-center shadow-2xl transform hover:rotate-12 transition-transform">
                      <Terminal className="w-12 h-12 text-white" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-3xl font-bold text-white mb-2">CAMADA DE CONTROLE</h4>
                      <p className="text-blue-200 text-lg mb-4">JCL (Job Control Language)</p>
                      <div className="grid grid-cols-3 gap-3">
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-3 border border-blue-400/30">
                          <Zap className="w-5 h-5 text-blue-300 mb-1" />
                          <p className="text-xs text-blue-200">PARM='202510'</p>
                        </div>
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-3 border border-blue-400/30">
                          <FileText className="w-5 h-5 text-blue-300 mb-1" />
                          <p className="text-xs text-blue-200">PREMIT, PREMCED</p>
                        </div>
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-3 border border-blue-400/30">
                          <Settings className="w-5 h-5 text-blue-300 mb-1" />
                          <p className="text-xs text-blue-200">Config DB2</p>
                        </div>
                      </div>
                    </div>
                  </div>
                  {/* Glow effect */}
                  <div className="absolute inset-0 rounded-2xl bg-blue-500/20 blur-xl -z-10"></div>
                </div>
              </div>

              {/* Animated connector */}
              <div className="flex justify-center">
                <div className="w-2 h-12 bg-gradient-to-b from-blue-500 to-emerald-500 rounded-full relative">
                  <div className="absolute top-0 left-1/2 -translate-x-1/2 w-3 h-3 bg-emerald-400 rounded-full animate-ping"></div>
                </div>
              </div>

              {/* Layer 2: Aplicação (COBOL) */}
              <div
                className="transform transition-all duration-500 hover:scale-105"
                style={{
                  transformStyle: 'preserve-3d',
                  animation: 'float 3s ease-in-out 1s infinite'
                }}
              >
                <div className="relative border-4 border-emerald-500 rounded-2xl p-8 bg-gradient-to-br from-emerald-900/80 to-green-900/80 backdrop-blur-sm shadow-2xl">
                  <div className="absolute -top-6 -left-6 w-16 h-16 bg-gradient-to-br from-emerald-500 to-emerald-700 rounded-2xl flex items-center justify-center text-white font-bold text-3xl shadow-2xl transform rotate-12">
                    2
                  </div>
                  <div className="flex items-center gap-6">
                    <div className="w-24 h-24 bg-gradient-to-br from-emerald-500 to-emerald-700 rounded-2xl flex items-center justify-center shadow-2xl transform hover:rotate-12 transition-transform">
                      <Code2 className="w-12 h-12 text-white" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-3xl font-bold text-white mb-2">CAMADA DE APLICAÇÃO</h4>
                      <p className="text-emerald-200 text-lg mb-4">RG1866B.cbl (COBOL ANSI 85)</p>
                      <div className="grid grid-cols-2 gap-4">
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-emerald-400/30">
                          <div className="text-sm text-emerald-200 space-y-2">
                            <div className="flex items-center gap-2">
                              <div className="w-2 h-2 bg-emerald-400 rounded-full"></div>
                              <span>IDENTIFICATION DIVISION</span>
                            </div>
                            <div className="flex items-center gap-2">
                              <div className="w-2 h-2 bg-emerald-400 rounded-full"></div>
                              <span>ENVIRONMENT DIVISION</span>
                            </div>
                            <div className="flex items-center gap-2">
                              <div className="w-2 h-2 bg-emerald-400 rounded-full"></div>
                              <span>DATA DIVISION</span>
                            </div>
                            <div className="flex items-center gap-2 ml-4">
                              <div className="w-1.5 h-1.5 bg-emerald-300 rounded-full"></div>
                              <span className="text-xs">FILE SECTION</span>
                            </div>
                            <div className="flex items-center gap-2 ml-4">
                              <div className="w-1.5 h-1.5 bg-emerald-300 rounded-full"></div>
                              <span className="text-xs">WORKING-STORAGE (687 vars)</span>
                            </div>
                          </div>
                        </div>
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-emerald-400/30">
                          <div className="text-emerald-200 font-bold mb-3">PROCEDURE DIVISION</div>
                          <div className="grid grid-cols-2 gap-2">
                            <div className="text-center bg-emerald-500/30 rounded-lg p-2">
                              <div className="text-3xl font-bold text-white">63</div>
                              <div className="text-xs">Seções</div>
                            </div>
                            <div className="text-center bg-emerald-500/30 rounded-lg p-2">
                              <div className="text-3xl font-bold text-white">65</div>
                              <div className="text-xs">Parágrafos</div>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                  {/* Glow effect */}
                  <div className="absolute inset-0 rounded-2xl bg-emerald-500/20 blur-xl -z-10"></div>
                </div>
              </div>

              {/* Animated connector */}
              <div className="flex justify-center">
                <div className="w-2 h-12 bg-gradient-to-b from-emerald-500 to-purple-500 rounded-full relative">
                  <div className="absolute top-0 left-1/2 -translate-x-1/2 w-3 h-3 bg-purple-400 rounded-full animate-ping"></div>
                </div>
              </div>

              {/* Layer 3: Integração (Módulos) */}
              <div
                className="transform transition-all duration-500 hover:scale-105"
                style={{
                  transformStyle: 'preserve-3d',
                  animation: 'float 3s ease-in-out 1.5s infinite'
                }}
              >
                <div className="relative border-4 border-purple-500 rounded-2xl p-8 bg-gradient-to-br from-purple-900/80 to-pink-900/80 backdrop-blur-sm shadow-2xl">
                  <div className="absolute -top-6 -left-6 w-16 h-16 bg-gradient-to-br from-purple-500 to-purple-700 rounded-2xl flex items-center justify-center text-white font-bold text-3xl shadow-2xl transform rotate-12">
                    3
                  </div>
                  <div className="flex items-center gap-6">
                    <div className="w-24 h-24 bg-gradient-to-br from-purple-500 to-purple-700 rounded-2xl flex items-center justify-center shadow-2xl transform hover:rotate-12 transition-transform">
                      <Server className="w-12 h-12 text-white" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-3xl font-bold text-white mb-2">CAMADA DE INTEGRAÇÃO</h4>
                      <p className="text-purple-200 text-lg mb-4">Módulos Externos (Binários Compilados)</p>
                      <div className="grid grid-cols-3 gap-3">
                        {[
                          { name: 'RE0001S', desc: 'Resseguro', complexity: 85 },
                          { name: 'GE0009S', desc: 'Formatação', complexity: 60 },
                          { name: 'GE0010S', desc: 'Validação', complexity: 70 }
                        ].map((module, idx) => (
                          <div key={idx} className="bg-white/10 backdrop-blur-sm rounded-lg p-3 border border-purple-400/30">
                            <div className="font-mono text-lg font-bold text-purple-200 mb-1">{module.name}</div>
                            <div className="text-xs text-purple-300 mb-2">{module.desc}</div>
                            <div className="w-full bg-purple-900/50 rounded-full h-2 overflow-hidden">
                              <div
                                className="bg-gradient-to-r from-purple-400 to-pink-400 h-full rounded-full transition-all duration-1000"
                                style={{ width: `${module.complexity}%` }}
                              ></div>
                            </div>
                          </div>
                        ))}
                      </div>
                    </div>
                  </div>
                  {/* Glow effect */}
                  <div className="absolute inset-0 rounded-2xl bg-purple-500/20 blur-xl -z-10"></div>
                </div>
              </div>

              {/* Animated connector */}
              <div className="flex justify-center">
                <div className="w-2 h-12 bg-gradient-to-b from-purple-500 to-amber-500 rounded-full relative">
                  <div className="absolute top-0 left-1/2 -translate-x-1/2 w-3 h-3 bg-amber-400 rounded-full animate-ping"></div>
                </div>
              </div>

              {/* Layer 4: Dados (DB2) */}
              <div
                className="transform transition-all duration-500 hover:scale-105"
                style={{
                  transformStyle: 'preserve-3d',
                  animation: 'float 3s ease-in-out 2s infinite'
                }}
              >
                <div className="relative border-4 border-amber-500 rounded-2xl p-8 bg-gradient-to-br from-amber-900/80 to-yellow-900/80 backdrop-blur-sm shadow-2xl">
                  <div className="absolute -top-6 -left-6 w-16 h-16 bg-gradient-to-br from-amber-500 to-amber-700 rounded-2xl flex items-center justify-center text-white font-bold text-3xl shadow-2xl transform rotate-12">
                    4
                  </div>
                  <div className="flex items-center gap-6">
                    <div className="w-24 h-24 bg-gradient-to-br from-amber-500 to-amber-700 rounded-2xl flex items-center justify-center shadow-2xl transform hover:rotate-12 transition-transform">
                      <Database className="w-12 h-12 text-white" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-3xl font-bold text-white mb-2">CAMADA DE DADOS</h4>
                      <p className="text-amber-200 text-lg mb-4">IBM DB2 for z/OS</p>
                      <div className="grid grid-cols-3 gap-4">
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 text-center border border-amber-400/30">
                          <Database className="w-10 h-10 text-amber-300 mx-auto mb-2" />
                          <div className="text-4xl font-bold text-white">26+</div>
                          <div className="text-sm text-amber-200">Tabelas</div>
                        </div>
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 text-center border border-amber-400/30">
                          <Cpu className="w-10 h-10 text-amber-300 mx-auto mb-2" />
                          <div className="text-4xl font-bold text-white">4</div>
                          <div className="text-sm text-amber-200">Cursores</div>
                        </div>
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 text-center border border-amber-400/30">
                          <Code2 className="w-10 h-10 text-amber-300 mx-auto mb-2" />
                          <div className="text-lg font-mono font-bold text-white">EXEC SQL</div>
                          <div className="text-xs text-amber-200">Embarcado</div>
                        </div>
                      </div>
                    </div>
                  </div>
                  {/* Glow effect */}
                  <div className="absolute inset-0 rounded-2xl bg-amber-500/20 blur-xl -z-10"></div>
                </div>
              </div>

              {/* Animated connector */}
              <div className="flex justify-center">
                <div className="w-2 h-12 bg-gradient-to-b from-amber-500 to-red-500 rounded-full relative">
                  <div className="absolute top-0 left-1/2 -translate-x-1/2 w-3 h-3 bg-red-400 rounded-full animate-ping"></div>
                </div>
              </div>

              {/* Layer 5: Persistência (Arquivos) */}
              <div
                className="transform transition-all duration-500 hover:scale-105"
                style={{
                  transformStyle: 'preserve-3d',
                  animation: 'float 3s ease-in-out 2.5s infinite'
                }}
              >
                <div className="relative border-4 border-red-500 rounded-2xl p-8 bg-gradient-to-br from-red-900/80 to-orange-900/80 backdrop-blur-sm shadow-2xl">
                  <div className="absolute -top-6 -left-6 w-16 h-16 bg-gradient-to-br from-red-500 to-red-700 rounded-2xl flex items-center justify-center text-white font-bold text-3xl shadow-2xl transform rotate-12">
                    5
                  </div>
                  <div className="flex items-center gap-6">
                    <div className="w-24 h-24 bg-gradient-to-br from-red-500 to-red-700 rounded-2xl flex items-center justify-center shadow-2xl transform hover:rotate-12 transition-transform">
                      <HardDrive className="w-12 h-12 text-white" />
                    </div>
                    <div className="flex-1">
                      <h4 className="text-3xl font-bold text-white mb-2">CAMADA DE PERSISTÊNCIA</h4>
                      <p className="text-red-200 text-lg mb-4">Arquivos Sequenciais (DASD)</p>
                      <div className="grid grid-cols-2 gap-4">
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-red-400/30">
                          <div className="flex items-center gap-2 mb-3">
                            <FileText className="w-8 h-8 text-red-300" />
                            <div className="font-mono text-xl font-bold text-white">PREMIT.TXT</div>
                          </div>
                          <div className="space-y-2 text-sm text-red-200">
                            <div className="flex justify-between">
                              <span>Tamanho:</span>
                              <span className="font-bold text-white">1200 bytes</span>
                            </div>
                            <div className="flex justify-between">
                              <span>Volume:</span>
                              <span className="font-bold text-white">~50 MB/mês</span>
                            </div>
                            <div className="w-full bg-red-900/50 rounded-full h-3 overflow-hidden mt-2">
                              <div className="bg-gradient-to-r from-red-400 to-orange-400 h-full rounded-full" style={{ width: '75%' }}></div>
                            </div>
                          </div>
                        </div>
                        <div className="bg-white/10 backdrop-blur-sm rounded-lg p-4 border border-red-400/30">
                          <div className="flex items-center gap-2 mb-3">
                            <FileText className="w-8 h-8 text-red-300" />
                            <div className="font-mono text-xl font-bold text-white">PREMCED.TXT</div>
                          </div>
                          <div className="space-y-2 text-sm text-red-200">
                            <div className="flex justify-between">
                              <span>Tamanho:</span>
                              <span className="font-bold text-white">800 bytes</span>
                            </div>
                            <div className="flex justify-between">
                              <span>Volume:</span>
                              <span className="font-bold text-white">~20 MB/mês</span>
                            </div>
                            <div className="w-full bg-red-900/50 rounded-full h-3 overflow-hidden mt-2">
                              <div className="bg-gradient-to-r from-red-400 to-orange-400 h-full rounded-full" style={{ width: '45%' }}></div>
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                  {/* Glow effect */}
                  <div className="absolute inset-0 rounded-2xl bg-red-500/20 blur-xl -z-10"></div>
                </div>
              </div>

            </div>
          </div>
        </div>

        {/* Add custom CSS animation */}
        <style>{`
          @keyframes float {
            0%, 100% { transform: translateY(0px) rotateX(0deg); }
            50% { transform: translateY(-10px) rotateX(2deg); }
          }
        `}</style>
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
