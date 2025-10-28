/**
 * Architecture Tab Component - Visual representation of RG1866B COBOL Architecture
 * Based on docs/legacy-system/02-architecture.md
 */

import { Layers, Server, Database, FileText, Box, Code2, Workflow } from 'lucide-react';

export const ArchitectureTab = () => {
  return (
    <div className="space-y-6">
      {/* Header Card */}
      <div className="bg-gradient-to-r from-caixa-blue to-indigo-700 rounded-xl shadow-2xl p-8 text-white">
        <div className="flex items-center gap-4 mb-4">
          <div className="w-16 h-16 bg-caixa-yellow rounded-lg flex items-center justify-center">
            <Layers className="w-10 h-10 text-caixa-blue" />
          </div>
          <div>
            <h2 className="text-4xl font-bold">Arquitetura do Sistema COBOL RG1866B</h2>
            <p className="text-xl text-blue-100 mt-2">
              Padrão clássico de batch processing mainframe com estrutura COBOL modular
            </p>
          </div>
        </div>
      </div>

      {/* Stack Tecnológico - Layered Architecture */}
      <div className="bg-white rounded-xl shadow-xl p-8">
        <div className="flex items-center gap-3 mb-6">
          <Box className="w-8 h-8 text-caixa-blue" />
          <h3 className="text-3xl font-bold text-gray-900">Stack Tecnológico</h3>
        </div>

        <div className="space-y-4">
          {/* Layer 1: Apresentação */}
          <div className="border-2 border-gray-300 rounded-lg p-6 bg-gray-50">
            <div className="flex items-center justify-between">
              <div>
                <h4 className="text-xl font-bold text-gray-700">CAMADA DE APRESENTAÇÃO</h4>
                <p className="text-gray-600 mt-2 italic">
                  (Não existe - Sistema Batch sem interface)
                </p>
              </div>
            </div>
          </div>

          {/* Layer 2: Controle */}
          <div className="border-2 border-caixa-blue rounded-lg p-6 bg-blue-50">
            <div className="flex items-center gap-3 mb-4">
              <Workflow className="w-6 h-6 text-caixa-blue" />
              <h4 className="text-xl font-bold text-caixa-blue">CAMADA DE CONTROLE</h4>
            </div>
            <div className="bg-white rounded-lg p-5 border border-blue-200">
              <h5 className="font-bold text-gray-800 mb-3">JCL (Job Control Language)</h5>
              <ul className="space-y-2 text-gray-700">
                <li className="flex items-start gap-2">
                  <span className="text-caixa-blue font-bold">•</span>
                  <span>Define parâmetros (PARM='202510')</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-caixa-blue font-bold">•</span>
                  <span>Aloca arquivos (PREMIT, PREMCED)</span>
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-caixa-blue font-bold">•</span>
                  <span>Configura ambiente DB2</span>
                </li>
              </ul>
            </div>
          </div>

          {/* Layer 3: Aplicação */}
          <div className="border-2 border-emerald-500 rounded-lg p-6 bg-emerald-50">
            <div className="flex items-center gap-3 mb-4">
              <Code2 className="w-6 h-6 text-emerald-600" />
              <h4 className="text-xl font-bold text-emerald-700">CAMADA DE APLICAÇÃO</h4>
            </div>
            <div className="bg-white rounded-lg p-5 border border-emerald-200">
              <h5 className="font-bold text-gray-800 mb-3">RG1866B.cbl (COBOL ANSI 85)</h5>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div>
                  <p className="font-semibold text-emerald-700 mb-2">Divisões COBOL:</p>
                  <ul className="space-y-1 text-gray-700 text-sm">
                    <li className="flex items-start gap-2">
                      <span className="text-emerald-600">▸</span>
                      <span>IDENTIFICATION DIVISION</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <span className="text-emerald-600">▸</span>
                      <span>ENVIRONMENT DIVISION</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <span className="text-emerald-600">▸</span>
                      <span>DATA DIVISION</span>
                    </li>
                    <li className="flex items-start gap-2 ml-4">
                      <span className="text-emerald-500">└─</span>
                      <span>FILE SECTION (PREMIT, PREMCED)</span>
                    </li>
                    <li className="flex items-start gap-2 ml-4">
                      <span className="text-emerald-500">└─</span>
                      <span>WORKING-STORAGE SECTION (687 vars)</span>
                    </li>
                  </ul>
                </div>
                <div>
                  <p className="font-semibold text-emerald-700 mb-2">Procedure Division:</p>
                  <ul className="space-y-1 text-gray-700 text-sm">
                    <li className="flex items-start gap-2">
                      <span className="text-emerald-600">▸</span>
                      <span><strong>63</strong> seções de processamento</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <span className="text-emerald-600">▸</span>
                      <span><strong>65</strong> parágrafos</span>
                    </li>
                  </ul>
                </div>
              </div>
            </div>
          </div>

          {/* Layer 4: Integração */}
          <div className="border-2 border-purple-500 rounded-lg p-6 bg-purple-50">
            <div className="flex items-center gap-3 mb-4">
              <Server className="w-6 h-6 text-purple-600" />
              <h4 className="text-xl font-bold text-purple-700">CAMADA DE INTEGRAÇÃO</h4>
            </div>
            <div className="bg-white rounded-lg p-5 border border-purple-200">
              <h5 className="font-bold text-gray-800 mb-3">Módulos Externos (Binários Compilados)</h5>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
                <div className="bg-purple-100 rounded p-3 border border-purple-300">
                  <p className="font-mono text-sm font-bold text-purple-800">RE0001S</p>
                  <p className="text-xs text-gray-700 mt-1">Cálculos de resseguro</p>
                </div>
                <div className="bg-purple-100 rounded p-3 border border-purple-300">
                  <p className="font-mono text-sm font-bold text-purple-800">GE0009S</p>
                  <p className="text-xs text-gray-700 mt-1">Formatações especiais</p>
                </div>
                <div className="bg-purple-100 rounded p-3 border border-purple-300">
                  <p className="font-mono text-sm font-bold text-purple-800">GE0010S</p>
                  <p className="text-xs text-gray-700 mt-1">Validações auxiliares</p>
                </div>
              </div>
            </div>
          </div>

          {/* Layer 5: Dados */}
          <div className="border-2 border-amber-500 rounded-lg p-6 bg-amber-50">
            <div className="flex items-center gap-3 mb-4">
              <Database className="w-6 h-6 text-amber-600" />
              <h4 className="text-xl font-bold text-amber-700">CAMADA DE DADOS</h4>
            </div>
            <div className="bg-white rounded-lg p-5 border border-amber-200">
              <h5 className="font-bold text-gray-800 mb-3">IBM DB2 for z/OS</h5>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                <div className="text-center p-4 bg-amber-100 rounded-lg">
                  <p className="text-3xl font-bold text-amber-700">26+</p>
                  <p className="text-sm text-gray-700 mt-1">Tabelas/Views</p>
                </div>
                <div className="text-center p-4 bg-amber-100 rounded-lg">
                  <p className="text-3xl font-bold text-amber-700">4</p>
                  <p className="text-sm text-gray-700 mt-1">Cursores Ativos</p>
                </div>
                <div className="text-center p-4 bg-amber-100 rounded-lg">
                  <p className="text-xs font-mono text-amber-900 mt-2">EXEC SQL ... END-EXEC</p>
                  <p className="text-sm text-gray-700 mt-1">SQL Embarcado</p>
                </div>
              </div>
            </div>
          </div>

          {/* Layer 6: Persistência */}
          <div className="border-2 border-red-500 rounded-lg p-6 bg-red-50">
            <div className="flex items-center gap-3 mb-4">
              <FileText className="w-6 h-6 text-red-600" />
              <h4 className="text-xl font-bold text-red-700">CAMADA DE PERSISTÊNCIA</h4>
            </div>
            <div className="bg-white rounded-lg p-5 border border-red-200">
              <h5 className="font-bold text-gray-800 mb-3">Arquivos Sequenciais (DASD)</h5>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="border-2 border-red-300 rounded-lg p-4 bg-red-50">
                  <div className="flex items-center gap-2 mb-2">
                    <FileText className="w-5 h-5 text-red-600" />
                    <p className="font-mono font-bold text-red-800">PREMIT.TXT</p>
                  </div>
                  <p className="text-sm text-gray-700">Fixed-width format</p>
                  <p className="text-sm text-gray-700"><strong>1200 bytes</strong> por registro</p>
                </div>
                <div className="border-2 border-red-300 rounded-lg p-4 bg-red-50">
                  <div className="flex items-center gap-2 mb-2">
                    <FileText className="w-5 h-5 text-red-600" />
                    <p className="font-mono font-bold text-red-800">PREMCED.TXT</p>
                  </div>
                  <p className="text-sm text-gray-700">Fixed-width format</p>
                  <p className="text-sm text-gray-700"><strong>800 bytes</strong> por registro</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Fluxo de Execução */}
      <div className="bg-white rounded-xl shadow-xl p-8">
        <div className="flex items-center gap-3 mb-6">
          <Workflow className="w-8 h-8 text-caixa-blue" />
          <h3 className="text-3xl font-bold text-gray-900">Fluxo de Execução Completo</h3>
        </div>

        <div className="bg-gradient-to-r from-blue-50 to-indigo-50 border-l-4 border-caixa-blue p-6 rounded-lg">
          <ol className="space-y-3">
            {[
              { section: 'R0000-INICIO', desc: 'Ponto de entrada do programa' },
              { section: 'R0100-INICIALIZACAO', desc: 'Inicializar variáveis, contadores, flags' },
              { section: 'R0200-ABRIR-ARQUIVOS', desc: 'Abrir arquivos de saída (PREMIT.TXT, PREMCED.TXT)' },
              { section: 'R0300-LER-PARAMETROS', desc: 'Ler data de processamento e código da companhia' },
              { section: 'R0400-ABRIR-CURSORES', desc: 'Declarar e abrir cursores DB2' },
              { section: 'R0500-PROCESSAR-LOTE', desc: 'Loop principal processando todos os registros' },
              { section: 'R0600-PROCESSAR-PREMIO', desc: 'Processar cada prêmio individualmente' },
              { section: 'R0700-R1800', desc: 'Cálculos por tipo de movimento (emissão, endosso, cancelamento)' },
              { section: 'R3000-R3900', desc: 'Processamento de cosseguro (se aplicável)' },
              { section: 'R4000-FORMATAR-PREMIT', desc: 'Formatar registro PREMIT (1200 bytes)' },
              { section: 'R5000-ESCREVER-REGISTRO', desc: 'Escrever no arquivo de saída' },
              { section: 'R8000-FECHAR-CURSORES', desc: 'Fechar todos os cursores DB2' },
              { section: 'R8100-FECHAR-ARQUIVOS', desc: 'Fechar arquivos de saída' },
              { section: 'R8200-GERAR-TOTALIZADORES', desc: 'Gerar relatório de totais' },
              { section: 'R9999-FIM', desc: 'Término normal do programa' },
            ].map((step, idx) => (
              <li key={idx} className="flex items-start gap-3 group">
                <div className="flex-shrink-0 w-8 h-8 bg-caixa-blue text-white rounded-full flex items-center justify-center font-bold text-sm mt-0.5 group-hover:bg-caixa-yellow group-hover:text-caixa-blue transition-colors">
                  {idx + 1}
                </div>
                <div className="flex-grow">
                  <span className="font-mono font-bold text-caixa-blue">{step.section}:</span>{' '}
                  <span className="text-gray-700">{step.desc}</span>
                </div>
              </li>
            ))}
          </ol>
        </div>
      </div>

      {/* Info Footer */}
      <div className="bg-gradient-to-r from-gray-100 to-gray-200 rounded-lg p-6 border-l-4 border-gray-400">
        <div className="flex items-start gap-3">
          <div className="text-2xl">📋</div>
          <div>
            <p className="text-sm font-semibold text-gray-700 mb-1">Documento de Referência</p>
            <p className="text-sm text-gray-600">
              <strong>Arquivo:</strong> 02-architecture.md • <strong>Versão:</strong> 1.0
            </p>
          </div>
        </div>
      </div>
    </div>
  );
};
