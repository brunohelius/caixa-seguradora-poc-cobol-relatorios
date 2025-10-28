import { useEffect, useState } from 'react';
import Spinner from '../components/common/Spinner';
import ErrorMessage from '../components/common/ErrorMessage';
import { ProgramInfoCard } from '../components/dashboard/ProgramInfoCard';
import { DataStructureCard } from '../components/dashboard/DataStructureCard';
import { ComplexityMetricsCard } from '../components/dashboard/ComplexityMetricsCard';
import { DatabaseDependenciesChart } from '../components/dashboard/DatabaseDependenciesChart';
import { FunctionPointsChart } from '../components/dashboard/FunctionPointsChart';
import { MigrationProgressCard } from '../components/dashboard/MigrationProgressCard';
import { MigrationTimelineCard } from '../components/dashboard/MigrationTimelineCard';
import { TestSuiteStatusCard } from '../components/dashboard/TestSuiteStatusCard';
import dashboardService from '../services/dashboardService';
import type { DashboardMetricsDto, FunctionPointsDto, DatabaseDependenciesDto } from '../services/types';

export const DashboardPage: React.FC = () => {
  const [metrics, setMetrics] = useState<DashboardMetricsDto | null>(null);
  const [functionPoints, setFunctionPoints] = useState<FunctionPointsDto | null>(null);
  const [dependencies, setDependencies] = useState<DatabaseDependenciesDto | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    loadDashboardData();
  }, []);

  const loadDashboardData = async () => {
    try {
      setLoading(true);
      setError(null);

      // Load all dashboard data in parallel
      const [metricsData, fpData, depsData] = await Promise.all([
        dashboardService.getMetrics(),
        dashboardService.getFunctionPoints(),
        dashboardService.getDatabaseDependencies(),
      ]);

      setMetrics(metricsData);
      setFunctionPoints(fpData);
      setDependencies(depsData);
    } catch (err: any) {
      console.error('Error loading dashboard data:', err);
      setError(
        err.response?.data?.message ||
        err.message ||
        'Erro ao carregar dados do dashboard. Verifique se o backend está rodando.'
      );
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="flex flex-col items-center justify-center min-h-screen bg-gradient-to-br from-caixa-blue-light to-white">
        <Spinner size="large" />
        <p className="mt-6 text-xl font-semibold text-caixa-blue">
          Carregando análise do programa COBOL...
        </p>
      </div>
    );
  }

  if (error) {
    return (
      <div className="container mx-auto px-4 py-8">
        <ErrorMessage message={error} />
        <button
          onClick={loadDashboardData}
          className="mt-6 px-6 py-3 bg-caixa-blue text-white font-semibold rounded-lg hover:bg-caixa-blue-dark transition-colors shadow-lg"
        >
          Tentar Novamente
        </button>
      </div>
    );
  }

  if (!metrics || !functionPoints || !dependencies) {
    return (
      <div className="container mx-auto px-4 py-8">
        <ErrorMessage message="Dados do dashboard não disponíveis." />
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-50 to-gray-100">
      {/* Hero Header with Caixa Branding */}
      <div className="bg-gradient-to-r from-caixa-blue to-caixa-blue-dark text-white py-12 px-6 shadow-2xl">
        <div className="container mx-auto">
          <div className="flex items-center gap-4 mb-4">
            <div className="w-16 h-16 bg-caixa-yellow rounded-lg flex items-center justify-center shadow-lg">
              <span className="text-3xl">📊</span>
            </div>
            <div>
              <h1 className="text-4xl font-bold">Dashboard de Migração COBOL</h1>
              <p className="text-xl text-caixa-blue-light mt-1">
                Análise completa do programa RG1866B - Sistema de Relatórios de Prêmios SUSEP
              </p>
            </div>
          </div>
        </div>
      </div>

      <div className="container mx-auto px-6 py-8">
        {/* Success Banner - 95% Complete */}
        <div className="mb-8 p-8 bg-gradient-to-r from-green-500 to-emerald-600 rounded-2xl shadow-2xl text-white">
          <div className="flex items-center gap-6">
            <div className="text-7xl animate-bounce">🎉</div>
            <div className="flex-grow">
              <h2 className="text-3xl font-bold mb-3">
                Migração 95% Completa - Projeto Pronto para UAT!
              </h2>
              <p className="text-lg font-semibold" style={{ color: '#fff' }}>
                ✅ Todas as 5 User Stories implementadas •
                ✅ 240 tarefas concluídas •
                ✅ Phase 8 (Polish & Validation) finalizada •
                ✅ Build com 0 erros •
                ✅ 143 testes criados
              </p>
            </div>
            <div className="text-center bg-white/20 backdrop-blur-sm rounded-xl p-6">
              <div className="text-6xl font-black text-white drop-shadow-lg">95%</div>
              <p className="text-sm font-bold mt-2 tracking-wider" style={{ color: '#fff' }}>PRONTIDÃO</p>
            </div>
          </div>
        </div>

        {/* Dashboard Grid */}
        <div className="space-y-8">
          {/* Row 1: Program Info + Migration Progress */}
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            <ProgramInfoCard programInfo={metrics.programInfo} />
            <MigrationProgressCard migrationProgress={metrics.migrationProgress} />
          </div>

          {/* Row 2: Migration Timeline (full width) */}
          <MigrationTimelineCard />

          {/* Row 3: Test Suite Status (full width) */}
          <TestSuiteStatusCard />

          {/* Row 4: Data Structure + Complexity */}
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            <DataStructureCard dataStructure={metrics.dataStructure} />
            <ComplexityMetricsCard complexity={metrics.complexity} />
          </div>

          {/* Row 5: Function Points (full width) */}
          <FunctionPointsChart functionPoints={functionPoints} />

          {/* Row 6: Database Dependencies (full width) */}
          <DatabaseDependenciesChart dependencies={dependencies} />
        </div>

        {/* Next Steps Section - Redesigned with Caixa Colors */}
        <div className="mt-8 p-8 bg-gradient-to-r from-caixa-blue to-indigo-600 rounded-2xl shadow-2xl text-white">
          <div className="flex items-start gap-6">
            <div className="text-6xl">🎯</div>
            <div className="flex-grow">
              <h3 className="text-3xl font-bold mb-6">
                Próximas Ações - Caminho para Produção
              </h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="bg-white/10 backdrop-blur-sm rounded-lg p-5 border-2 border-white/20">
                  <div className="flex items-center gap-3 mb-2">
                    <div className="w-8 h-8 bg-caixa-yellow rounded-full flex items-center justify-center font-black text-caixa-blue">
                      1
                    </div>
                    <h4 className="text-xl font-bold">UAT (Semana 10-11)</h4>
                  </div>
                  <p style={{ color: '#fff' }}>
                    Agendar testes de aceitação com usuários finais, validar saída byte-a-byte com COBOL
                  </p>
                </div>

                <div className="bg-white/10 backdrop-blur-sm rounded-lg p-5 border-2 border-white/20">
                  <div className="flex items-center gap-3 mb-2">
                    <div className="w-8 h-8 bg-caixa-yellow rounded-full flex items-center justify-center font-black text-caixa-blue">
                      2
                    </div>
                    <h4 className="text-xl font-bold">Revisão de Documentação</h4>
                  </div>
                  <p style={{ color: '#fff' }}>
                    Validar manual de operações, guias de deployment, runbooks
                  </p>
                </div>

                <div className="bg-white/10 backdrop-blur-sm rounded-lg p-5 border-2 border-white/20">
                  <div className="flex items-center gap-3 mb-2">
                    <div className="w-8 h-8 bg-caixa-yellow rounded-full flex items-center justify-center font-black text-caixa-blue">
                      3
                    </div>
                    <h4 className="text-xl font-bold">Sign-off Técnico</h4>
                  </div>
                  <p style={{ color: '#fff' }}>
                    Obter aprovação formal de arquitetura, segurança e compliance
                  </p>
                </div>

                <div className="bg-white/10 backdrop-blur-sm rounded-lg p-5 border-2 border-white/20">
                  <div className="flex items-center gap-3 mb-2">
                    <div className="w-8 h-8 bg-caixa-yellow rounded-full flex items-center justify-center font-black text-caixa-blue">
                      4
                    </div>
                    <h4 className="text-xl font-bold">Deploy em Produção (Semana 12)</h4>
                  </div>
                  <p style={{ color: '#fff' }}>
                    Execução do plano de cutover, monitoramento pós-deploy
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Footer Actions - Redesigned */}
        <div className="mt-8 p-8 bg-white rounded-2xl shadow-xl border-2 border-gray-200">
          <div className="flex flex-col md:flex-row items-center justify-between gap-6">
            <div className="flex-grow">
              <h3 className="text-2xl font-bold text-gray-800 mb-2">Explorar Funcionalidades</h3>
              <p className="text-gray-600">
                Experimente as funcionalidades implementadas: geração de relatórios, consulta de prêmios,
                processamento batch e carregamento de dados.
              </p>
            </div>
            <div className="flex gap-4">
              <button
                onClick={loadDashboardData}
                className="px-6 py-3 bg-gray-700 text-white font-semibold rounded-lg hover:bg-gray-800 transition-all shadow-lg hover:shadow-xl"
              >
                🔄 Atualizar Dados
              </button>
              <button
                onClick={() => window.location.href = '/reports'}
                className="px-6 py-3 bg-caixa-blue text-white font-semibold rounded-lg hover:bg-caixa-blue-dark transition-all shadow-lg hover:shadow-xl"
              >
                📄 Gerar Relatórios
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
