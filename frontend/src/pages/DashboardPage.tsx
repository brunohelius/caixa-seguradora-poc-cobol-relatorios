import { useEffect, useState } from 'react';
import Spinner from '../components/common/Spinner';
import ErrorMessage from '../components/common/ErrorMessage';
import { ProgramInfoCard } from '../components/dashboard/ProgramInfoCard';
import { DataStructureCard } from '../components/dashboard/DataStructureCard';
import { ComplexityMetricsCard } from '../components/dashboard/ComplexityMetricsCard';
import { DatabaseDependenciesChart } from '../components/dashboard/DatabaseDependenciesChart';
import { FunctionPointsChart } from '../components/dashboard/FunctionPointsChart';
import { MigrationProgressCard } from '../components/dashboard/MigrationProgressCard';
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
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <Spinner size="large" />
          <p className="mt-4 text-gray-600 dark:text-gray-400">
            Carregando análise do programa COBOL...
          </p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="container mx-auto px-4 py-8">
        <ErrorMessage message={error} />
        <button
          onClick={loadDashboardData}
          className="mt-4 px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 transition-colors"
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
    <div className="container mx-auto px-4 py-8">
      {/* Page Header */}
      <div className="mb-8">
        <h1 className="text-4xl font-bold text-gray-900 dark:text-white mb-2">
          Dashboard de Migração COBOL
        </h1>
        <p className="text-lg text-gray-600 dark:text-gray-400">
          Análise completa do programa RG1866B - Sistema de Relatórios de Prêmios SUSEP
        </p>
      </div>

      {/* Dashboard Grid */}
      <div className="space-y-6">
        {/* Row 1: Program Info + Migration Progress */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <ProgramInfoCard programInfo={metrics.programInfo} />
          <MigrationProgressCard migrationProgress={metrics.migrationProgress} />
        </div>

        {/* Row 2: Data Structure + Complexity */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <DataStructureCard dataStructure={metrics.dataStructure} />
          <ComplexityMetricsCard complexity={metrics.complexity} />
        </div>

        {/* Row 3: Function Points (full width) */}
        <div className="grid grid-cols-1 gap-6">
          <FunctionPointsChart functionPoints={functionPoints} />
        </div>

        {/* Row 4: Database Dependencies (full width) */}
        <div className="grid grid-cols-1 gap-6">
          <DatabaseDependenciesChart dependencies={dependencies} />
        </div>
      </div>

      {/* Footer Actions */}
      <div className="mt-8 p-6 bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700">
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-1">
              Próximos Passos
            </h3>
            <p className="text-sm text-gray-600 dark:text-gray-400">
              Com a análise completa, você pode prosseguir para a geração de relatórios ou
              consulta de dados migrados.
            </p>
          </div>
          <div className="flex gap-4">
            <button
              onClick={loadDashboardData}
              className="px-4 py-2 bg-gray-200 dark:bg-gray-700 text-gray-700 dark:text-gray-300 rounded hover:bg-gray-300 dark:hover:bg-gray-600 transition-colors"
            >
              🔄 Atualizar
            </button>
            <button
              onClick={() => {/* Navigate to reports */}}
              className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 transition-colors"
            >
              📄 Gerar Relatórios
            </button>
          </div>
        </div>
      </div>
    </div>
  );
};
