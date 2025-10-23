import React, { useState, useEffect } from 'react';
import queryService from '../../services/queryService';
import type { PremiumStatisticsResponse } from '../../services/types';
import QueryStatisticsCard from './QueryStatisticsCard';

interface QueryVisualizationPanelProps {
  startDate: string;
  endDate: string;
  enabled: boolean;
}

type GroupByOption = 'product' | 'lineOfBusiness' | 'movementType' | 'month';

/**
 * QueryVisualizationPanel Component
 *
 * Interactive data visualization panel with:
 * - Chart type selector (group by: product, LOB, movement type, month)
 * - Dynamic statistics loading based on selected grouping
 * - Integration with QueryStatisticsCard for rendering
 * - Automatic refresh when date range changes
 *
 * All text in Portuguese for Caixa Seguradora.
 */
const QueryVisualizationPanel: React.FC<QueryVisualizationPanelProps> = ({
  startDate,
  endDate,
  enabled,
}) => {
  const [groupBy, setGroupBy] = useState<GroupByOption>('product');
  const [statistics, setStatistics] = useState<PremiumStatisticsResponse[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (enabled && startDate && endDate) {
      loadStatistics();
    }
  }, [groupBy, startDate, endDate, enabled]);

  const loadStatistics = async () => {
    if (!startDate || !endDate) {
      setError('Por favor, selecione um intervalo de datas');
      return;
    }

    setLoading(true);
    setError(null);

    try {
      const data = await queryService.getPremiumStatistics({
        startDate,
        endDate,
        groupBy,
      });

      setStatistics(data);
    } catch (err) {
      console.error('Erro ao carregar estatísticas:', err);
      setError('Erro ao carregar estatísticas. Tente novamente.');
      setStatistics([]);
    } finally {
      setLoading(false);
    }
  };

  const handleGroupByChange = (newGroupBy: GroupByOption) => {
    setGroupBy(newGroupBy);
  };

  const groupByOptions = [
    { value: 'product' as GroupByOption, label: 'Por Produto', icon: '📦' },
    { value: 'lineOfBusiness' as GroupByOption, label: 'Por Ramo SUSEP', icon: '🏢' },
    { value: 'movementType' as GroupByOption, label: 'Por Tipo de Movimento', icon: '🔄' },
    { value: 'month' as GroupByOption, label: 'Por Mês', icon: '📅' },
  ];

  // If panel is not enabled, show message
  if (!enabled) {
    return (
      <div className="bg-white rounded-lg shadow p-6">
        <h3 className="text-lg font-semibold text-gray-800 mb-4">Visualizações</h3>
        <div className="text-center py-8 text-gray-500">
          Execute uma pesquisa para visualizar estatísticas e gráficos.
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Chart Type Selector */}
      <div className="bg-white rounded-lg shadow p-6">
        <h3 className="text-lg font-semibold text-gray-800 mb-4">Tipo de Visualização</h3>

        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-3">
          {groupByOptions.map((option) => (
            <button
              key={option.value}
              onClick={() => handleGroupByChange(option.value)}
              className={`flex flex-col items-center justify-center p-4 rounded-lg border-2 transition-all ${
                groupBy === option.value
                  ? 'border-blue-600 bg-blue-50 text-blue-900'
                  : 'border-gray-200 bg-white text-gray-700 hover:border-blue-300 hover:bg-blue-50'
              }`}
            >
              <span className="text-3xl mb-2">{option.icon}</span>
              <span className="text-sm font-medium text-center">{option.label}</span>
            </button>
          ))}
        </div>

        {!startDate || !endDate ? (
          <div className="mt-4 p-3 bg-yellow-50 border border-yellow-200 rounded-md">
            <p className="text-sm text-yellow-800">
              Por favor, selecione um intervalo de datas nos filtros para visualizar estatísticas.
            </p>
          </div>
        ) : null}
      </div>

      {/* Error State */}
      {error && (
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex items-start">
            <svg
              className="w-5 h-5 text-red-600 mt-0.5 mr-3"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <div>
              <h4 className="text-sm font-medium text-red-800">Erro ao Carregar Estatísticas</h4>
              <p className="text-sm text-red-700 mt-1">{error}</p>
              <button
                onClick={loadStatistics}
                className="mt-2 text-sm font-medium text-red-800 hover:text-red-900 underline"
              >
                Tentar Novamente
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Statistics Card */}
      <QueryStatisticsCard
        statistics={statistics}
        groupBy={groupBy}
        loading={loading}
      />

      {/* Info Panel */}
      {statistics.length > 0 && (
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <div className="flex items-start">
            <svg
              className="w-5 h-5 text-blue-600 mt-0.5 mr-3"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <div className="text-sm text-blue-800">
              <p className="font-medium">Dica:</p>
              <p className="mt-1">
                Use os botões acima para alternar entre diferentes tipos de visualização.
                Os gráficos são atualizados automaticamente quando você muda o agrupamento
                ou o intervalo de datas.
              </p>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default QueryVisualizationPanel;
