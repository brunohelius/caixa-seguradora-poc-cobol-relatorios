import React from 'react';
import { BarChart, Bar, PieChart, Pie, Cell, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import type { PremiumStatisticsResponse } from '../../services/types';

interface QueryStatisticsCardProps {
  statistics: PremiumStatisticsResponse[];
  groupBy: 'product' | 'lineOfBusiness' | 'movementType' | 'month';
  loading?: boolean;
}

/**
 * QueryStatisticsCard Component
 *
 * Displays premium statistics with visualizations:
 * - Summary cards showing totals, averages, record count
 * - Bar chart for trend analysis
 * - Pie chart for distribution breakdown
 * - Supports grouping by: product, line of business, movement type, month
 *
 * Uses Recharts for data visualization and Portuguese labels.
 */
const QueryStatisticsCard: React.FC<QueryStatisticsCardProps> = ({
  statistics,
  groupBy,
  loading = false,
}) => {
  // Calculate aggregated totals
  const calculateTotals = () => {
    if (!statistics || statistics.length === 0) {
      return {
        totalPremium: 0,
        totalCommission: 0,
        recordCount: 0,
        averagePremium: 0,
      };
    }

    const totalPremium = statistics.reduce((sum, stat) => sum + stat.totalPremium, 0);
    const totalCommission = statistics.reduce((sum, stat) => sum + (stat.totalCommission || 0), 0);
    const recordCount = statistics.reduce((sum, stat) => sum + stat.recordCount, 0);
    const averagePremium = recordCount > 0 ? totalPremium / recordCount : 0;

    return {
      totalPremium,
      totalCommission,
      recordCount,
      averagePremium,
    };
  };

  const formatCurrency = (value: number): string => {
    return new Intl.NumberFormat('pt-BR', {
      style: 'currency',
      currency: 'BRL',
    }).format(value);
  };

  const formatNumber = (value: number): string => {
    return new Intl.NumberFormat('pt-BR').format(value);
  };

  const getGroupLabel = (groupValue: string): string => {
    if (groupBy === 'movementType') {
      const types: Record<string, string> = {
        'E': 'Emissão',
        'C': 'Cancelamento',
        'R': 'Renovação',
        'S': 'Substituição',
        'A': 'Alteração',
      };
      return types[groupValue] || groupValue;
    }
    return groupValue;
  };

  const getChartTitle = (): string => {
    const titles: Record<string, string> = {
      product: 'Por Produto',
      lineOfBusiness: 'Por Ramo SUSEP',
      movementType: 'Por Tipo de Movimento',
      month: 'Por Mês',
    };
    return titles[groupBy] || 'Estatísticas';
  };

  // Prepare chart data
  const chartData = statistics.map(stat => ({
    name: getGroupLabel(stat.groupValue || ''),
    premium: stat.totalPremium,
    commission: stat.totalCommission || 0,
    count: stat.recordCount,
  }));

  // Colors for pie chart
  const COLORS = ['#0088FE', '#00C49F', '#FFBB28', '#FF8042', '#8884D8', '#82CA9D', '#FFC658', '#FF6B9D'];

  const totals = calculateTotals();

  // Loading state
  if (loading) {
    return (
      <div className="bg-white rounded-lg shadow p-6">
        <div className="flex items-center justify-center py-12">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
          <span className="ml-3 text-gray-600">Carregando estatísticas...</span>
        </div>
      </div>
    );
  }

  // Empty state
  if (!statistics || statistics.length === 0) {
    return (
      <div className="bg-white rounded-lg shadow p-6">
        <h3 className="text-lg font-semibold text-gray-800 mb-4">Estatísticas</h3>
        <div className="text-center py-8 text-gray-500">
          Nenhuma estatística disponível. Execute uma pesquisa para visualizar dados.
        </div>
      </div>
    );
  }

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h3 className="text-lg font-semibold text-gray-800 mb-6">
        Estatísticas {getChartTitle()}
      </h3>

      {/* Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4 mb-8">
        <div className="bg-blue-50 rounded-lg p-4 border border-blue-100">
          <div className="text-sm text-blue-600 font-medium mb-1">Total de Prêmios</div>
          <div className="text-2xl font-bold text-blue-900">
            {formatCurrency(totals.totalPremium)}
          </div>
        </div>

        <div className="bg-green-50 rounded-lg p-4 border border-green-100">
          <div className="text-sm text-green-600 font-medium mb-1">Total de Comissões</div>
          <div className="text-2xl font-bold text-green-900">
            {formatCurrency(totals.totalCommission)}
          </div>
        </div>

        <div className="bg-purple-50 rounded-lg p-4 border border-purple-100">
          <div className="text-sm text-purple-600 font-medium mb-1">Prêmio Médio</div>
          <div className="text-2xl font-bold text-purple-900">
            {formatCurrency(totals.averagePremium)}
          </div>
        </div>

        <div className="bg-orange-50 rounded-lg p-4 border border-orange-100">
          <div className="text-sm text-orange-600 font-medium mb-1">Registros</div>
          <div className="text-2xl font-bold text-orange-900">
            {formatNumber(totals.recordCount)}
          </div>
        </div>
      </div>

      {/* Charts */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Bar Chart */}
        <div>
          <h4 className="text-sm font-medium text-gray-700 mb-3">Prêmios e Comissões {getChartTitle()}</h4>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={chartData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis
                dataKey="name"
                angle={-45}
                textAnchor="end"
                height={80}
                tick={{ fontSize: 12 }}
              />
              <YAxis
                tickFormatter={(value) => formatCurrency(value)}
                tick={{ fontSize: 12 }}
              />
              <Tooltip
                formatter={(value: number) => formatCurrency(value)}
                labelStyle={{ color: '#1f2937' }}
              />
              <Legend />
              <Bar dataKey="premium" name="Prêmio" fill="#0088FE" />
              <Bar dataKey="commission" name="Comissão" fill="#00C49F" />
            </BarChart>
          </ResponsiveContainer>
        </div>

        {/* Pie Chart */}
        <div>
          <h4 className="text-sm font-medium text-gray-700 mb-3">Distribuição de Prêmios {getChartTitle()}</h4>
          <ResponsiveContainer width="100%" height={300}>
            <PieChart>
              <Pie
                data={chartData}
                cx="50%"
                cy="50%"
                labelLine={false}
                label={({ name, percent }: any) => `${name}: ${((percent as number) * 100).toFixed(1)}%`}
                outerRadius={100}
                fill="#8884d8"
                dataKey="premium"
              >
                {chartData.map((_entry, index) => (
                  <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                ))}
              </Pie>
              <Tooltip
                formatter={(value: number) => formatCurrency(value)}
              />
            </PieChart>
          </ResponsiveContainer>
        </div>
      </div>

      {/* Detailed Breakdown Table */}
      <div className="mt-6">
        <h4 className="text-sm font-medium text-gray-700 mb-3">Detalhamento {getChartTitle()}</h4>
        <div className="overflow-x-auto">
          <table className="min-w-full divide-y divide-gray-200 border border-gray-200 rounded-lg">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-4 py-2 text-left text-xs font-medium text-gray-500 uppercase">
                  Categoria
                </th>
                <th className="px-4 py-2 text-right text-xs font-medium text-gray-500 uppercase">
                  Registros
                </th>
                <th className="px-4 py-2 text-right text-xs font-medium text-gray-500 uppercase">
                  Prêmio Total
                </th>
                <th className="px-4 py-2 text-right text-xs font-medium text-gray-500 uppercase">
                  Comissão Total
                </th>
                <th className="px-4 py-2 text-right text-xs font-medium text-gray-500 uppercase">
                  Prêmio Médio
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {statistics.map((stat, index) => (
                <tr key={index} className="hover:bg-gray-50">
                  <td className="px-4 py-2 text-sm text-gray-900 font-medium">
                    {getGroupLabel(stat.groupValue || '')}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-500 text-right">
                    {formatNumber(stat.recordCount)}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-900 text-right font-medium">
                    {formatCurrency(stat.totalPremium)}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-500 text-right">
                    {formatCurrency(stat.totalCommission || 0)}
                  </td>
                  <td className="px-4 py-2 text-sm text-gray-500 text-right">
                    {formatCurrency(stat.averagePremium)}
                  </td>
                </tr>
              ))}
              <tr className="bg-gray-50 font-semibold">
                <td className="px-4 py-2 text-sm text-gray-900">Total</td>
                <td className="px-4 py-2 text-sm text-gray-900 text-right">
                  {formatNumber(totals.recordCount)}
                </td>
                <td className="px-4 py-2 text-sm text-gray-900 text-right">
                  {formatCurrency(totals.totalPremium)}
                </td>
                <td className="px-4 py-2 text-sm text-gray-900 text-right">
                  {formatCurrency(totals.totalCommission)}
                </td>
                <td className="px-4 py-2 text-sm text-gray-900 text-right">
                  {formatCurrency(totals.averagePremium)}
                </td>
              </tr>
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
};

export default QueryStatisticsCard;
