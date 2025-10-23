import Card from '../common/Card';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, RadarChart, PolarGrid, PolarAngleAxis, PolarRadiusAxis, Radar } from 'recharts';
import type { FunctionPointsDto } from '../../services/types';

interface FunctionPointsChartProps {
  functionPoints: FunctionPointsDto;
}

export const FunctionPointsChart: React.FC<FunctionPointsChartProps> = ({ functionPoints }) => {
  // Prepare data for function points breakdown
  const fpData = [
    { name: 'External Inputs', points: functionPoints.externalInputs.totalPoints },
    { name: 'External Outputs', points: functionPoints.externalOutputs.totalPoints },
    { name: 'External Inquiries', points: functionPoints.externalInquiries.totalPoints },
    { name: 'Internal Files', points: functionPoints.internalLogicalFiles.totalPoints },
    { name: 'External Interfaces', points: functionPoints.externalInterfaceFiles.totalPoints },
  ];

  // Prepare radar chart data
  const radarData = fpData.map(item => ({
    category: item.name.replace('External ', '').replace('Internal ', ''),
    value: item.points,
  }));

  // Get complexity distribution color
  const getComplexityColor = (complexity: string) => {
    switch (complexity) {
      case 'Low': return '#10B981';
      case 'Medium': return '#F59E0B';
      case 'High': return '#EF4444';
      default: return '#6B7280';
    }
  };

  return (
    <Card title="Análise de Pontos de Função (IFPUG)">
      <div className="space-y-6">
        {/* Summary Metrics */}
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          <div className="p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
            <p className="text-sm text-blue-600 dark:text-blue-400 mb-1">PF Não Ajustados</p>
            <p className="text-3xl font-bold text-blue-700 dark:text-blue-300">
              {functionPoints.totalUnadjustedFunctionPoints}
            </p>
          </div>
          <div className="p-4 bg-green-50 dark:bg-green-900/20 rounded-lg">
            <p className="text-sm text-green-600 dark:text-green-400 mb-1">PF Ajustados</p>
            <p className="text-3xl font-bold text-green-700 dark:text-green-300">
              {Math.round(functionPoints.totalAdjustedFunctionPoints)}
            </p>
          </div>
          <div className="p-4 bg-purple-50 dark:bg-purple-900/20 rounded-lg">
            <p className="text-sm text-purple-600 dark:text-purple-400 mb-1">Esforço Estimado</p>
            <p className="text-3xl font-bold text-purple-700 dark:text-purple-300">
              {functionPoints.estimatedEffortMonths.toFixed(1)}
              <span className="text-sm font-normal ml-1">meses</span>
            </p>
          </div>
          <div className="p-4 bg-orange-50 dark:bg-orange-900/20 rounded-lg">
            <p className="text-sm text-orange-600 dark:text-orange-400 mb-1">Complexidade</p>
            <p className="text-2xl font-bold text-orange-700 dark:text-orange-300">
              {functionPoints.complexityRating}
            </p>
          </div>
        </div>

        {/* Function Points Breakdown */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <h4 className="text-md font-semibold text-gray-700 dark:text-gray-300 mb-3">
              Distribuição de Pontos de Função
            </h4>
            <ResponsiveContainer width="100%" height={250}>
              <BarChart data={fpData} layout="vertical">
                <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />
                <XAxis type="number" tick={{ fontSize: 12 }} />
                <YAxis
                  dataKey="name"
                  type="category"
                  width={120}
                  tick={{ fontSize: 11 }}
                />
                <Tooltip />
                <Bar dataKey="points" fill="#8B5CF6" name="Pontos" />
              </BarChart>
            </ResponsiveContainer>
          </div>

          <div>
            <h4 className="text-md font-semibold text-gray-700 dark:text-gray-300 mb-3">
              Visão Radar - Complexidade
            </h4>
            <ResponsiveContainer width="100%" height={250}>
              <RadarChart cx="50%" cy="50%" outerRadius="80%" data={radarData}>
                <PolarGrid stroke="#e5e7eb" />
                <PolarAngleAxis dataKey="category" tick={{ fontSize: 10 }} />
                <PolarRadiusAxis angle={90} domain={[0, 'auto']} tick={{ fontSize: 10 }} />
                <Radar
                  name="Pontos de Função"
                  dataKey="value"
                  stroke="#8B5CF6"
                  fill="#8B5CF6"
                  fillOpacity={0.6}
                />
                <Tooltip />
              </RadarChart>
            </ResponsiveContainer>
          </div>
        </div>

        {/* Module Breakdown */}
        <div>
          <h4 className="text-md font-semibold text-gray-700 dark:text-gray-300 mb-3">
            Breakdown por Módulo COBOL
          </h4>
          <div className="space-y-2 max-h-[300px] overflow-y-auto">
            {functionPoints.moduleBreakdown.map((module, index) => (
              <div
                key={index}
                className="p-4 bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 hover:shadow-md transition-shadow"
              >
                <div className="flex items-center justify-between mb-2">
                  <div className="flex-1">
                    <p className="font-semibold text-gray-900 dark:text-white">
                      {module.moduleName}
                    </p>
                    <p className="text-sm text-gray-600 dark:text-gray-400">
                      {module.functionPoints} PF • {module.estimatedHours.toFixed(0)}h estimadas
                    </p>
                  </div>
                  <div className="flex items-center gap-2">
                    <span
                      className="px-3 py-1 rounded-full text-xs font-semibold text-white"
                      style={{ backgroundColor: getComplexityColor(module.complexity) }}
                    >
                      {module.complexity}
                    </span>
                    <span className={`px-3 py-1 rounded-full text-xs font-semibold ${
                      module.status === 'Complete' ? 'bg-green-100 text-green-800 dark:bg-green-900 dark:text-green-200' :
                      module.status === 'In Progress' ? 'bg-yellow-100 text-yellow-800 dark:bg-yellow-900 dark:text-yellow-200' :
                      'bg-gray-100 text-gray-800 dark:bg-gray-700 dark:text-gray-200'
                    }`}>
                      {module.status}
                    </span>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </Card>
  );
};
