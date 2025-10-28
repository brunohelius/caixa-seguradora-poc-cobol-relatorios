import Card from '../common/Card';
import { Badge } from '../ui/badge';
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
          <div className="p-4 rounded-lg bg-blue-50 border-2 border-caixa-blue">
            <p className="text-sm font-semibold mb-1 text-blue-700">PF Não Ajustados</p>
            <p className="text-3xl font-black text-blue-900">
              {functionPoints.totalUnadjustedFunctionPoints}
            </p>
          </div>
          <div className="p-4 rounded-lg bg-green-50 border-2 border-success">
            <p className="text-sm font-semibold mb-1 text-green-700">PF Ajustados</p>
            <p className="text-3xl font-black text-green-900">
              {Math.round(functionPoints.totalAdjustedFunctionPoints)}
            </p>
          </div>
          <div className="p-4 rounded-lg bg-purple-50 border-2 border-purple-700">
            <p className="text-sm font-semibold mb-1 text-purple-700">Esforço Estimado</p>
            <p className="text-3xl font-black text-purple-900">
              {functionPoints.estimatedEffortMonths.toFixed(1)}
              <span className="text-sm font-normal ml-1">meses</span>
            </p>
          </div>
          <div className="p-4 rounded-lg bg-orange-50 border-2 border-orange-500">
            <p className="text-sm font-semibold mb-1 text-orange-700">Complexidade</p>
            <p className="text-2xl font-black text-orange-900">
              {functionPoints.complexityRating}
            </p>
          </div>
        </div>

        {/* Function Points Breakdown */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <h4 className="text-md font-semibold mb-3 text-gray-700">
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
            <h4 className="text-md font-semibold mb-3 text-gray-700">
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
          <h4 className="text-md font-semibold mb-3 text-gray-700">
            Breakdown por Módulo COBOL
          </h4>
          <div className="space-y-2 max-h-[300px] overflow-y-auto">
            {functionPoints.moduleBreakdown.map((module, index) => (
              <div
                key={index}
                className="p-4 rounded-lg hover:shadow-md transition-shadow bg-gray-100 border border-site-grayDark"
              >
                <div className="flex items-center justify-between mb-2">
                  <div className="flex-1">
                    <p className="font-semibold text-black">
                      {module.moduleName}
                    </p>
                    <p className="text-sm text-gray-600">
                      {module.functionPoints} PF • {module.estimatedHours.toFixed(0)}h estimadas
                    </p>
                  </div>
                  <div className="flex items-center gap-2">
                    <Badge
                      className="text-white"
                      style={{ backgroundColor: getComplexityColor(module.complexity) }}
                    >
                      {module.complexity}
                    </Badge>
                    <Badge
                      className={
                        module.status === 'Complete' ? 'bg-green-200 text-green-900' :
                        module.status === 'In Progress' ? 'bg-yellow-200 text-yellow-900' :
                        'bg-gray-200 text-gray-700'
                      }
                    >
                      {module.status}
                    </Badge>
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
