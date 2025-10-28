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
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#E3F2FD', border: '2px solid #0047BB' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#1565C0' }}>PF Não Ajustados</p>
            <p className="text-3xl font-black" style={{ color: '#0D47A1' }}>
              {functionPoints.totalUnadjustedFunctionPoints}
            </p>
          </div>
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#E8F5E9', border: '2px solid #28A745' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#2E7D32' }}>PF Ajustados</p>
            <p className="text-3xl font-black" style={{ color: '#1B5E20' }}>
              {Math.round(functionPoints.totalAdjustedFunctionPoints)}
            </p>
          </div>
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#F3E5F5', border: '2px solid #7B1FA2' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#7B1FA2' }}>Esforço Estimado</p>
            <p className="text-3xl font-black" style={{ color: '#4A148C' }}>
              {functionPoints.estimatedEffortMonths.toFixed(1)}
              <span className="text-sm font-normal ml-1">meses</span>
            </p>
          </div>
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#FFF3E0', border: '2px solid #FF9800' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#E65100' }}>Complexidade</p>
            <p className="text-2xl font-black" style={{ color: '#BF360C' }}>
              {functionPoints.complexityRating}
            </p>
          </div>
        </div>

        {/* Function Points Breakdown */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <h4 className="text-md font-semibold mb-3" style={{ color: '#333' }}>
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
            <h4 className="text-md font-semibold mb-3" style={{ color: '#333' }}>
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
          <h4 className="text-md font-semibold mb-3" style={{ color: '#333' }}>
            Breakdown por Módulo COBOL
          </h4>
          <div className="space-y-2 max-h-[300px] overflow-y-auto">
            {functionPoints.moduleBreakdown.map((module, index) => (
              <div
                key={index}
                className="p-4 rounded-lg hover:shadow-md transition-shadow"
                style={{
                  backgroundColor: '#F5F5F5',
                  border: '1px solid #e2e2e2'
                }}
              >
                <div className="flex items-center justify-between mb-2">
                  <div className="flex-1">
                    <p className="font-semibold" style={{ color: '#000' }}>
                      {module.moduleName}
                    </p>
                    <p className="text-sm" style={{ color: '#666' }}>
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
                    <span
                      className="px-3 py-1 rounded-full text-xs font-semibold"
                      style={{
                        backgroundColor:
                          module.status === 'Complete' ? '#C8E6C9' :
                          module.status === 'In Progress' ? '#FFF9C4' : '#EEEEEE',
                        color:
                          module.status === 'Complete' ? '#1B5E20' :
                          module.status === 'In Progress' ? '#F57F17' : '#616161'
                      }}
                    >
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
