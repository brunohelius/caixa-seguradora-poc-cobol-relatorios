import Card from '../common/Card';
import type { ComplexityMetricsDto } from '../../services/types';

interface ComplexityMetricsCardProps {
  complexity: ComplexityMetricsDto;
}

export const ComplexityMetricsCard: React.FC<ComplexityMetricsCardProps> = ({ complexity }) => {
  const getComplexityLevel = (cyclomaticComplexity: number): { level: string; color: string } => {
    if (cyclomaticComplexity < 50) return { level: 'Baixa', color: 'bg-green-500' };
    if (cyclomaticComplexity < 100) return { level: 'Média', color: 'bg-yellow-500' };
    if (cyclomaticComplexity < 150) return { level: 'Alta', color: 'bg-orange-500' };
    return { level: 'Muito Alta', color: 'bg-red-500' };
  };

  const complexityLevel = getComplexityLevel(complexity.cyclomaticComplexity);

  const metrics = [
    { label: 'Seções', value: complexity.totalSections, icon: '📋' },
    { label: 'Parágrafos', value: complexity.totalParagraphs, icon: '📄' },
    { label: 'Pontos de Decisão', value: complexity.decisionPoints, icon: '🔀' },
    { label: 'Chamadas Externas', value: complexity.externalCalls, icon: '📞' },
    { label: 'Operações SQL', value: complexity.sqlStatements, icon: '🗄️' },
    { label: 'Operações I/O', value: complexity.fileOperations, icon: '📁' },
  ];

  return (
    <Card title="Métricas de Complexidade">
      <div className="mb-6">
        <div className="flex items-center justify-between mb-2">
          <span className="text-sm font-medium text-gray-700 dark:text-gray-300">
            Complexidade Ciclomática
          </span>
          <span className={`px-3 py-1 rounded-full text-white text-sm font-bold ${complexityLevel.color}`}>
            {complexityLevel.level}
          </span>
        </div>
        <div className="w-full bg-gray-200 dark:bg-gray-700 rounded-full h-3">
          <div
            className={`h-3 rounded-full ${complexityLevel.color}`}
            style={{ width: `${Math.min((complexity.cyclomaticComplexity / 200) * 100, 100)}%` }}
          />
        </div>
        <p className="text-2xl font-bold text-gray-900 dark:text-white mt-2">
          {complexity.cyclomaticComplexity} pontos
        </p>
      </div>

      <div className="grid grid-cols-2 md:grid-cols-3 gap-3">
        {metrics.map((metric, index) => (
          <div
            key={index}
            className="p-3 bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 hover:shadow-md transition-shadow"
          >
            <div className="flex items-center gap-2 mb-1">
              <span className="text-xl">{metric.icon}</span>
              <p className="text-xs text-gray-600 dark:text-gray-400">{metric.label}</p>
            </div>
            <p className="text-2xl font-bold text-gray-900 dark:text-white">
              {metric.value}
            </p>
          </div>
        ))}
      </div>
    </Card>
  );
};
