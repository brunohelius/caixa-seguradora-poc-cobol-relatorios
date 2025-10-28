import Card from '../common/Card';
import { Badge } from '../ui/badge';
import { Progress } from '../ui/progress';
import type { ComplexityMetricsDto } from '../../services/types';

interface ComplexityMetricsCardProps {
  complexity: ComplexityMetricsDto;
}

export const ComplexityMetricsCard: React.FC<ComplexityMetricsCardProps> = ({ complexity }) => {
  const getComplexityLevel = (cyclomaticComplexity: number): {
    level: string;
    badgeClass: string;
    progressVariant: 'default' | 'success' | 'warning' | 'danger';
  } => {
    if (cyclomaticComplexity < 50)
      return { level: 'Baixa', badgeClass: 'bg-success text-white', progressVariant: 'success' };
    if (cyclomaticComplexity < 100)
      return { level: 'Média', badgeClass: 'bg-warning text-black', progressVariant: 'warning' };
    if (cyclomaticComplexity < 150)
      return { level: 'Alta', badgeClass: 'bg-orange-500 text-white', progressVariant: 'warning' };
    return { level: 'Muito Alta', badgeClass: 'bg-error text-white', progressVariant: 'danger' };
  };

  const complexityLevel = getComplexityLevel(complexity.cyclomaticComplexity);

  const metrics = [
    {
      label: 'Seções',
      value: complexity.totalSections,
      icon: '📋',
      bgClass: 'bg-caixa-blue-light',
      textClass: 'text-caixa-blue'
    },
    {
      label: 'Parágrafos',
      value: complexity.totalParagraphs,
      icon: '📄',
      bgClass: 'bg-yellow-50',
      textClass: 'text-caixa-yellow-dark'
    },
    {
      label: 'Pontos de Decisão',
      value: complexity.decisionPoints,
      icon: '🔀',
      bgClass: 'bg-purple-50',
      textClass: 'text-purple-700'
    },
    {
      label: 'Chamadas Externas',
      value: complexity.externalCalls,
      icon: '📞',
      bgClass: 'bg-green-50',
      textClass: 'text-green-700'
    },
    {
      label: 'Operações SQL',
      value: complexity.sqlStatements,
      icon: '🗄️',
      bgClass: 'bg-blue-50',
      textClass: 'text-blue-700'
    },
    {
      label: 'Operações I/O',
      value: complexity.fileOperations,
      icon: '📁',
      bgClass: 'bg-pink-50',
      textClass: 'text-pink-700'
    },
  ];

  return (
    <Card title="Métricas de Complexidade">
      <div className="mb-6">
        <div className="flex items-center justify-between mb-3">
          <span className="text-base font-bold text-black">
            Complexidade Ciclomática
          </span>
          <Badge className={`px-4 py-1.5 shadow-md ${complexityLevel.badgeClass}`}>
            {complexityLevel.level}
          </Badge>
        </div>
        <Progress
          value={Math.min((complexity.cyclomaticComplexity / 200) * 100, 100)}
          variant={complexityLevel.progressVariant}
          className="h-4"
        />
        <p className="text-2xl font-black mt-3 text-black">
          {complexity.cyclomaticComplexity} pontos
        </p>
      </div>

      <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
        {metrics.map((metric, index) => (
          <div
            key={index}
            className={`p-4 rounded-lg shadow-sm hover:shadow-md transition-shadow border border-site-grayDark ${metric.bgClass}`}
          >
            <div className="flex items-center gap-2 mb-2">
              <span className="text-2xl">{metric.icon}</span>
              <p className="text-xs font-semibold text-gray-600">
                {metric.label}
              </p>
            </div>
            <p className={`text-2xl font-black ${metric.textClass}`}>
              {metric.value}
            </p>
          </div>
        ))}
      </div>
    </Card>
  );
};
