import Card from '../common/Card';
import type { ComplexityMetricsDto } from '../../services/types';

interface ComplexityMetricsCardProps {
  complexity: ComplexityMetricsDto;
}

export const ComplexityMetricsCard: React.FC<ComplexityMetricsCardProps> = ({ complexity }) => {
  const getComplexityLevel = (cyclomaticComplexity: number): {
    level: string;
    bgColor: string;
    progressColor: string;
  } => {
    if (cyclomaticComplexity < 50)
      return { level: 'Baixa', bgColor: '#28A745', progressColor: '#28A745' };
    if (cyclomaticComplexity < 100)
      return { level: 'Média', bgColor: '#FFC107', progressColor: '#FFC107' };
    if (cyclomaticComplexity < 150)
      return { level: 'Alta', bgColor: '#FF9800', progressColor: '#FF9800' };
    return { level: 'Muito Alta', bgColor: '#DC3545', progressColor: '#DC3545' };
  };

  const complexityLevel = getComplexityLevel(complexity.cyclomaticComplexity);

  const metrics = [
    {
      label: 'Seções',
      value: complexity.totalSections,
      icon: '📋',
      bgColor: '#E6F0FF',
      textColor: '#0047BB'
    },
    {
      label: 'Parágrafos',
      value: complexity.totalParagraphs,
      icon: '📄',
      bgColor: '#FFF8E1',
      textColor: '#E6A519'
    },
    {
      label: 'Pontos de Decisão',
      value: complexity.decisionPoints,
      icon: '🔀',
      bgColor: '#F3E5F5',
      textColor: '#7B1FA2'
    },
    {
      label: 'Chamadas Externas',
      value: complexity.externalCalls,
      icon: '📞',
      bgColor: '#E8F5E9',
      textColor: '#1e7e34'
    },
    {
      label: 'Operações SQL',
      value: complexity.sqlStatements,
      icon: '🗄️',
      bgColor: '#E3F2FD',
      textColor: '#1565C0'
    },
    {
      label: 'Operações I/O',
      value: complexity.fileOperations,
      icon: '📁',
      bgColor: '#FCE4EC',
      textColor: '#C2185B'
    },
  ];

  return (
    <Card title="Métricas de Complexidade">
      <div className="mb-6">
        <div className="flex items-center justify-between mb-3">
          <span className="text-base font-bold" style={{ color: '#000' }}>
            Complexidade Ciclomática
          </span>
          <span
            className="px-4 py-1.5 rounded-full text-white text-sm font-bold shadow-md"
            style={{ backgroundColor: complexityLevel.bgColor }}
          >
            {complexityLevel.level}
          </span>
        </div>
        <div
          className="w-full rounded-full h-4 shadow-inner"
          style={{ backgroundColor: '#e2e2e2' }}
        >
          <div
            className="h-4 rounded-full transition-all duration-300"
            style={{
              width: `${Math.min((complexity.cyclomaticComplexity / 200) * 100, 100)}%`,
              backgroundColor: complexityLevel.progressColor
            }}
          />
        </div>
        <p className="text-2xl font-black mt-3" style={{ color: '#000' }}>
          {complexity.cyclomaticComplexity} pontos
        </p>
      </div>

      <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
        {metrics.map((metric, index) => (
          <div
            key={index}
            className="p-4 rounded-lg shadow-sm hover:shadow-md transition-shadow"
            style={{
              backgroundColor: metric.bgColor,
              border: '1px solid #e2e2e2'
            }}
          >
            <div className="flex items-center gap-2 mb-2">
              <span className="text-2xl">{metric.icon}</span>
              <p className="text-xs font-semibold" style={{ color: '#666' }}>
                {metric.label}
              </p>
            </div>
            <p className="text-2xl font-black" style={{ color: metric.textColor }}>
              {metric.value}
            </p>
          </div>
        ))}
      </div>
    </Card>
  );
};
