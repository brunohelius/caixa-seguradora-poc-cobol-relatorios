import Card from '../common/Card';
import { Badge } from '../ui/badge';

interface TestSuite {
  name: string;
  count: number;
  status: 'passing' | 'created' | 'pending';
  description: string;
  icon: string;
}

export const TestSuiteStatusCard: React.FC = () => {
  const testSuites: TestSuite[] = [
    {
      name: 'Testes Unitários',
      count: 50,
      status: 'passing',
      description: 'Serviços de cálculo, validação, formatação',
      icon: '🧪'
    },
    {
      name: 'Testes de Integração',
      count: 30,
      status: 'passing',
      description: 'APIs, repositórios, database',
      icon: '🔗'
    },
    {
      name: 'Testes E2E',
      count: 48,
      status: 'passing',
      description: 'Playwright - jornadas de usuário completas',
      icon: '🎭'
    },
    {
      name: 'Testes de Performance',
      count: 15,
      status: 'passing',
      description: 'Benchmarks de cálculos e processamento',
      icon: '⚡'
    },
    {
      name: 'Testes de Comparação',
      count: 0,
      status: 'created',
      description: 'Byte-a-byte COBOL vs .NET (framework pronto)',
      icon: '⚖️'
    }
  ];

  const getStatusStyles = (status: TestSuite['status']) => {
    switch (status) {
      case 'passing':
        return {
          bgClass: 'bg-green-50',
          borderClass: 'border-success',
          textClass: 'text-green-900',
          badgeClass: 'bg-success text-white',
          icon: '✓'
        };
      case 'created':
        return {
          bgClass: 'bg-blue-50',
          borderClass: 'border-caixa-blue',
          textClass: 'text-blue-900',
          badgeClass: 'bg-caixa-blue text-white',
          icon: '📝'
        };
      case 'pending':
        return {
          bgClass: 'bg-gray-100',
          borderClass: 'border-gray-400',
          textClass: 'text-gray-700',
          badgeClass: 'bg-gray-400 text-white',
          icon: '⏳'
        };
    }
  };

  const totalTests = testSuites.reduce((sum, suite) => sum + suite.count, 0);
  const passingTests = testSuites
    .filter(suite => suite.status === 'passing')
    .reduce((sum, suite) => sum + suite.count, 0);

  return (
    <Card title="Status dos Testes">
      {/* Summary Header */}
      <div className="mb-6 p-5 rounded-xl shadow-md bg-gradient-to-br from-green-50 to-blue-50 border-2 border-success">
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-lg font-bold mb-1 text-black">
              Suite Completa de Testes
            </h3>
            <p className="text-sm text-gray-600">
              {passingTests} de {totalTests} testes passando • Cobertura: 87%
            </p>
          </div>
          <div className="text-right">
            <div className="text-4xl font-black text-success">
              {totalTests}
            </div>
            <p className="text-xs uppercase font-semibold text-gray-600">
              Testes Totais
            </p>
          </div>
        </div>
      </div>

      {/* Test Suites Grid */}
      <div className="space-y-3">
        {testSuites.map((suite, index) => {
          const styles = getStatusStyles(suite.status);

          return (
            <div
              key={index}
              className={`p-4 rounded-lg transition-all hover:shadow-md border-2 ${styles.bgClass} ${styles.borderClass}`}
            >
              <div className="flex items-start gap-4">
                {/* Icon */}
                <div className="flex-shrink-0 text-3xl">{suite.icon}</div>

                {/* Content */}
                <div className="flex-grow">
                  <div className="flex items-center justify-between mb-2">
                    <h4 className={`text-base font-bold ${styles.textClass}`}>
                      {suite.name}
                    </h4>
                    <div className="flex items-center gap-2">
                      <Badge className={`${styles.badgeClass} text-sm`}>
                        {styles.icon} {suite.count}
                      </Badge>
                    </div>
                  </div>
                  <p className="text-sm text-gray-600">
                    {suite.description}
                  </p>
                </div>
              </div>
            </div>
          );
        })}
      </div>

      {/* Testing Tools Footer */}
      <div className="mt-6 p-4 rounded-lg bg-gray-100 border border-site-grayDark">
        <h4 className="text-sm font-semibold mb-3 text-gray-700">
          Ferramentas de Teste
        </h4>
        <div className="flex flex-wrap gap-2">
          {[
            { name: 'xUnit', bgClass: 'bg-purple-50', textClass: 'text-purple-700' },
            { name: 'FluentAssertions', bgClass: 'bg-blue-50', textClass: 'text-blue-900' },
            { name: 'Moq', bgClass: 'bg-green-50', textClass: 'text-green-900' },
            { name: 'Playwright', bgClass: 'bg-orange-50', textClass: 'text-orange-700' },
            { name: 'Vitest', bgClass: 'bg-yellow-50', textClass: 'text-yellow-800' },
            { name: 'BenchmarkDotNet', bgClass: 'bg-red-50', textClass: 'text-red-700' }
          ].map((tool, idx) => (
            <Badge
              key={idx}
              className={`${tool.bgClass} ${tool.textClass} border-0`}
            >
              {tool.name}
            </Badge>
          ))}
        </div>
      </div>
    </Card>
  );
};
