import Card from '../common/Card';

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
          bgColor: '#E8F5E9',
          borderColor: '#28A745',
          textColor: '#1B5E20',
          badgeBg: '#28A745',
          badgeText: '#fff',
          icon: '✓'
        };
      case 'created':
        return {
          bgColor: '#E3F2FD',
          borderColor: '#0047BB',
          textColor: '#0D47A1',
          badgeBg: '#0047BB',
          badgeText: '#fff',
          icon: '📝'
        };
      case 'pending':
        return {
          bgColor: '#F5F5F5',
          borderColor: '#BDBDBD',
          textColor: '#616161',
          badgeBg: '#9E9E9E',
          badgeText: '#fff',
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
      <div
        className="mb-6 p-5 rounded-xl shadow-md"
        style={{
          background: 'linear-gradient(135deg, #E8F5E9 0%, #E3F2FD 100%)',
          border: '2px solid #28A745'
        }}
      >
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-lg font-bold mb-1" style={{ color: '#000' }}>
              Suite Completa de Testes
            </h3>
            <p className="text-sm" style={{ color: '#666' }}>
              {passingTests} de {totalTests} testes passando • Cobertura: 87%
            </p>
          </div>
          <div className="text-right">
            <div className="text-4xl font-black" style={{ color: '#28A745' }}>
              {totalTests}
            </div>
            <p className="text-xs uppercase font-semibold" style={{ color: '#666' }}>
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
              className="p-4 rounded-lg transition-all hover:shadow-md"
              style={{
                backgroundColor: styles.bgColor,
                border: `2px solid ${styles.borderColor}`
              }}
            >
              <div className="flex items-start gap-4">
                {/* Icon */}
                <div className="flex-shrink-0 text-3xl">{suite.icon}</div>

                {/* Content */}
                <div className="flex-grow">
                  <div className="flex items-center justify-between mb-2">
                    <h4 className="text-base font-bold" style={{ color: styles.textColor }}>
                      {suite.name}
                    </h4>
                    <div className="flex items-center gap-2">
                      <span
                        className="px-3 py-1 rounded-full text-sm font-bold"
                        style={{
                          backgroundColor: styles.badgeBg,
                          color: styles.badgeText
                        }}
                      >
                        {styles.icon} {suite.count}
                      </span>
                    </div>
                  </div>
                  <p className="text-sm" style={{ color: '#666' }}>
                    {suite.description}
                  </p>
                </div>
              </div>
            </div>
          );
        })}
      </div>

      {/* Testing Tools Footer */}
      <div
        className="mt-6 p-4 rounded-lg"
        style={{
          backgroundColor: '#F5F5F5',
          border: '1px solid #e2e2e2'
        }}
      >
        <h4 className="text-sm font-semibold mb-3" style={{ color: '#333' }}>
          Ferramentas de Teste
        </h4>
        <div className="flex flex-wrap gap-2">
          {[
            { name: 'xUnit', bgColor: '#F3E5F5', textColor: '#6A1B9A' },
            { name: 'FluentAssertions', bgColor: '#E3F2FD', textColor: '#0D47A1' },
            { name: 'Moq', bgColor: '#E8F5E9', textColor: '#1B5E20' },
            { name: 'Playwright', bgColor: '#FFF3E0', textColor: '#E65100' },
            { name: 'Vitest', bgColor: '#FFF8E1', textColor: '#F57F17' },
            { name: 'BenchmarkDotNet', bgColor: '#FFEBEE', textColor: '#C62828' }
          ].map((tool, idx) => (
            <span
              key={idx}
              className="px-3 py-1 rounded-md text-xs font-medium"
              style={{
                backgroundColor: tool.bgColor,
                color: tool.textColor
              }}
            >
              {tool.name}
            </span>
          ))}
        </div>
      </div>
    </Card>
  );
};
