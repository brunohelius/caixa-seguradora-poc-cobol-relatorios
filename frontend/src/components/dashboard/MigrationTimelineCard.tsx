import Card from '../common/Card';

interface TimelinePhase {
  phase: string;
  week: string;
  description: string;
  status: 'completed' | 'current' | 'pending';
  icon: string;
  userStories?: string[];
}

export const MigrationTimelineCard: React.FC = () => {
  const phases: TimelinePhase[] = [
    {
      phase: 'Phase 1: Setup & Infrastructure',
      week: 'Semana 1-2',
      description: 'Configuração do projeto, estrutura base, EF Core, SQLite',
      status: 'completed',
      icon: '🔧',
      userStories: []
    },
    {
      phase: 'Phase 2: Foundational',
      week: 'Semana 2-3',
      description: 'Entidades, repositórios, configurações EF Core',
      status: 'completed',
      icon: '🏗️',
      userStories: []
    },
    {
      phase: 'Phase 3: User Story 1',
      week: 'Semana 3',
      description: 'Dashboard interativo com métricas COBOL',
      status: 'completed',
      icon: '📊',
      userStories: ['US1: Dashboard de Análise COBOL']
    },
    {
      phase: 'Phase 4: User Story 2',
      week: 'Semana 4-5',
      description: 'Geração de relatórios PREMIT/PREMCED',
      status: 'completed',
      icon: '📄',
      userStories: ['US2: Geração de Relatórios']
    },
    {
      phase: 'Phase 5: User Story 3',
      week: 'Semana 6',
      description: 'Sistema de consulta de prêmios e estatísticas',
      status: 'completed',
      icon: '🔍',
      userStories: ['US3: Consulta e Estatísticas']
    },
    {
      phase: 'Phase 6: User Story 4',
      week: 'Semana 7',
      description: 'Agendamento e execução de jobs batch',
      status: 'completed',
      icon: '⚙️',
      userStories: ['US4: Processamento Batch']
    },
    {
      phase: 'Phase 7: User Story 5',
      week: 'Semana 8',
      description: 'Carregamento e validação de dados mock',
      status: 'completed',
      icon: '💾',
      userStories: ['US5: Gerenciamento de Dados Mock']
    },
    {
      phase: 'Phase 8: Polish & Validation',
      week: 'Semana 9',
      description: 'Testes E2E, performance, documentação, validação final',
      status: 'completed',
      icon: '✨',
      userStories: []
    },
    {
      phase: 'UAT (User Acceptance Testing)',
      week: 'Semana 10-11',
      description: 'Testes de aceitação com usuários finais',
      status: 'pending',
      icon: '🧪',
      userStories: []
    },
    {
      phase: 'Implantação em Produção',
      week: 'Semana 12',
      description: 'Deploy final e go-live',
      status: 'pending',
      icon: '🚀',
      userStories: []
    }
  ];

  const getStatusStyles = (status: TimelinePhase['status']) => {
    switch (status) {
      case 'completed':
        return {
          bgColor: '#E8F5E9',
          borderColor: '#28A745',
          dotColor: '#28A745',
          textColor: '#000',
          badgeBg: '#C8E6C9',
          badgeText: '#1B5E20'
        };
      case 'current':
        return {
          bgColor: '#E3F2FD',
          borderColor: '#0047BB',
          dotColor: '#0047BB',
          textColor: '#000',
          badgeBg: '#BBDEFB',
          badgeText: '#0D47A1'
        };
      case 'pending':
        return {
          bgColor: '#F5F5F5',
          borderColor: '#BDBDBD',
          dotColor: '#9E9E9E',
          textColor: '#666',
          badgeBg: '#EEEEEE',
          badgeText: '#616161'
        };
    }
  };

  return (
    <Card title="Linha do Tempo da Migração">
      <div className="space-y-4">
        {phases.map((phase, index) => {
          const styles = getStatusStyles(phase.status);
          const isLast = index === phases.length - 1;

          return (
            <div key={index} className="relative">
              {/* Connecting Line */}
              {!isLast && (
                <div
                  className="absolute left-6 top-12 w-0.5 h-full"
                  style={{
                    backgroundColor: phase.status === 'completed' ? '#A5D6A7' : '#E0E0E0'
                  }}
                />
              )}

              {/* Phase Card */}
              <div
                className="relative flex gap-4 p-5 rounded-xl shadow-md"
                style={{
                  backgroundColor: styles.bgColor,
                  border: `2px solid ${styles.borderColor}`
                }}
              >
                {/* Icon & Dot */}
                <div className="flex-shrink-0 flex flex-col items-center">
                  <div className="text-3xl mb-2">{phase.icon}</div>
                  <div
                    className={`w-4 h-4 rounded-full ${phase.status === 'current' ? 'animate-pulse' : ''}`}
                    style={{ backgroundColor: styles.dotColor }}
                  />
                </div>

                {/* Content */}
                <div className="flex-grow">
                  <div className="flex items-start justify-between mb-2">
                    <div>
                      <div className="flex items-center gap-2 mb-1">
                        <h3 className="text-lg font-bold" style={{ color: styles.textColor }}>
                          {phase.phase}
                        </h3>
                        {phase.status === 'completed' && (
                          <span className="text-green-600 text-xl font-bold">✓</span>
                        )}
                        {phase.status === 'current' && (
                          <span
                            className="px-2 py-1 rounded text-xs font-bold"
                            style={{
                              backgroundColor: styles.badgeBg,
                              color: styles.badgeText
                            }}
                          >
                            EM ANDAMENTO
                          </span>
                        )}
                      </div>
                      <p className="text-sm" style={{ color: '#666' }}>{phase.description}</p>
                    </div>
                    <span
                      className="px-3 py-1 rounded-full text-xs font-semibold whitespace-nowrap"
                      style={{
                        backgroundColor: styles.badgeBg,
                        color: styles.badgeText
                      }}
                    >
                      {phase.week}
                    </span>
                  </div>

                  {/* User Stories */}
                  {phase.userStories && phase.userStories.length > 0 && (
                    <div className="mt-2 flex flex-wrap gap-2">
                      {phase.userStories.map((us, usIndex) => (
                        <span
                          key={usIndex}
                          className="inline-flex items-center px-3 py-1 rounded-md text-xs font-medium"
                          style={{
                            backgroundColor: '#fff',
                            border: '1px solid #BDBDBD',
                            color: '#333'
                          }}
                        >
                          {us}
                        </span>
                      ))}
                    </div>
                  )}
                </div>
              </div>
            </div>
          );
        })}
      </div>

      {/* Summary Footer */}
      <div
        className="mt-6 p-5 rounded-xl shadow-md"
        style={{
          background: 'linear-gradient(135deg, #E8F5E9 0%, #E3F2FD 100%)',
          border: '2px solid #28A745'
        }}
      >
        <div className="flex items-center justify-between">
          <div>
            <h4 className="text-base font-bold mb-1" style={{ color: '#000' }}>
              Status do Projeto
            </h4>
            <p className="text-sm" style={{ color: '#666' }}>
              8 de 10 fases completas • 2 fases pendentes (UAT e Produção)
            </p>
          </div>
          <div className="text-right">
            <p className="text-4xl font-black" style={{ color: '#28A745' }}>80%</p>
            <p className="text-xs font-medium" style={{ color: '#666' }}>do cronograma</p>
          </div>
        </div>
      </div>
    </Card>
  );
};
