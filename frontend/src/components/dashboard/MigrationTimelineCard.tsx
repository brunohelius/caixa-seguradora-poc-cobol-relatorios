import Card from '../common/Card';
import { Badge } from '../ui/badge';

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
          bgClass: 'bg-green-50',
          borderClass: 'border-success',
          dotClass: 'bg-success',
          textClass: 'text-black',
          badgeClass: 'bg-green-200 text-green-900'
        };
      case 'current':
        return {
          bgClass: 'bg-blue-50',
          borderClass: 'border-caixa-blue',
          dotClass: 'bg-caixa-blue',
          textClass: 'text-black',
          badgeClass: 'bg-blue-200 text-blue-900'
        };
      case 'pending':
        return {
          bgClass: 'bg-gray-100',
          borderClass: 'border-gray-400',
          dotClass: 'bg-gray-400',
          textClass: 'text-gray-600',
          badgeClass: 'bg-gray-200 text-gray-700'
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
                  className={`absolute left-6 top-12 w-0.5 h-full ${
                    phase.status === 'completed' ? 'bg-green-300' : 'bg-gray-300'
                  }`}
                />
              )}

              {/* Phase Card */}
              <div
                className={`relative flex gap-4 p-5 rounded-xl shadow-md border-2 ${styles.bgClass} ${styles.borderClass}`}
              >
                {/* Icon & Dot */}
                <div className="flex-shrink-0 flex flex-col items-center">
                  <div className="text-3xl mb-2">{phase.icon}</div>
                  <div
                    className={`w-4 h-4 rounded-full ${styles.dotClass} ${
                      phase.status === 'current' ? 'animate-pulse' : ''
                    }`}
                  />
                </div>

                {/* Content */}
                <div className="flex-grow">
                  <div className="flex items-start justify-between mb-2">
                    <div>
                      <div className="flex items-center gap-2 mb-1">
                        <h3 className={`text-lg font-bold ${styles.textClass}`}>
                          {phase.phase}
                        </h3>
                        {phase.status === 'completed' && (
                          <span className="text-green-600 text-xl font-bold">✓</span>
                        )}
                        {phase.status === 'current' && (
                          <Badge className={`${styles.badgeClass}`}>
                            EM ANDAMENTO
                          </Badge>
                        )}
                      </div>
                      <p className="text-sm text-gray-600">{phase.description}</p>
                    </div>
                    <Badge className={`${styles.badgeClass} whitespace-nowrap`}>
                      {phase.week}
                    </Badge>
                  </div>

                  {/* User Stories */}
                  {phase.userStories && phase.userStories.length > 0 && (
                    <div className="mt-2 flex flex-wrap gap-2">
                      {phase.userStories.map((us, usIndex) => (
                        <Badge
                          key={usIndex}
                          variant="outline"
                          className="bg-white border-gray-400 text-gray-700"
                        >
                          {us}
                        </Badge>
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
      <div className="mt-6 p-5 rounded-xl shadow-md bg-gradient-to-br from-green-50 to-blue-50 border-2 border-success">
        <div className="flex items-center justify-between">
          <div>
            <h4 className="text-base font-bold mb-1 text-black">
              Status do Projeto
            </h4>
            <p className="text-sm text-gray-600">
              8 de 10 fases completas • 2 fases pendentes (UAT e Produção)
            </p>
          </div>
          <div className="text-right">
            <p className="text-4xl font-black text-success">80%</p>
            <p className="text-xs font-medium text-gray-600">do cronograma</p>
          </div>
        </div>
      </div>
    </Card>
  );
};
