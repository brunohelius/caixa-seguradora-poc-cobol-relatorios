import Card from '../common/Card';
import type { MigrationProgressDto } from '../../services/types';

interface MigrationProgressCardProps {
  migrationProgress: MigrationProgressDto;
}

export const MigrationProgressCard: React.FC<MigrationProgressCardProps> = ({ migrationProgress }) => {
  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('pt-BR', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  const completionPercentage = migrationProgress.completionPercentage;
  const validationPercentage = migrationProgress.validationMatchPercentage;

  return (
    <Card title="Progresso da Migração">
      <div className="space-y-6">
        {/* Overall Progress */}
        <div>
          <div className="flex items-center justify-between mb-3">
            <span className="text-xl font-bold" style={{ color: '#000' }}>
              Progresso Geral
            </span>
            <span className="text-4xl font-black" style={{ color: '#0047BB' }}>
              {completionPercentage.toFixed(1)}%
            </span>
          </div>
          <div className="w-full rounded-full h-6" style={{ backgroundColor: '#e2e2e2' }}>
            <div
              className="h-6 rounded-full transition-all duration-500"
              style={{
                width: `${completionPercentage}%`,
                background: 'linear-gradient(to right, #0047BB, #003380)',
              }}
            />
          </div>
          <p className="text-base mt-3 font-medium" style={{ color: '#333' }}>
            {migrationProgress.tasksCompleted} de {migrationProgress.totalTasks} tarefas concluídas
          </p>
        </div>

        {/* Validation Progress */}
        <div>
          <div className="flex items-center justify-between mb-3">
            <span className="text-xl font-bold" style={{ color: '#000' }}>
              Validação Byte-a-Byte
            </span>
            <span className="text-4xl font-black" style={{ color: '#28A745' }}>
              {validationPercentage.toFixed(1)}%
            </span>
          </div>
          <div className="w-full rounded-full h-6" style={{ backgroundColor: '#e2e2e2' }}>
            <div
              className="h-6 rounded-full transition-all duration-500"
              style={{
                width: `${validationPercentage}%`,
                background: 'linear-gradient(to right, #28A745, #1e7e34)',
              }}
            />
          </div>
          <p className="text-base mt-3 font-medium" style={{ color: '#333' }}>
            {validationPercentage > 0
              ? `Saída compatível com COBOL em ${validationPercentage}% dos casos de teste`
              : 'Validação ainda não iniciada'}
          </p>
        </div>

        {/* Status Cards */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="p-6 rounded-xl shadow-lg" style={{ background: 'linear-gradient(135deg, #0047BB 0%, #003380 100%)', color: '#fff' }}>
            <div className="flex items-center gap-3 mb-3">
              <div className="w-4 h-4 rounded-full animate-pulse" style={{ backgroundColor: '#FFB81C' }} />
              <p className="text-sm font-semibold uppercase tracking-wide">Status</p>
            </div>
            <p className="text-3xl font-black">
              {migrationProgress.status}
            </p>
          </div>

          <div className="p-6 rounded-xl shadow-lg" style={{ background: 'linear-gradient(135deg, #7ac0da 0%, #5a9cb8 100%)', color: '#fff' }}>
            <div className="flex items-center gap-3 mb-3">
              <span className="text-3xl">✨</span>
              <p className="text-sm font-semibold uppercase tracking-wide">Fase Atual</p>
            </div>
            <p className="text-xl font-bold">
              {migrationProgress.currentPhase}
            </p>
          </div>
        </div>

        {/* Timeline */}
        <div className="p-6 rounded-xl shadow-lg" style={{ background: 'linear-gradient(135deg, #333 0%, #1a1a1a 100%)', color: '#fff' }}>
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm font-medium mb-1" style={{ color: '#ccc' }}>Última Atualização</p>
              <p className="text-xl font-bold">
                {formatDate(migrationProgress.lastUpdated)}
              </p>
            </div>
            <div className="text-right">
              <p className="text-sm font-medium mb-1" style={{ color: '#ccc' }}>Tarefas Restantes</p>
              <p className="text-4xl font-black" style={{ color: '#FFB81C' }}>
                {migrationProgress.totalTasks - migrationProgress.tasksCompleted}
              </p>
            </div>
          </div>
        </div>

        {/* Milestone Indicators */}
        <div className="space-y-3">
          <p className="text-lg font-bold" style={{ color: '#000' }}>Marcos Principais (8 Fases)</p>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
            {[
              { name: 'Setup', progress: 12.5, icon: '🔧' },
              { name: 'Foundation', progress: 25, icon: '🏗️' },
              { name: 'US1', progress: 37.5, icon: '📊' },
              { name: 'US2', progress: 50, icon: '📄' },
              { name: 'US3', progress: 62.5, icon: '🔍' },
              { name: 'US4', progress: 75, icon: '⚙️' },
              { name: 'US5', progress: 87.5, icon: '📦' },
              { name: 'Polish', progress: 100, icon: '✨' }
            ].map((milestone) => {
              const isCompleted = completionPercentage >= milestone.progress;
              const isCurrent = completionPercentage >= (milestone.progress - 12.5) && completionPercentage < milestone.progress;

              let bgStyle = {};
              let textColor = '#666';

              if (isCompleted) {
                bgStyle = { background: 'linear-gradient(135deg, #28A745 0%, #1e7e34 100%)' };
                textColor = '#fff';
              } else if (isCurrent) {
                bgStyle = { background: 'linear-gradient(135deg, #FFB81C 0%, #E6A519 100%)', border: '3px solid #0047BB' };
                textColor = '#000';
              } else {
                bgStyle = { backgroundColor: '#e2e2e2' };
                textColor = '#666';
              }

              return (
                <div
                  key={milestone.name}
                  className="p-4 rounded-xl text-center font-bold shadow-md"
                  style={bgStyle}
                >
                  <div className="text-2xl mb-1">{milestone.icon}</div>
                  <div className="text-xs font-bold" style={{ color: textColor }}>{milestone.name}</div>
                  {isCompleted && <div className="text-xl mt-1">✓</div>}
                  {isCurrent && <div className="text-xl mt-1 animate-pulse">▶</div>}
                </div>
              );
            })}
          </div>
        </div>

        {/* Build & Test Status */}
        {(migrationProgress.buildStatus || migrationProgress.testsCreated) && (
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {migrationProgress.buildStatus && (
              <div className="p-6 rounded-xl shadow-lg" style={{ background: 'linear-gradient(135deg, #28A745 0%, #1e7e34 100%)', color: '#fff' }}>
                <p className="text-sm font-semibold mb-3 uppercase tracking-wide">Build Status</p>
                <div className="flex items-center gap-2">
                  <span className="text-3xl">✓</span>
                  <span className="text-2xl font-black">{migrationProgress.buildStatus}</span>
                </div>
              </div>
            )}

            {migrationProgress.testsCreated && (
              <div className="p-6 rounded-xl shadow-lg" style={{ background: 'linear-gradient(135deg, #0047BB 0%, #003380 100%)', color: '#fff' }}>
                <p className="text-sm font-semibold mb-3 uppercase tracking-wide">Testes Criados</p>
                <p className="text-5xl font-black">
                  {migrationProgress.testsCreated}
                </p>
                <p className="text-xs mt-2 font-medium">
                  Unit + Integration + E2E + Performance
                </p>
              </div>
            )}

            {migrationProgress.codeCoveragePercentage !== undefined && (
              <div className="p-6 rounded-xl shadow-lg" style={{ background: 'linear-gradient(135deg, #7ac0da 0%, #5a9cb8 100%)', color: '#fff' }}>
                <p className="text-sm font-semibold mb-3 uppercase tracking-wide">Cobertura de Código</p>
                <p className="text-5xl font-black mb-2">
                  {migrationProgress.codeCoveragePercentage.toFixed(0)}%
                </p>
                <div className="w-full rounded-full h-3" style={{ backgroundColor: 'rgba(255,255,255,0.3)' }}>
                  <div
                    className="h-3 rounded-full"
                    style={{ width: `${migrationProgress.codeCoveragePercentage}%`, backgroundColor: '#FFB81C' }}
                  />
                </div>
              </div>
            )}
          </div>
        )}

        {/* Production Readiness */}
        {migrationProgress.productionReadinessPercentage !== undefined && (
          <div className="p-8 rounded-2xl shadow-2xl" style={{ background: 'linear-gradient(to right, #FFB81C 0%, #E6A519 100%)' }}>
            <div className="flex items-center justify-between mb-4">
              <div className="flex items-center gap-3">
                <span className="text-5xl">🎯</span>
                <h4 className="text-2xl font-black" style={{ color: '#000' }}>
                  Prontidão para Produção
                </h4>
              </div>
              <span className="text-6xl font-black" style={{ color: '#0047BB' }}>
                {migrationProgress.productionReadinessPercentage.toFixed(0)}%
              </span>
            </div>
            <div className="w-full bg-white rounded-full h-8 shadow-inner">
              <div
                className="h-8 rounded-full transition-all duration-500 flex items-center justify-end pr-4"
                style={{
                  width: `${migrationProgress.productionReadinessPercentage}%`,
                  background: 'linear-gradient(to right, #0047BB, #003380)',
                }}
              >
                <span className="text-white text-sm font-black tracking-wider">PRONTO</span>
              </div>
            </div>
            <div className="mt-4 grid grid-cols-2 md:grid-cols-4 gap-2 text-sm font-bold" style={{ color: '#000' }}>
              <div>✓ User Stories</div>
              <div>✓ Build OK</div>
              <div>✓ Testes Passando</div>
              <div>✓ Documentação</div>
            </div>
          </div>
        )}
      </div>
    </Card>
  );
};
