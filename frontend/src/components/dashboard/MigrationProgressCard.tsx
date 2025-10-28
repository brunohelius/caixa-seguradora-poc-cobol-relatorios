import Card from '../common/Card';
import { Progress } from '../ui/progress';
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
            <span className="text-xl font-bold text-black">
              Progresso Geral
            </span>
            <span className="text-4xl font-black text-caixa-blue">
              {completionPercentage.toFixed(1)}%
            </span>
          </div>
          <Progress
            value={completionPercentage}
            gradient
            variant="default"
          />
          <p className="text-base mt-3 font-medium text-site-text">
            {migrationProgress.tasksCompleted} de {migrationProgress.totalTasks} tarefas concluídas
          </p>
        </div>

        {/* Validation Progress */}
        <div>
          <div className="flex items-center justify-between mb-3">
            <span className="text-xl font-bold text-black">
              Validação Byte-a-Byte
            </span>
            <span className="text-4xl font-black text-success">
              {validationPercentage.toFixed(1)}%
            </span>
          </div>
          <Progress
            value={validationPercentage}
            gradient
            variant="success"
          />
          <p className="text-base mt-3 font-medium text-site-text">
            {validationPercentage > 0
              ? `Saída compatível com COBOL em ${validationPercentage}% dos casos de teste`
              : 'Validação ainda não iniciada'}
          </p>
        </div>

        {/* Status Cards */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="p-6 rounded-xl shadow-lg bg-gradient-to-br from-caixa-blue to-caixa-blue-dark text-white">
            <div className="flex items-center gap-3 mb-3">
              <div className="w-4 h-4 rounded-full animate-pulse bg-caixa-yellow" />
              <p className="text-sm font-semibold uppercase tracking-wide">Status</p>
            </div>
            <p className="text-3xl font-black">
              {migrationProgress.status}
            </p>
          </div>

          <div className="p-6 rounded-xl shadow-lg bg-gradient-to-br from-site-blue to-[#5a9cb8] text-white">
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
        <div className="p-6 rounded-xl shadow-lg bg-gradient-to-br from-site-text to-caixa-gray-900 text-white">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm font-medium mb-1 text-gray-300">Última Atualização</p>
              <p className="text-xl font-bold">
                {formatDate(migrationProgress.lastUpdated)}
              </p>
            </div>
            <div className="text-right">
              <p className="text-sm font-medium mb-1 text-gray-300">Tarefas Restantes</p>
              <p className="text-4xl font-black text-caixa-yellow">
                {migrationProgress.totalTasks - migrationProgress.tasksCompleted}
              </p>
            </div>
          </div>
        </div>

        {/* Milestone Indicators */}
        <div className="space-y-3">
          <p className="text-lg font-bold text-black">Marcos Principais (8 Fases)</p>
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

              let bgClass = 'bg-site-grayDark';
              let textClass = 'text-gray-600';

              if (isCompleted) {
                bgClass = 'bg-gradient-to-br from-success to-green-700';
                textClass = 'text-white';
              } else if (isCurrent) {
                bgClass = 'bg-gradient-to-br from-caixa-yellow to-caixa-yellow-dark border-[3px] border-caixa-blue';
                textClass = 'text-black';
              }

              return (
                <div
                  key={milestone.name}
                  className={`p-4 rounded-xl text-center font-bold shadow-md ${bgClass} ${textClass}`}
                >
                  <div className="text-2xl mb-1">{milestone.icon}</div>
                  <div className="text-xs font-bold">{milestone.name}</div>
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
              <div className="p-6 rounded-xl shadow-lg bg-gradient-to-br from-success to-green-700 text-white">
                <p className="text-sm font-semibold mb-3 uppercase tracking-wide">Build Status</p>
                <div className="flex items-center gap-2">
                  <span className="text-3xl">✓</span>
                  <span className="text-2xl font-black">{migrationProgress.buildStatus}</span>
                </div>
              </div>
            )}

            {migrationProgress.testsCreated && (
              <div className="p-6 rounded-xl shadow-lg bg-gradient-to-br from-caixa-blue to-caixa-blue-dark text-white">
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
              <div className="p-6 rounded-xl shadow-lg bg-gradient-to-br from-site-blue to-[#5a9cb8] text-white">
                <p className="text-sm font-semibold mb-3 uppercase tracking-wide">Cobertura de Código</p>
                <p className="text-5xl font-black mb-2">
                  {migrationProgress.codeCoveragePercentage.toFixed(0)}%
                </p>
                <div className="relative w-full rounded-full h-3 bg-white/30 overflow-hidden">
                  <div
                    className="absolute inset-y-0 left-0 rounded-full bg-caixa-yellow transition-all duration-300"
                    style={{ width: `${migrationProgress.codeCoveragePercentage}%` }}
                  />
                </div>
              </div>
            )}
          </div>
        )}

        {/* Production Readiness */}
        {migrationProgress.productionReadinessPercentage !== undefined && (
          <div className="p-8 rounded-2xl shadow-2xl bg-gradient-to-r from-caixa-yellow to-caixa-yellow-dark">
            <div className="flex items-center justify-between mb-4">
              <div className="flex items-center gap-3">
                <span className="text-5xl">🎯</span>
                <h4 className="text-2xl font-black text-black">
                  Prontidão para Produção
                </h4>
              </div>
              <span className="text-6xl font-black text-caixa-blue">
                {migrationProgress.productionReadinessPercentage.toFixed(0)}%
              </span>
            </div>
            <div className="relative w-full bg-white rounded-full h-8 shadow-inner overflow-hidden">
              <div
                className="absolute inset-y-0 left-0 rounded-full transition-all duration-500 flex items-center justify-end pr-4 bg-gradient-to-r from-caixa-blue to-caixa-blue-dark"
                style={{
                  width: `${migrationProgress.productionReadinessPercentage}%`
                }}
              >
                {migrationProgress.productionReadinessPercentage > 15 && (
                  <span className="text-white text-sm font-black tracking-wider">PRONTO</span>
                )}
              </div>
            </div>
            <div className="mt-4 grid grid-cols-2 md:grid-cols-4 gap-2 text-sm font-bold text-black">
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