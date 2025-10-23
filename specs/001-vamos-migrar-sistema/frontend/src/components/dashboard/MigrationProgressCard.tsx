import Card from '../common/Card';
import type { MigrationProgressDto } from '../../services/types';

interface MigrationProgressCardProps {
  migrationProgress: MigrationProgressDto;
}

export const MigrationProgressCard: React.FC<MigrationProgressCardProps> = ({ migrationProgress }) => {
  const getStatusColor = (status: string) => {
    switch (status) {
      case 'Complete': return 'bg-green-500';
      case 'In Progress': return 'bg-blue-500';
      case 'Testing': return 'bg-yellow-500';
      case 'Not Started': return 'bg-gray-500';
      default: return 'bg-gray-500';
    }
  };

  const getPhaseIcon = (phase: string) => {
    if (phase.includes('Setup')) return '🔧';
    if (phase.includes('Foundation')) return '🏗️';
    if (phase.includes('Dashboard')) return '📊';
    if (phase.includes('Report')) return '📄';
    if (phase.includes('Query')) return '🔍';
    if (phase.includes('Batch')) return '⚙️';
    if (phase.includes('Testing')) return '🧪';
    return '🚀';
  };

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
          <div className="flex items-center justify-between mb-2">
            <span className="text-lg font-semibold text-gray-700 dark:text-gray-300">
              Progresso Geral
            </span>
            <span className="text-3xl font-bold text-blue-600 dark:text-blue-400">
              {completionPercentage.toFixed(1)}%
            </span>
          </div>
          <div className="w-full bg-gray-200 dark:bg-gray-700 rounded-full h-4">
            <div
              className="bg-gradient-to-r from-blue-500 to-blue-600 h-4 rounded-full transition-all duration-500"
              style={{ width: `${completionPercentage}%` }}
            />
          </div>
          <p className="text-sm text-gray-600 dark:text-gray-400 mt-2">
            {migrationProgress.tasksCompleted} de {migrationProgress.totalTasks} tarefas concluídas
          </p>
        </div>

        {/* Validation Progress */}
        <div>
          <div className="flex items-center justify-between mb-2">
            <span className="text-lg font-semibold text-gray-700 dark:text-gray-300">
              Validação Byte-a-Byte
            </span>
            <span className="text-3xl font-bold text-green-600 dark:text-green-400">
              {validationPercentage.toFixed(1)}%
            </span>
          </div>
          <div className="w-full bg-gray-200 dark:bg-gray-700 rounded-full h-4">
            <div
              className="bg-gradient-to-r from-green-500 to-green-600 h-4 rounded-full transition-all duration-500"
              style={{ width: `${validationPercentage}%` }}
            />
          </div>
          <p className="text-sm text-gray-600 dark:text-gray-400 mt-2">
            {validationPercentage > 0
              ? `Saída compatível com COBOL em ${validationPercentage}% dos casos de teste`
              : 'Validação ainda não iniciada'}
          </p>
        </div>

        {/* Status Cards */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="p-4 bg-gradient-to-br from-blue-50 to-blue-100 dark:from-blue-900/20 dark:to-blue-800/20 rounded-lg border border-blue-200 dark:border-blue-800">
            <div className="flex items-center gap-3 mb-2">
              <div className={`w-3 h-3 rounded-full ${getStatusColor(migrationProgress.status)}`} />
              <p className="text-sm text-blue-600 dark:text-blue-400 font-medium">Status</p>
            </div>
            <p className="text-2xl font-bold text-blue-900 dark:text-blue-100">
              {migrationProgress.status}
            </p>
          </div>

          <div className="p-4 bg-gradient-to-br from-purple-50 to-purple-100 dark:from-purple-900/20 dark:to-purple-800/20 rounded-lg border border-purple-200 dark:border-purple-800">
            <div className="flex items-center gap-3 mb-2">
              <span className="text-2xl">{getPhaseIcon(migrationProgress.currentPhase)}</span>
              <p className="text-sm text-purple-600 dark:text-purple-400 font-medium">Fase Atual</p>
            </div>
            <p className="text-lg font-bold text-purple-900 dark:text-purple-100">
              {migrationProgress.currentPhase}
            </p>
          </div>
        </div>

        {/* Timeline */}
        <div className="p-4 bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm text-gray-600 dark:text-gray-400">Última Atualização</p>
              <p className="text-base font-semibold text-gray-900 dark:text-white">
                {formatDate(migrationProgress.lastUpdated)}
              </p>
            </div>
            <div className="text-right">
              <p className="text-sm text-gray-600 dark:text-gray-400">Tarefas Restantes</p>
              <p className="text-2xl font-bold text-orange-600 dark:text-orange-400">
                {migrationProgress.totalTasks - migrationProgress.tasksCompleted}
              </p>
            </div>
          </div>
        </div>

        {/* Milestone Indicators */}
        <div className="space-y-2">
          <p className="text-sm font-semibold text-gray-700 dark:text-gray-300">Marcos Principais</p>
          <div className="grid grid-cols-5 gap-2">
            {['Setup', 'Foundation', 'Dashboard', 'Reports', 'Complete'].map((milestone, index) => {
              const milestoneProgress = ((index + 1) / 5) * 100;
              const isCompleted = completionPercentage >= milestoneProgress;
              const isCurrent = completionPercentage >= (index / 5) * 100 && completionPercentage < milestoneProgress;

              return (
                <div
                  key={milestone}
                  className={`p-2 rounded text-center text-xs font-medium transition-all ${
                    isCompleted
                      ? 'bg-green-100 dark:bg-green-900 text-green-800 dark:text-green-200'
                      : isCurrent
                      ? 'bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200 ring-2 ring-blue-500'
                      : 'bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-400'
                  }`}
                >
                  {isCompleted ? '✓' : isCurrent ? '▶' : '○'} {milestone}
                </div>
              );
            })}
          </div>
        </div>
      </div>
    </Card>
  );
};
