import Card from '../common/Card';
import type { ProgramInfoDto } from '../../services/types';

interface ProgramInfoCardProps {
  programInfo: ProgramInfoDto;
}

export const ProgramInfoCard: React.FC<ProgramInfoCardProps> = ({ programInfo }) => {
  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('pt-BR', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric',
    });
  };

  return (
    <Card title="Informações do Programa">
      <div className="space-y-4">
        <div className="grid grid-cols-2 gap-4">
          <div>
            <p className="text-sm text-gray-600 dark:text-gray-400">Programa</p>
            <p className="text-2xl font-bold text-gray-900 dark:text-white">
              {programInfo.programName}
            </p>
          </div>
          <div>
            <p className="text-sm text-gray-600 dark:text-gray-400">Tipo</p>
            <p className="text-lg font-semibold text-gray-700 dark:text-gray-300">
              {programInfo.programType}
            </p>
          </div>
        </div>

        <div>
          <p className="text-sm text-gray-600 dark:text-gray-400">Descrição</p>
          <p className="text-base text-gray-800 dark:text-gray-200">
            {programInfo.description}
          </p>
        </div>

        <div>
          <p className="text-sm text-gray-600 dark:text-gray-400 mb-2">Arquivos de Saída</p>
          <div className="flex flex-wrap gap-2">
            {programInfo.outputFiles.map((file, index) => (
              <span
                key={index}
                className="px-3 py-1 bg-blue-100 dark:bg-blue-900 text-blue-800 dark:text-blue-200 rounded-full text-sm font-medium"
              >
                {file}
              </span>
            ))}
          </div>
        </div>

        <div className="grid grid-cols-2 gap-4 pt-4 border-t border-gray-200 dark:border-gray-700">
          <div>
            <p className="text-sm text-gray-600 dark:text-gray-400">Linhas de Código</p>
            <p className="text-xl font-bold text-gray-900 dark:text-white">
              {programInfo.totalLinesOfCode.toLocaleString('pt-BR')}
            </p>
          </div>
          <div>
            <p className="text-sm text-gray-600 dark:text-gray-400">Última Análise</p>
            <p className="text-sm font-medium text-gray-700 dark:text-gray-300">
              {formatDate(programInfo.lastAnalyzed)}
            </p>
          </div>
        </div>
      </div>
    </Card>
  );
};
