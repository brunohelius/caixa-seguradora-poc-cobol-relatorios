import Card from '../common/Card';
import { Badge } from '../ui/badge';
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
      <div className="space-y-6">
        {/* Program Name and Type */}
        <div className="grid grid-cols-2 gap-4">
          <div>
            <p className="text-sm font-semibold uppercase tracking-wide mb-2 text-gray-600">
              Programa:
            </p>
            <p className="text-3xl font-black text-caixa-blue">
              {programInfo.programName}
            </p>
          </div>
          <div>
            <p className="text-sm font-semibold uppercase tracking-wide mb-2 text-gray-600">
              Tipo:
            </p>
            <p className="text-2xl font-bold text-black">
              {programInfo.programType}
            </p>
          </div>
        </div>

        {/* Description */}
        <div className="p-5 rounded-xl shadow-md bg-caixa-blue-light border-2 border-caixa-blue">
          <p className="text-sm font-bold uppercase tracking-wide mb-3 text-caixa-blue">
            Descrição:
          </p>
          <p className="text-base leading-relaxed text-black">
            {programInfo.description}
          </p>
        </div>

        {/* Output Files */}
        <div>
          <p className="text-sm font-semibold uppercase tracking-wide mb-3 text-gray-600">
            Arquivos de Saída:
          </p>
          <div className="flex flex-wrap gap-3">
            {programInfo.outputFiles.map((file, index) => (
              <Badge
                key={index}
                className="px-4 py-2 font-bold rounded-lg shadow-md text-sm bg-gradient-to-br from-caixa-blue to-caixa-blue-dark text-white hover:from-caixa-blue-dark hover:to-caixa-blue"
              >
                {file}
              </Badge>
            ))}
          </div>
        </div>

        {/* Statistics */}
        <div className="grid grid-cols-2 gap-4 pt-4 border-t-2 border-site-grayDark">
          <div className="p-5 rounded-xl shadow-md bg-purple-50 border-2 border-purple-700">
            <p className="text-sm font-bold uppercase tracking-wide mb-2 text-purple-700">
              Linhas de Código:
            </p>
            <p className="text-3xl font-black text-purple-900">
              {programInfo.totalLinesOfCode.toLocaleString('pt-BR')}
            </p>
          </div>
          <div className="p-5 rounded-xl shadow-md bg-green-50 border-2 border-success">
            <p className="text-sm font-bold uppercase tracking-wide mb-2 text-green-700">
              Última Análise:
            </p>
            <p className="text-xl font-bold text-green-900">
              {formatDate(programInfo.lastAnalyzed)}
            </p>
          </div>
        </div>
      </div>
    </Card>
  );
};