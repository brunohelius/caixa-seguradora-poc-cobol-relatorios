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
      <div className="space-y-6">
        {/* Program Name and Type */}
        <div className="grid grid-cols-2 gap-4">
          <div>
            <p className="text-sm font-semibold uppercase tracking-wide mb-2" style={{ color: '#666' }}>
              Programa:
            </p>
            <p className="text-3xl font-black" style={{ color: '#0047BB' }}>
              {programInfo.programName}
            </p>
          </div>
          <div>
            <p className="text-sm font-semibold uppercase tracking-wide mb-2" style={{ color: '#666' }}>
              Tipo:
            </p>
            <p className="text-2xl font-bold" style={{ color: '#000' }}>
              {programInfo.programType}
            </p>
          </div>
        </div>

        {/* Description */}
        <div
          className="p-5 rounded-xl shadow-md"
          style={{
            backgroundColor: '#E6F0FF',
            border: '2px solid #0047BB'
          }}
        >
          <p className="text-sm font-bold uppercase tracking-wide mb-3" style={{ color: '#0047BB' }}>
            Descrição:
          </p>
          <p className="text-base leading-relaxed" style={{ color: '#000' }}>
            {programInfo.description}
          </p>
        </div>

        {/* Output Files */}
        <div>
          <p className="text-sm font-semibold uppercase tracking-wide mb-3" style={{ color: '#666' }}>
            Arquivos de Saída:
          </p>
          <div className="flex flex-wrap gap-3">
            {programInfo.outputFiles.map((file, index) => (
              <span
                key={index}
                className="px-4 py-2 font-bold rounded-lg shadow-md text-sm"
                style={{
                  background: 'linear-gradient(135deg, #0047BB 0%, #003380 100%)',
                  color: '#fff'
                }}
              >
                {file}
              </span>
            ))}
          </div>
        </div>

        {/* Statistics */}
        <div className="grid grid-cols-2 gap-4 pt-4" style={{ borderTop: '2px solid #e2e2e2' }}>
          <div
            className="p-5 rounded-xl shadow-md"
            style={{
              backgroundColor: '#F3E5F5',
              border: '2px solid #7B1FA2'
            }}
          >
            <p className="text-sm font-bold uppercase tracking-wide mb-2" style={{ color: '#6A1B9A' }}>
              Linhas de Código:
            </p>
            <p className="text-3xl font-black" style={{ color: '#4A148C' }}>
              {programInfo.totalLinesOfCode.toLocaleString('pt-BR')}
            </p>
          </div>
          <div
            className="p-5 rounded-xl shadow-md"
            style={{
              backgroundColor: '#E8F5E9',
              border: '2px solid #28A745'
            }}
          >
            <p className="text-sm font-bold uppercase tracking-wide mb-2" style={{ color: '#1e7e34' }}>
              Última Análise:
            </p>
            <p className="text-xl font-bold" style={{ color: '#155724' }}>
              {formatDate(programInfo.lastAnalyzed)}
            </p>
          </div>
        </div>
      </div>
    </Card>
  );
};
