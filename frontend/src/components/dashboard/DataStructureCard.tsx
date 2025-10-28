import Card from '../common/Card';
import type { DataStructureMetricsDto } from '../../services/types';

interface DataStructureCardProps {
  dataStructure: DataStructureMetricsDto;
}

export const DataStructureCard: React.FC<DataStructureCardProps> = ({ dataStructure }) => {
  const metrics = [
    {
      label: 'Data Items Total',
      value: dataStructure.totalDataItems,
      bgColor: '#E6F0FF',
      textColor: '#0047BB',
      borderColor: '#0047BB'
    },
    {
      label: 'Working Storage',
      value: dataStructure.workingStorageSections,
      bgColor: '#E8F5E9',
      textColor: '#1e7e34',
      borderColor: '#28A745'
    },
    {
      label: 'File Sections',
      value: dataStructure.fileSections,
      bgColor: '#F3E5F5',
      textColor: '#6A1B9A',
      borderColor: '#7B1FA2'
    },
    {
      label: 'Linkage Sections',
      value: dataStructure.linkageSections,
      bgColor: '#FFF3E0',
      textColor: '#E65100',
      borderColor: '#F57C00'
    },
    {
      label: 'Tabelas DB',
      value: dataStructure.databaseTables,
      bgColor: '#FFEBEE',
      textColor: '#C62828',
      borderColor: '#DC3545'
    },
    {
      label: 'Cursores',
      value: dataStructure.cursorDeclarations,
      bgColor: '#E8EAF6',
      textColor: '#283593',
      borderColor: '#3949AB'
    },
  ];

  return (
    <Card title="Estrutura de Dados">
      <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
        {metrics.map((metric, index) => (
          <div
            key={index}
            className="p-4 rounded-lg shadow-sm"
            style={{
              backgroundColor: metric.bgColor,
              borderLeft: `4px solid ${metric.borderColor}`
            }}
          >
            <p className="text-sm font-semibold mb-2" style={{ color: '#666' }}>
              {metric.label}
            </p>
            <p className="text-3xl font-black" style={{ color: metric.textColor }}>
              {metric.value.toLocaleString('pt-BR')}
            </p>
          </div>
        ))}
      </div>

      <div className="mt-6 p-5 rounded-xl shadow-md" style={{
        backgroundColor: '#E6F0FF',
        border: '2px solid #0047BB'
      }}>
        <p className="text-base leading-relaxed" style={{ color: '#000' }}>
          <strong style={{ color: '#0047BB' }}>
            Total de {dataStructure.totalDataItems} data items
          </strong>{' '}
          mapeados do COBOL, distribuídos em{' '}
          <strong style={{ color: '#0047BB' }}>
            {dataStructure.workingStorageSections} seções de Working Storage
          </strong>.
          O programa acessa{' '}
          <strong style={{ color: '#0047BB' }}>
            {dataStructure.databaseTables} tabelas/views
          </strong>{' '}
          através de {dataStructure.cursorDeclarations} cursores.
        </p>
      </div>
    </Card>
  );
};
