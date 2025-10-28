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
      bgClass: 'bg-caixa-blue-light',
      textClass: 'text-caixa-blue',
      borderClass: 'border-l-4 border-caixa-blue'
    },
    {
      label: 'Working Storage',
      value: dataStructure.workingStorageSections,
      bgClass: 'bg-green-50',
      textClass: 'text-green-700',
      borderClass: 'border-l-4 border-success'
    },
    {
      label: 'File Sections',
      value: dataStructure.fileSections,
      bgClass: 'bg-purple-50',
      textClass: 'text-purple-700',
      borderClass: 'border-l-4 border-purple-700'
    },
    {
      label: 'Linkage Sections',
      value: dataStructure.linkageSections,
      bgClass: 'bg-orange-50',
      textClass: 'text-orange-700',
      borderClass: 'border-l-4 border-orange-600'
    },
    {
      label: 'Tabelas DB',
      value: dataStructure.databaseTables,
      bgClass: 'bg-red-50',
      textClass: 'text-red-700',
      borderClass: 'border-l-4 border-error'
    },
    {
      label: 'Cursores',
      value: dataStructure.cursorDeclarations,
      bgClass: 'bg-indigo-50',
      textClass: 'text-indigo-700',
      borderClass: 'border-l-4 border-indigo-700'
    },
  ];

  return (
    <Card title="Estrutura de Dados">
      <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
        {metrics.map((metric, index) => (
          <div
            key={index}
            className={`p-4 rounded-lg shadow-sm ${metric.bgClass} ${metric.borderClass}`}
          >
            <p className="text-sm font-semibold mb-2 text-gray-600">
              {metric.label}
            </p>
            <p className={`text-3xl font-black ${metric.textClass}`}>
              {metric.value.toLocaleString('pt-BR')}
            </p>
          </div>
        ))}
      </div>

      <div className="mt-6 p-5 rounded-xl shadow-md bg-caixa-blue-light border-2 border-caixa-blue">
        <p className="text-base leading-relaxed text-black">
          <strong className="text-caixa-blue">
            Total de {dataStructure.totalDataItems} data items
          </strong>{' '}
          mapeados do COBOL, distribuídos em{' '}
          <strong className="text-caixa-blue">
            {dataStructure.workingStorageSections} seções de Working Storage
          </strong>.
          O programa acessa{' '}
          <strong className="text-caixa-blue">
            {dataStructure.databaseTables} tabelas/views
          </strong>{' '}
          através de {dataStructure.cursorDeclarations} cursores.
        </p>
      </div>
    </Card>
  );
};
