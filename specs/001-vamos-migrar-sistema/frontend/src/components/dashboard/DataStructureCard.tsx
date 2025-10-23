import Card from '../common/Card';
import type { DataStructureMetricsDto } from '../../services/types';

interface DataStructureCardProps {
  dataStructure: DataStructureMetricsDto;
}

export const DataStructureCard: React.FC<DataStructureCardProps> = ({ dataStructure }) => {
  const metrics = [
    { label: 'Data Items Total', value: dataStructure.totalDataItems, color: 'text-blue-600 dark:text-blue-400' },
    { label: 'Working Storage', value: dataStructure.workingStorageSections, color: 'text-green-600 dark:text-green-400' },
    { label: 'File Sections', value: dataStructure.fileSections, color: 'text-purple-600 dark:text-purple-400' },
    { label: 'Linkage Sections', value: dataStructure.linkageSections, color: 'text-orange-600 dark:text-orange-400' },
    { label: 'Tabelas DB', value: dataStructure.databaseTables, color: 'text-red-600 dark:text-red-400' },
    { label: 'Cursores', value: dataStructure.cursorDeclarations, color: 'text-indigo-600 dark:text-indigo-400' },
  ];

  return (
    <Card title="Estrutura de Dados">
      <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
        {metrics.map((metric, index) => (
          <div
            key={index}
            className="p-4 bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700"
          >
            <p className="text-sm text-gray-600 dark:text-gray-400 mb-1">{metric.label}</p>
            <p className={`text-3xl font-bold ${metric.color}`}>
              {metric.value.toLocaleString('pt-BR')}
            </p>
          </div>
        ))}
      </div>

      <div className="mt-6 p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg border border-blue-200 dark:border-blue-800">
        <p className="text-sm text-blue-800 dark:text-blue-200">
          <strong>Total de {dataStructure.totalDataItems} data items</strong> mapeados do COBOL,
          distribuídos em {dataStructure.workingStorageSections} seções de Working Storage.
          O programa acessa <strong>{dataStructure.databaseTables} tabelas/views</strong> através de{' '}
          {dataStructure.cursorDeclarations} cursores.
        </p>
      </div>
    </Card>
  );
};
