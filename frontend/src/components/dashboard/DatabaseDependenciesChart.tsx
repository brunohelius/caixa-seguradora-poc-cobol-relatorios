import Card from '../common/Card';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, PieChart, Pie, Cell } from 'recharts';
import type { DatabaseDependenciesDto } from '../../services/types';

interface DatabaseDependenciesChartProps {
  dependencies: DatabaseDependenciesDto;
}

export const DatabaseDependenciesChart: React.FC<DatabaseDependenciesChartProps> = ({ dependencies }) => {
  // Prepare data for bar chart - top 10 tables by estimated row count
  const tableData = dependencies.tables
    .sort((a, b) => b.estimatedRowCount - a.estimatedRowCount)
    .slice(0, 10)
    .map(table => ({
      name: table.name,
      rows: table.estimatedRowCount,
      type: table.type,
    }));

  // Prepare data for pie chart - SQL operations distribution
  const sqlData = [
    { name: 'SELECT', value: dependencies.sqlStats.selectCount, color: '#3B82F6' },
    { name: 'INSERT', value: dependencies.sqlStats.insertCount, color: '#10B981' },
    { name: 'UPDATE', value: dependencies.sqlStats.updateCount, color: '#F59E0B' },
    { name: 'DELETE', value: dependencies.sqlStats.deleteCount, color: '#EF4444' },
  ].filter(item => item.value > 0);

  return (
    <Card title="Dependências de Banco de Dados">
      <div className="space-y-6">
        {/* Summary Stats */}
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          <div className="p-4 bg-blue-50 dark:bg-blue-900/20 rounded-lg">
            <p className="text-sm text-blue-600 dark:text-blue-400 mb-1">Total de Tabelas</p>
            <p className="text-3xl font-bold text-blue-700 dark:text-blue-300">
              {dependencies.totalTables}
            </p>
          </div>
          <div className="p-4 bg-green-50 dark:bg-green-900/20 rounded-lg">
            <p className="text-sm text-green-600 dark:text-green-400 mb-1">Cursores</p>
            <p className="text-3xl font-bold text-green-700 dark:text-green-300">
              {dependencies.totalCursors}
            </p>
          </div>
          <div className="p-4 bg-purple-50 dark:bg-purple-900/20 rounded-lg">
            <p className="text-sm text-purple-600 dark:text-purple-400 mb-1">Operações SQL</p>
            <p className="text-3xl font-bold text-purple-700 dark:text-purple-300">
              {dependencies.sqlStats.totalOperations}
            </p>
          </div>
          <div className="p-4 bg-orange-50 dark:bg-orange-900/20 rounded-lg">
            <p className="text-sm text-orange-600 dark:text-orange-400 mb-1">Somente Leitura</p>
            <p className="text-3xl font-bold text-orange-700 dark:text-orange-300">
              {dependencies.sqlStats.readOnlyPercentage.toFixed(1)}%
            </p>
          </div>
        </div>

        {/* Top Tables by Row Count */}
        <div>
          <h4 className="text-md font-semibold text-gray-700 dark:text-gray-300 mb-3">
            Top 10 Tabelas por Volume de Registros
          </h4>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={tableData}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />
              <XAxis
                dataKey="name"
                angle={-45}
                textAnchor="end"
                height={100}
                tick={{ fontSize: 12 }}
              />
              <YAxis
                tickFormatter={(value) => value.toLocaleString('pt-BR')}
                tick={{ fontSize: 12 }}
              />
              <Tooltip
                formatter={(value: number) => value.toLocaleString('pt-BR') + ' registros'}
                labelStyle={{ color: '#1f2937' }}
              />
              <Bar dataKey="rows" fill="#3B82F6" name="Registros Estimados" />
            </BarChart>
          </ResponsiveContainer>
        </div>

        {/* SQL Operations Distribution */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <h4 className="text-md font-semibold text-gray-700 dark:text-gray-300 mb-3">
              Distribuição de Operações SQL
            </h4>
            <ResponsiveContainer width="100%" height={200}>
              <PieChart>
                <Pie
                  data={sqlData}
                  cx="50%"
                  cy="50%"
                  labelLine={false}
                  label={({ name, percent }: any) => `${name}: ${((percent as number) * 100).toFixed(0)}%`}
                  outerRadius={80}
                  fill="#8884d8"
                  dataKey="value"
                >
                  {sqlData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.color} />
                  ))}
                </Pie>
                <Tooltip />
              </PieChart>
            </ResponsiveContainer>
          </div>

          {/* Cursor Information */}
          <div>
            <h4 className="text-md font-semibold text-gray-700 dark:text-gray-300 mb-3">
              Cursores Declarados ({dependencies.cursors.length})
            </h4>
            <div className="space-y-2 max-h-[200px] overflow-y-auto">
              {dependencies.cursors.map((cursor, index) => (
                <div
                  key={index}
                  className="p-3 bg-gray-50 dark:bg-gray-800 rounded border border-gray-200 dark:border-gray-700"
                >
                  <p className="text-sm font-semibold text-gray-900 dark:text-white">
                    {cursor.cursorName}
                  </p>
                  <p className="text-xs text-gray-600 dark:text-gray-400">
                    Tabela: {cursor.targetTable}
                  </p>
                  <p className="text-xs text-gray-500 dark:text-gray-500">
                    ~{cursor.estimatedRecordsProcessed.toLocaleString('pt-BR')} registros
                  </p>
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </Card>
  );
};
