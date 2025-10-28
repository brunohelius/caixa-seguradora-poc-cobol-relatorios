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
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#E3F2FD', border: '2px solid #0047BB' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#1565C0' }}>Total de Tabelas</p>
            <p className="text-3xl font-black" style={{ color: '#0D47A1' }}>
              {dependencies.totalTables}
            </p>
          </div>
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#E8F5E9', border: '2px solid #28A745' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#2E7D32' }}>Cursores</p>
            <p className="text-3xl font-black" style={{ color: '#1B5E20' }}>
              {dependencies.totalCursors}
            </p>
          </div>
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#F3E5F5', border: '2px solid #7B1FA2' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#7B1FA2' }}>Operações SQL</p>
            <p className="text-3xl font-black" style={{ color: '#4A148C' }}>
              {dependencies.sqlStats.totalOperations}
            </p>
          </div>
          <div className="p-4 rounded-lg" style={{ backgroundColor: '#FFF3E0', border: '2px solid #FF9800' }}>
            <p className="text-sm font-semibold mb-1" style={{ color: '#E65100' }}>Somente Leitura</p>
            <p className="text-3xl font-black" style={{ color: '#BF360C' }}>
              {dependencies.sqlStats.readOnlyPercentage.toFixed(1)}%
            </p>
          </div>
        </div>

        {/* Top Tables by Row Count */}
        <div>
          <h4 className="text-md font-semibold mb-3" style={{ color: '#333' }}>
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
            <h4 className="text-md font-semibold mb-3" style={{ color: '#333' }}>
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
            <h4 className="text-md font-semibold mb-3" style={{ color: '#333' }}>
              Cursores Declarados ({dependencies.cursors.length})
            </h4>
            <div className="space-y-2 max-h-[200px] overflow-y-auto">
              {dependencies.cursors.map((cursor, index) => (
                <div
                  key={index}
                  className="p-3 rounded"
                  style={{
                    backgroundColor: '#F5F5F5',
                    border: '1px solid #e2e2e2'
                  }}
                >
                  <p className="text-sm font-semibold" style={{ color: '#000' }}>
                    {cursor.cursorName}
                  </p>
                  <p className="text-xs" style={{ color: '#666' }}>
                    Tabela: {cursor.targetTable}
                  </p>
                  <p className="text-xs" style={{ color: '#999' }}>
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
