import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Layout from './components/Layout';
import { DashboardPage } from './pages/DashboardPage';
import './styles/globals.css';

// Placeholder pages (will be implemented in Phase 4-7)

const ReportGenerationPage = () => (
  <div className="text-center py-12">
    <h1 className="text-3xl font-bold text-gray-900 mb-4">Geração de Relatórios</h1>
    <p className="text-gray-600">Página de geração de relatórios será implementada na Fase 4</p>
  </div>
);

const QueryPage = () => (
  <div className="text-center py-12">
    <h1 className="text-3xl font-bold text-gray-900 mb-4">Consulta de Dados</h1>
    <p className="text-gray-600">Página de consulta será implementada na Fase 5</p>
  </div>
);

const BatchJobsPage = () => (
  <div className="text-center py-12">
    <h1 className="text-3xl font-bold text-gray-900 mb-4">Jobs Agendados</h1>
    <p className="text-gray-600">Página de jobs será implementada na Fase 6</p>
  </div>
);

const DataManagementPage = () => (
  <div className="text-center py-12">
    <h1 className="text-3xl font-bold text-gray-900 mb-4">Gerenciamento de Dados</h1>
    <p className="text-gray-600">Página de gerenciamento de dados será implementada na Fase 7</p>
  </div>
);

const NotFoundPage = () => (
  <div className="text-center py-12">
    <h1 className="text-6xl font-bold text-gray-900 mb-4">404</h1>
    <h2 className="text-2xl font-semibold text-gray-700 mb-4">Página não encontrada</h2>
    <p className="text-gray-600 mb-6">A página que você está procurando não existe.</p>
    <a href="/" className="text-caixa-blue hover:text-caixa-blue-dark font-medium">
      Voltar para o Dashboard
    </a>
  </div>
);

function App() {
  return (
    <Router>
      <Layout>
        <Routes>
          <Route path="/" element={<DashboardPage />} />
          <Route path="/reports" element={<ReportGenerationPage />} />
          <Route path="/query" element={<QueryPage />} />
          <Route path="/batch-jobs" element={<BatchJobsPage />} />
          <Route path="/data-management" element={<DataManagementPage />} />
          <Route path="*" element={<NotFoundPage />} />
        </Routes>
      </Layout>
    </Router>
  );
}

export default App;
