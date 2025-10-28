import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Layout from './components/Layout';
import { ErrorBoundary } from './components/ErrorBoundary';
import { DashboardPage } from './pages/DashboardPage';
import ReportGenerationPage from './pages/ReportGenerationPage';
import ReportGenerationPageV2 from './pages/ReportGenerationPageV2';
import QueryPage from './pages/QueryPage';
import PoliciesQueryPage from './pages/PoliciesQueryPage';
import ProductsQueryPage from './pages/ProductsQueryPage';
import ClientsQueryPage from './pages/ClientsQueryPage';
import BatchJobsPage from './pages/BatchJobsPage';
import MockDataPage from './pages/MockDataPage';

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
    <ErrorBoundary>
      <Router>
        <Layout>
          <Routes>
            <Route path="/" element={<DashboardPage />} />
            <Route path="/reports" element={<ReportGenerationPage />} />
            <Route path="/reports-v2" element={<ReportGenerationPageV2 />} />
            <Route path="/query" element={<QueryPage />} />
            <Route path="/query/policies" element={<PoliciesQueryPage />} />
            <Route path="/query/products" element={<ProductsQueryPage />} />
            <Route path="/query/clients" element={<ClientsQueryPage />} />
            <Route path="/batch-jobs" element={<BatchJobsPage />} />
            <Route path="/data-management" element={<MockDataPage />} />
            <Route path="/mock-data" element={<MockDataPage />} />
            <Route path="*" element={<NotFoundPage />} />
          </Routes>
        </Layout>
      </Router>
    </ErrorBoundary>
  );
}

export default App;
