# 🚀 Como Rodar o Projeto

## Inicio Rápido

### Opção 1: Scripts Automatizados (Recomendado)

#### Terminal 1 - Backend:
```bash
./start-backend.sh
```

#### Terminal 2 - Frontend:
```bash
./start-frontend.sh
```

### Opção 2: Manual

#### Terminal 1 - Backend:
```bash
cd backend/src/CaixaSeguradora.Api
dotnet run --launch-profile http
```

#### Terminal 2 - Frontend:
```bash
cd frontend
npm run dev
```

## 📡 Endereços

- **Backend API**: http://localhost:5555
- **Swagger UI**: http://localhost:5555/swagger
- **Frontend Dashboard**: http://localhost:5173

## ✅ Verificar Status

### Backend:
```bash
curl http://localhost:5555/api/v1/health
```

### Frontend:
Abra o navegador em: http://localhost:5173

## 🎯 Features Disponíveis

1. **Dashboard** (http://localhost:5173) - Métricas do projeto (95% completo)
2. **Geração de Relatórios** - PREMIT/PREMCED
3. **Consulta de Dados** - Query interativo
4. **Batch Jobs** - Agendamento de tarefas
5. **Gerenciamento de Dados** - Upload e validação

## 🔧 Troubleshooting

### Backend não inicia
```bash
# Limpar porta 5000 se estiver em uso
lsof -ti:5000 | xargs kill -9

# Ou usar porta alternativa editando launchSettings.json
```

### Frontend não conecta ao backend
- Verifique se o backend está rodando em http://localhost:5555
- O frontend está configurado para usar essa porta automaticamente

### Porta 5000 em uso (macOS)
- O Control Center do macOS usa a porta 5000
- Por isso usamos a porta 5555 para o backend

## 📚 Documentação Completa

- **API**: http://localhost:5555/swagger
- **Documentação**: `/docs/` folder
- **Operations Manual**: `/docs/operations.md`
- **UAT Plan**: `/docs/uat-plan.md`
