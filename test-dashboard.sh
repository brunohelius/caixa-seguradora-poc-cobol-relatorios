#!/bin/bash

echo "==================================="
echo "Dashboard Diagnostic Script"
echo "==================================="
echo ""

# Change to project root
cd "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol"

echo "1. Verificando estrutura de diretórios..."
echo "Backend existe:" && [ -d "backend" ] && echo "✅ Sim" || echo "❌ Não"
echo "Frontend existe:" && [ -d "frontend" ] && echo "✅ Sim" || echo "❌ Não"
echo ""

echo "2. Verificando processos nas portas..."
BACKEND_PID=$(lsof -ti:5000,5001 2>/dev/null | head -1)
if [ -n "$BACKEND_PID" ]; then
    echo "✅ Backend rodando (PID: $BACKEND_PID)"
else
    echo "❌ Backend NÃO está rodando"
fi

FRONTEND_PID=$(lsof -ti:5173 2>/dev/null | head -1)
if [ -n "$FRONTEND_PID" ]; then
    echo "✅ Frontend rodando (PID: $FRONTEND_PID)"
else
    echo "❌ Frontend NÃO está rodando"
fi
echo ""

echo "3. Testando endpoints do backend..."
if [ -n "$BACKEND_PID" ]; then
    echo "Testando /api/Dashboard/health..."
    curl -s -m 3 http://localhost:5000/api/Dashboard/health 2>/dev/null | jq '.' || \
    curl -s -m 3 http://localhost:5001/api/Dashboard/health 2>/dev/null | jq '.'

    echo ""
    echo "Testando /api/Dashboard/metrics..."
    curl -s -m 3 http://localhost:5000/api/Dashboard/metrics 2>/dev/null | jq '.programInfo.programName' || \
    curl -s -m 3 http://localhost:5001/api/Dashboard/metrics 2>/dev/null | jq '.programInfo.programName'
else
    echo "⚠️  Backend não está rodando. Iniciando..."
    cd backend/src/CaixaSeguradora.Api
    nohup dotnet run --urls "http://localhost:5000" > /tmp/backend-root.log 2>&1 &
    echo "Backend iniciado. Aguarde 10 segundos..."
    sleep 10
    echo "Teste novamente executando: curl http://localhost:5000/api/Dashboard/health"
fi
echo ""

echo "4. Verificando correções aplicadas..."
echo "Program.cs tem ReferenceHandler.IgnoreCycles?"
grep -q "ReferenceHandler.IgnoreCycles" backend/src/CaixaSeguradora.Api/Program.cs && echo "✅ Sim" || echo "❌ Não"

echo "Policy.cs tem CossuredPolicies navigation?"
grep -q "ICollection<CossuredPolicy> CossuredPolicies" backend/src/CaixaSeguradora.Core/Entities/Policy.cs && echo "✅ Sim" || echo "❌ Não"

echo "dashboardService.ts usa /api/Dashboard/ (maiúscula)?"
grep -q "/api/Dashboard/metrics" frontend/src/services/dashboardService.ts && echo "✅ Sim" || echo "❌ Não"

echo ""
echo "==================================="
echo "Diagnóstico completo!"
echo "==================================="
