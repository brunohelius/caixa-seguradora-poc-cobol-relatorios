# Manual de Operações - Sistema de Relatórios SUSEP

**Versão**: 1.0
**Data**: Outubro 2025
**Público-Alvo**: Equipe de Operações, SRE, Suporte N2/N3

## Sumário

- [Visão Geral](#visão-geral)
- [Monitoramento](#monitoramento)
- [Manutenção Preventiva](#manutenção-preventiva)
- [Resolução de Problemas](#resolução-de-problemas)
- [Procedimentos Operacionais](#procedimentos-operacionais)
- [Escalação de Incidentes](#escalação-de-incidentes)
- [Backup e Recuperação](#backup-e-recuperação)
- [Performance Tuning](#performance-tuning)
- [Runbooks](#runbooks)

---

## Visão Geral

### Arquitetura do Sistema

```
┌─────────────┐     HTTPS      ┌──────────────┐
│   Usuário   │ ───────────────>│   Ingress    │
└─────────────┘                 │   (nginx)    │
                                └──────┬───────┘
                                       │
                         ┌─────────────┴──────────────┐
                         │                            │
                    ┌────▼─────┐             ┌────────▼────┐
                    │ Frontend │             │   Backend   │
                    │  React   │             │  .NET API   │
                    └──────────┘             └──────┬──────┘
                                                    │
                                             ┌──────▼──────┐
                                             │   DB2 (RO)  │
                                             │  26+ Views  │
                                             └─────────────┘
```

### Componentes Críticos

1. **Backend API** (.NET 9 Web API)
   - Responsável por: Lógica de negócios, geração de relatórios, processamento de dados
   - SLA: 99.5% uptime
   - RTO: 15 minutos
   - RPO: N/A (read-only)

2. **Frontend SPA** (React 18)
   - Responsável por: Interface do usuário, visualizações, interações
   - SLA: 99.9% uptime
   - Pode operar em modo degradado se backend estiver indisponível

3. **Banco de Dados** (DB2 Read-Only)
   - Responsável por: Fonte de dados de apólices, prêmios, clientes
   - SLA: 99.95% uptime (gerenciado por equipe de BD)
   - Acesso: Somente leitura (SELECT)

---

## Monitoramento

### Métricas-Chave

#### Disponibilidade

| Métrica | Alerta | Criticidade | Ação |
|---------|--------|-------------|------|
| Backend Health Check | < 95% success rate (5min) | CRÍTICA | Reiniciar pods, verificar logs |
| Frontend HTTP 200 Rate | < 98% (5min) | ALTA | Verificar nginx, cache |
| DB2 Connection Pool | > 90% uso | ALTA | Investigar queries lentas |

#### Performance

| Métrica | Threshold Warning | Threshold Critical | Ação |
|---------|-------------------|-------------------|------|
| API Response Time (p95) | > 2s | > 5s | Escalar pods, otimizar queries |
| Report Generation Time | > 5min | > 10min | Verificar DB, aumentar recursos |
| Memory Usage (Backend) | > 3GB | > 3.8GB (de 4GB) | Escalar, investigar memory leak |
| CPU Usage (Backend) | > 70% | > 90% | Escalar pods |

#### Erros

| Métrica | Threshold | Ação |
|---------|-----------|------|
| Error Rate (5xx) | > 1% | Verificar logs, rollback se necessário |
| DB2 Connection Errors | > 5 em 1min | Verificar conectividade, credenciais |
| Timeout Errors | > 10 em 5min | Aumentar timeouts, verificar DB |

### Dashboards

**Grafana - Dashboard Principal**
- URL: `https://grafana.caixa.local/d/caixa-relatorios-overview`
- Painéis:
  - Request Rate & Error Rate
  - Response Time (p50, p95, p99)
  - Active Users
  - Database Query Duration
  - Pod CPU & Memory
  - Report Generation Queue Length

**Kibana - Logs**
- URL: `https://kibana.caixa.local/app/discover#/caixa-relatorios`
- Índice: `caixa-relatorios-*`
- Filtros salvos:
  - Erros últimas 24h
  - Queries lentas (> 1s)
  - Geração de relatórios

### Alertas

**PagerDuty Escalation:**
1. Alerta CRÍTICO → Plantão N2 (imediato)
2. Sem resposta em 10min → Gestor de Plantão
3. Sem resposta em 20min → Gerente de TI

**Canais de Notificação:**
- Slack: `#caixa-relatorios-alerts`
- Email: `plantao-ti@caixaseguradora.com.br`
- SMS: Para incidentes P1

---

## Manutenção Preventiva

### Diariamente

**Automático:**
- [ ] Backup de configurações (02:00 AM)
- [ ] Rotação de logs (03:00 AM)
- [ ] Health check de todos os pods (a cada 30s)
- [ ] Limpeza de arquivos temporários (04:00 AM)

**Manual:**
- [ ] Revisar dashboard de métricas (09:00 AM)
- [ ] Verificar alertas das últimas 24h
- [ ] Revisar logs de erro

### Semanalmente

- [ ] Analisar tendências de performance (Segundas, 10:00 AM)
- [ ] Revisar capacidade e uso de recursos
- [ ] Verificar atualizações de segurança disponíveis
- [ ] Testar procedimento de rollback em HML

### Mensalmente

- [ ] Atualização de dependências (.NET, npm packages)
- [ ] Revisão de políticas de retenção de logs
- [ ] Teste de recuperação de disaster recovery
- [ ] Auditoria de acesso e permissões
- [ ] Relatório de disponibilidade para stakeholders

### Trimestralmente

- [ ] Atualização de certificados SSL (se necessário)
- [ ] Revisão e atualização de runbooks
- [ ] Treinamento da equipe em novos procedimentos
- [ ] Teste de carga e stress
- [ ] Revisão de SLA e ajuste de thresholds de alerta

---

## Resolução de Problemas

### Fluxograma de Diagnóstico

```
┌─────────────────┐
│  Incidente      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Frontend ou     │◄─── Frontend: Verificar nginx, cache
│ Backend?        │
└────────┬────────┘
         │
         │ Backend
         ▼
┌─────────────────┐
│ Logs mostram    │◄─── Sim: Verificar DB2, testar query
│ erro de DB?     │      Não: Continuar
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Performance     │◄─── Sim: Escalar pods, verificar memória
│ degradada?      │      Não: Continuar
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Erro 5xx?       │◄─── Sim: Verificar exception logs, rollback
│                 │      Não: Escalar para N3
└─────────────────┘
```

### Problemas Comuns

#### 1. Backend Retorna 503 Service Unavailable

**Sintomas:**
- Todos os requests retornam 503
- Health check falha
- Pods em estado `CrashLoopBackOff`

**Diagnóstico:**
```bash
# Verificar status dos pods
kubectl get pods -n caixa-relatorios

# Ver logs do pod que está crashando
kubectl logs <pod-name> -n caixa-relatorios --tail=100

# Verificar eventos
kubectl describe pod <pod-name> -n caixa-relatorios
```

**Causas Comuns:**
- Falha de conexão com DB2
- Configuração incorreta (ConfigMap/Secret)
- OOM (Out of Memory)
- Imagem corrompida

**Resolução:**
1. Se OOM: Aumentar memory limit
   ```bash
   kubectl edit deployment caixa-relatorios-backend -n caixa-relatorios
   # Aumentar resources.limits.memory para 6Gi
   ```

2. Se erro de DB:
   ```bash
   # Testar conexão DB2
   kubectl exec -it <pod-name> -n caixa-relatorios -- /bin/bash
   dotnet CaixaSeguradora.Api.dll --test-db-connection
   ```

3. Se configuração:
   ```bash
   # Verificar ConfigMap
   kubectl get configmap caixa-relatorios-config -n caixa-relatorios -o yaml

   # Verificar Secret
   kubectl get secret caixa-relatorios-secrets -n caixa-relatorios -o yaml
   ```

4. Rollback se necessário:
   ```bash
   kubectl rollout undo deployment/caixa-relatorios-backend -n caixa-relatorios
   ```

#### 2. Performance Degradada - Timeout em Relatórios

**Sintomas:**
- Geração de relatórios > 10 minutos
- Timeouts em requisições `/api/reports/generate`
- CPU em 100% nos pods backend

**Diagnóstico:**
```bash
# Verificar uso de recursos
kubectl top pods -n caixa-relatorios

# Verificar HPA
kubectl get hpa -n caixa-relatorios

# Ver queries lentas no Kibana
# Filtro: duration > 1000 AND component: "database"
```

**Resolução:**
1. Escalar pods imediatamente:
   ```bash
   kubectl scale deployment caixa-relatorios-backend --replicas=10 -n caixa-relatorios
   ```

2. Verificar query plan no DB2 (se aplicável):
   ```sql
   EXPLAIN PLAN FOR
   SELECT * FROM V0PREMIOS WHERE ...;
   ```

3. Aumentar timeout temporariamente (se necessário):
   ```bash
   kubectl set env deployment/caixa-relatorios-backend REQUEST_TIMEOUT=600 -n caixa-relatorios
   ```

4. Investigar causa raiz:
   - Volume de dados aumentou?
   - DB2 está lento (verificar com DBA)?
   - Índices faltando?

#### 3. Frontend - Página em Branco / Erro 404

**Sintomas:**
- Usuários veem página em branco
- Console do navegador mostra erro 404 para `/assets/*`
- nginx retorna 404

**Diagnóstico:**
```bash
# Verificar nginx config
kubectl exec -it <frontend-pod> -n caixa-relatorios -- cat /etc/nginx/nginx.conf

# Testar nginx
kubectl exec -it <frontend-pod> -n caixa-relatorios -- nginx -t

# Verificar arquivos estáticos
kubectl exec -it <frontend-pod> -n caixa-relatorios -- ls -la /usr/share/nginx/html/
```

**Resolução:**
1. Reiniciar nginx:
   ```bash
   kubectl rollout restart deployment/caixa-relatorios-frontend -n caixa-relatorios
   ```

2. Verificar build do frontend:
   ```bash
   # Localmente
   cd frontend
   npm run build
   ls -la dist/
   ```

3. Se problema persiste, reconstruir imagem:
   ```bash
   docker build -t caixa-relatorios-frontend:hotfix-1 -f deployment/docker/Dockerfile.frontend ./frontend
   docker push registry.caixaseguradora.com.br/caixa-relatorios-frontend:hotfix-1
   kubectl set image deployment/caixa-relatorios-frontend frontend=registry.caixaseguradora.com.br/caixa-relatorios-frontend:hotfix-1 -n caixa-relatorios
   ```

#### 4. Erro de Autenticação JWT

**Sintomas:**
- Requisições retornam 401 Unauthorized
- Usuários são deslogados frequentemente
- Logs mostram "Invalid token"

**Diagnóstico:**
```bash
# Verificar secret JWT
kubectl get secret caixa-relatorios-secrets -n caixa-relatorios -o jsonpath='{.data.jwt-secret}' | base64 -d
echo

# Verificar configuração JWT
kubectl exec -it <backend-pod> -n caixa-relatorios -- env | grep JWT
```

**Resolução:**
1. Verificar sincronização de relógio (JWT expirado):
   ```bash
   kubectl exec -it <backend-pod> -n caixa-relatorios -- date
   # Comparar com hora do servidor
   ```

2. Se secret mudou, atualizar todos os pods:
   ```bash
   kubectl rollout restart deployment/caixa-relatorios-backend -n caixa-relatorios
   ```

3. Verificar configuração de expiração:
   ```json
   {
     "JwtSettings": {
       "Secret": "***",
       "ExpirationMinutes": 480,
       "Issuer": "CaixaSeguradora",
       "Audience": "RelatoriosSUSEP"
     }
   }
   ```

#### 5. Erro de Conexão com DB2

**Sintomas:**
- Logs mostram "Connection timeout" ou "Connection refused"
- Todas as requisições retornam erro 500
- Health check falha com erro de DB

**Diagnóstico:**
```bash
# Verificar conectividade
kubectl exec -it <backend-pod> -n caixa-relatorios -- /bin/bash
ping db2prod.caixa.local
telnet db2prod.caixa.local 50000

# Verificar credenciais
kubectl get secret caixa-relatorios-secrets -n caixa-relatorios -o yaml
```

**Resolução:**
1. Verificar se DB2 está online (entrar em contato com DBA):
   ```bash
   # DBA verifica
   db2 list active databases
   ```

2. Testar credenciais manualmente:
   ```bash
   db2 connect to SUSEP user relatorios_ro using <password>
   ```

3. Verificar firewall/network policy:
   ```bash
   kubectl get networkpolicy -n caixa-relatorios
   ```

4. Se credenciais expiraram, atualizar secret:
   ```bash
   kubectl delete secret caixa-relatorios-secrets -n caixa-relatorios
   kubectl create secret generic caixa-relatorios-secrets \
     --from-literal=db-password='NovaSenha123!' \
     --from-literal=jwt-secret='JWTSecretKey123!' \
     -n caixa-relatorios
   kubectl rollout restart deployment/caixa-relatorios-backend -n caixa-relatorios
   ```

---

## Procedimentos Operacionais

### 1. Deploy de Nova Versão

**Pré-Requisitos:**
- Versão testada em HML
- Change Request aprovado
- Janela de manutenção agendada (se P1)
- Backup de configuração atual

**Passos:**
1. Notificar usuários (30min antes):
   ```bash
   # Enviar email via sistema de notificações
   ./scripts/notify-users.sh "Deploy versão 1.1.0 às 22:00"
   ```

2. Criar backup pré-deploy:
   ```bash
   ./deployment/scripts/backup.sh pre-deploy-1.1.0
   ```

3. Deploy:
   ```bash
   ./deployment/scripts/deploy.sh 1.1.0 prd
   ```

4. Verificar health checks (5 min):
   ```bash
   watch -n 5 kubectl get pods -n caixa-relatorios
   ```

5. Smoke tests:
   ```bash
   ./deployment/scripts/smoke-tests.sh prd
   ```

6. Monitorar por 30 minutos:
   - Verificar dashboard Grafana
   - Revisar logs no Kibana
   - Validar métricas de erro (deve estar < 0.1%)

7. Se falhar, rollback:
   ```bash
   kubectl rollout undo deployment/caixa-relatorios-backend -n caixa-relatorios
   kubectl rollout undo deployment/caixa-relatorios-frontend -n caixa-relatorios
   ```

8. Notificar conclusão:
   ```bash
   ./scripts/notify-users.sh "Deploy concluído com sucesso"
   ```

### 2. Escalar Sistema para Alta Demanda

**Cenário:** Período de fechamento mensal (alta carga prevista)

**Passos:**
1. Aumentar réplicas com antecedência (2h antes):
   ```bash
   kubectl scale deployment caixa-relatorios-backend --replicas=15 -n caixa-relatorios
   kubectl scale deployment caixa-relatorios-frontend --replicas=8 -n caixa-relatorios
   ```

2. Ajustar HPA temporariamente:
   ```bash
   kubectl patch hpa caixa-relatorios-backend-hpa -n caixa-relatorios -p '{"spec":{"maxReplicas":30}}'
   ```

3. Monitorar ativamente:
   - Dashboard Grafana aberto
   - Alertas configurados para 80% de threshold (ao invés de 90%)

4. Após pico, retornar ao normal:
   ```bash
   kubectl scale deployment caixa-relatorios-backend --replicas=4 -n caixa-relatorios
   kubectl patch hpa caixa-relatorios-backend-hpa -n caixa-relatorios -p '{"spec":{"maxReplicas":20}}'
   ```

### 3. Rotação de Credenciais (Secrets)

**Frequência:** A cada 90 dias

**Passos:**
1. Gerar novas credenciais:
   ```bash
   # Gerar nova senha DB2 (DBA)
   NEW_DB_PASSWORD=$(openssl rand -base64 32)

   # Gerar novo JWT secret
   NEW_JWT_SECRET=$(openssl rand -base64 64)
   ```

2. Atualizar secret em K8s:
   ```bash
   kubectl create secret generic caixa-relatorios-secrets-new \
     --from-literal=db-password="$NEW_DB_PASSWORD" \
     --from-literal=jwt-secret="$NEW_JWT_SECRET" \
     -n caixa-relatorios --dry-run=client -o yaml | kubectl apply -f -
   ```

3. Atualizar deployment para usar novo secret:
   ```bash
   kubectl set env deployment/caixa-relatorios-backend --from=secret/caixa-relatorios-secrets-new -n caixa-relatorios
   ```

4. Rolling restart:
   ```bash
   kubectl rollout restart deployment/caixa-relatorios-backend -n caixa-relatorios
   ```

5. Verificar funcionamento (30 min de monitoramento)

6. Remover secret antigo:
   ```bash
   kubectl delete secret caixa-relatorios-secrets -n caixa-relatorios
   ```

### 4. Limpeza de Logs e Armazenamento

**Frequência:** Semanal (Domingos, 03:00 AM)

**Automático (cronjob K8s):**
```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: log-cleanup
  namespace: caixa-relatorios
spec:
  schedule: "0 3 * * 0"
  jobTemplate:
    spec:
      template:
        spec:
          containers:
          - name: cleanup
            image: busybox
            command:
            - /bin/sh
            - -c
            - find /app/logs -name "*.log" -mtime +7 -delete
            volumeMounts:
            - name: logs
              mountPath: /app/logs
          volumes:
          - name: logs
            persistentVolumeClaim:
              claimName: caixa-relatorios-logs-pvc
          restartPolicy: OnFailure
```

**Manual (se necessário):**
```bash
# Listar uso de disco
kubectl exec -it <backend-pod> -n caixa-relatorios -- df -h

# Limpar logs antigos (> 7 dias)
kubectl exec -it <backend-pod> -n caixa-relatorios -- find /app/logs -name "*.log" -mtime +7 -delete

# Verificar espaço liberado
kubectl exec -it <backend-pod> -n caixa-relatorios -- du -sh /app/logs
```

---

## Escalação de Incidentes

### Níveis de Severidade

| Severidade | Descrição | Tempo de Resposta | Exemplos |
|------------|-----------|-------------------|----------|
| P1 - CRÍTICO | Sistema completamente indisponível | 15 minutos | Backend down, DB inacessível |
| P2 - ALTO | Funcionalidade crítica indisponível | 1 hora | Geração de relatórios falhando |
| P3 - MÉDIO | Degradação de performance | 4 horas | Lentidão, timeouts ocasionais |
| P4 - BAIXO | Problema menor, workaround disponível | 1 dia útil | Bug visual, erro de tradução |

### Matriz de Escalação

**Horário Comercial (9h-18h)**
1. N1 (Suporte): Usuários finais
2. N2 (Ops): Equipe de operações
3. N3 (Dev): Equipe de desenvolvimento
4. Gestor: Gerente de TI

**Fora do Horário (18h-9h, fins de semana)**
1. Plantão N2 (via PagerDuty)
2. Gestor de Plantão
3. Gerente de TI (apenas P1)

### Contatos

| Papel | Nome | Telefone | Email |
|-------|------|----------|-------|
| N2 Plantão | Plantão TI | (11) 9999-9999 | plantao-ti@caixa.com.br |
| N3 Dev Lead | [Nome] | [Tel] | dev-lead@caixa.com.br |
| DBA DB2 | Equipe BD | (11) 8888-8888 | dba@caixa.com.br |
| Gestor Plantão | [Nome] | [Tel] | gestor-plantao@caixa.com.br |
| Gerente TI | [Nome] | [Tel] | gerente-ti@caixa.com.br |

### Processo de Escalação

1. **Detecção:**
   - Alerta automático (PagerDuty)
   - Relato de usuário (Service Desk)

2. **Triagem (N1/N2):**
   - Classificar severidade
   - Reunir informações básicas
   - Verificar status do sistema
   - Aplicar workaround se disponível

3. **Investigação (N2/N3):**
   - Diagnóstico técnico
   - Análise de logs
   - Reprodução do problema
   - Identificar causa raiz

4. **Resolução:**
   - Aplicar correção
   - Verificar funcionamento
   - Monitorar por período adequado

5. **Documentação:**
   - Registrar no sistema de tickets
   - Atualizar runbook se novo problema
   - Post-mortem para P1/P2

---

## Backup e Recuperação

### Estratégia de Backup

**Dados:**
- Sistema é **read-only** (não gera dados, apenas consome)
- Não há necessidade de backup de banco de dados
- Backup necessário apenas para configurações e logs

**Configurações:**
- ConfigMaps (K8s)
- Secrets (K8s) - encriptados
- Manifests de deployment
- Certificados SSL

**Logs:**
- Logs de aplicação (retenção: 30 dias)
- Logs de acesso (retenção: 90 dias)
- Logs de auditoria (retenção: 1 ano)

### Procedimento de Backup

**Automático (diário, 02:00 AM):**
```bash
#!/bin/bash
# /opt/scripts/backup-daily.sh

DATE=$(date +%Y%m%d)
BACKUP_DIR="/backups/caixa-relatorios"
S3_BUCKET="s3://caixa-backups/relatorios-susep"

# Backup K8s resources
kubectl get all -n caixa-relatorios -o yaml > $BACKUP_DIR/k8s-resources-$DATE.yaml
kubectl get configmap -n caixa-relatorios -o yaml > $BACKUP_DIR/configmaps-$DATE.yaml
kubectl get secret -n caixa-relatorios -o yaml > $BACKUP_DIR/secrets-$DATE.yaml

# Backup certificados
cp -r /etc/letsencrypt/live/relatorios.caixaseguradora.com.br $BACKUP_DIR/certs-$DATE/

# Upload para S3
aws s3 sync $BACKUP_DIR/ $S3_BUCKET/ --sse AES256

# Limpeza local (> 7 dias)
find $BACKUP_DIR -name "*.yaml" -mtime +7 -delete

echo "Backup completed: $DATE"
```

### Procedimento de Recuperação

**Cenário: Disaster Recovery Completo**

1. Restaurar namespace:
   ```bash
   kubectl create namespace caixa-relatorios
   ```

2. Restaurar secrets:
   ```bash
   BACKUP_DATE=20251022
   kubectl apply -f /backups/caixa-relatorios/secrets-$BACKUP_DATE.yaml
   ```

3. Restaurar configmaps:
   ```bash
   kubectl apply -f /backups/caixa-relatorios/configmaps-$BACKUP_DATE.yaml
   ```

4. Restaurar deployments:
   ```bash
   kubectl apply -f /backups/caixa-relatorios/k8s-resources-$BACKUP_DATE.yaml
   ```

5. Verificar pods:
   ```bash
   kubectl get pods -n caixa-relatorios --watch
   ```

6. Restaurar certificados:
   ```bash
   cp -r /backups/caixa-relatorios/certs-$BACKUP_DATE/* /etc/letsencrypt/live/relatorios.caixaseguradora.com.br/
   ```

7. Validar funcionamento:
   ```bash
   curl -k https://relatorios.caixaseguradora.com.br/health
   ./deployment/scripts/smoke-tests.sh
   ```

**RTO Esperado:** 15-30 minutos
**RPO:** 24 horas (último backup)

---

## Performance Tuning

### Otimização do Backend

**Connection Pooling DB2:**
```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Server=db2prod;Database=SUSEP;User Id=relatorios_ro;Password=***;Pooling=true;Min Pool Size=10;Max Pool Size=100;Connection Timeout=30;Command Timeout=60;"
  }
}
```

**Garbage Collection (.NET):**
```xml
<!-- CaixaSeguradora.Api.csproj -->
<PropertyGroup>
  <ServerGarbageCollection>true</ServerGarbageCollection>
  <ConcurrentGarbageCollection>true</ConcurrentGarbageCollection>
  <RetainVMGarbageCollection>true</RetainVMGarbageCollection>
</PropertyGroup>
```

**Async Processing:**
- Geração de relatórios: Background jobs (Hangfire)
- Queries grandes: Paginação obrigatória
- Cursor processing: `IAsyncEnumerable<T>` para streaming

**Caching:**
- Metadata (produtos, agências): Cache in-memory (30 min)
- Resultados de queries frequentes: Redis (se necessário)

### Otimização do Frontend

**Code Splitting:**
```typescript
// React.lazy para rotas
const ReportGenerationPage = lazy(() => import('./pages/ReportGenerationPage'));
const QueryPage = lazy(() => import('./pages/QueryPage'));
```

**Asset Optimization:**
- Imagens: WebP format
- Fonts: Woff2
- Bundle: Vite code splitting

**CDN:**
- Assets estáticos servidos via CDN
- Cache-Control: `public, max-age=31536000, immutable`

### Otimização de Rede

**nginx Tuning:**
```nginx
# nginx.conf
worker_processes auto;
worker_connections 4096;

http {
    gzip on;
    gzip_vary on;
    gzip_min_length 1024;
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml;

    keepalive_timeout 65;
    keepalive_requests 100;

    proxy_buffering on;
    proxy_buffer_size 4k;
    proxy_buffers 8 4k;
    proxy_busy_buffers_size 8k;
}
```

**TCP Tuning (Linux):**
```bash
# /etc/sysctl.conf
net.core.somaxconn = 4096
net.ipv4.tcp_max_syn_backlog = 4096
net.ipv4.tcp_fin_timeout = 30
net.ipv4.tcp_keepalive_time = 300
net.ipv4.tcp_keepalive_probes = 5
net.ipv4.tcp_keepalive_intvl = 15
```

---

## Runbooks

### Runbook 1: Backend Pod Crashando

**Trigger:** PagerDuty alerta "Backend pods em CrashLoopBackOff"

**Passos:**
1. Verificar status:
   ```bash
   kubectl get pods -n caixa-relatorios | grep backend
   ```

2. Ver logs do pod:
   ```bash
   POD=$(kubectl get pods -n caixa-relatorios -l component=backend -o jsonpath='{.items[0].metadata.name}')
   kubectl logs $POD -n caixa-relatorios --tail=200
   ```

3. Verificar eventos:
   ```bash
   kubectl describe pod $POD -n caixa-relatorios | grep -A 10 Events
   ```

4. Análise:
   - **Se erro de conexão DB**: Verificar conectividade DB2, escalar para DBA
   - **Se OOM Killed**: Aumentar memory limit
   - **Se erro de configuração**: Verificar ConfigMap/Secret

5. Ação corretiva:
   ```bash
   # Se OOM
   kubectl set resources deployment caixa-relatorios-backend --limits=memory=6Gi -n caixa-relatorios

   # Se config
   kubectl edit configmap caixa-relatorios-config -n caixa-relatorios
   kubectl rollout restart deployment caixa-relatorios-backend -n caixa-relatorios
   ```

6. Monitorar recuperação (10 min)

7. Documentar incidente no Jira

### Runbook 2: Alto Tempo de Resposta (> 5s)

**Trigger:** Grafana alerta "API Response Time p95 > 5s"

**Passos:**
1. Verificar métricas:
   - Grafana: CPU, memória, request rate
   - Kibana: Queries lentas

2. Identificar gargalo:
   ```bash
   # Verificar uso de recursos
   kubectl top pods -n caixa-relatorios

   # Verificar HPA status
   kubectl get hpa -n caixa-relatorios -o wide
   ```

3. Se CPU alto (> 80%):
   ```bash
   # Escalar imediatamente
   kubectl scale deployment caixa-relatorios-backend --replicas=12 -n caixa-relatorios
   ```

4. Se memória alta (> 3.5GB):
   ```bash
   # Aumentar limite
   kubectl set resources deployment caixa-relatorios-backend --limits=memory=6Gi -n caixa-relatorios
   ```

5. Se DB lento:
   - Verificar com DBA: índices, locks, query plan
   - Considerar aumentar timeout temporariamente

6. Monitorar por 30 min

7. Se não melhorar, escalar para N3 Dev

### Runbook 3: Certificado SSL Expirando

**Trigger:** Email automático "Certificado expira em 7 dias"

**Passos:**
1. Verificar data de expiração:
   ```bash
   echo | openssl s_client -connect relatorios.caixaseguradora.com.br:443 2>/dev/null | openssl x509 -noout -dates
   ```

2. Se usando Let's Encrypt (cert-manager):
   ```bash
   # Forçar renovação
   kubectl delete certificaterequest -n caixa-relatorios --all
   kubectl delete certificate caixa-relatorios-tls -n caixa-relatorios
   kubectl apply -f k8s/ingress.yaml
   ```

3. Aguardar provisioning (5-10 min):
   ```bash
   kubectl get certificate -n caixa-relatorios --watch
   ```

4. Verificar novo certificado:
   ```bash
   echo | openssl s_client -connect relatorios.caixaseguradora.com.br:443 2>/dev/null | openssl x509 -noout -dates
   ```

5. Confirmar funcionamento via browser

6. Documentar renovação

---

**Última Atualização**: Outubro 2025
**Responsável**: Equipe SRE Caixa Seguradora
**Contato**: sre@caixaseguradora.com.br
**On-Call**: plantao-ti@caixaseguradora.com.br | (11) 9999-9999
