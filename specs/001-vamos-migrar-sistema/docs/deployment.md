# Guia de Implantação - Sistema de Relatórios SUSEP

**Versão**: 1.0
**Data**: Outubro 2025
**Público-Alvo**: DevOps, SRE, Administradores de Sistema

## Sumário

- [Pré-Requisitos](#pré-requisitos)
- [Ambientes](#ambientes)
- [Implantação com Docker](#implantação-com-docker)
- [Implantação em Kubernetes](#implantação-em-kubernetes)
- [Configuração de Banco de Dados](#configuração-de-banco-de-dados)
- [Segurança e Certificados](#segurança-e-certificados)
- [Monitoramento](#monitoramento)
- [Backup e Recuperação](#backup-e-recuperação)
- [Troubleshooting](#troubleshooting)

---

## Pré-Requisitos

### Infraestrutura Mínima

**Ambiente de Desenvolvimento:**
- CPU: 2 cores
- RAM: 4 GB
- Disco: 20 GB SSD
- OS: Linux (Ubuntu 22.04+), Windows 10+, macOS 12+

**Ambiente de Produção:**
- CPU: 8 cores (16 recomendados)
- RAM: 16 GB (32 GB recomendados)
- Disco: 100 GB SSD (IOPS >= 3000)
- OS: Linux (Ubuntu 22.04 LTS ou RHEL 9)

### Software

**Backend:**
- .NET 9.0 Runtime
- ASP.NET Core 9.0
- SQLite 3.x (dev) ou DB2 11.5+ (prod)

**Frontend:**
- Node.js 20 LTS
- nginx 1.24+ (para servir arquivos estáticos)

**Container Runtime:**
- Docker 24+
- Docker Compose 2.20+ (dev)
- Kubernetes 1.28+ (prod)

**Observabilidade:**
- Prometheus 2.45+
- Grafana 10+
- Elasticsearch 8.x + Kibana (logs)

---

## Ambientes

### Desenvolvimento (DEV)

- **URL**: `http://localhost:5173` (frontend), `https://localhost:5001` (backend)
- **Banco**: SQLite em `backend/data/premium_reporting.db`
- **Logs**: Console + arquivo em `logs/`
- **Hot Reload**: Habilitado

### Homologação (HML)

- **URL**: `https://hml-relatorios.caixaseguradora.com.br`
- **Banco**: DB2 (somente leitura)
- **Logs**: Elasticsearch
- **Escala**: 2 réplicas backend, 2 réplicas frontend

### Produção (PRD)

- **URL**: `https://relatorios.caixaseguradora.com.br`
- **Banco**: DB2 (somente leitura, cluster HA)
- **Logs**: Elasticsearch (retenção 90 dias)
- **Escala**: Auto-scaling (min: 4, max: 20 réplicas)
- **Backup**: Diário (retenção 30 dias)

---

## Implantação com Docker

### Estrutura de Arquivos

```
deployment/
├── docker/
│   ├── Dockerfile.backend
│   ├── Dockerfile.frontend
│   ├── docker-compose.yml
│   ├── docker-compose.prod.yml
│   └── nginx.conf
└── scripts/
    ├── deploy.sh
    ├── rollback.sh
    └── health-check.sh
```

### 1. Build das Imagens

```bash
# Backend
cd backend
docker build -t caixa-relatorios-backend:1.0.0 -f ../deployment/docker/Dockerfile.backend .

# Frontend
cd frontend
docker build -t caixa-relatorios-frontend:1.0.0 -f ../deployment/docker/Dockerfile.frontend .
```

### 2. Docker Compose (Desenvolvimento)

```yaml
# docker-compose.yml
version: '3.8'

services:
  backend:
    image: caixa-relatorios-backend:1.0.0
    ports:
      - "5001:5001"
    environment:
      - ASPNETCORE_ENVIRONMENT=Development
      - ASPNETCORE_URLS=https://+:5001;http://+:5000
      - ConnectionStrings__DefaultConnection=Data Source=/app/data/premium_reporting.db
    volumes:
      - ./backend/data:/app/data
      - ./backend/logs:/app/logs
    restart: unless-stopped

  frontend:
    image: caixa-relatorios-frontend:1.0.0
    ports:
      - "5173:80"
    environment:
      - VITE_API_URL=https://localhost:5001/api
    depends_on:
      - backend
    restart: unless-stopped

  nginx:
    image: nginx:1.24-alpine
    ports:
      - "443:443"
      - "80:80"
    volumes:
      - ./deployment/docker/nginx.conf:/etc/nginx/nginx.conf:ro
      - ./certs:/etc/nginx/certs:ro
    depends_on:
      - frontend
      - backend
    restart: unless-stopped
```

### 3. Executar

```bash
# Iniciar
docker-compose up -d

# Verificar logs
docker-compose logs -f backend

# Parar
docker-compose down

# Parar e remover volumes
docker-compose down -v
```

### 4. Docker Compose (Produção)

```yaml
# docker-compose.prod.yml
version: '3.8'

services:
  backend:
    image: registry.caixaseguradora.com.br/caixa-relatorios-backend:1.0.0
    deploy:
      replicas: 4
      resources:
        limits:
          cpus: '2'
          memory: 4G
        reservations:
          cpus: '1'
          memory: 2G
      restart_policy:
        condition: on-failure
        delay: 5s
        max_attempts: 3
      update_config:
        parallelism: 2
        delay: 10s
        failure_action: rollback
    environment:
      - ASPNETCORE_ENVIRONMENT=Production
      - ConnectionStrings__DefaultConnection=Server=db2prod.caixa.local;Database=SUSEP;User Id=relatorios_ro;Password=${DB_PASSWORD};
    secrets:
      - db_password
      - jwt_secret
    healthcheck:
      test: ["CMD", "curl", "-f", "https://localhost:5001/health"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s

  frontend:
    image: registry.caixaseguradora.com.br/caixa-relatorios-frontend:1.0.0
    deploy:
      replicas: 4
      resources:
        limits:
          cpus: '0.5'
          memory: 1G
    environment:
      - VITE_API_URL=https://relatorios.caixaseguradora.com.br/api

secrets:
  db_password:
    external: true
  jwt_secret:
    external: true
```

### 5. Script de Deploy

```bash
#!/bin/bash
# deployment/scripts/deploy.sh

set -e

VERSION=$1
ENVIRONMENT=$2

if [ -z "$VERSION" ] || [ -z "$ENVIRONMENT" ]; then
    echo "Uso: ./deploy.sh <version> <dev|hml|prd>"
    exit 1
fi

echo "🚀 Deploying version $VERSION to $ENVIRONMENT..."

# Build
echo "📦 Building images..."
docker build -t caixa-relatorios-backend:$VERSION -f deployment/docker/Dockerfile.backend ./backend
docker build -t caixa-relatorios-frontend:$VERSION -f deployment/docker/Dockerfile.frontend ./frontend

# Tag
echo "🏷️  Tagging images..."
docker tag caixa-relatorios-backend:$VERSION registry.caixaseguradora.com.br/caixa-relatorios-backend:$VERSION
docker tag caixa-relatorios-frontend:$VERSION registry.caixaseguradora.com.br/caixa-relatorios-frontend:$VERSION

# Push
echo "📤 Pushing to registry..."
docker push registry.caixaseguradora.com.br/caixa-relatorios-backend:$VERSION
docker push registry.caixaseguradora.com.br/caixa-relatorios-frontend:$VERSION

# Deploy
if [ "$ENVIRONMENT" = "prd" ]; then
    echo "🔵 Deploying to production..."
    docker stack deploy -c deployment/docker/docker-compose.prod.yml caixa-relatorios
else
    echo "🟢 Deploying to $ENVIRONMENT..."
    docker-compose -f deployment/docker/docker-compose.yml up -d
fi

# Health check
echo "🏥 Running health checks..."
sleep 30
./deployment/scripts/health-check.sh

echo "✅ Deployment completed successfully!"
```

---

## Implantação em Kubernetes

### 1. Namespace

```yaml
# k8s/namespace.yaml
apiVersion: v1
kind: Namespace
metadata:
  name: caixa-relatorios
  labels:
    app: caixa-relatorios
    environment: production
```

### 2. ConfigMap

```yaml
# k8s/configmap.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: caixa-relatorios-config
  namespace: caixa-relatorios
data:
  ASPNETCORE_ENVIRONMENT: "Production"
  ASPNETCORE_URLS: "http://+:5000"
  VITE_API_URL: "https://relatorios.caixaseguradora.com.br/api"
```

### 3. Secret

```yaml
# k8s/secret.yaml
apiVersion: v1
kind: Secret
metadata:
  name: caixa-relatorios-secrets
  namespace: caixa-relatorios
type: Opaque
data:
  db-password: <base64-encoded-password>
  jwt-secret: <base64-encoded-secret>
```

**Criar secret:**
```bash
kubectl create secret generic caixa-relatorios-secrets \
  --from-literal=db-password='SenhaSegura123!' \
  --from-literal=jwt-secret='JWTSecretKey123!' \
  -n caixa-relatorios
```

### 4. Backend Deployment

```yaml
# k8s/backend-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: caixa-relatorios-backend
  namespace: caixa-relatorios
spec:
  replicas: 4
  selector:
    matchLabels:
      app: caixa-relatorios
      component: backend
  template:
    metadata:
      labels:
        app: caixa-relatorios
        component: backend
    spec:
      containers:
      - name: backend
        image: registry.caixaseguradora.com.br/caixa-relatorios-backend:1.0.0
        ports:
        - containerPort: 5000
          name: http
        envFrom:
        - configMapRef:
            name: caixa-relatorios-config
        env:
        - name: ConnectionStrings__DefaultConnection
          value: "Server=db2prod.caixa.local;Database=SUSEP;User Id=relatorios_ro;Password=$(DB_PASSWORD);"
        - name: DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: caixa-relatorios-secrets
              key: db-password
        - name: JwtSettings__Secret
          valueFrom:
            secretKeyRef:
              name: caixa-relatorios-secrets
              key: jwt-secret
        resources:
          requests:
            memory: "2Gi"
            cpu: "1"
          limits:
            memory: "4Gi"
            cpu: "2"
        livenessProbe:
          httpGet:
            path: /health/live
            port: 5000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 5000
          initialDelaySeconds: 10
          periodSeconds: 5
        volumeMounts:
        - name: logs
          mountPath: /app/logs
      volumes:
      - name: logs
        persistentVolumeClaim:
          claimName: caixa-relatorios-logs-pvc
```

### 5. Backend Service

```yaml
# k8s/backend-service.yaml
apiVersion: v1
kind: Service
metadata:
  name: caixa-relatorios-backend
  namespace: caixa-relatorios
spec:
  selector:
    app: caixa-relatorios
    component: backend
  ports:
  - protocol: TCP
    port: 80
    targetPort: 5000
  type: ClusterIP
```

### 6. Frontend Deployment

```yaml
# k8s/frontend-deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: caixa-relatorios-frontend
  namespace: caixa-relatorios
spec:
  replicas: 4
  selector:
    matchLabels:
      app: caixa-relatorios
      component: frontend
  template:
    metadata:
      labels:
        app: caixa-relatorios
        component: frontend
    spec:
      containers:
      - name: frontend
        image: registry.caixaseguradora.com.br/caixa-relatorios-frontend:1.0.0
        ports:
        - containerPort: 80
          name: http
        resources:
          requests:
            memory: "512Mi"
            cpu: "250m"
          limits:
            memory: "1Gi"
            cpu: "500m"
```

### 7. Ingress

```yaml
# k8s/ingress.yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: caixa-relatorios-ingress
  namespace: caixa-relatorios
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
    nginx.ingress.kubernetes.io/force-ssl-redirect: "true"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - relatorios.caixaseguradora.com.br
    secretName: caixa-relatorios-tls
  rules:
  - host: relatorios.caixaseguradora.com.br
    http:
      paths:
      - path: /api
        pathType: Prefix
        backend:
          service:
            name: caixa-relatorios-backend
            port:
              number: 80
      - path: /
        pathType: Prefix
        backend:
          service:
            name: caixa-relatorios-frontend
            port:
              number: 80
```

### 8. HorizontalPodAutoscaler

```yaml
# k8s/hpa.yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: caixa-relatorios-backend-hpa
  namespace: caixa-relatorios
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: caixa-relatorios-backend
  minReplicas: 4
  maxReplicas: 20
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
  behavior:
    scaleDown:
      stabilizationWindowSeconds: 300
      policies:
      - type: Percent
        value: 50
        periodSeconds: 60
    scaleUp:
      stabilizationWindowSeconds: 0
      policies:
      - type: Percent
        value: 100
        periodSeconds: 15
```

### 9. Deploy em Kubernetes

```bash
# Aplicar todos os recursos
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/configmap.yaml
kubectl apply -f k8s/secret.yaml
kubectl apply -f k8s/backend-deployment.yaml
kubectl apply -f k8s/backend-service.yaml
kubectl apply -f k8s/frontend-deployment.yaml
kubectl apply -f k8s/ingress.yaml
kubectl apply -f k8s/hpa.yaml

# Verificar status
kubectl get pods -n caixa-relatorios
kubectl get svc -n caixa-relatorios
kubectl get ingress -n caixa-relatorios

# Ver logs
kubectl logs -f deployment/caixa-relatorios-backend -n caixa-relatorios

# Escalar manualmente
kubectl scale deployment caixa-relatorios-backend --replicas=10 -n caixa-relatorios
```

---

## Configuração de Banco de Dados

### SQLite (Desenvolvimento)

```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Data Source=data/premium_reporting.db"
  }
}
```

**Inicialização:**
```bash
cd backend/src/CaixaSeguradora.Api
dotnet ef database update
```

### DB2 (Produção)

```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Server=db2prod.caixa.local;Database=SUSEP;User Id=relatorios_ro;Password=${DB_PASSWORD};Pooling=true;Min Pool Size=5;Max Pool Size=100;Connection Timeout=30;"
  }
}
```

**Permissões Necessárias:**
- SELECT em todas as views `V0*`
- SELECT em tabela `GE399`
- EXECUTE em procedures (se aplicável)
- **NÃO** conceder INSERT, UPDATE, DELETE (sistema read-only)

**Test Connection:**
```bash
dotnet run --project backend/src/CaixaSeguradora.Api -- --test-db-connection
```

---

## Segurança e Certificados

### Gerar Certificado Self-Signed (Dev)

```bash
dotnet dev-certs https --clean
dotnet dev-certs https --trust
```

### Certificado Let's Encrypt (Prod)

```bash
# Usando cert-manager em Kubernetes
kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.13.0/cert-manager.yaml

# ClusterIssuer
cat <<EOF | kubectl apply -f -
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: ssl@caixaseguradora.com.br
    privateKeySecretRef:
      name: letsencrypt-prod
    solvers:
    - http01:
        ingress:
          class: nginx
EOF
```

### Firewall

**Portas de Entrada:**
- 443 (HTTPS) - Frontend e API
- 80 (HTTP) - Redirect para HTTPS

**Portas de Saída:**
- 50000 (DB2)
- 443 (HTTPS) - APIs externas

---

## Monitoramento

### Prometheus Metrics

```yaml
# k8s/servicemonitor.yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: caixa-relatorios-backend
  namespace: caixa-relatorios
spec:
  selector:
    matchLabels:
      app: caixa-relatorios
      component: backend
  endpoints:
  - port: http
    path: /metrics
    interval: 30s
```

### Grafana Dashboard

Importar dashboard pré-configurado: `deployment/monitoring/grafana-dashboard.json`

**Métricas Principais:**
- Request rate (req/s)
- Error rate (%)
- Response time (p50, p95, p99)
- Active connections
- Database query duration
- Memory usage
- CPU usage

---

## Backup e Recuperação

### Backup Diário

```bash
#!/bin/bash
# deployment/scripts/backup.sh

DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="/backups/caixa-relatorios"

# Backup configurações
kubectl get configmap caixa-relatorios-config -n caixa-relatorios -o yaml > $BACKUP_DIR/configmap_$DATE.yaml
kubectl get secret caixa-relatorios-secrets -n caixa-relatorios -o yaml > $BACKUP_DIR/secret_$DATE.yaml

# Backup logs
tar -czf $BACKUP_DIR/logs_$DATE.tar.gz /var/log/caixa-relatorios/

# Remover backups antigos (> 30 dias)
find $BACKUP_DIR -name "*.yaml" -mtime +30 -delete
find $BACKUP_DIR -name "*.tar.gz" -mtime +30 -delete

echo "Backup completed: $DATE"
```

### Recuperação

```bash
# Restaurar configurações
kubectl apply -f /backups/caixa-relatorios/configmap_20251022_020000.yaml
kubectl apply -f /backups/caixa-relatorios/secret_20251022_020000.yaml

# Recriar pods
kubectl rollout restart deployment/caixa-relatorios-backend -n caixa-relatorios
kubectl rollout restart deployment/caixa-relatorios-frontend -n caixa-relatorios
```

---

## Troubleshooting

### Backend não inicia

```bash
# Verificar logs
kubectl logs -f deployment/caixa-relatorios-backend -n caixa-relatorios --tail=100

# Verificar configuração
kubectl describe pod <pod-name> -n caixa-relatorios

# Testar conexão DB
kubectl exec -it <pod-name> -n caixa-relatorios -- /bin/bash
curl -X GET http://localhost:5000/health/ready
```

### Frontend não carrega

```bash
# Verificar nginx
kubectl exec -it <frontend-pod> -n caixa-relatorios -- nginx -t

# Verificar variáveis de ambiente
kubectl exec -it <frontend-pod> -n caixa-relatorios -- env | grep VITE
```

### Performance degradada

```bash
# Verificar HPA
kubectl get hpa -n caixa-relatorios

# Escalar manualmente
kubectl scale deployment caixa-relatorios-backend --replicas=10 -n caixa-relatorios

# Verificar métricas
kubectl top pods -n caixa-relatorios
```

### Rollback

```bash
# Ver histórico de deploy
kubectl rollout history deployment/caixa-relatorios-backend -n caixa-relatorios

# Rollback para versão anterior
kubectl rollout undo deployment/caixa-relatorios-backend -n caixa-relatorios

# Rollback para versão específica
kubectl rollout undo deployment/caixa-relatorios-backend --to-revision=2 -n caixa-relatorios
```

---

## Checklist de Deploy

- [ ] Variáveis de ambiente configuradas
- [ ] Secrets criados
- [ ] Certificados SSL válidos
- [ ] Conexão DB2 testada
- [ ] Migrations aplicadas (se aplicável)
- [ ] Health checks configurados
- [ ] Monitoramento ativo (Prometheus + Grafana)
- [ ] Logs configurados (Elasticsearch)
- [ ] Backup agendado
- [ ] Firewall configurado
- [ ] DNS atualizado
- [ ] Load balancer configurado
- [ ] Auto-scaling configurado
- [ ] Smoke tests executados
- [ ] Documentação atualizada
- [ ] Stakeholders notificados

---

**Última Atualização**: Outubro 2025
**Responsável**: Equipe DevOps Caixa Seguradora
**Contato**: devops@caixaseguradora.com.br
