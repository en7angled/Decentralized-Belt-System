### Structura unui manifest Kubernetes și cele mai uzuale „kind”-uri

Acest ghid explică structura de bază a unui manifest YAML pentru Kubernetes, care sunt câmpurile standard și ce înseamnă, apoi trece prin cele mai comune „kind”-uri (tipuri de resurse) și atributele lor importante.

## 1) Structura generală a unui manifest YAML

Orice manifest K8s are, de regulă, următoarele câmpuri în „rădăcină”:

- `apiVersion`: versiunea API-ului pentru acel tip de resursă (ex.: `v1`, `apps/v1`, `networking.k8s.io/v1`).
- `kind`: tipul (resursa) pe care o definim (ex.: `Deployment`, `Service`, `Ingress`).
- `metadata`: informații despre resursă (nume, etichete, adnotări, namespace).
  - `name`: numele resursei (obligatoriu pentru majoritatea kind-urilor).
  - `namespace`: spațiul logic (dacă lipsește, se folosește `default`).
  - `labels`: perechi cheie:valoare pentru selecție/organizare.
  - `annotations`: metadate nefuncționale, folosite de tool‑uri (ex.: cert‑manager, ingress controllers).
- `spec`: „ce vrei” – configurația dorită pentru resursă (conținutul depinde de `kind`).
- `status`: „ce este” – starea curentă (completat de cluster; de obicei nu pui `status` în fișierul tău).

Exemplu minimal:
```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: exemplu
```

Manifeste „combinate” (documente multiple) se separă cu `---` într-un singur fișier YAML.

## 2) Kind-uri uzuale și atribute-cheie

### Namespace (v1)
- Scop: izolează resursele într-un spațiu logic.
- Chei: `metadata.name`.

### ConfigMap (v1)
- Scop: variabile de configurare (text) montate ca fișiere sau injectate ca env vars.
- Chei: `data` (chei/valori), `binaryData` (B64), `metadata.name`, `metadata.namespace`.

### Secret (v1)
- Scop: valori sensibile (chei, token‑uri). Valorile sunt Base64 în `data` sau text în `stringData`.
- Tipuri: `Opaque`, `kubernetes.io/tls`, etc.
- Chei: `type`, `data`/`stringData`.

### ServiceAccount (v1)
- Scop: identitate pentru poduri (autentificare la API K8s sau servicii externe).
- Chei: `metadata.name`, legare în Pod/Deployment prin `spec.serviceAccountName`.

### Role / ClusterRole, RoleBinding / ClusterRoleBinding (rbac.authorization.k8s.io/v1)
- Scop: permisiuni RBAC la nivel de namespace (`Role`) sau cluster (`ClusterRole`).
- Chei: reguli în `rules`, subiecte în `subjects`, referință la rol în `roleRef`.

### PersistentVolumeClaim (v1)
- Scop: ceri stocare persistentă; se mapează la un PersistentVolume.
- Chei: `spec.accessModes` (ex.: `ReadWriteOnce`), `spec.resources.requests.storage` (mărimea), `storageClassName` (opțional).

Exemplu:
```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: data-pvc
spec:
  accessModes: [ReadWriteOnce]
  resources:
    requests:
      storage: 5Gi
```

### Deployment (apps/v1)
- Scop: gestionează un set de replici de Pod-uri; suportă rolling updates și rollback.
- Chei:
  - `spec.replicas`: câte replici vrei.
  - `spec.selector.matchLabels`: ce etichete definesc setul de Pod-uri (trebuie să corespundă cu `template.metadata.labels`).
  - `spec.template`: modelul Pod-ului (containers, volumes, env, probes, resources, initContainers etc.).
  - `spec.strategy`: cum se face rollout (ex.: `RollingUpdate` cu `maxSurge`/`maxUnavailable`).

Fragment exemplar pentru un container:
```yaml
spec:
  template:
    spec:
      containers:
        - name: app
          image: repo/imagename:tag
          ports:
            - name: http
              containerPort: 8080
          env:
            - name: EX_VAR
              value: "123"
          volumeMounts:
            - name: data
              mountPath: /data
      volumes:
        - name: data
          persistentVolumeClaim:
            claimName: data-pvc
```

### StatefulSet (apps/v1)
- Scop: similar cu Deployment, dar pentru aplicații cu identitate stabilă per pod (ex.: baze de date). Nume predictibile, volume persistente per instanță.

### DaemonSet (apps/v1)
- Scop: rulează câte un Pod pe fiecare nod (ex.: agenți de monitoring, log collectors).

### Job / CronJob (batch/v1)
- Scop: task unic (Job) sau periodic (CronJob).
- Chei: `spec.template` (podul efectiv), `spec.schedule` (CronJob), politici de retry.

### Service (v1)
- Scop: IP stabil și DNS intern pentru un set de poduri; face load‑balancing.
- Tipuri: `ClusterIP` (default, intern), `NodePort`, `LoadBalancer`.
- Chei:
  - `spec.selector`: etichete pe care le urmărește.
  - `spec.ports`: lista de porturi expuse (`port` – extern în cluster, `targetPort` – în container, `name` – pentru referințe).

Exemplu:
```yaml
apiVersion: v1
kind: Service
metadata:
  name: app-svc
spec:
  selector:
    app: app
  ports:
    - name: http
      port: 80
      targetPort: 8080
```

### Gateway API (gateway.networking.k8s.io/v1)
- Resurse principale:
  - `Gateway`: listener-e pe porturi (80/443), TLS, SNI/hostname
  - `HTTPRoute`: reguli L7 (host/path) care trimit către Service-uri
- Chei la `Gateway`:
  - `spec.gatewayClassName` (ex.: `traefik`)
  - `spec.listeners[]`:
    - `name`, `hostname`, `port`, `protocol: HTTP|HTTPS`
    - `tls.mode: Terminate` și `tls.certificateRefs` pentru TLS
  - `metadata.annotations["cert-manager.io/cluster-issuer"]` — opțional, pentru integrare automată TLS
- Chei la `HTTPRoute`:
  - `spec.parentRefs[]` — referă `Gateway`
  - `spec.hostnames[]` — host-urile servite
  - `spec.rules[]` — `matches` (path), `filters` (Redirect), `backendRefs` (Service/port)

### GatewayClass (gateway.networking.k8s.io/v1)
- Scop: definește un controller pentru Gateway API (ex.: Traefik). De obicei este creat de operatorul controller‑ului (k3s include Traefik cu GatewayClass).

### PodDisruptionBudget (policy/v1)
- Scop: asigură un minim de poduri disponibile în timpul întreruperilor planificate.
- Chei: `spec.minAvailable` sau `spec.maxUnavailable`, plus selectorul.

### HorizontalPodAutoscaler (autoscaling/v2)
- Scop: scalează automat replicile în funcție de metrici (CPU, mem, custom metrics).
- Chei: ținte (`scaleTargetRef`), metrici, intervale min/max replici.

### NetworkPolicy (networking.k8s.io/v1)
- Scop: controlează traficul de rețea între poduri/noduri (ingress/egress).
- Chei: selectori de poduri, reguli de trafic, namespace selectors.

## 3) Resurse specifice cert-manager (TLS automat)

### Issuer / ClusterIssuer (cert-manager.io/v1)
- Scop: configurează „de unde” emitem certificate (ex.: Let’s Encrypt) și modul de validare (HTTP‑01, DNS‑01).
- `ClusterIssuer` este global; `Issuer` este local unui namespace.
- Chei la ACME: `spec.acme.server`, `spec.acme.email`, `privateKeySecretRef`, `solvers` (ex.: DNS‑01 cu webhook GoDaddy).

### Certificate (cert-manager.io/v1)
- Scop: definește ce certificat vrei (host-uri) și unde să fie salvat (`secretName`).
- cert‑manager creează `CertificateRequest` → `Order` → `Challenge` automat.

### CertificateRequest, Order, Challenge
- Resurse intermediare create/administrate de cert‑manager în procesul ACME.
- Utile pentru diagnoză (vezi mesaje de eroare, stări).

## 4) Sfaturi practice

- Etichete (`labels`) consecvente (ex.: `app: nume-app`) ușurează selectarea podurilor/serviciilor.
- Păstrează `selector.matchLabels` în Deployments în sincronie cu `template.metadata.labels`.
- Pentru baze de date și fișiere persistente, folosește PVC și montează volume în `volumeMounts`.
- Pentru TLS automat, combină Ingress + cert‑manager + un Issuer adecvat (HTTP‑01 sau DNS‑01).
- Nu pune `status` în manifestele tale – este gestionat de control plane.

## 5) Structura `spec` pe kind-uri uzuale (detaliat)

Mai jos sunt câmpurile importante din `spec` pentru resursele cele mai folosite. Nu sunt exhaustive, dar acoperă 90% din cazurile reale.

### Namespace (v1)
- Nu are `spec`. Doar `metadata.name`.

### PersistentVolumeClaim (v1)
- `spec.accessModes`: lista modurilor de acces, de ex. `ReadWriteOnce`, `ReadOnlyMany`, `ReadWriteMany`.
- `spec.resources.requests.storage`: cantitatea solicitată (ex. `5Gi`).
- `spec.storageClassName` (opțional): numele StorageClass-ului.
- `spec.volumeMode` (opțional): `Filesystem` (implicit) sau `Block`.
- `spec.dataSource` (avansat): clonare din alt PVC/VolumeSnapshot.

### Deployment (apps/v1)
- `spec.replicas`: număr dorit de replici de pod.
- `spec.selector.matchLabels`: selectorul care identifică podurile gestionate (trebuie să corespundă cu `template.metadata.labels`).
- `spec.template`:
  - `metadata.labels`: etichetele podului.
  - `spec.serviceAccountName` (opțional): SA folosit de pod.
  - `spec.imagePullSecrets` (opțional): pentru registry privat.
  - `spec.containers[]`:
    - `name`, `image`, `imagePullPolicy`
    - `ports[]` (ex. `containerPort: 8082`)
    - `env[]`, `envFrom[]` (ConfigMap/Secret)
    - `resources.requests/limits` (CPU/memorie)
    - `livenessProbe`, `readinessProbe` (HTTP/TCP/exec)
    - `volumeMounts[]` (nume volum + `mountPath`)
    - `securityContext` (uid/gid, readOnlyRootFilesystem etc.)
  - `spec.initContainers[]`: pași de inițializare (ex. așteptare DB).
  - `spec.volumes[]`: definirea volumelor (ex. PVC, emptyDir, configMap, secret).
  - `spec.nodeSelector`, `affinity`, `tolerations` (plasare pe noduri).
- `spec.strategy`:
  - `type: RollingUpdate|Recreate`
  - `rollingUpdate.maxSurge`, `rollingUpdate.maxUnavailable`
- `spec.revisionHistoryLimit` (default 10): câte ReplicaSet-uri păstrate pentru rollback.
- `spec.progressDeadlineSeconds`: timp maxim pentru un rollout reușit.
- `spec.minReadySeconds`: timp minim ca un pod să fie considerat ready.

### StatefulSet (apps/v1)
- `spec.serviceName`: numele Service-ului headless asociat (rezoluție stabilă per pod).
- `spec.replicas`, `spec.selector`, `spec.template`: la fel ca Deployment.
- `spec.volumeClaimTemplates[]`: PVC per pod, create automat.
- `spec.updateStrategy`: `RollingUpdate` (cu `partition`) sau `OnDelete`.
- `spec.podManagementPolicy`: `OrderedReady` (implicit) sau `Parallel`.
- `spec.ordinals` (v1.27+): control asupra index-ului podurilor.

### DaemonSet (apps/v1)
- `spec.selector`, `spec.template`: ca la Deployment.
- `spec.updateStrategy`: `RollingUpdate` (cu `maxUnavailable`) sau `OnDelete`.
- Rulează câte un pod pe fiecare nod potrivit selectorului.

### Job (batch/v1)
- `spec.template`: podul care va rula task-ul.
- `spec.backoffLimit`: câte retry-uri la eșec.
- `spec.completions`: câte execuții dorite (default 1).
- `spec.parallelism`: câte în paralel (default 1).
- `spec.activeDeadlineSeconds`: timeout total.

### CronJob (batch/v1)
- `spec.schedule`: crontab (ex. `"*/5 * * * *"`).
- `spec.jobTemplate`: template-ul Job-ului lansat periodic.
- `spec.concurrencyPolicy`: `Allow|Forbid|Replace`.
- `spec.failedJobsHistoryLimit`, `spec.successfulJobsHistoryLimit`.

### Service (v1)
- `spec.type`: `ClusterIP` (implicit), `NodePort`, `LoadBalancer`, `ExternalName`.
- `spec.selector`: etichetele podurilor țintă.
- `spec.ports[]`:
  - `name` (pentru referințe/Ingress)
  - `port`: portul expus în Service
  - `targetPort`: portul din container (număr sau nume)
  - `protocol`: `TCP|UDP|SCTP`
  - `nodePort` (doar pentru `NodePort`)
- `spec.sessionAffinity`: `None|ClientIP`.
- `spec.externalTrafficPolicy` (LB/NodePort): `Cluster|Local`.

### Ingress (networking.k8s.io/v1)
- `spec.ingressClassName` (sau adnotare): alegerea controller-ului (ex. `traefik`).
- `spec.tls[]`:
  - `hosts[]`
  - `secretName`: secret TLS (creat de cert-manager de obicei)
- `spec.rules[]`:
  - `host`
  - `http.paths[]`:
    - `path`, `pathType: Prefix|Exact`
    - `backend.service.name`, `backend.service.port.{number|name}`
- `spec.defaultBackend` (opțional): backend implicit dacă nu se potrivește nicio regulă.

### PodDisruptionBudget (policy/v1)
- `spec.minAvailable` sau `spec.maxUnavailable` (unul singur).
- `spec.selector`: podurile țintă (prin labels).

### HorizontalPodAutoscaler (autoscaling/v2)
- `spec.scaleTargetRef`: resursa de scalat (Deployment/StatefulSet etc.).
- `spec.minReplicas`, `spec.maxReplicas`.
- `spec.metrics[]`: sursa metricilor (ex. CPU: `type: Resource`, `resource.name: cpu`, `target.type: Utilization`, `averageUtilization: 80`).
- `spec.behavior` (v2): reguli de ramp‑up/down.

### NetworkPolicy (networking.k8s.io/v1)
- `spec.podSelector`: ce poduri protejezi.
- `spec.policyTypes[]`: `Ingress`, `Egress`.
- `spec.ingress[]`/`spec.egress[]`: reguli (from/to, ports, namespaceSelector, podSelector, ipBlock).

### Issuer / ClusterIssuer (cert-manager.io/v1)
- `spec.acme.server`: endpoint ACME (staging/prod).
- `spec.acme.email`: emailul contului ACME.
- `spec.acme.privateKeySecretRef.name`: unde se păstrează cheia contului ACME.
- `spec.acme.solvers[]`:
  - `http01` (ingress): validează prin HTTP.
  - `dns01.webhook`: validează prin DNS (GoDaddy etc.).

### Certificate (cert-manager.io/v1)
- `spec.secretName`: unde salvează certificatul+cheia privată.
- `spec.dnsNames[]` / `spec.commonName`.
- `spec.issuerRef`: `name`, `kind: Issuer|ClusterIssuer`.
- `spec.duration`, `spec.renewBefore`: valabilitate și fereastra de reînnoire.


