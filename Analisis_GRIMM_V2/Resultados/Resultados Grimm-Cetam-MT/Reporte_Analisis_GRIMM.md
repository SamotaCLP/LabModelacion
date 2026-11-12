# Reporte de análisis GRIMM: Cetam vs MT

## 1. Datos de entrada y configuración

**Pares de archivos utilizados:**

|Fecha      |Archivo Cetam                 |Archivo MT                 |
|:----------|:-----------------------------|:--------------------------|
|2025-05-22 |GRIMM 11D Cetam 22052025.xlsx |GRIMM 11D MT 22052025.xlsx |
|2025-05-30 |GRIMM 11D Cetam 30052025.xlsx |GRIMM 11D MT 30052025.xlsx |
|2025-06-19 |GRIMM 11D Cetam 19062025.xlsx |GRIMM 11D MT 19062025.xlsx |

**Configuración del análisis:**

- Intervalo solicitado: sin límite inferior → 2025-05-18
- Años aceptados: 2025
- Criterio de outliers: Q3 + 5 × IQR (solo valores superiores)
- Resolución del análisis: promedios por minuto
- Umbral para reportar intervalos sin datos: 60 minutos

## 2. Rango de tiempo y cobertura

|Serie              |Inicio           |Fin              |Duración del rango |Minutos con datos |Cobertura del rango |
|:------------------|:----------------|:----------------|:------------------|:-----------------|:-------------------|
|Cetam              |2025-04-24 18:46 |2025-05-18 14:51 |23 d 20 h          |34,305            |99.94 %             |
|MT                 |2025-04-24 19:48 |2025-05-18 09:01 |23 d 13 h          |33,904            |99.97 %             |
|Simultáneo (ambos) |2025-04-24 19:48 |2025-05-18 09:01 |23 d 13 h          |33,898            |99.95 %             |

### Intervalos sin datos

No se detectaron intervalos sin datos mayores a 60 minutos.

## 3. Descripción de la data

**Observación por instrumento:**

|Instrumento |Registros originales |Minutos con datos |Intervalo de muestreo mediano |
|:-----------|:--------------------|:-----------------|:-----------------------------|
|Cetam       |34,325               |34,305            |60 s                          |
|MT          |33,904               |33,904            |60 s                          |

**Conteo de registros por fracción** (antes de eliminar outliers; % sobre registros válidos):

|Instrumento |Fracción |Registros |Válidos |Vacíos / NA |Negativos  |Ceros           |Outliers     |
|:-----------|:--------|:---------|:-------|:-----------|:----------|:---------------|:------------|
|Cetam       |TSP      |34,325    |34,325  |0           |0 (0.00 %) |3,466 (10.10 %) |137 (0.40 %) |
|Cetam       |PM10     |34,325    |34,325  |0           |0 (0.00 %) |3,466 (10.10 %) |133 (0.39 %) |
|Cetam       |PM4      |34,325    |34,325  |0           |0 (0.00 %) |3,471 (10.11 %) |175 (0.51 %) |
|Cetam       |PM2.5    |34,325    |34,325  |0           |0 (0.00 %) |4,569 (13.31 %) |44 (0.13 %)  |
|Cetam       |PM1      |34,325    |34,325  |0           |0 (0.00 %) |7,131 (20.77 %) |218 (0.64 %) |
|MT          |TSP      |33,904    |33,904  |0           |0 (0.00 %) |288 (0.85 %)    |359 (1.06 %) |
|MT          |PM10     |33,904    |33,904  |0           |0 (0.00 %) |288 (0.85 %)    |279 (0.82 %) |
|MT          |PM4      |33,904    |33,904  |0           |0 (0.00 %) |288 (0.85 %)    |175 (0.52 %) |
|MT          |PM2.5    |33,904    |33,904  |0           |0 (0.00 %) |291 (0.86 %)    |81 (0.24 %)  |
|MT          |PM1      |33,904    |33,904  |0           |0 (0.00 %) |891 (2.63 %)    |33 (0.10 %)  |

**Estadísticos de la data original** [µg/m³]:

|Instrumento |Fracción |Mínimo |Media |Mediana |Máximo |Mayor valor outlier |
|:-----------|:--------|:------|:-----|:-------|:------|:-------------------|
|Cetam       |TSP      |0.00   |0.30  |0.20    |22.10  |22.10               |
|Cetam       |PM10     |0.00   |0.30  |0.20    |17.90  |17.90               |
|Cetam       |PM4      |0.00   |0.25  |0.20    |6.70   |6.70                |
|Cetam       |PM2.5    |0.00   |0.20  |0.10    |6.40   |6.40                |
|Cetam       |PM1      |0.00   |0.16  |0.10    |6.10   |6.10                |
|MT          |TSP      |0.00   |1.43  |1.00    |192.40 |192.40              |
|MT          |PM10     |0.00   |1.36  |0.90    |90.10  |90.10               |
|MT          |PM4      |0.00   |1.20  |0.90    |43.60  |43.60               |
|MT          |PM2.5    |0.00   |0.90  |0.70    |41.70  |41.70               |
|MT          |PM1      |0.00   |0.60  |0.40    |40.40  |40.40               |

## 4. Criterios de outliers

- **Método:** Q3 + 5 × IQR.
- Solo se eliminan valores **superiores** al límite; los valores negativos y los ceros **no** se eliminan, solo se reportan en la sección 3.
- Los valores eliminados se reemplazan por NA antes de promediar por minuto.

**Límite superior aplicado** [µg/m³]:

|Fracción |Límite Cetam |Límite MT |
|:--------|:------------|:---------|
|TSP      |1.90         |7.70      |
|PM10     |1.90         |7.70      |
|PM4      |1.30         |7.10      |
|PM2.5    |1.30         |5.20      |
|PM1      |0.70         |3.80      |

## 5. Outliers eliminados

|Fracción |Cetam: válidos originales |Cetam: eliminados |Cetam: % |MT: válidos originales |MT: eliminados |MT: %  |
|:--------|:-------------------------|:-----------------|:--------|:----------------------|:--------------|:------|
|TSP      |34,325                    |137               |0.40 %   |33,904                 |359            |1.06 % |
|PM10     |34,325                    |133               |0.39 %   |33,904                 |279            |0.82 % |
|PM4      |34,325                    |175               |0.51 %   |33,904                 |175            |0.52 % |
|PM2.5    |34,325                    |44                |0.13 %   |33,904                 |81             |0.24 % |
|PM1      |34,325                    |218               |0.64 %   |33,904                 |33             |0.10 % |

## 6. Resultados del análisis de concordancia

Calculados con los pares de minutos en que ambos instrumentos tienen dato válido, después de eliminar outliers. Diferencia = MT − Cetam.

**Acuerdo y correlación:**

|Fracción |N° de pares |Media Cetam |Media MT |Pearson |R² (OLS) |CCC de Lin |
|:--------|:-----------|:-----------|:--------|:-------|:--------|:----------|
|TSP      |33,533      |0.28        |1.26     |0.777   |0.604    |0.181      |
|PM10     |33,613      |0.28        |1.25     |0.798   |0.636    |0.191      |
|PM4      |33,690      |0.24        |1.14     |0.854   |0.729    |0.190      |
|PM2.5    |33,813      |0.20        |0.88     |0.924   |0.853    |0.228      |
|PM1      |33,675      |0.16        |0.58     |0.959   |0.919    |0.315      |

**Sesgo, errores y regresión** (MT = β0 + β1 · Cetam):

|Fracción |Sesgo (MT − Cetam) |Sesgo relativo |LoA inferior |LoA superior |MAE   |RMSE  |β0    |β1    |PoA (±tolerancia) |
|:--------|:------------------|:--------------|:------------|:------------|:-----|:-----|:-----|:-----|:-----------------|
|TSP      |0.987              |357.94 %       |-0.87        |2.85         |0.989 |1.370 |0.253 |3.660 |89.12 %           |
|PM10     |0.969              |349.73 %       |-0.81        |2.75         |0.971 |1.327 |0.246 |3.608 |89.62 %           |
|PM4      |0.893              |364.92 %       |-0.61        |2.39         |0.893 |1.175 |0.183 |3.901 |92.29 %           |
|PM2.5    |0.684              |342.77 %       |-0.37        |1.74         |0.684 |0.869 |0.150 |3.678 |97.67 %           |
|PM1      |0.423              |265.73 %       |-0.29        |1.13         |0.423 |0.557 |0.065 |3.249 |99.94 %           |

*PoA = proporción de pares con |MT − Cetam| ≤ 2 µg/m³.*

**Estadísticos de la diferencia (MT − Cetam)** [µg/m³]:

|Fracción |Media |Mediana |SD    |Mínimo |Q1    |Q3    |Máximo |
|:--------|:-----|:-------|:-----|:------|:-----|:-----|:------|
|TSP      |0.987 |0.700   |0.950 |-0.300 |0.300 |1.300 |7.500  |
|PM10     |0.969 |0.700   |0.906 |-0.300 |0.300 |1.300 |7.400  |
|PM4      |0.893 |0.700   |0.764 |-0.100 |0.300 |1.200 |6.600  |
|PM2.5    |0.684 |0.500   |0.537 |-0.100 |0.300 |0.900 |4.900  |
|PM1      |0.423 |0.300   |0.362 |-0.100 |0.100 |0.600 |3.300  |

