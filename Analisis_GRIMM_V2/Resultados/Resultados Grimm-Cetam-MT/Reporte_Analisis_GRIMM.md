# Reporte de análisis GRIMM: Cetam vs MT

## 1. Datos de entrada y configuración

**Archivos utilizados:**

|Instrumento |Archivo                       |
|:-----------|:-----------------------------|
|Cetam       |1804_al_2404_Cetam.xlsx       |
|Cetam       |GRIMM 11D Cetam 19062025.xlsx |
|Cetam       |GRIMM 11D Cetam 22052025.xlsx |
|Cetam       |GRIMM 11D Cetam 30052025.xlsx |
|MT          |GRIMM 11D MT 19062025.xlsx    |
|MT          |GRIMM 11D MT 22052025.xlsx    |
|MT          |GRIMM 11D MT 30052025.xlsx    |

**Configuración del análisis:**

- Intervalo solicitado: sin límite inferior → 2025-05-18
- Años aceptados: 2025
- Criterio de outliers: Q3 + 5 × IQR (solo valores superiores)
- Resolución del análisis: promedios por minuto
- Umbral para reportar intervalos sin datos: 60 minutos

## 2. Rango de tiempo y cobertura

|Serie              |Inicio           |Fin              |Duración del rango |Minutos con datos |Cobertura del rango |
|:------------------|:----------------|:----------------|:------------------|:-----------------|:-------------------|
|Cetam              |2025-04-18 01:40 |2025-05-18 14:51 |30 d 13 h          |43,636            |99.19 %             |
|MT                 |2025-04-24 19:48 |2025-05-18 09:01 |23 d 13 h          |33,904            |99.97 %             |
|Simultáneo (ambos) |2025-04-24 19:48 |2025-05-18 09:01 |23 d 13 h          |33,898            |99.95 %             |

### Intervalos sin datos

|Serie |N° de intervalos |Tiempo total sin datos |Intervalo más largo |
|:-----|:----------------|:----------------------|:-------------------|
|Cetam |1                |5.6 h                  |5.6 h               |

Detalle (mayores a 60 min):

|Serie |Último dato antes del hueco |Primer dato después |Duración |
|:-----|:---------------------------|:-------------------|:--------|
|Cetam |2025-04-24 13:10            |2025-04-24 18:46    |5.6 h    |

## 3. Descripción de la data

**Observación por instrumento:**

|Instrumento |Registros originales |Minutos con datos |Intervalo de muestreo mediano |
|:-----------|:--------------------|:-----------------|:-----------------------------|
|Cetam       |127,622              |43,636            |6 s                           |
|MT          |33,904               |33,904            |60 s                          |

**Conteo de registros por fracción** (antes de eliminar outliers; % sobre registros válidos):

|Instrumento |Fracción |Registros |Válidos |Vacíos / NA |Negativos  |Ceros            |Outliers     |
|:-----------|:--------|:---------|:-------|:-----------|:----------|:----------------|:------------|
|Cetam       |TSP      |127,622   |127,622 |0           |0 (0.00 %) |9,471 (7.42 %)   |816 (0.64 %) |
|Cetam       |PM10     |127,622   |127,622 |0           |0 (0.00 %) |9,471 (7.42 %)   |813 (0.64 %) |
|Cetam       |PM4      |127,622   |127,622 |0           |0 (0.00 %) |9,476 (7.43 %)   |537 (0.42 %) |
|Cetam       |PM2.5    |127,622   |127,622 |0           |0 (0.00 %) |10,650 (8.34 %)  |76 (0.06 %)  |
|Cetam       |PM1      |127,622   |127,622 |0           |0 (0.00 %) |15,213 (11.92 %) |840 (0.66 %) |
|MT          |TSP      |33,904    |33,904  |0           |0 (0.00 %) |288 (0.85 %)     |359 (1.06 %) |
|MT          |PM10     |33,904    |33,904  |0           |0 (0.00 %) |288 (0.85 %)     |279 (0.82 %) |
|MT          |PM4      |33,904    |33,904  |0           |0 (0.00 %) |288 (0.85 %)     |175 (0.52 %) |
|MT          |PM2.5    |33,904    |33,904  |0           |0 (0.00 %) |291 (0.86 %)     |81 (0.24 %)  |
|MT          |PM1      |33,904    |33,904  |0           |0 (0.00 %) |891 (2.63 %)     |33 (0.10 %)  |

**Estadísticos de la data original** [µg/m³]:

|Instrumento |Fracción |Mínimo |Media |Mediana |Máximo |Mayor valor outlier |
|:-----------|:--------|:------|:-----|:-------|:------|:-------------------|
|Cetam       |TSP      |0.00   |0.39  |0.20    |34.50  |34.50               |
|Cetam       |PM10     |0.00   |0.39  |0.20    |18.10  |18.10               |
|Cetam       |PM4      |0.00   |0.34  |0.20    |6.70   |6.70                |
|Cetam       |PM2.5    |0.00   |0.28  |0.20    |6.40   |6.40                |
|Cetam       |PM1      |0.00   |0.23  |0.10    |6.10   |6.10                |
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
|TSP      |2.50         |7.70      |
|PM10     |2.50         |7.70      |
|PM4      |1.90         |7.10      |
|PM2.5    |1.90         |5.20      |
|PM1      |1.30         |3.80      |

## 5. Outliers eliminados

|Fracción |Cetam: válidos originales |Cetam: eliminados |Cetam: % |MT: válidos originales |MT: eliminados |MT: %  |
|:--------|:-------------------------|:-----------------|:--------|:----------------------|:--------------|:------|
|TSP      |127,622                   |816               |0.64 %   |33,904                 |359            |1.06 % |
|PM10     |127,622                   |813               |0.64 %   |33,904                 |279            |0.82 % |
|PM4      |127,622                   |537               |0.42 %   |33,904                 |175            |0.52 % |
|PM2.5    |127,622                   |76                |0.06 %   |33,904                 |81             |0.24 % |
|PM1      |127,622                   |840               |0.66 %   |33,904                 |33             |0.10 % |

## 6. Resultados del análisis de concordancia

Calculados con los pares de minutos en que ambos instrumentos tienen dato válido, después de eliminar outliers. Diferencia = MT − Cetam.

**Acuerdo y correlación:**

|Fracción |N° de pares |Media Cetam |Media MT |Pearson |R² (OLS) |CCC de Lin |
|:--------|:-----------|:-----------|:--------|:-------|:--------|:----------|
|TSP      |33,538      |0.28        |1.26     |0.777   |0.604    |0.182      |
|PM10     |33,618      |0.28        |1.25     |0.798   |0.637    |0.192      |
|PM4      |33,723      |0.25        |1.14     |0.856   |0.733    |0.193      |
|PM2.5    |33,817      |0.20        |0.88     |0.924   |0.854    |0.229      |
|PM1      |33,865      |0.16        |0.59     |0.962   |0.925    |0.325      |

**Sesgo, errores y regresión** (MT = β0 + β1 · Cetam):

|Fracción |Sesgo (MT − Cetam) |Sesgo relativo |LoA inferior |LoA superior |MAE   |RMSE  |β0    |β1    |PoA (±tolerancia) |
|:--------|:------------------|:--------------|:------------|:------------|:-----|:-----|:-----|:-----|:-----------------|
|TSP      |0.988              |357.73 %       |-0.88        |2.85         |0.989 |1.371 |0.256 |3.650 |89.10 %           |
|PM10     |0.969              |349.56 %       |-0.81        |2.75         |0.971 |1.328 |0.248 |3.600 |89.61 %           |
|PM4      |0.896              |364.44 %       |-0.61        |2.41         |0.896 |1.181 |0.185 |3.891 |92.20 %           |
|PM2.5    |0.684              |342.68 %       |-0.37        |1.74         |0.684 |0.870 |0.150 |3.675 |97.66 %           |
|PM1      |0.431              |264.25 %       |-0.31        |1.17         |0.431 |0.572 |0.068 |3.225 |99.84 %           |

*PoA = proporción de pares con |MT − Cetam| ≤ 2 µg/m³.*

**Estadísticos de la diferencia (MT − Cetam)** [µg/m³]:

|Fracción |Media |Mediana |SD    |Mínimo |Q1    |Q3    |Máximo |
|:--------|:-----|:-------|:-----|:------|:-----|:-----|:------|
|TSP      |0.988 |0.700   |0.950 |-0.300 |0.300 |1.300 |7.500  |
|PM10     |0.969 |0.700   |0.907 |-0.300 |0.300 |1.300 |7.400  |
|PM4      |0.896 |0.700   |0.770 |-0.100 |0.300 |1.200 |6.600  |
|PM2.5    |0.684 |0.500   |0.538 |-0.100 |0.300 |0.900 |4.900  |
|PM1      |0.431 |0.300   |0.376 |-0.100 |0.100 |0.600 |3.300  |

