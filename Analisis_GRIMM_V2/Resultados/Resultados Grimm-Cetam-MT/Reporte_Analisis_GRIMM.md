# Reporte de análisis GRIMM: Cetam vs MT

## 1. Datos de entrada y configuración

**Pares de archivos utilizados:**

|Fecha      |Archivo Cetam                 |Archivo MT                 |
|:----------|:-----------------------------|:--------------------------|
|2025-05-22 |GRIMM 11D Cetam 22052025.xlsx |GRIMM 11D MT 22052025.xlsx |
|2025-05-30 |GRIMM 11D Cetam 30052025.xlsx |GRIMM 11D MT 30052025.xlsx |
|2025-06-19 |GRIMM 11D Cetam 19062025.xlsx |GRIMM 11D MT 19062025.xlsx |

**Configuración del análisis:**

- Intervalo solicitado: sin límite inferior → sin límite superior
- Años aceptados: 2025
- Criterio de outliers: Q3 + 5 × IQR (solo valores superiores)
- Resolución del análisis: promedios por minuto
- Umbral para reportar intervalos sin datos: 60 minutos

## 2. Rango de tiempo y cobertura

|Serie              |Inicio           |Fin              |Duración del rango |Minutos con datos |Cobertura del rango |
|:------------------|:----------------|:----------------|:------------------|:-----------------|:-------------------|
|Cetam              |2025-04-24 18:46 |2025-06-19 16:16 |55 d 22 h          |61,994            |77.02 %             |
|MT                 |2025-04-24 19:48 |2025-06-19 16:18 |55 d 20 h          |61,497            |76.46 %             |
|Simultáneo (ambos) |2025-04-24 19:48 |2025-06-19 16:16 |55 d 20 h          |61,388            |76.33 %             |

### Intervalos sin datos

|Serie              |N° de intervalos |Tiempo total sin datos |Intervalo más largo |
|:------------------|:----------------|:----------------------|:-------------------|
|Cetam              |4                |12 d 20 h              |6 d 22 h            |
|MT                 |4                |13 d 3 h               |6 d 22 h            |
|Simultáneo (ambos) |5                |13 d 5 h               |6 d 22 h            |

Detalle (mayores a 60 min):

|Serie              |Último dato antes del hueco |Primer dato después |Duración |
|:------------------|:---------------------------|:-------------------|:--------|
|Cetam              |2025-05-18 14:51            |2025-05-22 13:16    |3 d 22 h |
|Cetam              |2025-05-22 20:03            |2025-05-22 21:12    |1.1 h    |
|Cetam              |2025-05-28 17:14            |2025-05-30 14:47    |1 d 22 h |
|Cetam              |2025-06-12 14:20            |2025-06-19 12:44    |6 d 22 h |
|MT                 |2025-05-18 09:01            |2025-05-22 13:05    |4 d 4 h  |
|MT                 |2025-05-28 17:14            |2025-05-30 14:46    |1 d 22 h |
|MT                 |2025-05-30 16:03            |2025-05-30 19:14    |3.2 h    |
|MT                 |2025-06-12 14:20            |2025-06-19 12:43    |6 d 22 h |
|Simultáneo (ambos) |2025-05-18 09:01            |2025-05-22 13:16    |4 d 4 h  |
|Simultáneo (ambos) |2025-05-22 20:03            |2025-05-22 21:12    |1.1 h    |
|Simultáneo (ambos) |2025-05-28 17:14            |2025-05-30 14:47    |1 d 22 h |
|Simultáneo (ambos) |2025-05-30 16:01            |2025-05-30 19:14    |3.2 h    |
|Simultáneo (ambos) |2025-06-12 14:20            |2025-06-19 12:44    |6 d 22 h |

## 3. Descripción de la data

**Observación por instrumento:**

|Instrumento |Registros originales |Minutos con datos |Intervalo de muestreo mediano |
|:-----------|:--------------------|:-----------------|:-----------------------------|
|Cetam       |62,516               |61,994            |60 s                          |
|MT          |62,039               |61,497            |60 s                          |

**Conteo de registros por fracción** (antes de eliminar outliers; % sobre registros válidos):

|Instrumento |Fracción |Registros |Válidos |Vacíos / NA |Negativos  |Ceros            |Outliers       |
|:-----------|:--------|:---------|:-------|:-----------|:----------|:----------------|:--------------|
|Cetam       |TSP      |62,516    |62,516  |0           |0 (0.00 %) |7,522 (12.03 %)  |2,025 (3.24 %) |
|Cetam       |PM10     |62,516    |62,516  |0           |0 (0.00 %) |7,522 (12.03 %)  |1,799 (2.88 %) |
|Cetam       |PM4      |62,516    |62,516  |0           |0 (0.00 %) |7,566 (12.10 %)  |816 (1.31 %)   |
|Cetam       |PM2.5    |62,516    |62,516  |0           |0 (0.00 %) |11,368 (18.18 %) |664 (1.06 %)   |
|Cetam       |PM1      |62,516    |62,516  |0           |0 (0.00 %) |16,959 (27.13 %) |159 (0.25 %)   |
|MT          |TSP      |62,039    |62,039  |0           |0 (0.00 %) |1,456 (2.35 %)   |1,615 (2.60 %) |
|MT          |PM10     |62,039    |62,039  |0           |0 (0.00 %) |1,456 (2.35 %)   |1,346 (2.17 %) |
|MT          |PM4      |62,039    |62,039  |0           |0 (0.00 %) |1,456 (2.35 %)   |773 (1.25 %)   |
|MT          |PM2.5    |62,039    |62,039  |0           |0 (0.00 %) |1,480 (2.39 %)   |331 (0.53 %)   |
|MT          |PM1      |62,039    |62,039  |0           |0 (0.00 %) |3,720 (6.00 %)   |325 (0.52 %)   |

**Estadísticos de la data original** [µg/m³]:

|Instrumento |Fracción |Mínimo |Media |Mediana |Máximo |Mayor valor outlier |
|:-----------|:--------|:------|:-----|:-------|:------|:-------------------|
|Cetam       |TSP      |0.00   |0.56  |0.30    |193.50 |193.50              |
|Cetam       |PM10     |0.00   |0.53  |0.30    |180.50 |180.50              |
|Cetam       |PM4      |0.00   |0.35  |0.20    |83.70  |83.70               |
|Cetam       |PM2.5    |0.00   |0.23  |0.10    |32.20  |32.20               |
|Cetam       |PM1      |0.00   |0.16  |0.10    |6.90   |6.90                |
|MT          |TSP      |0.00   |2.06  |1.00    |633.50 |633.50              |
|MT          |PM10     |0.00   |1.85  |1.00    |495.10 |495.10              |
|MT          |PM4      |0.00   |1.49  |0.90    |433.50 |433.50              |
|MT          |PM2.5    |0.00   |0.98  |0.70    |214.50 |214.50              |
|MT          |PM1      |0.00   |0.56  |0.40    |40.40  |40.40               |

## 4. Criterios de outliers

- **Método:** Q3 + 5 × IQR.
- Solo se eliminan valores **superiores** al límite; los valores negativos y los ceros **no** se eliminan, solo se reportan en la sección 3.
- Los valores eliminados se reemplazan por NA antes de promediar por minuto.

**Límite superior aplicado** [µg/m³]:

|Fracción |Límite Cetam |Límite MT |
|:--------|:------------|:---------|
|TSP      |2.50         |10.60     |
|PM10     |2.50         |10.00     |
|PM4      |1.90         |8.80      |
|PM2.5    |1.30         |6.30      |
|PM1      |1.20         |3.20      |

## 5. Outliers eliminados

|Fracción |Cetam: válidos originales |Cetam: eliminados |Cetam: % |MT: válidos originales |MT: eliminados |MT: %  |
|:--------|:-------------------------|:-----------------|:--------|:----------------------|:--------------|:------|
|TSP      |62,516                    |2,025             |3.24 %   |62,039                 |1,615          |2.60 % |
|PM10     |62,516                    |1,799             |2.88 %   |62,039                 |1,346          |2.17 % |
|PM4      |62,516                    |816               |1.31 %   |62,039                 |773            |1.25 % |
|PM2.5    |62,516                    |664               |1.06 %   |62,039                 |331            |0.53 % |
|PM1      |62,516                    |159               |0.25 %   |62,039                 |325            |0.52 % |

## 6. Resultados del análisis de concordancia

Calculados con los pares de minutos en que ambos instrumentos tienen dato válido, después de eliminar outliers. Diferencia = MT − Cetam.

**Acuerdo y correlación:**

|Fracción |N° de pares |Media Cetam |Media MT |Pearson |R² (OLS) |CCC de Lin |
|:--------|:-----------|:-----------|:--------|:-------|:--------|:----------|
|TSP      |59,078      |0.38        |1.44     |0.789   |0.623    |0.279      |
|PM10     |59,419      |0.38        |1.40     |0.820   |0.673    |0.303      |
|PM4      |60,419      |0.30        |1.28     |0.890   |0.792    |0.268      |
|PM2.5    |60,697      |0.21        |0.91     |0.929   |0.863    |0.266      |
|PM1      |61,042      |0.15        |0.53     |0.952   |0.906    |0.372      |

**Sesgo, errores y regresión** (MT = β0 + β1 · Cetam):

|Fracción |Sesgo (MT − Cetam) |Sesgo relativo |LoA inferior |LoA superior |MAE   |RMSE  |β0    |β1    |PoA (±tolerancia) |
|:--------|:------------------|:--------------|:------------|:------------|:-----|:-----|:-----|:-----|:-----------------|
|TSP      |1.059              |280.53 %       |-1.43        |3.55         |1.070 |1.653 |0.330 |2.932 |85.84 %           |
|PM10     |1.018              |267.95 %       |-1.24        |3.28         |1.028 |1.540 |0.317 |2.846 |86.51 %           |
|PM4      |0.974              |319.80 %       |-1.05        |3.00         |0.976 |1.420 |0.164 |3.661 |88.33 %           |
|PM2.5    |0.699              |335.27 %       |-0.59        |1.98         |0.699 |0.958 |0.132 |3.719 |95.33 %           |
|PM1      |0.381              |249.99 %       |-0.36        |1.12         |0.381 |0.536 |0.072 |3.026 |99.85 %           |

*PoA = proporción de pares con |MT − Cetam| ≤ 2 µg/m³.*

**Estadísticos de la diferencia (MT − Cetam)** [µg/m³]:

|Fracción |Media |Mediana |SD    |Mínimo |Q1    |Q3    |Máximo |
|:--------|:-----|:-------|:-----|:------|:-----|:-----|:------|
|TSP      |1.059 |0.700   |1.270 |-2.000 |0.300 |1.400 |10.300 |
|PM10     |1.018 |0.700   |1.155 |-2.200 |0.300 |1.400 |9.300  |
|PM4      |0.974 |0.700   |1.033 |-1.700 |0.300 |1.300 |8.500  |
|PM2.5    |0.699 |0.500   |0.656 |-1.100 |0.200 |1.000 |5.900  |
|PM1      |0.381 |0.200   |0.378 |-0.900 |0.100 |0.500 |2.700  |

