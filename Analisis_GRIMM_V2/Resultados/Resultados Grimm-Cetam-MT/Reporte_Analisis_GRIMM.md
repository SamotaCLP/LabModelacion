# Reporte de análisis GRIMM: Cetam vs MT

## 1. Datos de entrada y configuración

**Archivos utilizados:**

|Instrumento |Archivo                       |
|:-----------|:-----------------------------|
|Cetam       |0102_al_0302_Cetam.xlsx       |
|Cetam       |0602_al_1202_Cetam.xlsx       |
|Cetam       |0604_al_1804_Cetam.xlsx       |
|Cetam       |1202_al_1303_Cetam.xlsx       |
|Cetam       |1303_al_2303_Cetam.xlsx       |
|Cetam       |1804_al_2404_Cetam.xlsx       |
|Cetam       |2303_al_0604_Cetam.xlsx       |
|Cetam       |GRIMM 11D Cetam 19062025.xlsx |
|Cetam       |GRIMM 11D Cetam 22052025.xlsx |
|Cetam       |GRIMM 11D Cetam 30052025.xlsx |
|MT          |GRIMM 11D MT 19062025.xlsx    |
|MT          |GRIMM 11D MT 22052025.xlsx    |
|MT          |GRIMM 11D MT 24042025.xlsx    |
|MT          |GRIMM 11D MT 25072025.xlsx    |
|MT          |GRIMM 11D MT 30052025.xlsx    |

**Configuración del análisis:**

- Intervalo solicitado: 2025-03-13 → 2025-05-18
- Años aceptados: 2025
- Criterio de outliers: Q3 + 5 × IQR (solo valores superiores)
- Resolución del análisis: promedios por minuto
- Umbral para reportar intervalos sin datos: 60 minutos

## 2. Rango de tiempo y cobertura

|Serie              |Inicio           |Fin              |Duración del rango |Minutos con datos |Cobertura del rango |
|:------------------|:----------------|:----------------|:------------------|:-----------------|:-------------------|
|Cetam              |2025-03-13 00:00 |2025-05-18 14:51 |66 d 15 h          |95,504            |99.55 %             |
|MT                 |2025-03-13 19:35 |2025-05-18 09:01 |65 d 13 h          |94,005            |99.57 %             |
|Simultáneo (ambos) |2025-03-13 19:35 |2025-05-18 09:01 |65 d 13 h          |93,994            |99.56 %             |

### Intervalos sin datos

|Serie              |N° de intervalos |Tiempo total sin datos |Intervalo más largo |
|:------------------|:----------------|:----------------------|:-------------------|
|Cetam              |2                |6.8 h                  |5.6 h               |
|MT                 |1                |6.5 h                  |6.5 h               |
|Simultáneo (ambos) |1                |6.6 h                  |6.6 h               |

Detalle (mayores a 60 min):

|Serie              |Último dato antes del hueco |Primer dato después |Duración |
|:------------------|:---------------------------|:-------------------|:--------|
|Cetam              |2025-03-13 15:39            |2025-03-13 16:52    |1.2 h    |
|Cetam              |2025-04-24 13:10            |2025-04-24 18:46    |5.6 h    |
|MT                 |2025-04-24 13:15            |2025-04-24 19:48    |6.5 h    |
|Simultáneo (ambos) |2025-04-24 13:10            |2025-04-24 19:48    |6.6 h    |

## 3. Descripción de la data

**Observación por instrumento:**

|Instrumento |Registros originales |Minutos con datos |Intervalo de muestreo mediano |
|:-----------|:--------------------|:-----------------|:-----------------------------|
|Cetam       |637,826              |95,504            |6 s                           |
|MT          |94,005               |94,005            |60 s                          |

**Conteo de registros por fracción** (antes de eliminar outliers; % sobre registros válidos):

|Instrumento |Fracción |Registros |Válidos |Vacíos / NA |Negativos  |Ceros           |Outliers       |
|:-----------|:--------|:---------|:-------|:-----------|:----------|:---------------|:--------------|
|Cetam       |TSP      |637,826   |637,826 |0           |0 (0.00 %) |28,583 (4.48 %) |4,479 (0.70 %) |
|Cetam       |PM10     |637,826   |637,826 |0           |0 (0.00 %) |28,583 (4.48 %) |4,411 (0.69 %) |
|Cetam       |PM4      |637,826   |637,826 |0           |0 (0.00 %) |28,588 (4.48 %) |901 (0.14 %)   |
|Cetam       |PM2.5    |637,826   |637,826 |0           |0 (0.00 %) |30,008 (4.70 %) |493 (0.08 %)   |
|Cetam       |PM1      |637,826   |637,826 |0           |0 (0.00 %) |39,717 (6.23 %) |207 (0.03 %)   |
|MT          |TSP      |94,005    |94,005  |0           |0 (0.00 %) |292 (0.31 %)    |468 (0.50 %)   |
|MT          |PM10     |94,005    |94,005  |0           |0 (0.00 %) |292 (0.31 %)    |349 (0.37 %)   |
|MT          |PM4      |94,005    |94,005  |0           |0 (0.00 %) |292 (0.31 %)    |189 (0.20 %)   |
|MT          |PM2.5    |94,005    |94,005  |0           |0 (0.00 %) |295 (0.31 %)    |68 (0.07 %)    |
|MT          |PM1      |94,005    |94,005  |0           |0 (0.00 %) |902 (0.96 %)    |48 (0.05 %)    |

**Estadísticos de la data original** [µg/m³]:

|Instrumento |Fracción |Mínimo |Media |Mediana |Máximo |Mayor valor outlier |
|:-----------|:--------|:------|:-----|:-------|:------|:-------------------|
|Cetam       |TSP      |0.00   |0.55  |0.40    |291.60 |291.60              |
|Cetam       |PM10     |0.00   |0.55  |0.40    |291.60 |291.60              |
|Cetam       |PM4      |0.00   |0.49  |0.40    |111.50 |111.50              |
|Cetam       |PM2.5    |0.00   |0.42  |0.30    |59.50  |59.50               |
|Cetam       |PM1      |0.00   |0.35  |0.30    |44.80  |44.80               |
|MT          |TSP      |0.00   |2.25  |1.60    |307.80 |307.80              |
|MT          |PM10     |0.00   |2.18  |1.60    |241.00 |241.00              |
|MT          |PM4      |0.00   |1.97  |1.50    |153.20 |153.20              |
|MT          |PM2.5    |0.00   |1.57  |1.20    |63.90  |63.90               |
|MT          |PM1      |0.00   |1.15  |0.80    |40.40  |40.40               |

## 4. Criterios de outliers

- **Método:** Q3 + 5 × IQR.
- Solo se eliminan valores **superiores** al límite; los valores negativos y los ceros **no** se eliminan, solo se reportan en la sección 3.
- Los valores eliminados se reemplazan por NA antes de promediar por minuto.

**Límite superior aplicado** [µg/m³]:

|Fracción |Límite Cetam |Límite MT |
|:--------|:------------|:---------|
|TSP      |3.20         |13.40     |
|PM10     |3.20         |13.40     |
|PM4      |3.20         |12.20     |
|PM2.5    |2.60         |10.20     |
|PM1      |2.50         |7.60      |

## 5. Outliers eliminados

|Fracción |Cetam: válidos originales |Cetam: eliminados |Cetam: % |MT: válidos originales |MT: eliminados |MT: %  |
|:--------|:-------------------------|:-----------------|:--------|:----------------------|:--------------|:------|
|TSP      |637,826                   |4,479             |0.70 %   |94,005                 |468            |0.50 % |
|PM10     |637,826                   |4,411             |0.69 %   |94,005                 |349            |0.37 % |
|PM4      |637,826                   |901               |0.14 %   |94,005                 |189            |0.20 % |
|PM2.5    |637,826                   |493               |0.08 %   |94,005                 |68             |0.07 % |
|PM1      |637,826                   |207               |0.03 %   |94,005                 |48             |0.05 % |

## 6. Resultados del análisis de concordancia

Calculados con los pares de minutos en que ambos instrumentos tienen dato válido, después de eliminar outliers. Diferencia = MT − Cetam.

**Acuerdo y correlación:**

|Fracción |N° de pares |Media Cetam |Media MT |Pearson |R² (OLS) |CCC de Lin |
|:--------|:-----------|:-----------|:--------|:-------|:--------|:----------|
|TSP      |93,525      |0.44        |2.13     |0.848   |0.719    |0.178      |
|PM10     |93,644      |0.44        |2.10     |0.864   |0.746    |0.186      |
|PM4      |93,805      |0.41        |1.93     |0.909   |0.827    |0.200      |
|PM2.5    |93,926      |0.35        |1.56     |0.952   |0.906    |0.230      |
|PM1      |93,946      |0.29        |1.14     |0.966   |0.934    |0.290      |

**Sesgo, errores y regresión** (MT = β0 + β1 · Cetam):

|Fracción |Sesgo (MT − Cetam) |Sesgo relativo |LoA inferior |LoA superior |MAE   |RMSE  |β0    |β1    |PoA (±tolerancia) |
|:--------|:------------------|:--------------|:------------|:------------|:-----|:-----|:-----|:-----|:-----------------|
|TSP      |1.685              |380.05 %       |-1.42        |4.79         |1.685 |2.313 |0.184 |4.386 |70.15 %           |
|PM10     |1.653              |371.91 %       |-1.34        |4.64         |1.654 |2.249 |0.177 |4.321 |70.61 %           |
|PM4      |1.522              |372.62 %       |-1.05        |4.10         |1.522 |2.010 |0.180 |4.285 |73.42 %           |
|PM2.5    |1.211              |351.14 %       |-0.77        |3.20         |1.211 |1.579 |0.147 |4.087 |81.73 %           |
|PM1      |0.851              |292.50 %       |-0.72        |2.42         |0.851 |1.168 |0.046 |3.768 |90.71 %           |

*PoA = proporción de pares con |MT − Cetam| ≤ 2 µg/m³.*

**Estadísticos de la diferencia (MT − Cetam)** [µg/m³]:

|Fracción |Media |Mediana |SD    |Mínimo |Q1    |Q3    |Máximo |
|:--------|:-----|:-------|:-----|:------|:-----|:-----|:------|
|TSP      |1.685 |1.200   |1.585 |-0.300 |0.600 |2.290 |12.822 |
|PM10     |1.653 |1.200   |1.525 |-0.300 |0.600 |2.250 |12.800 |
|PM4      |1.522 |1.160   |1.313 |-0.120 |0.600 |2.100 |11.400 |
|PM2.5    |1.211 |0.900   |1.012 |-0.100 |0.490 |1.690 |9.500  |
|PM1      |0.851 |0.600   |0.800 |-0.100 |0.280 |1.200 |6.690  |

