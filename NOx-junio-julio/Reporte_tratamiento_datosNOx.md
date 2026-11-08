# Análisis Preliminar de Bases de Datos: Medición de NOx Portillo

## 1. Descripción General y Frecuencia de Medición

El presente informe detalla el análisis preliminar de las mediciones de Óxidos de Nitrógeno (NO, NO2, NOx) tomadas por analizadores Thermo Scientific (modelos 42iQ y 42i, operando con y sin sonda).

* **Frecuencia de medición:** El intervalo de tiempo entre mediciones es de **1 minuto**. El 100% de las observaciones en todas las bases de datos mantiene esta periodicidad constante.

## 2. Periodos de Medición y Volumen de Datos

Se evaluaron cuatro conjuntos de datos independientes. Tras realizar un análisis de contención temporal para eliminar archivos redundantes (solapamiento del 100%), los periodos y volúmenes crudos son:

1. **42iQ Sin Sonda:** 22/05/2025 al 25/07/2025 (79.355 registros únicos).

2. **42iQ Con Sonda:** 22/05/2025 al 07/10/2025 (176.255 registros únicos).

3. **42i Sin Sonda:** 24/07/2025 al 07/10/2025 (98.097 registros únicos).

4. **42i Con Sonda:** 22/05/2025 al 25/07/2025 (79.656 registros únicos).

## 3. Criterios de Outliers y Tratamiento de Negativos

Para la limpieza de datos se utilizaron los lineamientos técnicos del equipo y los criterios de validación de la US EPA.

* **Outliers (Eliminados a NA):**

  1. *Valores constantes:* Rachas de 4 o más minutos con el mismo valor exacto. Se consideran periodos de autocalibración interna o estabilización donde no se mide aire ambiental.

  2. *Fallas críticas:* Valores inferiores a -3.0 ppb. Se consideran desviaciones inaceptables (a más de 15 desviaciones estándar del ruido del sensor) o errores de hardware.

* **Valores Negativos (Tratamiento de Ceros):**

  * Los valores comprendidos entre 0 y -3.0 ppb se identifican como ruido térmico normal del sensor óptico midiendo concentraciones nulas. Estos valores fueron fijados a `0`.

## 4. Cantidad y Porcentaje de Limpieza

**Equipo 42iQ Sin Sonda (Base: 79.355 datos)**

* Outliers por calibración eliminados: 80 (0.10%)

* Outliers por falla crítica (< -3.0) eliminados: NO: 0 (0%) | NO2: 16 (0.02%) | NOx: 0 (0%)

* Negativos tratados y fijados a cero: NO: 16.097 (20.28%) | NO2: 2.216 (2.79%) | NOx: 5.483 (6.91%)

**Equipo 42iQ Con Sonda (Base: 176.255 datos)**

* Outliers por calibración eliminados: 109 (0.06%)

* Outliers por falla crítica (< -3.0) eliminados: NO: 0 (0%) | NO2: 82 (0.05%) | NOx: 2 (0.001%)

* Negativos tratados y fijados a cero: NO: 93.587 (53.13%) | NO2: 24.825 (14.09%) | NOx: 52.851 (30.00%)

*(Nota: Los equipos 42i antiguos pasaron por el filtro de valores constantes sin presentar outliers por calibración).*

## 5. Medidas de Resumen

A continuación se muestran las estadísticas considerando todas las mediciones (Datos Crudos) versus las mediciones excluyendo outliers y con el tratamiento de ceros aplicado (Datos Validados Finales).

### Thermo 42iQ Sin Sonda

**Datos Crudos (Todas las mediciones)**

| Gas | Mínimo | Máximo | Promedio | Mediana | Desv. Estándar | 
| ----- | ----- | ----- | ----- | ----- | ----- | 
| NO | \-2.1997 | 216.1970 | 7.3015 | 3.5102 | 11.7017 | 
| NO2 | \-20.0517 | 96.0046 | 11.7633 | 7.9482 | 11.3261 | 
| NOx | \-2.7580 | 260.0270 | 19.0647 | 12.1156 | 21.5489 | 

**Datos Validados (Sin outliers y tratamiento de ceros)**

| Gas | Mínimo | Máximo | Promedio | Mediana | Desv. Estándar | 
| ----- | ----- | ----- | ----- | ----- | ----- | 
| NO | 0.0000 | 216.1970 | 7.5173 | 3.5213 | 11.5599 | 
| NO2 | 0.0000 | 96.0046 | 11.7896 | 7.9593 | 11.3121 | 
| NOx | 0.0000 | 260.0270 | 19.1679 | 12.1349 | 21.4734 | 

### Thermo 42iQ Con Sonda

**Datos Crudos (Todas las mediciones)**

| Gas | Mínimo | Máximo | Promedio | Mediana | Desv. Estándar | 
| ----- | ----- | ----- | ----- | ----- | ----- | 
| NO | \-2.3346 | 225.3060 | 3.2868 | \-0.3852 | 9.7637 | 
| NO2 | \-90.3004 | 96.0046 | 8.3303 | 4.2771 | 10.6740 | 
| NOx | \-9.1785 | 260.0270 | 11.6170 | 4.6323 | 18.8827 | 

**Datos Validados (Sin outliers y tratamiento de ceros)**

| Gas | Mínimo | Máximo | Promedio | Mediana | Desv. Estándar | 
| ----- | ----- | ----- | ----- | ----- | ----- | 
| NO | 0.0000 | 225.3060 | 4.1433 | 0.0000 | 9.3537 | 
| NO2 | 0.0000 | 96.0046 | 8.3953 | 4.2874 | 10.6208 | 
| NOx | 0.0000 | 260.0270 | 12.1199 | 4.6443 | 18.5457 | 

### Thermo 42i Sin Sonda

**Datos Crudos / Filtro de Constantes**

| Gas | Mínimo | Máximo | Promedio | Mediana | Desv. Estándar | 
| ----- | ----- | ----- | ----- | ----- | ----- | 
| NO | \-4.5335 | 84.3166 | \-3.2373 | \-3.1506 | 0.7208 | 
| NOx | \-8.6057 | 78.5431 | \-7.3861 | \-7.2937 | 0.7085 | 

### Thermo 42i Con Sonda

**Datos Crudos / Filtro de Constantes**

| Gas | Mínimo | Máximo | Promedio | Mediana | Desv. Estándar | 
| ----- | ----- | ----- | ----- | ----- | ----- | 
| NO | \-10.9753 | 131.1800 | \-3.6376 | \-3.8974 | 2.6229 | 
| NOx | \-15.0230 | 167.3530 | \-7.3773 | \-8.0148 | 5.3925 | 
