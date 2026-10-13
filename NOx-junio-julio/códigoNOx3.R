# 42iQ sin sonda ----

iQ_sinsonda_archivo_julio <- "12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv"

iQ_sinsonda_nombres_orig <- names(read.csv(iQ_sinsonda_archivo_julio, nrows = 0, check.names = FALSE))
iQ_sinsonda_datos_crudos <- read.csv(iQ_sinsonda_archivo_julio, skip = 1, header = FALSE)
colnames(iQ_sinsonda_datos_crudos)[1:length(iQ_sinsonda_nombres_orig)] <- iQ_sinsonda_nombres_orig

columnas_de_interes <- c(
  "Time Stamp",
  "NO Concentration (ppb or ug/m3)",
  "NO2 Concentration (ppb or ug/m3)",
  "NOx Concentration (ppb or ug/m3)"
)
nombres_intuitivos <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")

iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_crudos[, columnas_de_interes]
colnames(iQ_sinsonda_datos_totales) <- nombres_intuitivos
iQ_sinsonda_datos_totales$Fecha_y_Hora <- as.POSIXct(iQ_sinsonda_datos_totales$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# VERIFICACIÓN DE INTEGRIDAD (MAYO Y JUNIO)
iQ_sinsonda_archivo_mayo <- "12218618650_UserData42iQ_2025-05-30_15-24-10_Sinsonda.csv"
iQ_sinsonda_archivo_junio <- "12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv"

# Procesamiento de Mayo
iQ_sinsonda_crudos_mayo <- read.csv(iQ_sinsonda_archivo_mayo, skip = 1, header = FALSE)
colnames(iQ_sinsonda_crudos_mayo)[1:length(iQ_sinsonda_nombres_orig)] <- iQ_sinsonda_nombres_orig
iQ_sinsonda_datos_mayo <- iQ_sinsonda_crudos_mayo[, columnas_de_interes]
colnames(iQ_sinsonda_datos_mayo) <- nombres_intuitivos
iQ_sinsonda_datos_mayo$Fecha_y_Hora <- as.POSIXct(iQ_sinsonda_datos_mayo$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# Procesamiento de Junio
iQ_sinsonda_crudos_junio <- read.csv(iQ_sinsonda_archivo_junio, skip = 1, header = FALSE)
colnames(iQ_sinsonda_crudos_junio)[1:length(iQ_sinsonda_nombres_orig)] <- iQ_sinsonda_nombres_orig
iQ_sinsonda_datos_junio <- iQ_sinsonda_crudos_junio[, columnas_de_interes]
colnames(iQ_sinsonda_datos_junio) <- nombres_intuitivos
iQ_sinsonda_datos_junio$Fecha_y_Hora <- as.POSIXct(iQ_sinsonda_datos_junio$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# Función de comprobación
verificar_inclusion <- function(df_menor, df_mayor) {
  registros_incluidos <- df_menor$Fecha_y_Hora %in% df_mayor$Fecha_y_Hora
  if (all(registros_incluidos)) {
    cat("Confirmado: El 100% de los datos están contenidos en la base mayor.\n")
  } else {
    faltantes <- sum(!registros_incluidos)
    cat(sprintf("Alerta: Faltan %d registros en la base principal.\n", faltantes))
  }
}

cat("\nAnálisis de integridad (Base 05_30 contenida en 07_25):\n")
cat("-------------------------------------------------------\n")
verificar_inclusion(iQ_sinsonda_datos_mayo, iQ_sinsonda_datos_totales)

cat("\nAnálisis de integridad (Base 06_19 contenida en 07_25):\n")
cat("-------------------------------------------------------\n")
verificar_inclusion(iQ_sinsonda_datos_junio, iQ_sinsonda_datos_totales)



# DIAGNÓSTICO PROFUNDO DE NEGATIVOS (SIN SONDA)


reportar_negativos_detallado <- function(df, columna) {
  # Contabilizar el total de datos en el dataframe
  total_filas <- nrow(df)
  
  # Filtrar los valores negativos
  filas_neg <- df[which(df[[columna]] < 0), ]
  n_neg <- nrow(filas_neg)
  
  cat(sprintf("\n--- %s ---\n", columna))
  cat(sprintf("Datos totales en el periodo: %d\n", total_filas))
  
  if (n_neg > 0) {
    porcentaje <- (n_neg / total_filas) * 100
    cat(sprintf("Cantidad de negativos: %d (%.4f%% del total)\n", n_neg, porcentaje))
    
    # Identificar el valor más extremo y su fecha
    indice_minimo <- which.min(filas_neg[[columna]])
    valor_minimo <- filas_neg[[columna]][indice_minimo]
    fecha_minimo <- filas_neg$Fecha_y_Hora[indice_minimo]
    
    cat(sprintf("Valor más negativo (Mínimo): %.5f\n", valor_minimo))
    cat(sprintf("Fecha y hora del mínimo: %s\n", format(fecha_minimo, "%d/%m/%Y %H:%M:%S")))
    
    cat("\nMuestra de los primeros registros negativos:\n")
    print(head(filas_neg[, c("Fecha_y_Hora", columna)], 5))
  } else {
    cat("Cantidad de negativos: 0 (0% del total)\n")
    cat("No hay valores mínimos negativos que reportar.\n")
  }
}

cat("\nReporte Detallado de Negativos en CSV (24/06 - 24/07)\n")
cat("====================================================\n")
reportar_negativos_detallado(iQ_sinsonda_datos_periodo, "Concentracion_NO")
reportar_negativos_detallado(iQ_sinsonda_datos_periodo, "Concentracion_NO2")
reportar_negativos_detallado(iQ_sinsonda_datos_periodo, "Concentracion_NOx")


# SECCIÓN 13: CORRECCIÓN JUSTIFICADA DE NEGATIVOS


# Definimos el umbral estadístico de ruido (límite máximo aceptable)
umbral_ruido <- -3.0

# Función de corrección basada en límites de detección
limpiar_negativos <- function(vector, limite_ruido) {
  # 1. Fallas extremas (< -3.0) se vuelven NA
  vector[vector < limite_ruido & !is.na(vector)] <- NA
  
  # 2. Ruido estadístico normal (entre -3.0 y 0) se trunca a 0
  vector[vector < 0 & vector >= limite_ruido & !is.na(vector)] <- 0
  
  return(vector)
}

# Aplicar la limpieza a tu base sin sonda
datos_validados_sinsonda <- iQ_sinsonda_datos_periodo

datos_validados_sinsonda$Concentracion_NO <- limpiar_negativos(
  datos_validados_sinsonda$Concentracion_NO, umbral_ruido
)
datos_validados_sinsonda$Concentracion_NO2 <- limpiar_negativos(
  datos_validados_sinsonda$Concentracion_NO2, umbral_ruido
)
datos_validados_sinsonda$Concentracion_NOx <- limpiar_negativos(
  datos_validados_sinsonda$Concentracion_NOx, umbral_ruido
)

# Evaluar cuántos datos se perdieron realmente (pasaron a NA)
cat("\nReporte de Datos Invalidados (Fallas < -3.0)\n")
cat("NO  : ", sum(is.na(datos_validados_sinsonda$Concentracion_NO)), " registros descartados.\n")
cat("NO2 : ", sum(is.na(datos_validados_sinsonda$Concentracion_NO2)), " registros descartados.\n")
cat("NOx : ", sum(is.na(datos_validados_sinsonda$Concentracion_NOx)), " registros descartados.\n")



# EXTRACCIÓN PERIODO ESTUDIO SIN SONDA (24 JUNIO - 24 JULIO)
fecha_inicio <- as.POSIXct("24/06/2025 00:00:00", format="%d/%m/%Y %H:%M:%S")
fecha_fin <- as.POSIXct("24/07/2025 23:59:59", format="%d/%m/%Y %H:%M:%S")

datos_validados_sinsonda <- subset(iQ_sinsonda_datos_totales, Fecha_y_Hora >= fecha_inicio & Fecha_y_Hora <= fecha_fin)

datos_validados_sinsonda$Concentracion_NO <- limpiar_negativos(datos_validados_sinsonda$Concentracion_NO, umbral_ruido)
datos_validados_sinsonda$Concentracion_NO2 <- limpiar_negativos(datos_validados_sinsonda$Concentracion_NO2, umbral_ruido)
datos_validados_sinsonda$Concentracion_NOx <- limpiar_negativos(datos_validados_sinsonda$Concentracion_NOx, umbral_ruido)


analizar_falla <- function(valores, limite_minutos = 3) {
  rachas <- rle(valores)
  fallas <- rachas$lengths >= limite_minutos
  
  if(any(fallas)) {
    cat("Advertencia: Se registraron valores constantes.\n")
    print(data.frame(Valor = rachas$values[fallas], 
                     Minutos_Consecutivos = rachas$lengths[fallas]))
  } else {
    cat("Estado normal: Sin anomalías detectadas.\n")
  }
}

cat("\nReporte de parámetros Sin Sonda (Límite: 3 min)\n")
cat("--------------------------------------\n")
cat("NO:\n")
analizar_falla(iQ_sinsonda_datos_periodo$Concentracion_NO)
cat("\nNO2:\n")
analizar_falla(iQ_sinsonda_datos_periodo$Concentracion_NO2)
cat("\nNOx:\n")
analizar_falla(iQ_sinsonda_datos_periodo$Concentracion_NOx)






# 42iQ con sonda ----

iQ_sonda_archivo_08_14 <- "12218618650_UserData42iQ_2025-08-14_12-34-21_Sonda.csv"
iQ_sonda_archivo_08_28 <- "12218618650_UserData42iQ_2025-08-28_23-16-34_Sonda.csv"
iQ_sonda_archivo_10_07 <- "12218618650_UserData42iQ_2025-10-07_13-48-55_Sonda.csv"

iQ_sonda_nombres_orig <- names(read.csv(iQ_sonda_archivo_08_14, nrows = 0, check.names = FALSE))

iQ_sonda_crudos_08_14 <- read.csv(iQ_sonda_archivo_08_14, skip = 1, header = FALSE)
iQ_sonda_crudos_08_28 <- read.csv(iQ_sonda_archivo_08_28, skip = 1, header = FALSE)
iQ_sonda_crudos_10_07 <- read.csv(iQ_sonda_archivo_10_07, skip = 1, header = FALSE)

colnames(iQ_sonda_crudos_08_14)[1:length(iQ_sonda_nombres_orig)] <- iQ_sonda_nombres_orig
colnames(iQ_sonda_crudos_08_28)[1:length(iQ_sonda_nombres_orig)] <- iQ_sonda_nombres_orig
colnames(iQ_sonda_crudos_10_07)[1:length(iQ_sonda_nombres_orig)] <- iQ_sonda_nombres_orig

# PROCESAMIENTO ARCHIVO 08_14
iQ_sonda_datos_08_14 <- iQ_sonda_crudos_08_14[, columnas_de_interes]
colnames(iQ_sonda_datos_08_14) <- nombres_intuitivos
iQ_sonda_datos_08_14$Fecha_y_Hora <- as.POSIXct(iQ_sonda_datos_08_14$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# PROCESAMIENTO ARCHIVO 08_28
iQ_sonda_datos_08_28 <- iQ_sonda_crudos_08_28[, columnas_de_interes]
colnames(iQ_sonda_datos_08_28) <- nombres_intuitivos
iQ_sonda_datos_08_28$Fecha_y_Hora <- as.POSIXct(iQ_sonda_datos_08_28$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# PROCESAMIENTO ARCHIVO 10_07
iQ_sonda_datos_10_07 <- iQ_sonda_crudos_10_07[, columnas_de_interes]
colnames(iQ_sonda_datos_10_07) <- nombres_intuitivos
iQ_sonda_datos_10_07$Fecha_y_Hora <- as.POSIXct(iQ_sonda_datos_10_07$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

cat("\nAnálisis de integridad (08_14 contenido en 08_28):\n")
cat("--------------------------------------------------\n")
verificar_inclusion(iQ_sonda_datos_08_14, iQ_sonda_datos_08_28)

# EXTRACCIÓN PERIODO ESTUDIO CON SONDA (25 JUNIO - 25 JULIO)
iQ_sonda_datos_periodo <- subset(iQ_sonda_datos_08_28, Fecha_y_Hora >= fecha_inicio & Fecha_y_Hora <= fecha_fin)

cat("\nReporte de parámetros Con Sonda (Límite: 3 min)\n")
cat("-----------------------------------------------\n")
cat("NO:\n")
analizar_falla(iQ_sonda_datos_periodo$Concentracion_NO)
cat("\nNO2:\n")
analizar_falla(iQ_sonda_datos_periodo$Concentracion_NO2)
cat("\nNOx:\n")
analizar_falla(iQ_sonda_datos_periodo$Concentracion_NOx)



# Igualdad de datos en csv con y sin sonda (24/06 - 24/07) ----


# 1. Definir el nuevo periodo estricto de prueba
fecha_inicio_prueba <- as.POSIXct("24/06/2025 00:00:00", format="%d/%m/%Y %H:%M:%S")
fecha_fin_prueba <- as.POSIXct("24/07/2025 23:59:59", format="%d/%m/%Y %H:%M:%S")

# 2. Extraer los datos de este periodo exacto desde las bases totales
test_sinsonda <- subset(iQ_sinsonda_datos_totales, Fecha_y_Hora >= fecha_inicio_prueba & Fecha_y_Hora <= fecha_fin_prueba)
test_sonda <- subset(iQ_sonda_datos_08_28, Fecha_y_Hora >= fecha_inicio_prueba & Fecha_y_Hora <= fecha_fin_prueba)

cat("\nAnálisis de similitud (Sin Sonda vs Con Sonda) - [24/06 - 24/07]\n")
cat("----------------------------------------------------------------\n")

# 3. Verificar cantidad de registros
filas_sin <- nrow(test_sinsonda)
filas_con <- nrow(test_sonda)
cat(sprintf("Registros Sin Sonda: %d\n", filas_sin))
cat(sprintf("Registros Con Sonda: %d\n", filas_con))

# 4. Comparación matemática valor por valor
if (filas_sin == filas_con && filas_sin > 0) {
  dif_NO  <- sum(test_sinsonda$Concentracion_NO != test_sonda$Concentracion_NO, na.rm = TRUE)
  dif_NO2 <- sum(test_sinsonda$Concentracion_NO2 != test_sonda$Concentracion_NO2, na.rm = TRUE)
  dif_NOx <- sum(test_sinsonda$Concentracion_NOx != test_sonda$Concentracion_NOx, na.rm = TRUE)
  
  cat("\nDiferencias numéricas encontradas:\n")
  cat(sprintf("NO : %d datos distintos.\n", dif_NO))
  cat(sprintf("NO2: %d datos distintos.\n", dif_NO2))
  cat(sprintf("NOx: %d datos distintos.\n", dif_NOx))
  
  if (dif_NO == 0 && dif_NO2 == 0 && dif_NOx == 0) {
    cat("\nLos datos son 100% idénticos en este periodo.\n")
  }
} else {
  cat("\nNota: Las bases siguen teniendo distinta cantidad de registros o no hay datos en este corte.\n")
}





# 42i sin sonda -------------------------####################################

#lectura 3 bases ----

# 1. Definir los tres archivos "Sinsonda" de la carpeta
archivos_dat_sinsonda <- c("NOx_42i_07oct_PO_Sinsonda.dat", 
                           "NOx_42i_Portillo_14_Agosto_2025_Sinsonda.dat",
                           "NOx_42i_Portillo_28_agosto_2025_Sinsonda.dat")

# 2. Función para procesar cada archivo
leer_y_procesar_dat_sinsonda <- function(archivo) {
  datos_crudos <- read.table(archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
  datos_interes <- datos_crudos[, c("Time", "Date", "no", "nox")]
  
  # Formatear fecha y hora
  datos_interes$Fecha_y_Hora <- as.POSIXct(
    paste(datos_interes$Date, datos_interes$Time), 
    format = "%m-%d-%y %H:%M"
  )
  
  datos_final <- datos_interes[, c("Fecha_y_Hora", "no", "nox")]
  colnames(datos_final) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NOx")
  
  return(datos_final)
}

# 3. Leer los archivos y unirlos en una sola base
lista_datos_sinsonda <- lapply(archivos_dat_sinsonda, leer_y_procesar_dat_sinsonda)
i_sinsonda_datos_totales <- do.call(rbind, lista_datos_sinsonda)

# 1. CORRECCIÓN DE TRASLAPES (SOLAPAMIENTO)

# Ordenar toda la base combinada cronológicamente de principio a fin
i_sinsonda_datos_totales <- i_sinsonda_datos_totales[order(i_sinsonda_datos_totales$Fecha_y_Hora), ]

# Eliminar duplicados forzando que solo exista un dato por cada Fecha_y_Hora exacta
i_sinsonda_datos_totales <- i_sinsonda_datos_totales[!duplicated(i_sinsonda_datos_totales$Fecha_y_Hora), ]

# Imprimir en consola la cantidad real de datos únicos
cat(sprintf("Base de datos unificada y sin duplicados de tiempo: %d registros únicos.\n", nrow(i_sinsonda_datos_totales)))


# análisis valores constantes -----

cat("\nReporte de parámetros Base .DAT Sin Sonda (Límite: 3 min)\n")
cat("-----------------------------------------------\n")

cat("NO:\n")
analizar_falla(i_sinsonda_datos_periodo$Concentracion_NO)

cat("\nNOx:\n")
analizar_falla(i_sinsonda_datos_periodo$Concentracion_NOx)




###############################################################################




# 42i con sonda -------------------------####################################

#lectura de los 3 archivos ----

archivos_dat_sonda <- c("42i 053025 1425_Sonda.dat", 
                        "NOx_42i_Portillo_junio_2025_Sonda.dat", 
                        "NOx_42i_julio_Portillo_Sonda.dat")

# 2. Leer y combinar los 3 archivos en un solo dataframe automáticamente
lista_datos <- lapply(archivos_dat_sonda, function(archivo) {
  read.table(archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
})
i_sonda_datos_crudos <- do.call(rbind, lista_datos)

# 3. Limpiar y formatear fechas
i_sonda_datos_interes <- i_sonda_datos_crudos[, c("Time", "Date", "no", "nox")]
i_sonda_datos_interes$Fecha_y_Hora <- as.POSIXct(
  paste(i_sonda_datos_interes$Date, i_sonda_datos_interes$Time), 
  format = "%m-%d-%y %H:%M"
)

# 4. Crear base final con columnas renombradas
i_sonda_datos_final <- i_sonda_datos_interes[, c("Fecha_y_Hora", "no", "nox")]
colnames(i_sonda_datos_final) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NOx")

# Ordenar cronológicamente por si los archivos se cargaron desordenados
i_sonda_datos_final <- i_sonda_datos_final[order(i_sonda_datos_final$Fecha_y_Hora), ]



# extraccion de periodo ----


# 1. Definir el periodo estricto de prueba
fecha_inicio_dat <- as.POSIXct("24/06/2025 00:00:00", format="%d/%m/%Y %H:%M:%S")
fecha_fin_dat <- as.POSIXct("24/07/2025 23:59:59", format="%d/%m/%Y %H:%M:%S")

# 2. Extraer los datos de este periodo
i_sonda_datos_periodo <- subset(i_sonda_datos_final, Fecha_y_Hora >= fecha_inicio_dat & Fecha_y_Hora <= fecha_fin_dat)

# 3. Cálculos de volumen de datos
total_datos_crudos <- nrow(i_sonda_datos_final)
total_datos_periodo <- nrow(i_sonda_datos_periodo)
porcentaje_uso <- (total_datos_periodo / total_datos_crudos) * 100

# 4. Imprimir Reporte de Conteo
cat("\n==============================================\n")
cat("   REPORTE DE VOLUMEN DE DATOS (.DAT CON SONDA)   \n")
cat("==============================================\n")
cat(sprintf("Total de registros en los 3 archivos: %d datos\n", total_datos_crudos))
cat(sprintf("Registros en el periodo (24 Jun - 24 Jul): %d datos\n", total_datos_periodo))
cat(sprintf("Porcentaje del total a utilizar: %.2f%%\n", porcentaje_uso))
cat("==============================================\n\n")



# Análisis de valores constantes ----


analizar_falla <- function(valores, limite_minutos = 3) {
  valores <- na.omit(valores) 
  
  if(length(valores) == 0) {
    cat("No hay datos válidos en este periodo.\n")
    return()
  }
  
  rachas <- rle(valores)
  fallas <- rachas$lengths >= limite_minutos
  
  if(any(fallas)) {
    cat("Advertencia: Se registraron valores constantes.\n")
    print(data.frame(Valor = rachas$values[fallas], 
                     Minutos_Consecutivos = rachas$lengths[fallas]))
  } else {
    cat("Estado normal: Sin anomalías detectadas.\n")
  }
}

# Ejecutar análisis
cat("Reporte de parámetros Base .DAT (Límite: 3 min)\n")
cat("-----------------------------------------------\n")

cat("NO:\n")
analizar_falla(i_sonda_datos_periodo$Concentracion_NO)

cat("\nNOx:\n")
analizar_falla(i_sonda_datos_periodo$Concentracion_NOx)






###############################################################################


# COMPARACIÓN DE PATRONES NORMALIZADOS ----

#Función para normalizar entre 0 y 1

normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

datos_validados_sinsonda$NO_norm <- normalizar(datos_validados_sinsonda$Concentracion_NO)
i_sonda_datos_periodo$NO_norm <- normalizar(i_sonda_datos_periodo$Concentracion_NO)

plot(x = datos_validados_sinsonda$Fecha_y_Hora, y = datos_validados_sinsonda$NO_norm, 
     type = "l", col = adjustcolor("blue", alpha.f = 0.6), lwd = 2,
     main = "Comparación de Patrones (Escala Normalizada 0 a 1)", 
     xlab = "Fecha y Hora", ylab = "Concentración Relativa")

lines(x = i_sonda_datos_periodo$Fecha_y_Hora, y = i_sonda_datos_periodo$NO_norm, 
      col = adjustcolor("red", alpha.f = 0.6), lwd = 1.5)

legend("topright", legend = c("Sin Sonda (CSV Validado)", "Con Sonda (DAT)"), 
       col = c("blue", "red"), lty = 1, lwd = 2, cex = 0.8)


#Gráficos (plots) Comparaciones ----

#COMPARACIÓN DE DATOS CRUDOS VS FILTRADOS (NO)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

plot(x = iQ_sinsonda_datos_periodo$Fecha_y_Hora, 
     y = iQ_sinsonda_datos_periodo$Concentracion_NO, 
     type = "l", col = "gray50", lwd = 1.2,
     main = "Concentración de NO: Datos Crudos (Sin Filtro)", 
     xlab = "", ylab = "NO Crudo")

abline(h = 0, col = "red", lty = 2)      # Línea del cero ideal
abline(h = -3.0, col = "darkred", lty = 3) # Línea del umbral de invalidación (-3.0)

plot(x = datos_validados_sinsonda$Fecha_y_Hora, 
     y = datos_validados_sinsonda$Concentracion_NO, 
     type = "l", col = "blue", lwd = 1.2,
     main = "Concentración de NO: Datos Validados (Umbral -3.0)", 
     xlab = "Fecha y Hora", ylab = "NO Validado")

abline(h = 0, col = "red", lty = 2)
par(mfrow = c(1, 1))


#GRÁFICO COMPARATIVO 42iQ sin sonda (validado) vs 42i con sonda (.DAT)

# Ajuste de escala para el eje Y
rango_NO  <- range(c(datos_validados_sinsonda$Concentracion_NO, i_sonda_datos_periodo$Concentracion_NO), na.rm = TRUE)
rango_NOx <- range(c(datos_validados_sinsonda$Concentracion_NOx, i_sonda_datos_periodo$Concentracion_NOx), na.rm = TRUE)

color_sin_sonda <- adjustcolor("blue", alpha.f = 0.6)
color_con_sonda <- adjustcolor("red", alpha.f = 0.6)

# 1. Gráfico estático para NO
plot(x = datos_validados_sinsonda$Fecha_y_Hora, y = datos_validados_sinsonda$Concentracion_NO, 
     type = "l", col = color_sin_sonda, ylim = rango_NO, lwd = 2,
     main = "Comparación NO: CSV Validado vs DAT (24/06 - 24/07)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NO")
lines(x = i_sonda_datos_periodo$Fecha_y_Hora, y = i_sonda_datos_periodo$Concentracion_NO, col = color_con_sonda, lwd = 1)
legend("topright", legend = c("Sin Sonda (Validado)", "Con Sonda (DAT)"), col = c("blue", "red"), lty = 1, lwd = c(2, 1), cex = 0.8)

# 2. Gráfico estático para NOx
plot(x = datos_validados_sinsonda$Fecha_y_Hora, y = datos_validados_sinsonda$Concentracion_NOx, 
     type = "l", col = color_sin_sonda, ylim = rango_NOx, lwd = 2,
     main = "Comparación NOx: CSV Validado vs DAT (24/06 - 24/07)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NOx")
lines(x = i_sonda_datos_periodo$Fecha_y_Hora, y = i_sonda_datos_periodo$Concentracion_NOx, col = color_con_sonda, lwd = 1)
legend("topright", legend = c("Sin Sonda (Validado)", "Con Sonda (DAT)"), col = c("blue", "red"), lty = 1, lwd = c(2, 1), cex = 0.8)

# Añadir línea de referencia estricta en el cero
abline(h = 0, col = "black", lty = 2, lwd = 1.5)




# Gráfica .DAT sin sonda


# 1. Gráfico Estático
plot(x = i_sonda_datos_periodo$Fecha_y_Hora, 
     y = i_sonda_datos_periodo$Concentracion_NO, 
     type = "l", col = "darkgreen", lwd = 1.5,
     main = "Evaluación de Desfase: NO (DAT con Sonda)", 
     xlab = "Fecha y Hora", ylab = "Concentración NO")

# Añadir la línea de referencia en Y = 0
abline(h = 0, col = "red", lty = 2, lwd = 2)




#Gráficos (ploty) ----

library(plotly)

# 1. Gráfico interactivo para NO
grafico_NO_val <- plot_ly() %>%
  add_lines(x = ~datos_validados_sinsonda$Fecha_y_Hora, y = ~datos_validados_sinsonda$Concentracion_NO, 
            name = 'Sin Sonda (Validado)', line = list(color = 'rgba(0, 0, 255, 0.6)', width = 2)) %>%
  add_lines(x = ~i_sonda_datos_periodo$Fecha_y_Hora, y = ~i_sonda_datos_periodo$Concentracion_NO, 
            name = 'Con Sonda (DAT)', line = list(color = 'rgba(255, 0, 0, 0.6)', width = 1)) %>%
  layout(title = "Comparación Interactiva NO (CSV Validado vs DAT)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NO"))

# 2. Gráfico interactivo para NOx
grafico_NOx_val <- plot_ly() %>%
  add_lines(x = ~datos_validados_sinsonda$Fecha_y_Hora, y = ~datos_validados_sinsonda$Concentracion_NOx, 
            name = 'Sin Sonda (Validado)', line = list(color = 'rgba(0, 0, 255, 0.6)', width = 2)) %>%
  add_lines(x = ~i_sonda_datos_periodo$Fecha_y_Hora, y = ~i_sonda_datos_periodo$Concentracion_NOx, 
            name = 'Con Sonda (DAT)', line = list(color = 'rgba(255, 0, 0, 0.6)', width = 1)) %>%
  layout(title = "Comparación Interactiva NOx (CSV Validado vs DAT)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NOx"))

# Mostrar gráficos
grafico_NO_val
grafico_NOx_val



# 2. Gráfico Interactivo (Recomendado para ver la forma de las fluctuaciones)
library(plotly)

grafico_desfase_NO <- plot_ly() %>%
  add_lines(x = ~i_sonda_datos_periodo$Fecha_y_Hora, 
            y = ~i_sonda_datos_periodo$Concentracion_NO, 
            name = 'NO con Sonda (DAT)', 
            line = list(color = 'darkgreen', width = 1.5)) %>%
  add_segments(x = min(i_sonda_datos_periodo$Fecha_y_Hora, na.rm = TRUE), 
               xend = max(i_sonda_datos_periodo$Fecha_y_Hora, na.rm = TRUE), 
               y = 0, yend = 0, 
               name = 'Línea de Referencia (Cero)', 
               line = list(color = 'red', dash = 'dash', width = 2)) %>%
  layout(title = "Evaluación de Desfase: NO (DAT con Sonda)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración NO"))

# Mostrar el gráfico interactivo
grafico_desfase_NO


# ANÁLISIS ESTADÍSTICO (NO y NOx) -----


# 1. Alinear las bases de datos para NO y NOx simultáneamente
datos_alineados <- merge(
  x = iQ_sinsonda_datos_periodo[, c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NOx")],
  y = i_sonda_datos_periodo[, c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NOx")],
  by = "Fecha_y_Hora",
  suffixes = c("_SinSonda", "_ConSonda")
)

# 2. Modelos de Regresión Lineal (Con Sonda en función de Sin Sonda)
modelo_NO <- lm(Concentracion_NO_ConSonda ~ Concentracion_NO_SinSonda, data = datos_alineados)
modelo_NOx <- lm(Concentracion_NOx_ConSonda ~ Concentracion_NOx_SinSonda, data = datos_alineados)

# Extraer coeficientes y formatear las ecuaciones de la recta (y = mx + b)
coef_NO <- coef(modelo_NO)
ecuacion_NO <- sprintf("y = %.4fx %s %.4f", coef_NO[2], ifelse(coef_NO[1] >= 0, "+", "-"), abs(coef_NO[1]))

coef_NOx <- coef(modelo_NOx)
ecuacion_NOx <- sprintf("y = %.4fx %s %.4f", coef_NOx[2], ifelse(coef_NOx[1] >= 0, "+", "-"), abs(coef_NOx[1]))

# Calcular correlaciones de Pearson
cor_NO <- cor(datos_alineados$Concentracion_NO_SinSonda, datos_alineados$Concentracion_NO_ConSonda, use = "complete.obs")
cor_NOx <- cor(datos_alineados$Concentracion_NOx_SinSonda, datos_alineados$Concentracion_NOx_ConSonda, use = "complete.obs")

cat(sprintf("Correlación Pearson NO : %.3f\n", cor_NO))
cat(sprintf("Correlación Pearson NOx: %.3f\n", cor_NOx))

# 3. Configurar la ventana gráfica para mostrar 2 gráficos lado a lado (1 fila, 2 columnas)
par(mfrow = c(1, 2))

# 4. Gráfico de dispersión para NO
plot(x = datos_alineados$Concentracion_NO_SinSonda, 
     y = datos_alineados$Concentracion_NO_ConSonda,
     pch = 16, col = adjustcolor("purple", alpha.f = 0.3),
     main = paste("NO (r =", round(cor_NO, 3), ")"),
     xlab = "NO Sin Sonda", ylab = "NO Con Sonda")
abline(modelo_NO, col = "red", lwd = 2)
# Añadir la ecuación en la esquina superior izquierda
legend("topleft", legend = ecuacion_NO, text.col = "red", bty = "n", cex = 0.9)

# 5. Gráfico de dispersión para NOx
plot(x = datos_alineados$Concentracion_NOx_SinSonda, 
     y = datos_alineados$Concentracion_NOx_ConSonda,
     pch = 16, col = adjustcolor("darkorange", alpha.f = 0.3),
     main = paste("NOx (r =", round(cor_NOx, 3), ")"),
     xlab = "NOx Sin Sonda", ylab = "NOx Con Sonda")
abline(modelo_NOx, col = "red", lwd = 2)
# Añadir la ecuación en la esquina superior izquierda
legend("topleft", legend = ecuacion_NOx, text.col = "red", bty = "n", cex = 0.9)

# 6. Restaurar la configuración gráfica a 1 solo panel para futuros gráficos
par(mfrow = c(1, 1))

