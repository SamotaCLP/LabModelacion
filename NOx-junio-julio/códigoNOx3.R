# ==========================================
# SECCIÓN 1: DATOS SIN SONDA
# ==========================================

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

# GRÁFICO HISTÓRICO SIN SONDA
plot(x = iQ_sinsonda_datos_totales$Fecha_y_Hora, y = iQ_sinsonda_datos_totales$Concentracion_NO, 
     type = "l", col = "blue", lwd = 1.5, 
     main = "Concentración de NO (Sin Sonda - Histórico)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NO")

# EXTRACCIÓN PERIODO ESTUDIO SIN SONDA (25 JUNIO - 25 JULIO)
fecha_inicio <- as.POSIXct("25/06/2025 00:00:00", format="%d/%m/%Y %H:%M:%S")
fecha_fin <- as.POSIXct("25/07/2025 23:59:59", format="%d/%m/%Y %H:%M:%S")

iQ_sinsonda_datos_periodo <- subset(iQ_sinsonda_datos_totales, Fecha_y_Hora >= fecha_inicio & Fecha_y_Hora <= fecha_fin)

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








# ==========================================
# SECCIÓN 2: DATOS CON SONDA
# ==========================================

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








# ==========================================
# SECCIÓN 3: GRÁFICOS COMPARATIVOS
# ==========================================

rango_NO  <- range(c(iQ_sinsonda_datos_periodo$Concentracion_NO, iQ_sonda_datos_periodo$Concentracion_NO), na.rm = TRUE)
rango_NO2 <- range(c(iQ_sinsonda_datos_periodo$Concentracion_NO2, iQ_sonda_datos_periodo$Concentracion_NO2), na.rm = TRUE)
rango_NOx <- range(c(iQ_sinsonda_datos_periodo$Concentracion_NOx, iQ_sonda_datos_periodo$Concentracion_NOx), na.rm = TRUE)

color_sin_sonda <- adjustcolor("blue", alpha.f = 0.6)
color_con_sonda <- adjustcolor("red", alpha.f = 0.6)

# 1. Gráfico para NO
plot(x = iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = iQ_sinsonda_datos_periodo$Concentracion_NO, 
     type = "l", col = color_sin_sonda, ylim = rango_NO, lwd = 2,
     main = "Comparación NO (25/06 - 25/07)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NO")
lines(x = iQ_sonda_datos_periodo$Fecha_y_Hora, y = iQ_sonda_datos_periodo$Concentracion_NO, col = color_con_sonda, lwd = 1)
legend("topright", legend = c("Sin Sonda", "Con Sonda"), col = c("blue", "red"), lty = 1, lwd = c(2, 1), cex = 0.8)

# 2. Gráfico para NO2
plot(x = iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = iQ_sinsonda_datos_periodo$Concentracion_NO2, 
     type = "l", col = color_sin_sonda, ylim = rango_NO2, lwd = 2,
     main = "Comparación NO2 (25/06 - 25/07)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NO2")
lines(x = iQ_sonda_datos_periodo$Fecha_y_Hora, y = iQ_sonda_datos_periodo$Concentracion_NO2, col = color_con_sonda, lwd = 1)
legend("topright", legend = c("Sin Sonda", "Con Sonda"), col = c("blue", "red"), lty = 1, lwd = c(2, 1), cex = 0.8)

# 3. Gráfico para NOx
plot(x = iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = iQ_sinsonda_datos_periodo$Concentracion_NOx, 
     type = "l", col = color_sin_sonda, ylim = rango_NOx, lwd = 2,
     main = "Comparación NOx (25/06 - 25/07)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NOx")
lines(x = iQ_sonda_datos_periodo$Fecha_y_Hora, y = iQ_sonda_datos_periodo$Concentracion_NOx, col = color_con_sonda, lwd = 1)
legend("topright", legend = c("Sin Sonda", "Con Sonda"), col = c("blue", "red"), lty = 1, lwd = c(2, 1), cex = 0.8)

# ==========================================
# SECCIÓN 4: GRÁFICOS INTERACTIVOS (PLOTLY)
# ==========================================
library(plotly)

# 1. Gráfico interactivo para NO
grafico_NO <- plot_ly() %>%
  add_lines(x = ~iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sinsonda_datos_periodo$Concentracion_NO, 
            name = 'Sin Sonda', line = list(color = 'rgba(0, 0, 255, 0.6)', width = 2)) %>%
  add_lines(x = ~iQ_sonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sonda_datos_periodo$Concentracion_NO, 
            name = 'Con Sonda', line = list(color = 'rgba(255, 0, 0, 0.6)', width = 1)) %>%
  layout(title = "Comparación NO (25/06 - 25/07)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NO"))

# 2. Gráfico interactivo para NO2
grafico_NO2 <- plot_ly() %>%
  add_lines(x = ~iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sinsonda_datos_periodo$Concentracion_NO2, 
            name = 'Sin Sonda', line = list(color = 'rgba(0, 0, 255, 0.6)', width = 2)) %>%
  add_lines(x = ~iQ_sonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sonda_datos_periodo$Concentracion_NO2, 
            name = 'Con Sonda', line = list(color = 'rgba(255, 0, 0, 0.6)', width = 1)) %>%
  layout(title = "Comparación NO2 (25/06 - 25/07)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NO2"))

# 3. Gráfico interactivo para NOx
grafico_NOx <- plot_ly() %>%
  add_lines(x = ~iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sinsonda_datos_periodo$Concentracion_NOx, 
            name = 'Sin Sonda', line = list(color = 'rgba(0, 0, 255, 0.6)', width = 2)) %>%
  add_lines(x = ~iQ_sonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sonda_datos_periodo$Concentracion_NOx, 
            name = 'Con Sonda', line = list(color = 'rgba(255, 0, 0, 0.6)', width = 1)) %>%
  layout(title = "Comparación NOx (25/06 - 25/07)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NOx"))

# Mostrar gráficos (ejecutar uno a la vez en la consola para visualizarlos en la pestaña Viewer)
grafico_NO
grafico_NO2
grafico_NOx

# ==========================================
# SECCIÓN 5: DIAGNÓSTICO DE IGUALDAD DE DATOS (24/06 - 24/07)
# ==========================================

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
    cat("\nALERTA CRÍTICA: Los datos son 100% idénticos en este periodo.\n")
  }
} else {
  cat("\nNota: Las bases siguen teniendo distinta cantidad de registros o no hay datos en este corte.\n")
}







# LECTURA DE ARCHIVO .DAT
i_sonda_archivo <- "NOx_42i_julio_Portillo_Sonda.dat"
i_sonda_datos_crudos <- read.table(i_sonda_archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
i_sonda_datos_interes <- i_sonda_datos_crudos[, c("Time", "Date", "no", "nox")]
i_sonda_datos_interes$Fecha_y_Hora <- as.POSIXct(
  paste(i_sonda_datos_interes$Date, i_sonda_datos_interes$Time), 
  format = "%m-%d-%y %H:%M"
)
i_sonda_datos_final <- i_sonda_datos_interes[, c("Fecha_y_Hora", "no", "nox")]
colnames(i_sonda_datos_final) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NOx")



# ==========================================
# SECCIÓN 6: ANÁLISIS DE DATOS .DAT (24 JUNIO - 24 JULIO)
# ==========================================

# 1. Definir el periodo estricto de prueba
fecha_inicio_dat <- as.POSIXct("24/06/2025 00:00:00", format="%d/%m/%Y %H:%M:%S")
fecha_fin_dat <- as.POSIXct("24/07/2025 23:59:59", format="%d/%m/%Y %H:%M:%S")

# 2. Extraer los datos de este periodo
i_sonda_datos_periodo <- subset(i_sonda_datos_final, Fecha_y_Hora >= fecha_inicio_dat & Fecha_y_Hora <= fecha_fin_dat)

# (Reutilizamos la función por si limpiaste el entorno, si ya la tienes puedes omitir este bloque)
analizar_falla <- function(valores, limite_minutos = 3) {
  # Omitimos los NAs (valores vacíos) para que no interfieran en el conteo de rachas
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

# 3. Ejecutar análisis (Nota: Se omite NO2 porque no viene en el archivo .dat)
cat("\nReporte de parámetros Base .DAT (Límite: 3 min)\n")
cat("-----------------------------------------------\n")

cat("NO:\n")
analizar_falla(i_sonda_datos_periodo$Concentracion_NO)

cat("\nNOx:\n")
analizar_falla(i_sonda_datos_periodo$Concentracion_NOx)




# ==========================================
# SECCIÓN 7: GRÁFICOS COMPARATIVOS ESTÁNDAR
# ==========================================

# Ajuste de escala para el eje Y
rango_NO  <- range(c(iQ_sinsonda_datos_periodo$Concentracion_NO, i_sonda_datos_periodo$Concentracion_NO), na.rm = TRUE)
rango_NOx <- range(c(iQ_sinsonda_datos_periodo$Concentracion_NOx, i_sonda_datos_periodo$Concentracion_NOx), na.rm = TRUE)

color_sin_sonda <- adjustcolor("blue", alpha.f = 0.6)
color_con_sonda <- adjustcolor("red", alpha.f = 0.6)

# 1. Gráfico para NO
plot(x = iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = iQ_sinsonda_datos_periodo$Concentracion_NO, 
     type = "l", col = color_sin_sonda, ylim = rango_NO, lwd = 2,
     main = "Comparación NO: Sin Sonda vs DAT (24/06 - 24/07)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NO")
lines(x = i_sonda_datos_periodo$Fecha_y_Hora, y = i_sonda_datos_periodo$Concentracion_NO, col = color_con_sonda, lwd = 1)
legend("topright", legend = c("Sin Sonda (CSV)", "Con Sonda (DAT)"), col = c("blue", "red"), lty = 1, lwd = c(2, 1), cex = 0.8)

# 2. Gráfico para NOx
plot(x = iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = iQ_sinsonda_datos_periodo$Concentracion_NOx, 
     type = "l", col = color_sin_sonda, ylim = rango_NOx, lwd = 2,
     main = "Comparación NOx: Sin Sonda vs DAT (24/06 - 24/07)", 
     xlab = "Fecha y Hora", ylab = "Concentración de NOx")
lines(x = i_sonda_datos_periodo$Fecha_y_Hora, y = i_sonda_datos_periodo$Concentracion_NOx, col = color_con_sonda, lwd = 1)
legend("topright", legend = c("Sin Sonda (CSV)", "Con Sonda (DAT)"), col = c("blue", "red"), lty = 1, lwd = c(2, 1), cex = 0.8)


# ==========================================
# SECCIÓN 8: GRÁFICOS INTERACTIVOS (PLOTLY)
# ==========================================
library(plotly)

# 1. Gráfico interactivo para NO
grafico_dat_NO <- plot_ly() %>%
  add_lines(x = ~iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sinsonda_datos_periodo$Concentracion_NO, 
            name = 'Sin Sonda (CSV)', line = list(color = 'rgba(0, 0, 255, 0.6)', width = 2)) %>%
  add_lines(x = ~i_sonda_datos_periodo$Fecha_y_Hora, y = ~i_sonda_datos_periodo$Concentracion_NO, 
            name = 'Con Sonda (DAT)', line = list(color = 'rgba(255, 0, 0, 0.6)', width = 1)) %>%
  layout(title = "Comparación Interactiva NO (24/06 - 24/07)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NO"))

# 2. Gráfico interactivo para NOx
grafico_dat_NOx <- plot_ly() %>%
  add_lines(x = ~iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = ~iQ_sinsonda_datos_periodo$Concentracion_NOx, 
            name = 'Sin Sonda (CSV)', line = list(color = 'rgba(0, 0, 255, 0.6)', width = 2)) %>%
  add_lines(x = ~i_sonda_datos_periodo$Fecha_y_Hora, y = ~i_sonda_datos_periodo$Concentracion_NOx, 
            name = 'Con Sonda (DAT)', line = list(color = 'rgba(255, 0, 0, 0.6)', width = 1)) %>%
  layout(title = "Comparación Interactiva NOx (24/06 - 24/07)",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NOx"))

# Mostrar gráficos
grafico_dat_NO
grafico_dat_NOx


# ==========================================
# SECCIÓN 9: EVALUACIÓN DE DERIVA DE CERO (.DAT)
# ==========================================

# 1. Gráfico Estático
plot(x = i_sonda_datos_periodo$Fecha_y_Hora, 
     y = i_sonda_datos_periodo$Concentracion_NO, 
     type = "l", col = "darkgreen", lwd = 1.5,
     main = "Evaluación de Desfase: NO (DAT con Sonda)", 
     xlab = "Fecha y Hora", ylab = "Concentración NO")

# Añadir la línea de referencia en Y = 0
abline(h = 0, col = "red", lty = 2, lwd = 2)

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

# ==========================================
# SECCIÓN 10: COMPARACIÓN DE PATRONES NORMALIZADOS
# ==========================================

# 1. Función matemática para normalizar entre 0 y 1
normalizar <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# 2. Crear nuevas columnas con los datos escalados
iQ_sinsonda_datos_periodo$NO_norm <- normalizar(iQ_sinsonda_datos_periodo$Concentracion_NO)
i_sonda_datos_periodo$NO_norm <- normalizar(i_sonda_datos_periodo$Concentracion_NO)

# 3. Gráfico comparativo de las formas
plot(x = iQ_sinsonda_datos_periodo$Fecha_y_Hora, y = iQ_sinsonda_datos_periodo$NO_norm, 
     type = "l", col = adjustcolor("blue", alpha.f = 0.6), lwd = 2,
     main = "Comparación de Patrones (Escala Normalizada 0 a 1)", 
     xlab = "Fecha y Hora", ylab = "Concentración Relativa")

lines(x = i_sonda_datos_periodo$Fecha_y_Hora, y = i_sonda_datos_periodo$NO_norm, 
      col = adjustcolor("red", alpha.f = 0.6), lwd = 1.5)

legend("topright", legend = c("Sin Sonda (CSV)", "Con Sonda (DAT)"), 
       col = c("blue", "red"), lty = 1, lwd = 2, cex = 0.8)

