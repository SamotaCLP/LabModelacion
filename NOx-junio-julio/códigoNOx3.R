# LECTURA DE DATOS
archivo_julio <- "12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv"

nombres_originales <- names(read.csv(archivo_julio, nrows = 0, check.names = FALSE))
datos_crudos <- read.csv(archivo_julio, skip = 1, header = FALSE)
colnames(datos_crudos)[1:length(nombres_originales)] <- nombres_originales

columnas_de_interes <- c(
  "Time Stamp",
  "NO Concentration (ppb or ug/m3)",
  "NO2 Concentration (ppb or ug/m3)",
  "NOx Concentration (ppb or ug/m3)"
)

datos_totales <- datos_crudos[, columnas_de_interes]
colnames(datos_totales) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
datos_totales$Fecha_y_Hora <- as.POSIXct(datos_totales$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# GRÁFICO HISTÓRICO
plot(x = datos_totales$Fecha_y_Hora, y = datos_totales$Concentracion_NO, 
     type = "l", col = "blue", lwd = 1.5, 
     main = "Concentración de NO", 
     xlab = "Fecha y Hora", ylab = "Concentración de NO")

# ANÁLISIS DE DATOS (25 JUNIO - 25 JULIO)
fecha_inicio <- as.POSIXct("25/06/2025 00:00:00", format="%d/%m/%Y %H:%M:%S")
fecha_fin <- as.POSIXct("25/07/2025 23:59:59", format="%d/%m/%Y %H:%M:%S")

datos_periodo <- subset(datos_totales, Fecha_y_Hora >= fecha_inicio & Fecha_y_Hora <= fecha_fin)

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

cat("\nReporte de parámetros (Límite: 3 min)\n")
cat("--------------------------------------\n")

cat("NO:\n")
analizar_falla(datos_periodo$Concentracion_NO)

cat("\nNO2:\n")
analizar_falla(datos_periodo$Concentracion_NO2)

cat("\nNOx:\n")
analizar_falla(datos_periodo$Concentracion_NOx)