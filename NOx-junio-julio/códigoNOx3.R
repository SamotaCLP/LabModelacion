# LECTURA DE DATOS PRINCIPALES (JULIO)
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

# VERIFICACIÓN DE INTEGRIDAD (MAYO Y JUNIO)
archivo_mayo <- "12218618650_UserData42iQ_2025-05-30_15-24-10_Sinsonda.csv"
archivo_junio <- "12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv"

# Procesamiento de Mayo
datos_crudos_mayo <- read.csv(archivo_mayo, skip = 1, header = FALSE)
colnames(datos_crudos_mayo)[1:length(nombres_originales)] <- nombres_originales
datos_mayo <- datos_crudos_mayo[, columnas_de_interes]
colnames(datos_mayo) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
datos_mayo$Fecha_y_Hora <- as.POSIXct(datos_mayo$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# Procesamiento de Junio
datos_crudos_junio <- read.csv(archivo_junio, skip = 1, header = FALSE)
colnames(datos_crudos_junio)[1:length(nombres_originales)] <- nombres_originales
datos_junio <- datos_crudos_junio[, columnas_de_interes]
colnames(datos_junio) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
datos_junio$Fecha_y_Hora <- as.POSIXct(datos_junio$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# Función de comprobación
verificar_inclusion <- function(df_menor, df_mayor) {
  registros_incluidos <- df_menor$Fecha_y_Hora %in% df_mayor$Fecha_y_Hora
  if (all(registros_incluidos)) {
    cat("Confirmado: El 100% de los datos están contenidos en la base de julio.\n")
  } else {
    faltantes <- sum(!registros_incluidos)
    cat(sprintf("Alerta: Faltan %d registros en la base principal.\n", faltantes))
  }
}

cat("\nAnálisis de integridad (Base 05_30 contenida en 07_25):\n")
cat("-------------------------------------------------------\n")
verificar_inclusion(datos_mayo, datos_totales)

cat("\nAnálisis de integridad (Base 06_19 contenida en 07_25):\n")
cat("-------------------------------------------------------\n")
verificar_inclusion(datos_junio, datos_totales)

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

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

# LECTURA DE DATOS (SERIE "Sonda")
archivo_08_14 <- "12218618650_UserData42iQ_2025-08-14_12-34-21_Sonda.csv"
archivo_08_28 <- "12218618650_UserData42iQ_2025-08-28_23-16-34_Sonda.csv"
archivo_10_07 <- "12218618650_UserData42iQ_2025-10-07_13-48-55_Sonda.csv"

nombres_originales <- names(read.csv(archivo_08_14, nrows = 0, check.names = FALSE))

datos_crudos_08_14 <- read.csv(archivo_08_14, skip = 1, header = FALSE)
datos_crudos_08_28 <- read.csv(archivo_08_28, skip = 1, header = FALSE)
datos_crudos_10_07 <- read.csv(archivo_10_07, skip = 1, header = FALSE)

colnames(datos_crudos_08_14)[1:length(nombres_originales)] <- nombres_originales
colnames(datos_crudos_08_28)[1:length(nombres_originales)] <- nombres_originales
colnames(datos_crudos_10_07)[1:length(nombres_originales)] <- nombres_originales

columnas_de_interes <- c(
  "Time Stamp",
  "NO Concentration (ppb or ug/m3)",
  "NO2 Concentration (ppb or ug/m3)",
  "NOx Concentration (ppb or ug/m3)"
)

nombres_intuitivos <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")

# PROCESAMIENTO ARCHIVO 08_14
datos_08_14 <- datos_crudos_08_14[, columnas_de_interes]
colnames(datos_08_14) <- nombres_intuitivos
datos_08_14$Fecha_y_Hora <- as.POSIXct(datos_08_14$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# PROCESAMIENTO ARCHIVO 08_28
datos_08_28 <- datos_crudos_08_28[, columnas_de_interes]
colnames(datos_08_28) <- nombres_intuitivos
datos_08_28$Fecha_y_Hora <- as.POSIXct(datos_08_28$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# PROCESAMIENTO ARCHIVO 10_07
datos_10_07 <- datos_crudos_10_07[, columnas_de_interes]
colnames(datos_10_07) <- nombres_intuitivos
datos_10_07$Fecha_y_Hora <- as.POSIXct(datos_10_07$Fecha_y_Hora, format = "%d/%m/%Y %H:%M:%S")

# ANALIZADOR DE CONTENIDO
verificar_inclusion <- function(df_menor, df_mayor) {
  registros_incluidos <- df_menor$Fecha_y_Hora %in% df_mayor$Fecha_y_Hora
  
  if (all(registros_incluidos)) {
    cat("Confirmado: El 100% de los datos del primer archivo están contenidos en el segundo.\n")
  } else {
    faltantes <- sum(!registros_incluidos)
    cat(sprintf("Alerta: Faltan %d registros en el archivo de destino.\n", faltantes))
  }
}

cat("\nAnálisis de integridad (08_14 contenido en 08_28):\n")
cat("--------------------------------------------------\n")
verificar_inclusion(datos_08_14, datos_08_28)

# ANÁLISIS DE DATOS (25 JUNIO - 25 JULIO EN BASE 08_28)
fecha_inicio <- as.POSIXct("25/06/2025 00:00:00", format="%d/%m/%Y %H:%M:%S")
fecha_fin <- as.POSIXct("25/07/2025 23:59:59", format="%d/%m/%Y %H:%M:%S")

datos_periodo_sonda <- subset(datos_08_28, Fecha_y_Hora >= fecha_inicio & Fecha_y_Hora <= fecha_fin)

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

cat("\nReporte de parámetros con Sonda (Límite: 3 min)\n")
cat("-----------------------------------------------\n")

cat("NO:\n")
analizar_falla(datos_periodo_sonda$Concentracion_NO)

cat("\nNO2:\n")
analizar_falla(datos_periodo_sonda$Concentracion_NO2)

cat("\nNOx:\n")
analizar_falla(datos_periodo_sonda$Concentracion_NOx)


