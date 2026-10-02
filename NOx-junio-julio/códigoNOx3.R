iQ_06_19_sinsonda <- "12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv"
iQ_07_25_sinsonda <- "12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv"

datos_iQ_06_19_sinsonda <- read.csv(iQ_06_19_sinsonda, check.names = FALSE)
datos_iQ_07_25_sinsonda <- read.csv(iQ_07_25_sinsonda, check.names = FALSE)

columnas_de_interes <- c(
  "Time Stamp",
  "NO Concentration (ppb or ug/m3)",
  "NO2 Concentration (ppb or ug/m3)",
  "NOx Concentration (ppb or ug/m3)"
)

datos_interes_iQ_06_19_sinsonda <- datos_iQ_06_19_sinsonda[, columnas_de_interes]
datos_interes_iQ_07_25_sinsonda <- datos_iQ_07_25_sinsonda[, columnas_de_interes]

nombres_intuitivos_iQ_06_19_sinsonda <- c("Fecha_y_Hora_iQ_06_19_Sinsonda", "Concentracion_NO_iQ_06_19_sinsonda", "Concentracion_NO2_iQ_06_19_sinsonda", "Concentracion_NOx_iQ_06_19_sinsonda")
nombres_intuitivos_iQ_07_25_sinsonda <- c("Fecha_y_Hora_iQ_07_25_sinsonda", "Concentracion_NO_iQ_07_25_sinsonda", "Concentracion_NO2_iQ_07_25_sinsonda", "Concentracion_NOx_iQ_07_25_sinsonda")

colnames(datos_interes_iQ_06_19_sinsonda) <- nombres_intuitivos_iQ_06_19_sinsonda
colnames(datos_interes_iQ_07_25_sinsonda) <- nombres_intuitivos_iQ_07_25_sinsonda

head(datos_interes_iQ_06_19_sinsonda)
head(datos_interes_iQ_07_25_sinsonda)