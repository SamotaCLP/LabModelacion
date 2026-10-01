iQ_06_19_Sinsonda <- "12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv"

datos_originales <- read.csv(iQ_06_19_Sinsonda, check.names = FALSE)

columnas_de_interes <- c(
  "Time Stamp",
  "NO Concentration (ppb or ug/m3)",
  "NO2 Concentration (ppb or ug/m3)",
  "NOx Concentration (ppb or ug/m3)"
)

datos_concentraciones <- datos_originales[, columnas_de_interes]

# 5. Renombrar las columnas para que sean más simples de usar en tus cálculos o gráficos
nombres_intuitivos <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
colnames(datos_concentraciones) <- nombres_intuitivos

# 6. Mostrar las primeras 6 filas para confirmar que la lectura fue exitosa
head(datos_concentraciones)