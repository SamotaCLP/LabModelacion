# 42iQ sin sonda -----------------------####################################

#lectura 3 bases ----
archivos_csv <- c("12218618650_UserData42iQ_2025-05-30_15-24-10_Sinsonda.csv", 
                  "12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv", 
                  "12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv")

# 2. Función de lectura con corrección de row.names
leer_csv_42iQ <- function(archivo) {
  # row.names = NULL mantiene cada columna en su posición original
  datos <- read.csv(archivo, stringsAsFactors = FALSE, row.names = NULL)
  
  # Posiciones reales: 1 (Time Stamp), 8 (NO), 9 (NO2), 10 (NOx)
  datos_limpios <- datos[, c(1, 8, 9, 10)]
  
  colnames(datos_limpios) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
  datos_limpios$Fecha_y_Hora <- as.POSIXct(datos_limpios$Fecha_y_Hora, format="%d/%m/%Y %H:%M:%S")
  
  return(datos_limpios)
}

# 3. Unir las 3 bases
lista_csv <- lapply(archivos_csv, leer_csv_42iQ)
iQ_sinsonda_datos_totales <- do.call(rbind, lista_csv)

# 4. Ordenar cronológicamente y eliminar solapamientos exactos
iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[order(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]
iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[!duplicated(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]


#contención de bases ----
base1 <- leer_csv_42iQ(archivos_csv[1])
base2 <- leer_csv_42iQ(archivos_csv[2])
base3 <- leer_csv_42iQ(archivos_csv[3])

# 2. Extraer fechas de inicio y fin de cada archivo
rango1 <- range(base1$Fecha_y_Hora, na.rm = TRUE)
rango2 <- range(base2$Fecha_y_Hora, na.rm = TRUE)
rango3 <- range(base3$Fecha_y_Hora, na.rm = TRUE)

# 3. Imprimir la radiografía temporal para inspección visual
cat("\n--- Análisis de Periodos por Archivo ---\n")
cat("Base 1 (Mayo) :", format(rango1[1]), " al ", format(rango1[2]), "\n")
cat("Base 2 (Junio):", format(rango2[1]), " al ", format(rango2[2]), "\n")
cat("Base 3 (Julio):", format(rango3[1]), " al ", format(rango3[2]), "\n")

# 4. Comprobación lógica y matemática de contención
# ¿La base 3 empieza antes o igual y termina después o igual que las otras dos?
contenida_1 <- (rango3[1] <= rango1[1]) & (rango3[2] >= rango1[2])
contenida_2 <- (rango3[1] <= rango2[1]) & (rango3[2] >= rango2[2])

cat("\n--- Conclusión del Análisis de Solapamiento ---\n")
if(contenida_1 & contenida_2) {
  cat("RESULTADO: La Base 1 y Base 2 están temporalmente contenidas al 100% en la Base 3.\n")
  cat("ACCIÓN: Se descartan las dos primeras bases para evitar redundancia de procesamiento.\n")
  
  # Redefinimos la base total usando ÚNICAMENTE la tercera base
  iQ_sinsonda_datos_totales <- base3
  
} else {
  cat("RESULTADO: Las bases aportan datos en periodos distintos.\n")
  cat("ACCIÓN: Es necesario unirlas (rbind) y usar unique() para los traslapes parciales.\n")
  
  # Se mantiene la lógica de unión si resultan no estar contenidas
  lista_csv <- list(base1, base2, base3)
  iQ_sinsonda_datos_totales <- do.call(rbind, lista_csv)
  iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[order(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]
  iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[!duplicated(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]
}


#medidas de resumen crudas ----
generar_resumen_estadistico <- function(df, nombre_base) {
  gases <- c("Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
  
  resumen <- data.frame(
    Gas = c("NO", "NO2", "NOx"),
    Minimo = sapply(df[gases], min, na.rm = TRUE),
    Maximo = sapply(df[gases], max, na.rm = TRUE),
    Promedio = sapply(df[gases], mean, na.rm = TRUE),
    Mediana = sapply(df[gases], median, na.rm = TRUE),
    Desv_Estandar = sapply(df[gases], sd, na.rm = TRUE)
  )
  rownames(resumen) <- NULL
  
  cat("\n========================================================\n")
  cat("ESTADÍSTICAS:", nombre_base, "\n")
  cat("========================================================\n")
  # Imprimir con 4 decimales para mantener la precisión
  print(format(resumen, digits = 4, nsmall = 4))
  cat("========================================================\n")
}

# 2. Ejecutar la función para la base cruda sin sonda
generar_resumen_estadistico(iQ_sinsonda_datos_totales, "42iQ SIN SONDA (Datos Crudos)")
#analisis valores constantes ----
analizar_falla <- function(valores, limite = 4) {
  valores <- na.omit(valores)
  if(length(valores) == 0) return()
  
  rachas <- rle(valores)
  fallas <- rachas$lengths >= limite
  
  if(any(fallas)) {
    cat("Advertencia: Se detectaron valores constantes.\n")
    print(data.frame(Valor_Pegado = rachas$values[fallas], 
                     Repeticiones = rachas$lengths[fallas]))
  } else {
    cat("Estado normal: No hay valores constantes por 4 o más mediciones.\n")
  }
}

cat("\n--- Análisis de Mediciones constantes (42iQ) ---\n")
cat("NO:\n"); analizar_falla(iQ_sinsonda_datos_totales$Concentracion_NO)
cat("\nNO2:\n"); analizar_falla(iQ_sinsonda_datos_totales$Concentracion_NO2)
cat("\nNOx:\n"); analizar_falla(iQ_sinsonda_datos_totales$Concentracion_NOx)
#total de registros unicos y NAs iniciales ----
total_crudos <- nrow(iQ_sinsonda_datos_totales)

na_crudos_NO  <- sum(is.na(iQ_sinsonda_datos_totales$Concentracion_NO))
na_crudos_NO2 <- sum(is.na(iQ_sinsonda_datos_totales$Concentracion_NO2))
na_crudos_NOx <- sum(is.na(iQ_sinsonda_datos_totales$Concentracion_NOx))

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE CONTINUIDAD (BASE CRUDA)\n")
cat("==============================================\n")
cat(sprintf("Total de registros crudos: %d\n", total_crudos))
cat(sprintf("Valores NA iniciales -> NO: %d | NO2: %d | NOx: %d\n", 
            na_crudos_NO, na_crudos_NO2, na_crudos_NOx))
cat("==============================================\n")
#eliminacion de datos en calibración ----
# 1. Crear copia para el primer filtro (preservando la base cruda intacta)
datos_filtro1_sinsonda <- iQ_sinsonda_datos_totales

# 2. Función limpia para convertir rachas >= 4 en NA
filtrar_rachas <- function(vector_gases, limite = 4) {
  rachas <- rle(vector_gases)
  indices_pegados <- rep(rachas$lengths >= limite, rachas$lengths)
  vector_gases[indices_pegados] <- NA
  return(vector_gases)
}

# 3. Aplicar primer filtro a cada contaminante
datos_filtro1_sinsonda$Concentracion_NO  <- filtrar_rachas(datos_filtro1_sinsonda$Concentracion_NO, limite = 4)
datos_filtro1_sinsonda$Concentracion_NO2 <- filtrar_rachas(datos_filtro1_sinsonda$Concentracion_NO2, limite = 4)
datos_filtro1_sinsonda$Concentracion_NOx <- filtrar_rachas(datos_filtro1_sinsonda$Concentracion_NOx, limite = 4)

# 4. Contabilizar datos eliminados (nuevos NA) y su porcentaje respecto al total crudo
elim_f1_NO  <- sum(is.na(datos_filtro1_sinsonda$Concentracion_NO)) - na_crudos_NO
elim_f1_NO2 <- sum(is.na(datos_filtro1_sinsonda$Concentracion_NO2)) - na_crudos_NO2
elim_f1_NOx <- sum(is.na(datos_filtro1_sinsonda$Concentracion_NOx)) - na_crudos_NOx

# 5. Eliminar físicamente de la tabla las filas de calibración
filas_vacias_sin <- is.na(datos_filtro1_sinsonda$Concentracion_NO) & 
  is.na(datos_filtro1_sinsonda$Concentracion_NO2) & 
  is.na(datos_filtro1_sinsonda$Concentracion_NOx)
datos_filtro1_sinsonda <- datos_filtro1_sinsonda[!filas_vacias_sin, ]

cat("\n==============================================\n")
cat("REPORTE PRIMER FILTRO: VALORES CONSTANTES (>= 4)\n")
cat("==============================================\n")
cat(sprintf("Total de datos crudos evaluados: %d registros\n\n", total_crudos))
cat("Datos convertidos a NA (periodos de estabilización/calentamiento):\n")
cat(sprintf("- NO  : %d registros eliminados (%.4f%% del total)\n", elim_f1_NO, (elim_f1_NO / total_crudos) * 100))
cat(sprintf("- NO2 : %d registros eliminados (%.4f%% del total)\n", elim_f1_NO2, (elim_f1_NO2 / total_crudos) * 100))
cat(sprintf("- NOx : %d registros eliminados (%.4f%% del total)\n", elim_f1_NOx, (elim_f1_NOx / total_crudos) * 100))
cat("==============================================\n")
#filtrado de negativos ----
# 1. Definir el total válido (post-filtro 1) como base de cálculo
total_valido <- nrow(datos_filtro1_sinsonda)

# 2. Conteo de negativos ANTES de aplicar la limpieza
neg_NO  <- sum(datos_filtro1_sinsonda$Concentracion_NO < 0, na.rm = TRUE)
neg_NO2 <- sum(datos_filtro1_sinsonda$Concentracion_NO2 < 0, na.rm = TRUE)
neg_NOx <- sum(datos_filtro1_sinsonda$Concentracion_NOx < 0, na.rm = TRUE)

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE NEGATIVOS (PRE-FILTRO 2)\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo: %d registros válidos\n\n", total_valido))
cat(sprintf("- NO  : %d registros negativos (%.2f%% del total válido)\n", neg_NO, (neg_NO/total_valido)*100))
cat(sprintf("- NO2 : %d registros negativos (%.2f%% del total válido)\n", neg_NO2, (neg_NO2/total_valido)*100))
cat(sprintf("- NOx : %d registros negativos (%.2f%% del total válido)\n", neg_NOx, (neg_NOx/total_valido)*100))

# 3. Crear nueva base para aplicar el segundo filtro de forma segura
datos_filtro2_sinsonda <- datos_filtro1_sinsonda
umbral_ruido <- -3.0
columnas_gases <- c("Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")

# Lista para almacenar los resultados del filtro
reporte_f2 <- list()

# 4. Aplicar las reglas estadísticas de limpieza
for(col in columnas_gases) {
  vec <- datos_filtro2_sinsonda[[col]]
  
  # Identificar las posiciones exactas de ruido térmico y fallas críticas
  idx_ruido  <- which(vec < 0 & vec >= umbral_ruido)
  idx_fallas <- which(vec < umbral_ruido)
  
  # Guardar los conteos en la lista
  reporte_f2[[col]] <- list(ruido = length(idx_ruido), fallas = length(idx_fallas))
  
  # A) Ruido térmico a 0
  vec[idx_ruido] <- 0
  
  # B) Fallas críticas de hardware a NA
  vec[idx_fallas] <- NA
  
  # Sobrescribir la columna en la nueva base
  datos_filtro2_sinsonda[[col]] <- vec
}

# 5. Reporte final post-filtro 2 con porcentajes
cat("\n==============================================\n")
cat("REPORTE SEGUNDO FILTRO: CORRECCIÓN UMBRAL -3.0\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo: %d registros válidos (Post-Filtro 1)\n\n", total_valido))

cat("1. Ruido térmico ajustado a 0 (valores entre 0 y -3.0):\n")
cat(sprintf("- NO  : %d registros corregidos a cero (%.2f%%).\n", reporte_f2$Concentracion_NO$ruido, (reporte_f2$Concentracion_NO$ruido/total_valido)*100))
cat(sprintf("- NO2 : %d registros corregidos a cero (%.2f%%).\n", reporte_f2$Concentracion_NO2$ruido, (reporte_f2$Concentracion_NO2$ruido/total_valido)*100))
cat(sprintf("- NOx : %d registros corregidos a cero (%.2f%%).\n", reporte_f2$Concentracion_NOx$ruido, (reporte_f2$Concentracion_NOx$ruido/total_valido)*100))

cat("\n2. Fallas críticas descartadas a NA (valores < -3.0):\n")
cat(sprintf("- NO  : %d registros eliminados (%.4f%%).\n", reporte_f2$Concentracion_NO$fallas, (reporte_f2$Concentracion_NO$fallas/total_valido)*100))
cat(sprintf("- NO2 : %d registros eliminados (%.4f%%).\n", reporte_f2$Concentracion_NO2$fallas, (reporte_f2$Concentracion_NO2$fallas/total_valido)*100))
cat(sprintf("- NOx : %d registros eliminados (%.4f%%).\n", reporte_f2$Concentracion_NOx$fallas, (reporte_f2$Concentracion_NOx$fallas/total_valido)*100))
cat("==============================================\n")
#medidas de resumen filtradas ----
generar_resumen_estadistico(base_final_sinsonda, "42iQ SIN SONDA (Datos Validados Finales)")

############################################################################



# 42iQ con sonda -----------------------####################################

#lectura de las 3 bases ----
# 1. Nombres exactos de los archivos
archivos_csv_sonda <- c("12218618650_UserData42iQ_2025-08-14_12-34-21_Sonda.csv", 
                        "12218618650_UserData42iQ_2025-08-28_23-16-34_Sonda.csv", 
                        "12218618650_UserData42iQ_2025-10-07_13-48-55_Sonda.csv")

# 2. Función de lectura idéntica a la anterior (protegiendo row.names)
leer_csv_42iQ_sonda <- function(archivo) {
  datos <- read.csv(archivo, stringsAsFactors = FALSE, row.names = NULL)
  
  # Extraer Tiempo, NO, NO2, NOx
  datos_limpios <- datos[, c(1, 8, 9, 10)]
  colnames(datos_limpios) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
  
  # Formatear fecha
  datos_limpios$Fecha_y_Hora <- as.POSIXct(datos_limpios$Fecha_y_Hora, format="%d/%m/%Y %H:%M:%S")
  
  return(datos_limpios)
}

# 3. Leer las 3 bases de forma independiente
base1_sonda <- leer_csv_42iQ_sonda(archivos_csv_sonda[1])
base2_sonda <- leer_csv_42iQ_sonda(archivos_csv_sonda[2])
base3_sonda <- leer_csv_42iQ_sonda(archivos_csv_sonda[3])


#analisis de contención de bases ----
rango1_sonda <- range(base1_sonda$Fecha_y_Hora, na.rm = TRUE)
rango2_sonda <- range(base2_sonda$Fecha_y_Hora, na.rm = TRUE)
rango3_sonda <- range(base3_sonda$Fecha_y_Hora, na.rm = TRUE)

cat("\n--- Análisis de Periodos por Archivo (Con Sonda) ---\n")
cat("Base 1 (14 Ago):", format(rango1_sonda[1]), " al ", format(rango1_sonda[2]), "\n")
cat("Base 2 (28 Ago):", format(rango2_sonda[1]), " al ", format(rango2_sonda[2]), "\n")
cat("Base 3 (07 Oct):", format(rango3_sonda[1]), " al ", format(rango3_sonda[2]), "\n")

# Evaluar si la Base 1 está contenida en la Base 2
contenida_1_en_2 <- (rango2_sonda[1] <= rango1_sonda[1]) & (rango2_sonda[2] >= rango1_sonda[2])

cat("\n--- Conclusión de Solapamiento ---\n")
if(contenida_1_en_2) {
  cat("RESULTADO: La Base 1 está contenida 100% dentro de la Base 2.\n")
  cat("ACCIÓN: Se descarta la Base 1. Se unen (rbind) la Base 2 y la Base 3.\n")
  
  # Unimos solo la 2 y la 3
  iQ_sonda_datos_totales <- rbind(base2_sonda, base3_sonda)
  
} else {
  # Por si acaso, si la lógica falla, unimos las tres
  cat("ACCIÓN: Se unen las tres bases.\n")
  iQ_sonda_datos_totales <- do.call(rbind, list(base1_sonda, base2_sonda, base3_sonda))
}

# 4. Ordenar cronológicamente y forzar unicidad (eliminar traslapes exactos)
iQ_sonda_datos_totales <- iQ_sonda_datos_totales[order(iQ_sonda_datos_totales$Fecha_y_Hora), ]
iQ_sonda_datos_totales <- iQ_sonda_datos_totales[!duplicated(iQ_sonda_datos_totales$Fecha_y_Hora), ]



#total de registro unicos y NAs iniciales ----
total_crudos_sonda <- nrow(iQ_sonda_datos_totales)

na_crudos_NO_sonda  <- sum(is.na(iQ_sonda_datos_totales$Concentracion_NO))
na_crudos_NO2_sonda <- sum(is.na(iQ_sonda_datos_totales$Concentracion_NO2))
na_crudos_NOx_sonda <- sum(is.na(iQ_sonda_datos_totales$Concentracion_NOx))

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE CONTINUIDAD (BASE CRUDA - CON SONDA)\n")
cat("==============================================\n")
cat(sprintf("Total de registros crudos: %d\n", total_crudos_sonda))
cat(sprintf("Valores NA iniciales -> NO: %d | NO2: %d | NOx: %d\n", 
            na_crudos_NO_sonda, na_crudos_NO2_sonda, na_crudos_NOx_sonda))
cat("==============================================\n")
#medidas de resuemen crudas ----
generar_resumen_estadistico(iQ_sonda_datos_totales, "42iQ CON SONDA (Datos Crudos)")
#analisis de valores constantes ----
total_crudos_sonda <- nrow(iQ_sonda_datos_totales)
datos_filtro1_sonda <- iQ_sonda_datos_totales

# 2. Función para detectar rachas >= 4 y convertirlas en NA
filtrar_rachas <- function(vector_gases, limite = 4) {
  vector_seguro <- vector_gases
  vector_seguro[is.na(vector_seguro)] <- -999999
  
  rachas <- rle(vector_seguro)
  es_pegado <- rachas$lengths >= limite
  es_pegado[rachas$values == -999999] <- FALSE
  
  indices_pegados <- rep(es_pegado, rachas$lengths)
  vector_gases[indices_pegados] <- NA
  return(vector_gases)
}

# 3. Aplicar filtro a cada contaminante
datos_filtro1_sonda$Concentracion_NO  <- filtrar_rachas(datos_filtro1_sonda$Concentracion_NO, limite = 4)
datos_filtro1_sonda$Concentracion_NO2 <- filtrar_rachas(datos_filtro1_sonda$Concentracion_NO2, limite = 4)
datos_filtro1_sonda$Concentracion_NOx <- filtrar_rachas(datos_filtro1_sonda$Concentracion_NOx, limite = 4)

# 4. Contabilizar datos eliminados y nuevos totales válidos por gas
valido_NO_sonda  <- sum(!is.na(datos_filtro1_sonda$Concentracion_NO))
valido_NO2_sonda <- sum(!is.na(datos_filtro1_sonda$Concentracion_NO2))
valido_NOx_sonda <- sum(!is.na(datos_filtro1_sonda$Concentracion_NOx))

elim_f1_NO_sonda  <- total_crudos_sonda - valido_NO_sonda
elim_f1_NO2_sonda <- total_crudos_sonda - valido_NO2_sonda
elim_f1_NOx_sonda <- total_crudos_sonda - valido_NOx_sonda

# 5. Eliminar físicamente de la tabla las filas donde los 3 gases quedaron en NA
filas_vacias <- is.na(datos_filtro1_sonda$Concentracion_NO) & 
  is.na(datos_filtro1_sonda$Concentracion_NO2) & 
  is.na(datos_filtro1_sonda$Concentracion_NOx)
datos_filtro1_sonda <- datos_filtro1_sonda[!filas_vacias, ]

# 6. Reporte del Primer Filtro con el nuevo total
cat("\n==============================================\n")
cat("REPORTE PRIMER FILTRO: VALORES CONSTANTES (>= 4) CON SONDA\n")
cat("==============================================\n")
cat(sprintf("Total de datos crudos iniciales : %d registros\n\n", total_crudos_sonda))
cat("Datos eliminados y total válido restante (Post-Filtro 1):\n")
cat(sprintf("- NO  : %d eliminados (%.4f%%) -> Quedan %d datos válidos\n", 
            elim_f1_NO_sonda, (elim_f1_NO_sonda / total_crudos_sonda) * 100, valido_NO_sonda))
cat(sprintf("- NO2 : %d eliminados (%.4f%%) -> Quedan %d datos válidos\n", 
            elim_f1_NO2_sonda, (elim_f1_NO2_sonda / total_crudos_sonda) * 100, valido_NO2_sonda))
cat(sprintf("- NOx : %d eliminados (%.4f%%) -> Quedan %d datos válidos\n", 
            elim_f1_NOx_sonda, (elim_f1_NOx_sonda / total_crudos_sonda) * 100, valido_NOx_sonda))
cat("==============================================\n")
#analisis de valores negativos ----
# 1. Conteo de negativos ANTES de aplicar la limpieza (sobre datos_filtro1_sonda ya reducido)
neg_NO_sonda  <- sum(datos_filtro1_sonda$Concentracion_NO < 0, na.rm = TRUE)
neg_NO2_sonda <- sum(datos_filtro1_sonda$Concentracion_NO2 < 0, na.rm = TRUE)
neg_NOx_sonda <- sum(datos_filtro1_sonda$Concentracion_NOx < 0, na.rm = TRUE)

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE NEGATIVOS (PRE-FILTRO 2) CON SONDA\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo (Post-Filtro 1): NO=%d | NO2=%d | NOx=%d\n\n", 
            valido_NO_sonda, valido_NO2_sonda, valido_NOx_sonda))
cat(sprintf("- NO  : %d registros negativos (%.2f%% del total válido)\n", neg_NO_sonda, (neg_NO_sonda/valido_NO_sonda)*100))
cat(sprintf("- NO2 : %d registros negativos (%.2f%% del total válido)\n", neg_NO2_sonda, (neg_NO2_sonda/valido_NO2_sonda)*100))
cat(sprintf("- NOx : %d registros negativos (%.2f%% del total válido)\n", neg_NOx_sonda, (neg_NOx_sonda/valido_NOx_sonda)*100))

# 2. Crear nueva base para aplicar el segundo filtro
datos_filtro2_sonda <- datos_filtro1_sonda
umbral_ruido <- -3.0
columnas_gases <- c("Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")

reporte_f2_sonda <- list()

# 3. Aplicar las reglas estadísticas de limpieza
for(col in columnas_gases) {
  vec <- datos_filtro2_sonda[[col]]
  
  idx_ruido  <- which(vec < 0 & vec >= umbral_ruido)
  idx_fallas <- which(vec < umbral_ruido)
  
  reporte_f2_sonda[[col]] <- list(ruido = length(idx_ruido), fallas = length(idx_fallas))
  
  vec[idx_ruido] <- 0
  vec[idx_fallas] <- NA
  
  datos_filtro2_sonda[[col]] <- vec
}

# 4. Reporte final post-filtro 2
cat("\n==============================================\n")
cat("REPORTE SEGUNDO FILTRO: CORRECCIÓN UMBRAL -3.0 (CON SONDA)\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo (Post-Filtro 1): %d registros válidos\n\n", nrow(datos_filtro1_sonda)))

cat("1. Ruido térmico ajustado a 0 (valores entre 0 y -3.0):\n")
cat(sprintf("- NO  : %d registros corregidos a cero (%.2f%%).\n", reporte_f2_sonda$Concentracion_NO$ruido, (reporte_f2_sonda$Concentracion_NO$ruido/valido_NO_sonda)*100))
cat(sprintf("- NO2 : %d registros corregidos a cero (%.2f%%).\n", reporte_f2_sonda$Concentracion_NO2$ruido, (reporte_f2_sonda$Concentracion_NO2$ruido/valido_NO2_sonda)*100))
cat(sprintf("- NOx : %d registros corregidos a cero (%.2f%%).\n", reporte_f2_sonda$Concentracion_NOx$ruido, (reporte_f2_sonda$Concentracion_NOx$ruido/valido_NOx_sonda)*100))

cat("\n2. Fallas críticas descartadas a NA (valores < -3.0):\n")
cat(sprintf("- NO  : %d registros eliminados (%.4f%%).\n", reporte_f2_sonda$Concentracion_NO$fallas, (reporte_f2_sonda$Concentracion_NO$fallas/valido_NO_sonda)*100))
cat(sprintf("- NO2 : %d registros eliminados (%.4f%%).\n", reporte_f2_sonda$Concentracion_NO2$fallas, (reporte_f2_sonda$Concentracion_NO2$fallas/valido_NO2_sonda)*100))
cat(sprintf("- NOx : %d registros eliminados (%.4f%%).\n", reporte_f2_sonda$Concentracion_NOx$fallas, (reporte_f2_sonda$Concentracion_NOx$fallas/valido_NOx_sonda)*100))
cat("==============================================\n")
#medidas de resuemn filtradas ----
generar_resumen_estadistico(base_final_sonda, "42iQ CON SONDA (Datos Validados Finales)")
############################################################################

#reporte de datos 42iQ con y sin sonda ----
base_final_sinsonda <- datos_filtro2_sinsonda
base_final_sonda    <- datos_filtro2_sonda

# 2. Calcular los datos finales útiles (sin NAs) para el equipo SIN SONDA
final_NO_sinsonda  <- sum(!is.na(base_final_sinsonda$Concentracion_NO))
final_NO2_sinsonda <- sum(!is.na(base_final_sinsonda$Concentracion_NO2))
final_NOx_sinsonda <- sum(!is.na(base_final_sinsonda$Concentracion_NOx))

# 3. Calcular los datos finales útiles (sin NAs) para el equipo CON SONDA
final_NO_sonda  <- sum(!is.na(base_final_sonda$Concentracion_NO))
final_NO2_sonda <- sum(!is.na(base_final_sonda$Concentracion_NO2))
final_NOx_sonda <- sum(!is.na(base_final_sonda$Concentracion_NOx))

# 4. Imprimir Reporte Oficial para el documento
cat("\n==============================================\n")
cat("REPORTE OFICIAL: VOLUMEN DE DATOS TOTALMENTE VALIDADOS\n")
cat("==============================================\n")
cat("EQUIPO 42iQ SIN SONDA (Base para gráficos e informes):\n")
cat(sprintf("- NO  : %d registros 100%% válidos\n", final_NO_sinsonda))
cat(sprintf("- NO2 : %d registros 100%% válidos\n", final_NO2_sinsonda))
cat(sprintf("- NOx : %d registros 100%% válidos\n", final_NOx_sinsonda))
cat("----------------------------------------------\n")
cat("EQUIPO 42iQ CON SONDA (Base para gráficos e informes):\n")
cat(sprintf("- NO  : %d registros 100%% válidos\n", final_NO_sonda))
cat(sprintf("- NO2 : %d registros 100%% válidos\n", final_NO2_sonda))
cat(sprintf("- NOx : %d registros 100%% válidos\n", final_NOx_sonda))
cat("==============================================\n")









# 42i sin sonda -------------------------####################################
#lectura de las 3 bases ----
archivos_dat_sinsonda <- c("NOx_42i_07oct_PO_Sinsonda.dat", 
                           "NOx_42i_Portillo_14_Agosto_2025_Sinsonda.dat",
                           "NOx_42i_Portillo_28_agosto_2025_Sinsonda.dat")

lista_datos_sinsonda <- lapply(archivos_dat_sinsonda, function(archivo) {
  df <- read.table(archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
  df$Fecha_y_Hora <- as.POSIXct(paste(df$Date, df$Time), format = "%m-%d-%y %H:%M")
  df_limpio <- df[, c("Fecha_y_Hora", "no", "nox")]
  colnames(df_limpio) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NOx")
  return(df_limpio)
})

i_sinsonda_datos_totales <- do.call(rbind, lista_datos_sinsonda)

# 2. Ordenar y eliminar duplicados exactos de tiempo
i_sinsonda_datos_totales <- i_sinsonda_datos_totales[order(i_sinsonda_datos_totales$Fecha_y_Hora), ]
i_sinsonda_datos_totales <- i_sinsonda_datos_totales[!duplicated(i_sinsonda_datos_totales$Fecha_y_Hora), ]

total_crudos_42i_sin <- nrow(i_sinsonda_datos_totales)

#medidas de resumen crudas ----
generar_resumen_estadistico_42i <- function(df, nombre_base) {
  gases <- c("Concentracion_NO", "Concentracion_NOx")
  
  resumen <- data.frame(
    Gas = c("NO", "NOx"),
    Minimo = sapply(df[gases], min, na.rm = TRUE),
    Maximo = sapply(df[gases], max, na.rm = TRUE),
    Promedio = sapply(df[gases], mean, na.rm = TRUE),
    Mediana = sapply(df[gases], median, na.rm = TRUE),
    Desv_Estandar = sapply(df[gases], sd, na.rm = TRUE)
  )
  rownames(resumen) <- NULL
  
  cat("\n========================================================\n")
  cat("ESTADÍSTICAS:", nombre_base, "\n")
  cat("========================================================\n")
  print(format(resumen, digits = 4, nsmall = 4))
  cat("========================================================\n")
}

# 2. Ejecutar la función para la base cruda
generar_resumen_estadistico_42i(i_sinsonda_datos_totales, "42i SIN SONDA (Datos Crudos)")
#analisis valores constantes ----
datos_filtro1_42i_sin <- i_sinsonda_datos_totales

filtrar_rachas <- function(vector_gases, limite = 4) {
  vector_seguro <- vector_gases
  vector_seguro[is.na(vector_seguro)] <- -999999
  rachas <- rle(vector_seguro)
  es_pegado <- rachas$lengths >= limite
  es_pegado[rachas$values == -999999] <- FALSE
  indices_pegados <- rep(es_pegado, rachas$lengths)
  vector_gases[indices_pegados] <- NA
  return(vector_gases)
}

datos_filtro1_42i_sin$Concentracion_NO  <- filtrar_rachas(datos_filtro1_42i_sin$Concentracion_NO, limite = 4)
datos_filtro1_42i_sin$Concentracion_NOx <- filtrar_rachas(datos_filtro1_42i_sin$Concentracion_NOx, limite = 4)

# Contabilizar válidos y eliminar filas vacías
valido_NO_42i_sin  <- sum(!is.na(datos_filtro1_42i_sin$Concentracion_NO))
valido_NOx_42i_sin <- sum(!is.na(datos_filtro1_42i_sin$Concentracion_NOx))

filas_vacias_42i <- is.na(datos_filtro1_42i_sin$Concentracion_NO) & is.na(datos_filtro1_42i_sin$Concentracion_NOx)
datos_filtro1_42i_sin <- datos_filtro1_42i_sin[!filas_vacias_42i, ]

cat("\n==============================================\n")
cat("42i SIN SONDA - REPORTE FILTRO 1 (CALIBRACIONES)\n")
cat("==============================================\n")
cat(sprintf("Total de datos crudos: %d\n", total_crudos_42i_sin))
cat(sprintf("- NO eliminados : %d (Quedan %d válidos)\n", total_crudos_42i_sin - valido_NO_42i_sin, valido_NO_42i_sin))
cat(sprintf("- NOx eliminados: %d (Quedan %d válidos)\n", total_crudos_42i_sin - valido_NOx_42i_sin, valido_NOx_42i_sin))
cat("==============================================\n")
#medidas de resumen filtradas ----
generar_resumen_estadistico_42i(datos_filtro1_42i_sin, "42i SIN SONDA (Filtro de Valores Constantes)")
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



#medidas de resumen crudas ----
generar_resumen_estadistico_42i(i_sonda_datos_final, "42i CON SONDA (Datos Crudos)")
#analisis de valores constantes ----
datos_filtro1_42i_sonda <- i_sonda_datos_final

filtrar_rachas <- function(vector_gases, limite = 4) {
  vector_seguro <- vector_gases
  vector_seguro[is.na(vector_seguro)] <- -999999
  rachas <- rle(vector_seguro)
  es_pegado <- rachas$lengths >= limite
  es_pegado[rachas$values == -999999] <- FALSE
  indices_pegados <- rep(es_pegado, rachas$lengths)
  vector_gases[indices_pegados] <- NA
  return(vector_gases)
}

datos_filtro1_42i_sonda$Concentracion_NO  <- filtrar_rachas(datos_filtro1_42i_sonda$Concentracion_NO, limite = 4)
datos_filtro1_42i_sonda$Concentracion_NOx <- filtrar_rachas(datos_filtro1_42i_sonda$Concentracion_NOx, limite = 4)

# Eliminar filas completamente vacías
filas_vacias_sonda <- is.na(datos_filtro1_42i_sonda$Concentracion_NO) & is.na(datos_filtro1_42i_sonda$Concentracion_NOx)
datos_filtro1_42i_sonda <- datos_filtro1_42i_sonda[!filas_vacias_sonda, ]
#medidas de resumen filtradas ----
generar_resumen_estadistico_42i(datos_filtro1_42i_sonda, "42i CON SONDA (Filtro de Valores Constantes)")
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

