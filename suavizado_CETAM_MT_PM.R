###############################################################################
# PREPROCESAMIENTO Y SUAVIZADO
# HOMOLOGACION / CONCORDANCIA: CETAM (referencia) vs MT
#
# Variables de interes:
#   - PM1.0
#   - PM2.5
#   - PM10
#
# Los archivos GRIMM 11-D entregados tienen mediciones aproximadamente
# cada 1 minuto en la hoja "Mass values".
#
# En vez de aplicar una media movil, aqui se realiza un PROMEDIO TEMPORAL
# POR BLOQUES DE 5 MINUTOS. Esto tiene dos ventajas para el analisis posterior:
#
#   1) reduce la variabilidad minuto a minuto;
#   2) alinea temporalmente CETAM y MT aunque sus relojes no tengan exactamente
#      los mismos segundos.
#
# Si luego se desea usar 10 o 15 minutos, basta cambiar VENTANA_MIN.
###############################################################################


# =============================================================================
# 0. PAQUETES
# =============================================================================

paquetes <- c(
  "readxl",
  "dplyr",
  "lubridate",
  "writexl"
)

faltan <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltan) > 0) {
  install.packages(
    faltan,
    dependencies = TRUE
  )
}

library(readxl)
library(dplyr)
library(lubridate)
library(writexl)


# =============================================================================
# 1. CONFIGURACION
# =============================================================================

# Carpeta donde dejaste los Excel.
CARPETA_DATOS <- "datos"

# Fecha del experimento que queremos estudiar.
# Los archivos contienen registros de otras fechas, por lo que es importante
# filtrar el periodo relevante.
FECHA_ANALISIS <- as.Date("2025-06-19")

# Tamaño del bloque usado para suavizar.
# Los datos originales son aproximadamente cada 1 minuto.
VENTANA_MIN <- 5

# Cantidad minima de observaciones crudas exigidas dentro de cada bloque.
# En un bloque completo de 5 min esperamos aproximadamente 5 observaciones.
# Usar 3 evita conservar bloques demasiado incompletos.
MIN_OBS_POR_BLOQUE <- 3

# Carpeta donde se guardaran los datos ya suavizados.
CARPETA_SALIDA <- "datos_suavizados_PM"

dir.create(
  CARPETA_SALIDA,
  showWarnings = FALSE,
  recursive = TRUE
)


# =============================================================================
# 2. BUSCAR LOS DOS ARCHIVOS
# =============================================================================

archivo_cetam <- character(0)
archivo_mt <- character(0)

if (dir.exists(CARPETA_DATOS)) {
  
  archivo_cetam <- list.files(
    path = CARPETA_DATOS,
    pattern = "GRIMM.*Cetam.*\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  archivo_mt <- list.files(
    path = CARPETA_DATOS,
    pattern = "GRIMM.*MT.*\\.xlsx$",
    full.names = TRUE,
    ignore.case = TRUE
  )
}


# Si no encuentra CETAM automaticamente, se abre el selector.
if (length(archivo_cetam) == 0) {
  
  cat(
    "\nNo encontre automaticamente el archivo CETAM.\n",
    "Selecciona ahora el Excel de CETAM.\n\n",
    sep = ""
  )
  
  archivo_cetam <- file.choose()
  
} else {
  
  archivo_cetam <- archivo_cetam[1]
}


# Si no encuentra MT automaticamente, se abre el selector.
if (length(archivo_mt) == 0) {
  
  cat(
    "\nNo encontre automaticamente el archivo MT.\n",
    "Selecciona ahora el Excel de MT.\n\n",
    sep = ""
  )
  
  archivo_mt <- file.choose()
  
} else {
  
  archivo_mt <- archivo_mt[1]
}


cat("\nArchivo CETAM:\n")
cat(archivo_cetam, "\n")

cat("\nArchivo MT:\n")
cat(archivo_mt, "\n\n")


# =============================================================================
# 3. FUNCION PARA LEER LOS DATOS GRIMM 11-D
# =============================================================================
#
# En los dos Excel:
#
#   hoja: "Mass values"
#   fila 1: informacion general del instrumento
#   fila 2: nombres de las variables
#
# Por eso usamos skip = 1.
#
# Las columnas relevantes son:
#
#   date&time
#   PM10 [ug/m3]
#   PM2,5 [ug/m3]
#   PM1 [ug/m3]
#
# =============================================================================

leer_grimm <- function(
    archivo,
    instrumento
) {
  
  datos <- readxl::read_excel(
    path = archivo,
    sheet = "Mass values",
    skip = 1
  )
  
  
  # Comprobar que existan las columnas necesarias.
  columnas_necesarias <- c(
    "date&time",
    "PM10 [ug/m3]",
    "PM2,5 [ug/m3]",
    "PM1 [ug/m3]"
  )
  
  
  faltan_columnas <- setdiff(
    columnas_necesarias,
    names(datos)
  )
  
  
  if (length(faltan_columnas) > 0) {
    
    stop(
      paste0(
        "En el archivo ",
        instrumento,
        " faltan estas columnas: ",
        paste(
          faltan_columnas,
          collapse = ", "
        )
      )
    )
  }
  
  
  # Construir una base simple solamente con las variables de interes.
  datos_limpios <- datos |>
    transmute(
      
      fecha_hora = lubridate::dmy_hms(
        as.character(
          .data[["date&time"]]
        ),
        tz = "America/Santiago"
      ),
      
      PM10 = suppressWarnings(
        as.numeric(
          .data[["PM10 [ug/m3]"]]
        )
      ),
      
      PM2_5 = suppressWarnings(
        as.numeric(
          .data[["PM2,5 [ug/m3]"]]
        )
      ),
      
      PM1_0 = suppressWarnings(
        as.numeric(
          .data[["PM1 [ug/m3]"]]
        )
      )
    ) |>
    filter(
      !is.na(fecha_hora)
    ) |>
    arrange(
      fecha_hora
    )
  
  
  datos_limpios
}


# =============================================================================
# 4. LEER CETAM Y MT
# =============================================================================

cetam_raw <- leer_grimm(
  archivo = archivo_cetam,
  instrumento = "CETAM"
)

mt_raw <- leer_grimm(
  archivo = archivo_mt,
  instrumento = "MT"
)


# =============================================================================
# 5. FILTRAR SOLO EL DIA DEL EXPERIMENTO
# =============================================================================

cetam_dia <- cetam_raw |>
  filter(
    as.Date(fecha_hora) == FECHA_ANALISIS
  )


mt_dia <- mt_raw |>
  filter(
    as.Date(fecha_hora) == FECHA_ANALISIS
  )


if (nrow(cetam_dia) == 0) {
  stop(
    "CETAM no tiene observaciones en FECHA_ANALISIS."
  )
}


if (nrow(mt_dia) == 0) {
  stop(
    "MT no tiene observaciones en FECHA_ANALISIS."
  )
}


cat("============================================================\n")
cat("DATOS CRUDOS DEL DIA\n")
cat("============================================================\n")

cat(
  "CETAM: ",
  nrow(cetam_dia),
  " observaciones | ",
  format(
    min(cetam_dia$fecha_hora),
    "%H:%M:%S"
  ),
  " - ",
  format(
    max(cetam_dia$fecha_hora),
    "%H:%M:%S"
  ),
  "\n",
  sep = ""
)

cat(
  "MT:    ",
  nrow(mt_dia),
  " observaciones | ",
  format(
    min(mt_dia$fecha_hora),
    "%H:%M:%S"
  ),
  " - ",
  format(
    max(mt_dia$fecha_hora),
    "%H:%M:%S"
  ),
  "\n\n",
  sep = ""
)


# =============================================================================
# 6. FUNCION DE SUAVIZADO POR BLOQUES DE TIEMPO
# =============================================================================
#
# Ejemplo con VENTANA_MIN = 5:
#
# 12:45:03
# 12:46:03
# 12:47:03
# 12:48:03
# 12:49:03
#
# se transforman en una sola observacion correspondiente al bloque 12:45.
#
# Para cada bloque se calcula la media de PM1.0, PM2.5 y PM10.
#
# =============================================================================

media_sin_nan <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(
    x,
    na.rm = TRUE
  )
}


suavizar_por_bloques <- function(
    datos,
    instrumento
) {
  
  unidad_bloque <- paste(
    VENTANA_MIN,
    "minutes"
  )
  
  
  suavizado <- datos |>
    mutate(
      
      bloque_tiempo = lubridate::floor_date(
        fecha_hora,
        unit = unidad_bloque
      )
    ) |>
    group_by(
      bloque_tiempo
    ) |>
    summarise(
      
      n_observaciones = n(),
      
      PM1_0 = media_sin_nan(
        PM1_0
      ),
      
      PM2_5 = media_sin_nan(
        PM2_5
      ),
      
      PM10 = media_sin_nan(
        PM10
      ),
      
      .groups = "drop"
    ) |>
    filter(
      n_observaciones >= MIN_OBS_POR_BLOQUE
    ) |>
    arrange(
      bloque_tiempo
    )
  
  
  cat(
    instrumento,
    ": ",
    nrow(suavizado),
    " bloques validos de ",
    VENTANA_MIN,
    " minutos.\n",
    sep = ""
  )
  
  
  suavizado
}


# =============================================================================
# 7. SUAVIZAR AMBOS INSTRUMENTOS
# =============================================================================

cetam_suave <- suavizar_por_bloques(
  datos = cetam_dia,
  instrumento = "CETAM"
)


mt_suave <- suavizar_por_bloques(
  datos = mt_dia,
  instrumento = "MT"
)


# =============================================================================
# 8. RENOMBRAR VARIABLES ANTES DE JUNTAR LAS DOS SERIES
# =============================================================================

cetam_suave_final <- cetam_suave |>
  rename(
    
    CETAM_n = n_observaciones,
    
    CETAM_PM1_0 = PM1_0,
    
    CETAM_PM2_5 = PM2_5,
    
    CETAM_PM10 = PM10
  )


mt_suave_final <- mt_suave |>
  rename(
    
    MT_n = n_observaciones,
    
    MT_PM1_0 = PM1_0,
    
    MT_PM2_5 = PM2_5,
    
    MT_PM10 = PM10
  )


# =============================================================================
# 9. ALINEAR TEMPORALMENTE CETAM Y MT
# =============================================================================
#
# inner_join conserva solamente bloques donde ambos instrumentos tienen
# informacion suficiente. Esta sera la base correcta para la concordancia.
#
# =============================================================================

datos_alineados <- inner_join(
  
  cetam_suave_final,
  
  mt_suave_final,
  
  by = "bloque_tiempo"
) |>
  arrange(
    bloque_tiempo
  )


cat("\n")
cat("============================================================\n")
cat("RESULTADO DEL SUAVIZADO\n")
cat("============================================================\n")

cat(
  "Bloques CETAM: ",
  nrow(cetam_suave_final),
  "\n",
  sep = ""
)

cat(
  "Bloques MT:    ",
  nrow(mt_suave_final),
  "\n",
  sep = ""
)

cat(
  "Bloques comunes CETAM-MT: ",
  nrow(datos_alineados),
  "\n",
  sep = ""
)


if (nrow(datos_alineados) > 0) {
  
  cat(
    "Intervalo comun suavizado: ",
    format(
      min(datos_alineados$bloque_tiempo),
      "%Y-%m-%d %H:%M"
    ),
    " - ",
    format(
      max(datos_alineados$bloque_tiempo),
      "%Y-%m-%d %H:%M"
    ),
    "\n",
    sep = ""
  )
}


cat("============================================================\n\n")


# =============================================================================
# 10. GUARDAR RESULTADOS
# =============================================================================

# Datos crudos del dia.
write.csv(
  cetam_dia,
  file.path(
    CARPETA_SALIDA,
    "CETAM_crudo_dia.csv"
  ),
  row.names = FALSE
)


write.csv(
  mt_dia,
  file.path(
    CARPETA_SALIDA,
    "MT_crudo_dia.csv"
  ),
  row.names = FALSE
)


# Datos suavizados por separado.
write.csv(
  cetam_suave_final,
  file.path(
    CARPETA_SALIDA,
    "CETAM_suavizado_5min.csv"
  ),
  row.names = FALSE
)


write.csv(
  mt_suave_final,
  file.path(
    CARPETA_SALIDA,
    "MT_suavizado_5min.csv"
  ),
  row.names = FALSE
)


# Base final ya suavizada y alineada.
write.csv(
  datos_alineados,
  file.path(
    CARPETA_SALIDA,
    "CETAM_MT_suavizados_alineados.csv"
  ),
  row.names = FALSE
)


# Guardar tambien todo en un unico Excel.
writexl::write_xlsx(
  
  list(
    
    CETAM_crudo = cetam_dia,
    
    MT_crudo = mt_dia,
    
    CETAM_suavizado = cetam_suave_final,
    
    MT_suavizado = mt_suave_final,
    
    datos_alineados = datos_alineados
  ),
  
  path = file.path(
    CARPETA_SALIDA,
    "CETAM_MT_datos_preprocesados.xlsx"
  )
)


# =============================================================================
# 11. MOSTRAR LAS PRIMERAS FILAS
# =============================================================================

cat("Primeras filas de la base que usaremos despues para los graficos:\n\n")

print(
  head(
    datos_alineados,
    10
  )
)


cat("\n")
cat("============================================================\n")
cat("SUAVIZADO TERMINADO CORRECTAMENTE\n")
cat("============================================================\n")
cat(
  "Carpeta creada: ",
  CARPETA_SALIDA,
  "\n",
  sep = ""
)
cat(
  "Archivo principal: CETAM_MT_suavizados_alineados.csv\n"
)
cat(
  "Variables listas: PM1.0, PM2.5 y PM10\n"
)
cat("============================================================\n")
