###############################################################################
# PROYECTO DE CONCORDANCIA ENTRE INSTRUMENTOS
# AE33 - AE36 - MAAP
#
# Este script:
#   1) Lee uno o varios archivos Excel.
#   2) Detecta automáticamente el formato particular del archivo entregado:
#      datos CSV guardados dentro de una sola columna de Excel.
#   3) Ignora TCA09.
#   4) Limpia y alinea las observaciones por fecha/hora.
#   5) Genera descriptivos y todos los gráficos solicitados con colores claros.
#   6) Calcula Pearson, CCC de Lin, Bland-Altman, PA y curva PA(c).
#   7) Agrega un coeficiente de comovimiento basado en primeras diferencias
#      como complemento para series temporales.
#   8) Guarda automáticamente resultados, tablas y gráficos.
#
# IMPORTANTE:
# - Para concordancia (recta y=x, CCC, Bland-Altman, PA) ambos instrumentos
#   DEBEN estar en la misma unidad.
# - El archivo entregado llama a MAAP "BC_MAAP_ug_m3". Si AE33/AE36 están
#   en ng/m3, entonces hay que multiplicar MAAP por 1000. Confirma esto con
#   tu profesor/equipo antes de interpretar los resultados.
# - NO se interpolan datos faltantes: para concordancia solo se usan pares
#   realmente observados en el mismo instante.
###############################################################################


# =============================================================================
# 0. PAQUETES
# =============================================================================

paquetes <- c(
  "readxl",
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "lubridate",
  "stringr",
  "purrr",
  "tibble",
  "writexl"
)

faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltan) > 0) {
  install.packages(faltan, dependencies = TRUE)
}

invisible(lapply(paquetes, library, character.only = TRUE))

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

set.seed(1234)


# =============================================================================
# 1. CONFIGURACIÓN: CAMBIA SOLO ESTA SECCIÓN SI ES NECESARIO
# =============================================================================

CARPETA_DATOS <- "datos"
PATRON_ARCHIVOS <- "BC880.*\\.xlsx$"
HOJA_EXCEL <- "in"
TZ_DATOS <- "America/Santiago"
NOMBRE_ANALITO <- "Black Carbon (BC)"

# ---------------------------------------------------------------------------
# Selección de versión AE33/AE36
# ---------------------------------------------------------------------------
PREFERIR_ONA <- TRUE

# ---------------------------------------------------------------------------
# Cambio de instrumento de referencia AE33 -> AE36
# ---------------------------------------------------------------------------
# Si se conoce la fecha/hora oficial del cambio, escribirla aqui con formato:
# "YYYY-MM-DD HH:MM"
#
# Ejemplo:
# FECHA_CAMBIO_REFERENCIA_MANUAL <- "2026-08-10 12:10"
#
# Si se deja NA, el script estima automaticamente una fecha candidata:
# el primer registro valido de AE36 posterior al ultimo registro valido de AE33.
# La fecha automatica debe interpretarse como una inferencia desde los datos y
# conviene validarla con la informacion operativa de CETAM.
FECHA_CAMBIO_REFERENCIA_MANUAL <- NA_character_

# ---------------------------------------------------------------------------
# Conversión de MAAP
# ---------------------------------------------------------------------------
CONVERTIR_MAAP_UG_A_NG <- TRUE
FACTOR_MAAP <- if (CONVERTIR_MAAP_UG_A_NG) 1000 else 1

UNIDAD_COMUN <- if (CONVERTIR_MAAP_UG_A_NG) {
  "ng/m3 (MAAP convertido desde ug/m3)"
} else {
  "unidad original"
}

# ---------------------------------------------------------------------------
# Limpieza física
# ---------------------------------------------------------------------------
ELIMINAR_NEGATIVOS <- FALSE
LIMITE_SUPERIOR <- 30000

# ---------------------------------------------------------------------------
# Probability of Agreement (PA)
# ---------------------------------------------------------------------------
C_TOLERANCIA <- NA_real_
C_MAX_CURVA <- NA_real_

# ---------------------------------------------------------------------------
# Salidas
# ---------------------------------------------------------------------------
DIR_SALIDA <- "resultados_concordancia"
DIR_GRAFICOS <- file.path(DIR_SALIDA, "graficos")
DIR_TABLAS <- file.path(DIR_SALIDA, "tablas")

GUARDAR_GRAFICOS_PDF <- TRUE
DPI_PNG <- 300

dir.create(DIR_SALIDA, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_GRAFICOS, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR_TABLAS, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. FUNCIONES AUXILIARES
# =============================================================================

nombre_seguro <- function(x) {
  x |>
    stringr::str_replace_all("[^A-Za-z0-9_-]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("^_") |>
    stringr::str_remove("_$")
}

guardar_plot <- function(p, nombre, width = 8, height = 6) {
  nombre <- nombre_seguro(nombre)
  
  ggsave(
    filename = file.path(DIR_GRAFICOS, paste0(nombre, ".png")),
    plot = p,
    width = width,
    height = height,
    dpi = DPI_PNG,
    bg = "white"
  )
  
  if (GUARDAR_GRAFICOS_PDF) {
    ggsave(
      filename = file.path(DIR_GRAFICOS, paste0(nombre, ".pdf")),
      plot = p,
      width = width,
      height = height,
      bg = "white"
    )
  }
}

primero_no_na <- function(x) {
  y <- x[!is.na(x)]
  if (length(y) == 0) {
    return(NA)
  }
  y[1]
}

leer_excel_robusto <- function(archivo, hoja = HOJA_EXCEL) {
  message("Leyendo: ", basename(archivo))
  
  x <- readxl::read_excel(
    path = archivo,
    sheet = hoja,
    col_types = "text"
  )
  
  es_csv_embebido <- (
    ncol(x) == 1 &&
      (
        stringr::str_detect(names(x)[1], ",") ||
          any(stringr::str_detect(x[[1]][1:min(10, nrow(x))], ","),
              na.rm = TRUE)
      )
  )
  
  if (es_csv_embebido) {
    lineas <- c(names(x)[1], x[[1]])
    lineas <- lineas[!is.na(lineas)]
    
    tmp_csv <- tempfile(fileext = ".csv")
    writeLines(lineas, tmp_csv, useBytes = TRUE)
    
    x <- readr::read_csv(
      tmp_csv,
      show_col_types = FALSE,
      na = c("", "NA", "NaN", "null", "NULL")
    )
  }
  
  x$archivo_origen <- basename(archivo)
  x
}

descriptivos_variable <- function(x, nombre) {
  x_finito <- x[is.finite(x)]
  
  tibble::tibble(
    instrumento = nombre,
    n_total = length(x),
    n_validos = length(x_finito),
    n_missing = sum(is.na(x)),
    n_negativos = sum(x_finito < 0),
    minimo = ifelse(length(x_finito) > 0, min(x_finito), NA_real_),
    q01 = ifelse(length(x_finito) > 0, quantile(x_finito, 0.01), NA_real_),
    q25 = ifelse(length(x_finito) > 0, quantile(x_finito, 0.25), NA_real_),
    mediana = ifelse(length(x_finito) > 0, median(x_finito), NA_real_),
    media = ifelse(length(x_finito) > 0, mean(x_finito), NA_real_),
    q75 = ifelse(length(x_finito) > 0, quantile(x_finito, 0.75), NA_real_),
    q99 = ifelse(length(x_finito) > 0, quantile(x_finito, 0.99), NA_real_),
    maximo = ifelse(length(x_finito) > 0, max(x_finito), NA_real_),
    sd = ifelse(length(x_finito) > 1, sd(x_finito), NA_real_),
    IQR = ifelse(length(x_finito) > 1, IQR(x_finito), NA_real_)
  )
}

ccc_lin <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  
  if (length(x) < 3) {
    return(NA_real_)
  }
  
  2 * cov(x, y) / (var(x) + var(y) + (mean(x) - mean(y))^2)
}

pa_empirica <- function(x, y, c) {
  ok <- is.finite(x) & is.finite(y)
  d <- x[ok] - y[ok]
  
  if (length(d) == 0 || !is.finite(c)) {
    return(NA_real_)
  }
  
  mean(abs(d) <= c)
}

pa_normal <- function(x, y, c) {
  ok <- is.finite(x) & is.finite(y)
  d <- x[ok] - y[ok]
  
  if (length(d) < 2 || !is.finite(c)) {
    return(NA_real_)
  }
  
  mu_d <- mean(d)
  sd_d <- sd(d)
  
  if (!is.finite(sd_d) || sd_d == 0) {
    return(as.numeric(abs(mu_d) <= c))
  }
  
  pnorm((c - mu_d) / sd_d) - pnorm((-c - mu_d) / sd_d)
}

comovimiento <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  
  if (length(x) < 4) {
    return(NA_real_)
  }
  
  dx <- diff(x)
  dy <- diff(y)
  
  if (sd(dx) == 0 || sd(dy) == 0) {
    return(NA_real_)
  }
  
  cor(dx, dy, method = "pearson")
}


# =============================================================================
# 3. SELECCIONAR Y LEER ARCHIVOS EXCEL
# =============================================================================

if (dir.exists(CARPETA_DATOS)) {
  archivos <- list.files(
    path = CARPETA_DATOS,
    pattern = PATRON_ARCHIVOS,
    full.names = TRUE,
    ignore.case = TRUE
  )
  archivos <- archivos[!stringr::str_detect(basename(archivos), "^~\\$")]
} else {
  archivos <- character(0)
}

if (length(archivos) == 0) {
  message("No se encontraron archivos con el patrón definido en '", CARPETA_DATOS, "'. Selecciona manualmente el Excel.")
  archivos <- file.choose()
}

message("Archivos seleccionados:")
print(basename(archivos))

lista_datos <- purrr::map(archivos, ~ leer_excel_robusto(.x, hoja = HOJA_EXCEL))
datos_brutos <- dplyr::bind_rows(lista_datos)


# =============================================================================
# 4. LIMPIEZA DE DATOS
# =============================================================================

if (!"date" %in% names(datos_brutos)) {
  stop("No se encontró una columna llamada 'date'. Revisa el nombre de la columna temporal en el Excel.")
}

datos <- datos_brutos |>
  dplyr::select(-dplyr::matches("TCA09", ignore.case = TRUE))

datos <- datos |>
  dplyr::mutate(
    date = lubridate::parse_date_time(
      as.character(date),
      orders = c("Y-m-d H:M:S", "Y-m-d H:M", "Y/m/d H:M:S", "Y/m/d H:M"),
      tz = TZ_DATOS
    )
  ) |>
  dplyr::filter(!is.na(date))

columnas_numericas_posibles <- c("BC880_AE36", "BC880_AE36_ONA", "BC880_AE33", "BC880_AE33_ONA", "BC_MAAP_ug_m3")
columnas_numericas <- intersect(columnas_numericas_posibles, names(datos))

datos <- datos |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(columnas_numericas),
      ~ readr::parse_number(as.character(.x), locale = readr::locale(decimal_mark = "."))
    )
  )

datos <- datos |>
  dplyr::select(-dplyr::any_of("archivo_origen")) |>
  dplyr::arrange(date) |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    dplyr::across(dplyr::everything(), primero_no_na),
    .groups = "drop"
  ) |>
  dplyr::arrange(date)

if (PREFERIR_ONA && "BC880_AE33_ONA" %in% names(datos)) {
  COL_AE33 <- "BC880_AE33_ONA"
} else {
  COL_AE33 <- "BC880_AE33"
}

if (PREFERIR_ONA && "BC880_AE36_ONA" %in% names(datos)) {
  COL_AE36 <- "BC880_AE36_ONA"
} else {
  COL_AE36 <- "BC880_AE36"
}

columnas_necesarias <- c(COL_AE33, COL_AE36, "BC_MAAP_ug_m3")
faltan_columnas <- setdiff(columnas_necesarias, names(datos))

if (length(faltan_columnas) > 0) {
  stop("Faltan estas columnas necesarias: ", paste(faltan_columnas, collapse = ", "))
}

datos_analisis <- datos |>
  dplyr::transmute(
    date = date,
    AE33 = .data[[COL_AE33]],
    AE36 = .data[[COL_AE36]],
    MAAP = BC_MAAP_ug_m3 * FACTOR_MAAP
  )

if (ELIMINAR_NEGATIVOS) {
  datos_analisis <- datos_analisis |>
    dplyr::mutate(
      dplyr::across(c(AE33, AE36, MAAP), ~ ifelse(.x < 0, NA_real_, .x))
    )
}

if (is.finite(LIMITE_SUPERIOR)) {
  n_outliers <- datos_analisis |>
    dplyr::summarise(
      AE33 = sum(AE33 > LIMITE_SUPERIOR, na.rm = TRUE),
      AE36 = sum(AE36 > LIMITE_SUPERIOR, na.rm = TRUE),
      MAAP = sum(MAAP > LIMITE_SUPERIOR, na.rm = TRUE)
    )
  
  cat("\nValores eliminados por superar", LIMITE_SUPERIOR, ":\n")
  print(n_outliers)
  
  datos_analisis <- datos_analisis |>
    dplyr::mutate(
      dplyr::across(c(AE33, AE36, MAAP), ~ ifelse(.x > LIMITE_SUPERIOR, NA_real_, .x))
    )
}

datos_analisis <- datos_analisis |>
  dplyr::mutate(orden = dplyr::row_number())

readr::write_csv(datos_analisis, file.path(DIR_SALIDA, "datos_limpios_principales.csv"))


# =============================================================================
# 4B. ANÁLISIS TEMPORAL DEL CAMBIO DE REFERENCIA AE33 -> AE36
# =============================================================================
#
# Este bloque NO reemplaza ninguno de los análisis originales.
# Agrega:
#   1) fechas de disponibilidad de AE33 y AE36;
#   2) período de solapamiento entre ambos;
#   3) una fecha candidata de cambio de referencia;
#   4) una única serie de referencia oficial:
#          AE33 antes del cambio
#          AE36 desde el cambio en adelante.
#
# Si existe una fecha oficial entregada por CETAM, es preferible escribirla
# arriba en FECHA_CAMBIO_REFERENCIA_MANUAL.
# =============================================================================

fechas_ae33_validas <- datos_analisis$date[is.finite(datos_analisis$AE33)]
fechas_ae36_validas <- datos_analisis$date[is.finite(datos_analisis$AE36)]

if (length(fechas_ae33_validas) == 0) {
  stop("AE33 no tiene observaciones válidas para determinar el cambio de referencia.")
}

if (length(fechas_ae36_validas) == 0) {
  stop("AE36 no tiene observaciones válidas para determinar el cambio de referencia.")
}

PRIMER_AE33 <- min(fechas_ae33_validas)
ULTIMO_AE33 <- max(fechas_ae33_validas)

PRIMER_AE36 <- min(fechas_ae36_validas)
ULTIMO_AE36 <- max(fechas_ae36_validas)

datos_solapamiento_ae33_ae36 <- datos_analisis |>
  dplyr::filter(is.finite(AE33), is.finite(AE36))

if (nrow(datos_solapamiento_ae33_ae36) > 0) {
  INICIO_SOLAPAMIENTO <- min(datos_solapamiento_ae33_ae36$date)
  FIN_SOLAPAMIENTO <- max(datos_solapamiento_ae33_ae36$date)
} else {
  INICIO_SOLAPAMIENTO <- as.POSIXct(NA, tz = TZ_DATOS)
  FIN_SOLAPAMIENTO <- as.POSIXct(NA, tz = TZ_DATOS)
}

ae36_posterior_ultimo_ae33 <- datos_analisis$date[
  datos_analisis$date > ULTIMO_AE33 &
    is.finite(datos_analisis$AE36)
]

if (
  !is.na(FECHA_CAMBIO_REFERENCIA_MANUAL) &&
  nzchar(FECHA_CAMBIO_REFERENCIA_MANUAL)
) {
  FECHA_CAMBIO_REFERENCIA <- lubridate::parse_date_time(
    FECHA_CAMBIO_REFERENCIA_MANUAL,
    orders = c("Y-m-d H:M:S", "Y-m-d H:M"),
    tz = TZ_DATOS
  )
  
  if (is.na(FECHA_CAMBIO_REFERENCIA)) {
    stop(
      "FECHA_CAMBIO_REFERENCIA_MANUAL tiene formato inválido. ",
      "Usa 'YYYY-MM-DD HH:MM'."
    )
  }
  
  FUENTE_FECHA_CAMBIO <- "manual: fecha definida por el usuario"
  
} else {
  
  if (length(ae36_posterior_ultimo_ae33) == 0) {
    stop(
      "No fue posible inferir automáticamente una fecha de cambio. ",
      "Define FECHA_CAMBIO_REFERENCIA_MANUAL en la sección de configuración."
    )
  }
  
  FECHA_CAMBIO_REFERENCIA <- min(ae36_posterior_ultimo_ae33)
  FUENTE_FECHA_CAMBIO <- paste0(
    "automática: primer AE36 válido posterior al último AE33 válido"
  )
}

tabla_cambio_referencia <- tibble::tibble(
  indicador = c(
    "Primer registro válido AE33",
    "Último registro válido AE33",
    "Primer registro válido AE36",
    "Último registro válido AE36",
    "Inicio del solapamiento AE33-AE36",
    "Fin del solapamiento AE33-AE36",
    "Fecha utilizada para el cambio AE33->AE36",
    "Fuente de la fecha de cambio"
  ),
  valor = c(
    format(PRIMER_AE33, "%Y-%m-%d %H:%M:%S"),
    format(ULTIMO_AE33, "%Y-%m-%d %H:%M:%S"),
    format(PRIMER_AE36, "%Y-%m-%d %H:%M:%S"),
    format(ULTIMO_AE36, "%Y-%m-%d %H:%M:%S"),
    ifelse(
      is.na(INICIO_SOLAPAMIENTO),
      NA_character_,
      format(INICIO_SOLAPAMIENTO, "%Y-%m-%d %H:%M:%S")
    ),
    ifelse(
      is.na(FIN_SOLAPAMIENTO),
      NA_character_,
      format(FIN_SOLAPAMIENTO, "%Y-%m-%d %H:%M:%S")
    ),
    format(FECHA_CAMBIO_REFERENCIA, "%Y-%m-%d %H:%M:%S"),
    FUENTE_FECHA_CAMBIO
  )
)

cat("\n============================================================\n")
cat("ANÁLISIS DEL CAMBIO DE REFERENCIA AE33 -> AE36\n")
cat("============================================================\n")
print(tabla_cambio_referencia)
cat("============================================================\n\n")

# Construir una única serie de referencia.
# No se usa AE36 como reemplazo de AE33 antes de la fecha de cambio ni viceversa.
# Si el instrumento oficialmente vigente no tiene dato en un instante, la
# referencia queda NA en ese instante.
datos_referencia <- datos_analisis |>
  dplyr::mutate(
    instrumento_referencia = dplyr::if_else(
      date < FECHA_CAMBIO_REFERENCIA,
      "AE33",
      "AE36"
    ),
    fase_referencia = dplyr::if_else(
      date < FECHA_CAMBIO_REFERENCIA,
      "Fase AE33",
      "Fase AE36"
    ),
    REFERENCIA_OFICIAL = dplyr::if_else(
      date < FECHA_CAMBIO_REFERENCIA,
      AE33,
      AE36
    )
  )

datos_fase_ae33 <- datos_referencia |>
  dplyr::filter(date < FECHA_CAMBIO_REFERENCIA)

datos_fase_ae36 <- datos_referencia |>
  dplyr::filter(date >= FECHA_CAMBIO_REFERENCIA)

tabla_disponibilidad_fases <- datos_referencia |>
  dplyr::group_by(fase_referencia, instrumento_referencia) |>
  dplyr::summarise(
    fecha_inicio = min(date),
    fecha_fin = max(date),
    n_filas = dplyr::n(),
    n_referencia_valida = sum(is.finite(REFERENCIA_OFICIAL)),
    n_maap_valida = sum(is.finite(MAAP)),
    n_pares_referencia_maap = sum(
      is.finite(REFERENCIA_OFICIAL) & is.finite(MAAP)
    ),
    .groups = "drop"
  )

readr::write_csv(
  tabla_cambio_referencia,
  file.path(DIR_TABLAS, "analisis_fecha_cambio_referencia.csv")
)

readr::write_csv(
  tabla_disponibilidad_fases,
  file.path(DIR_TABLAS, "disponibilidad_por_fase_referencia.csv")
)

readr::write_csv(
  datos_referencia,
  file.path(DIR_SALIDA, "datos_con_referencia_oficial_AE33_AE36.csv")
)


# =============================================================================
# 5. ESTADÍSTICOS DESCRIPTIVOS
# =============================================================================

descriptivos <- dplyr::bind_rows(
  descriptivos_variable(datos_analisis$AE33, "AE33"),
  descriptivos_variable(datos_analisis$AE36, "AE36"),
  descriptivos_variable(datos_analisis$MAAP, "MAAP"),
  descriptivos_variable(
    datos_referencia$REFERENCIA_OFICIAL,
    "Referencia oficial AE33->AE36"
  )
)

readr::write_csv(descriptivos, file.path(DIR_TABLAS, "descriptivos.csv"))
print(descriptivos)


# =============================================================================
# 6. GRÁFICOS GENERALES DE LOS TRES INSTRUMENTOS
# =============================================================================

datos_long <- datos_analisis |>
  tidyr::pivot_longer(
    cols = c(AE33, AE36, MAAP),
    names_to = "instrumento",
    values_to = "medicion"
  )

# 6.1 Datos individuales
for (inst in c("AE33", "AE36", "MAAP")) {
  df_i <- datos_analisis |>
    dplyr::select(orden, date, valor = dplyr::all_of(inst)) |>
    dplyr::filter(is.finite(valor))
  
  p <- ggplot(df_i, aes(x = date, y = valor)) +
    geom_point(alpha = 0.45, size = 0.7, color = "#2B5C8F") +
    scale_x_datetime(date_breaks = "3 days", date_labels = "%d-%m") +
    labs(
      title = paste("Datos individuales -", inst),
      subtitle = paste(NOMBRE_ANALITO, "|", UNIDAD_COMUN),
      x = "Fecha",
      y = "Medición"
    )
  
  guardar_plot(p, paste0("01_datos_individuales_", inst), width = 9, height = 5)
}

# 6.2 Histogramas
for (inst in c("AE33", "AE36", "MAAP")) {
  df_i <- datos_analisis |>
    dplyr::select(valor = dplyr::all_of(inst)) |>
    dplyr::filter(is.finite(valor))
  
  p <- ggplot(df_i, aes(x = valor)) +
    geom_histogram(bins = 60, boundary = 0, fill = "#2B5C8F", color = "white") +
    labs(
      title = paste("Histograma -", inst),
      subtitle = paste(NOMBRE_ANALITO, "|", UNIDAD_COMUN),
      x = "Medición",
      y = "Frecuencia"
    )
  
  guardar_plot(p, paste0("02_histograma_", inst), width = 8, height = 5)
  
  if (nrow(df_i) >= 20) {
    lim <- quantile(df_i$valor, probs = c(0.005, 0.995), na.rm = TRUE)
    if (is.finite(lim[1]) && is.finite(lim[2]) && lim[1] < lim[2]) {
      p_zoom <- p + coord_cartesian(xlim = lim) +
        labs(title = paste("Histograma -", inst, "(zoom 99% central)"))
      guardar_plot(p_zoom, paste0("02b_histograma_zoom_", inst), width = 8, height = 5)
    }
  }
}

# 6.3 Boxplots
p_box <- datos_long |>
  dplyr::filter(is.finite(medicion)) |>
  ggplot(aes(x = instrumento, y = medicion, fill = instrumento)) +
  geom_boxplot(outlier.alpha = 0.20) +
  scale_fill_manual(values = c("AE33" = "#D95F02", "AE36" = "#7570B3", "MAAP" = "#1B9E77")) +
  labs(
    title = "Boxplots comparativos",
    subtitle = paste(NOMBRE_ANALITO, "|", UNIDAD_COMUN),
    x = NULL,
    y = "Medición"
  )

guardar_plot(p_box, "03_boxplot_comparativo", width = 8, height = 6)

valores_finitos <- datos_long$medicion[is.finite(datos_long$medicion)]
if (length(valores_finitos) >= 20) {
  lim_box <- quantile(valores_finitos, probs = c(0.005, 0.995), na.rm = TRUE)
  if (is.finite(lim_box[1]) && is.finite(lim_box[2]) && lim_box[1] < lim_box[2]) {
    p_box_zoom <- p_box + coord_cartesian(ylim = lim_box) +
      labs(title = "Boxplots comparativos (zoom 99% central)")
    guardar_plot(p_box_zoom, "03b_boxplot_comparativo_zoom", width = 8, height = 6)
  }
}

# 6.4 QQ-plots
for (inst in c("AE33", "AE36", "MAAP")) {
  df_i <- datos_analisis |>
    dplyr::select(valor = dplyr::all_of(inst)) |>
    dplyr::filter(is.finite(valor))
  
  if (nrow(df_i) > 5000) {
    df_i <- dplyr::slice_sample(df_i, n = 5000)
  }
  
  p <- ggplot(df_i, aes(sample = valor)) +
    stat_qq(alpha = 0.45, size = 0.8, color = "#2B5C8F") +
    stat_qq_line(color = "red") +
    labs(
      title = paste("QQ-plot -", inst),
      subtitle = "Evaluación gráfica de normalidad",
      x = "Cuantiles teóricos",
      y = "Cuantiles observados"
    )
  
  guardar_plot(p, paste0("04_qqplot_", inst), width = 7, height = 6)
}

# 6.5 Series temporales (Con colores diferenciados)
p_series_3 <- datos_long |>
  dplyr::filter(is.finite(medicion)) |>
  ggplot(aes(x = date, y = medicion, group = instrumento, color = instrumento)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  scale_color_manual(values = c("AE33" = "#D95F02", "AE36" = "#7570B3", "MAAP" = "#1B9E77")) +
  scale_x_datetime(date_breaks = "3 days", date_labels = "%d-%m") +
  labs(
    title = "Series temporales - AE33, AE36 y MAAP",
    subtitle = paste(NOMBRE_ANALITO, "|", UNIDAD_COMUN),
    x = "Fecha",
    y = "Medición",
    color = "Instrumento"
  )

guardar_plot(p_series_3, "05_series_temporales_3_instrumentos", width = 12, height = 6)

# 6.5B Series temporales con fecha de cambio de referencia marcada
p_series_cambio <- p_series_3 +
  geom_vline(
    xintercept = as.numeric(FECHA_CAMBIO_REFERENCIA),
    linetype = "dashed",
    linewidth = 0.9,
    color = "black"
  ) +
  labs(
    title = "Series temporales - cambio de referencia AE33 -> AE36",
    subtitle = paste0(
      "Línea vertical: fecha de cambio utilizada = ",
      format(FECHA_CAMBIO_REFERENCIA, "%Y-%m-%d %H:%M")
    )
  )

guardar_plot(
  p_series_cambio,
  "05b_series_temporales_con_cambio_referencia",
  width = 12,
  height = 6
)

# 6.5C Serie única de referencia oficial vs MAAP
serie_ref_maap <- datos_referencia |>
  dplyr::select(date, REFERENCIA_OFICIAL, MAAP) |>
  tidyr::pivot_longer(
    cols = c(REFERENCIA_OFICIAL, MAAP),
    names_to = "serie",
    values_to = "medicion"
  ) |>
  dplyr::mutate(
    serie = dplyr::recode(
      serie,
      REFERENCIA_OFICIAL = "Referencia oficial AE33->AE36",
      MAAP = "MAAP"
    )
  ) |>
  dplyr::filter(is.finite(medicion))

p_serie_ref_maap <- ggplot(
  serie_ref_maap,
  aes(x = date, y = medicion, color = serie, group = serie)
) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  geom_vline(
    xintercept = as.numeric(FECHA_CAMBIO_REFERENCIA),
    linetype = "dashed",
    linewidth = 0.8,
    color = "black"
  ) +
  scale_x_datetime(date_breaks = "3 days", date_labels = "%d-%m") +
  labs(
    title = "MAAP vs referencia oficial cambiante",
    subtitle = "AE33 antes del cambio y AE36 desde el cambio",
    x = "Fecha",
    y = "Medición",
    color = "Serie"
  )

guardar_plot(
  p_serie_ref_maap,
  "05c_MAAP_vs_referencia_oficial_serie",
  width = 12,
  height = 6
)

# 6.6 Series temporales estandarizadas
datos_z <- datos_analisis |>
  dplyr::mutate(dplyr::across(c(AE33, AE36, MAAP), ~ as.numeric(scale(.x)))) |>
  tidyr::pivot_longer(cols = c(AE33, AE36, MAAP), names_to = "instrumento", values_to = "z") |>
  dplyr::filter(is.finite(z))

p_series_z <- ggplot(datos_z, aes(x = date, y = z, group = instrumento, color = instrumento)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  scale_color_manual(values = c("AE33" = "#D95F02", "AE36" = "#7570B3", "MAAP" = "#1B9E77")) +
  scale_x_datetime(date_breaks = "3 days", date_labels = "%d-%m") +
  labs(
    title = "Series temporales estandarizadas",
    subtitle = "Cada instrumento expresado en puntajes z",
    x = "Fecha",
    y = "z",
    color = "Instrumento"
  )

guardar_plot(p_series_z, "06_series_temporales_estandarizadas", width = 12, height = 6)

# 6.7 Matriz de correlaciones
mat_cor <- cor(datos_analisis |> dplyr::select(AE33, AE36, MAAP), use = "pairwise.complete.obs", method = "pearson")
tabla_cor <- as.data.frame(as.table(mat_cor))
names(tabla_cor) <- c("instrumento_1", "instrumento_2", "Pearson")

p_cor <- ggplot(tabla_cor, aes(x = instrumento_1, y = instrumento_2, fill = Pearson)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.3f", Pearson))) +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0) +
  labs(
    title = "Matriz de correlación de Pearson",
    subtitle = "Correlación = asociación lineal; no equivale a concordancia",
    x = NULL, y = NULL, fill = "Pearson"
  ) +
  coord_equal()

guardar_plot(p_cor, "07_matriz_correlacion_3_instrumentos", width = 7, height = 6)
readr::write_csv(tabla_cor, file.path(DIR_TABLAS, "matriz_correlacion.csv"))


# =============================================================================
# 7. FUNCIÓN COMPLETA DE ANÁLISIS POR PAREJA
# =============================================================================

analizar_par <- function(df, x_var, y_var, nombre_x, nombre_y, prefijo, c_usuario = C_TOLERANCIA) {
  
  par <- df |>
    dplyr::transmute(date = date, x = .data[[x_var]], y = .data[[y_var]]) |>
    dplyr::filter(is.finite(x), is.finite(y)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(orden = dplyr::row_number())
  
  if (nrow(par) < 3) {
    warning("Muy pocos pares completos para ", nombre_x, " vs ", nombre_y)
    return(NULL)
  }
  
  par <- par |> dplyr::mutate(diferencia = x - y, promedio = (x + y) / 2)
  n <- nrow(par)
  
  sesgo <- mean(par$diferencia)
  sd_dif <- sd(par$diferencia)
  loa_inf <- sesgo - 1.96 * sd_dif
  loa_sup <- sesgo + 1.96 * sd_dif
  
  pearson <- cor(par$x, par$y, method = "pearson")
  ccc <- ccc_lin(par$x, par$y)
  cb <- if (is.finite(pearson) && abs(pearson) > .Machine$double.eps) ccc / pearson else NA_real_
  
  modelo <- lm(y ~ x, data = par)
  par$ajustado <- fitted(modelo)
  par$residuo <- residuals(modelo)
  
  intercepto <- unname(coef(modelo)[1])
  pendiente <- unname(coef(modelo)[2])
  r2 <- summary(modelo)$r.squared
  
  rmse <- sqrt(mean((par$y - par$x)^2))
  mae <- mean(abs(par$y - par$x))
  cm <- comovimiento(par$x, par$y)
  
  if (is.finite(c_usuario)) {
    c_val <- c_usuario
    fuente_c <- "definida_por_usuario"
  } else {
    c_val <- sd(par$x)
    fuente_c <- "exploratoria_sd_instrumento_referencia"
  }
  
  pa_e <- pa_empirica(par$x, par$y, c_val)
  pa_n <- pa_normal(par$x, par$y, c_val)
  
  resumen <- tibble::tibble(
    comparacion = paste(nombre_x, "vs", nombre_y),
    instrumento_referencia = nombre_x,
    instrumento_comparado = nombre_y,
    n_pares = n,
    Pearson = pearson,
    CCC_Lin = ccc,
    Cb_factor_correccion_sesgo = cb,
    sesgo_medio_X_menos_Y = sesgo,
    sd_diferencias = sd_dif,
    limite_concordancia_inferior_95 = loa_inf,
    limite_concordancia_superior_95 = loa_sup,
    regresion_intercepto = intercepto,
    regresion_pendiente = pendiente,
    R2_regresion = r2,
    RMSE_X_vs_Y = rmse,
    MAE_X_vs_Y = mae,
    comovimiento_primeras_diferencias = cm,
    c_tolerancia = c_val,
    fuente_c = fuente_c,
    PA_empirica = pa_e,
    PA_normal = pa_n
  )
  
  readr::write_csv(par, file.path(DIR_TABLAS, paste0(nombre_seguro(prefijo), "_datos_pareados.csv")))
  
  # 7.1 Nube de puntos
  p_scatter <- ggplot(par, aes(x = x, y = y)) +
    geom_point(alpha = 0.30, size = 0.9, color = "#2B5C8F") +
    labs(title = paste("Nube de puntos:", nombre_x, "vs", nombre_y), x = nombre_x, y = nombre_y)
  
  guardar_plot(p_scatter, paste0(prefijo, "_01_nube_puntos"))
  
  # 7.2 Recta de identidad
  p_identidad <- p_scatter +
    geom_abline(intercept = 0, slope = 1, linewidth = 0.9, linetype = "dashed", color = "black") +
    coord_equal() +
    labs(title = paste("Concordancia visual:", nombre_x, "vs", nombre_y), subtitle = "Línea discontinua: identidad y = x")
  
  guardar_plot(p_identidad, paste0(prefijo, "_02_nube_identidad"))
  
  lim_zoom <- quantile(c(par$x, par$y), probs = c(0.005, 0.995), na.rm = TRUE)
  if (all(is.finite(lim_zoom)) && lim_zoom[1] < lim_zoom[2]) {
    p_identidad_zoom <- p_identidad +
      coord_equal(xlim = lim_zoom, ylim = lim_zoom) +
      labs(title = paste("Concordancia visual:", nombre_x, "vs", nombre_y, "(zoom 99% central)"))
    guardar_plot(p_identidad_zoom, paste0(prefijo, "_02b_nube_identidad_zoom"))
  }
  
  # 7.3 Regresión
  p_reg <- ggplot(par, aes(x = x, y = y)) +
    geom_point(alpha = 0.30, size = 0.9, color = "#2B5C8F") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.8) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.9, color = "red") +
    labs(
      title = paste("Regresión:", nombre_x, "vs", nombre_y),
      subtitle = paste0("Pearson = ", round(pearson, 3), " | CCC = ", round(ccc, 3), " | y = ", round(intercepto, 3), " + ", round(pendiente, 3), "x"),
      x = nombre_x, y = nombre_y
    )
  
  guardar_plot(p_reg, paste0(prefijo, "_03_nube_regresion"))
  
  # 7.4 Series temporales por pareja (Colores diferenciados)
  par_long <- par |>
    dplyr::select(date, x, y) |>
    tidyr::pivot_longer(cols = c(x, y), names_to = "serie", values_to = "valor") |>
    dplyr::mutate(serie = dplyr::recode(serie, x = nombre_x, y = nombre_y))
  
  p_series <- ggplot(par_long, aes(x = date, y = valor, group = serie, color = serie)) +
    geom_line(linewidth = 0.5, alpha = 0.85) +
    scale_color_brewer(palette = "Set1") +
    labs(title = paste("Series temporales:", nombre_x, "y", nombre_y), x = "Fecha", y = "Medición", color = "Instrumento")
  
  guardar_plot(p_series, paste0(prefijo, "_04_series_temporales"), width = 12, height = 6)
  
  # 7.5 Diferencias temporales
  p_dif_t <- ggplot(par, aes(x = date, y = diferencia)) +
    geom_point(alpha = 0.35, size = 0.8, color = "#2B5C8F") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_hline(yintercept = sesgo, linewidth = 0.8, color = "red") +
    labs(title = paste("Diferencias temporales:", nombre_x, "-", nombre_y), subtitle = paste0("Sesgo medio = ", round(sesgo, 3)), x = "Fecha", y = paste(nombre_x, "-", nombre_y))
  
  guardar_plot(p_dif_t, paste0(prefijo, "_05_diferencias_tiempo"), width = 12, height = 5)
  
  # 7.6 Bland-Altman
  p_ba <- ggplot(par, aes(x = promedio, y = diferencia)) +
    geom_point(alpha = 0.35, size = 0.9, color = "#2B5C8F") +
    geom_hline(yintercept = sesgo, linewidth = 0.9, color = "blue") +
    geom_hline(yintercept = loa_inf, linetype = "dashed", linewidth = 0.8, color = "red") +
    geom_hline(yintercept = loa_sup, linetype = "dashed", linewidth = 0.8, color = "red") +
    labs(
      title = paste("Bland-Altman:", nombre_x, "vs", nombre_y),
      subtitle = paste0("Sesgo = ", round(sesgo, 3), " | LoA 95% = [", round(loa_inf, 3), ", ", round(loa_sup, 3), "]"),
      x = "Promedio de las dos mediciones", y = paste(nombre_x, "-", nombre_y)
    )
  
  guardar_plot(p_ba, paste0(prefijo, "_06_bland_altman"))
  
  # 7.7 Residuos
  p_res <- ggplot(par, aes(x = ajustado, y = residuo)) +
    geom_point(alpha = 0.35, size = 0.9, color = "#2B5C8F") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Residuos de regresión:", nombre_x, "vs", nombre_y), x = "Valores ajustados", y = "Residuos")
  
  guardar_plot(p_res, paste0(prefijo, "_07_residuos"))
  
  # 7.8 Zona de acuerdo
  xmin <- min(par$x)
  xmax <- max(par$x)
  banda <- tibble::tibble(x = seq(xmin, xmax, length.out = 300)) |>
    dplyr::mutate(inferior = x - c_val, superior = x + c_val)
  
  p_zona <- ggplot(par, aes(x = x, y = y)) +
    geom_ribbon(data = banda, aes(x = x, ymin = inferior, ymax = superior), inherit.aes = FALSE, alpha = 0.18, fill = "blue") +
    geom_point(alpha = 0.30, size = 0.9, color = "#2B5C8F") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.8) +
    labs(
      title = paste("Zona de acuerdo:", nombre_x, "vs", nombre_y),
      subtitle = paste0("|X - Y| <= c, con c = ", round(c_val, 3), " | PA empírica = ", round(pa_e, 3)),
      x = nombre_x, y = nombre_y
    )
  
  guardar_plot(p_zona, paste0(prefijo, "_08_zona_acuerdo"))
  
  # 7.9 Curva PA
  abs_d <- abs(par$diferencia)
  c_max <- if (is.finite(C_MAX_CURVA)) C_MAX_CURVA else max(as.numeric(quantile(abs_d, 0.99, na.rm = TRUE)), 1.20 * c_val, na.rm = TRUE)
  if (!is.finite(c_max) || c_max <= 0) c_max <- 1
  
  c_grid <- seq(from = 0, to = c_max, length.out = 250)
  curva_pa <- tibble::tibble(
    c = c_grid,
    PA_empirica = vapply(c_grid, function(cc) pa_empirica(par$x, par$y, cc), numeric(1)),
    PA_normal = vapply(c_grid, function(cc) pa_normal(par$x, par$y, cc), numeric(1))
  )
  
  curva_pa_long <- curva_pa |>
    tidyr::pivot_longer(cols = c(PA_empirica, PA_normal), names_to = "metodo", values_to = "PA")
  
  p_pa <- ggplot(curva_pa_long, aes(x = c, y = PA, color = metodo)) +
    geom_line(linewidth = 0.9) +
    geom_hline(yintercept = 0.95, linetype = "dotted") +
    geom_vline(xintercept = c_val, linetype = "dashed") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_manual(values = c("PA_empirica" = "#D95F02", "PA_normal" = "#7570B3")) +
    labs(
      title = paste("Probability of Agreement:", nombre_x, "vs", nombre_y),
      subtitle = paste0("Línea vertical: c = ", round(c_val, 3), " | PA empírica = ", round(pa_e, 3)),
      x = "Tolerancia c", y = "PA(c)", color = "Estimación"
    )
  
  guardar_plot(p_pa, paste0(prefijo, "_09_curva_PA"), width = 8, height = 6)
  readr::write_csv(curva_pa, file.path(DIR_TABLAS, paste0(nombre_seguro(prefijo), "_curva_PA.csv")))
  
  # 7.10 Comovimiento
  if (nrow(par) >= 4) {
    dif_temporales <- tibble::tibble(dx = diff(par$x), dy = diff(par$y)) |>
      dplyr::filter(is.finite(dx), is.finite(dy))
    
    p_comov <- ggplot(dif_temporales, aes(x = dx, y = dy)) +
      geom_point(alpha = 0.30, size = 0.8, color = "#2B5C8F") +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
      labs(
        title = paste("Comovimiento temporal:", nombre_x, "vs", nombre_y),
        subtitle = paste0("Correlación de primeras diferencias = ", round(cm, 3)),
        x = paste0("Delta ", nombre_x), y = paste0("Delta ", nombre_y)
      )
    
    guardar_plot(p_comov, paste0(prefijo, "_10_comovimiento"))
  }
  
  list(resumen = resumen, datos_pareados = par, curva_pa = curva_pa)
}


# =============================================================================
# 8. ANÁLISIS DE LAS TRES COMPARACIONES PRINCIPALES
# =============================================================================

comparaciones <- list(
  list(x = "AE33", y = "AE36", nx = "AE33", ny = "AE36", prefijo = "AE33_vs_AE36"),
  list(x = "AE33", y = "MAAP", nx = "AE33", ny = "MAAP", prefijo = "AE33_vs_MAAP"),
  list(x = "AE36", y = "MAAP", nx = "AE36", ny = "MAAP", prefijo = "AE36_vs_MAAP")
)

resultados_pares <- purrr::map(
  comparaciones,
  function(comp) {
    analizar_par(
      df = datos_analisis,
      x_var = comp$x,
      y_var = comp$y,
      nombre_x = comp$nx,
      nombre_y = comp$ny,
      prefijo = comp$prefijo
    )
  }
)

resultados_pares <- resultados_pares[!vapply(resultados_pares, is.null, logical(1))]

tabla_comparaciones <- dplyr::bind_rows(purrr::map(resultados_pares, "resumen"))
readr::write_csv(tabla_comparaciones, file.path(DIR_TABLAS, "resultados_comparaciones_principales.csv"))
print(tabla_comparaciones)


# =============================================================================
# 9. COMPARACIÓN ADICIONAL AE33 CRUDO vs AE36 CRUDO
# =============================================================================

resultado_crudo <- NULL

if (all(c("BC880_AE33", "BC880_AE36") %in% names(datos))) {
  datos_crudos_ae <- datos |>
    dplyr::transmute(date = date, AE33_crudo = BC880_AE33, AE36_crudo = BC880_AE36)
  
  if (ELIMINAR_NEGATIVOS) {
    datos_crudos_ae <- datos_crudos_ae |>
      dplyr::mutate(
        AE33_crudo = ifelse(AE33_crudo < 0, NA_real_, AE33_crudo),
        AE36_crudo = ifelse(AE36_crudo < 0, NA_real_, AE36_crudo)
      )
  }
  
  resultado_crudo <- analizar_par(
    df = datos_crudos_ae,
    x_var = "AE33_crudo",
    y_var = "AE36_crudo",
    nombre_x = "AE33 crudo",
    nombre_y = "AE36 crudo",
    prefijo = "AE33_crudo_vs_AE36_crudo"
  )
}


# =============================================================================
# 9B. ANÁLISIS AGREGADO DE LA REFERENCIA CAMBIANTE Y DEL SOLAPAMIENTO
# =============================================================================
#
# Se conservan todos los análisis anteriores.
# Aquí se agregan cuatro análisis explícitos:
#
#   A) MAAP vs referencia oficial única AE33->AE36
#   B) MAAP vs AE33 SOLO durante la fase en que AE33 es la referencia
#   C) MAAP vs AE36 SOLO durante la fase en que AE36 es la referencia
#   D) AE33 vs AE36 SOLO durante su período de solapamiento
#
# =============================================================================

resultado_referencia_oficial <- analizar_par(
  df = datos_referencia,
  x_var = "REFERENCIA_OFICIAL",
  y_var = "MAAP",
  nombre_x = "Referencia oficial AE33->AE36",
  nombre_y = "MAAP",
  prefijo = "REFERENCIA_OFICIAL_AE33_AE36_vs_MAAP"
)

resultado_fase_ae33 <- analizar_par(
  df = datos_fase_ae33,
  x_var = "AE33",
  y_var = "MAAP",
  nombre_x = "AE33 (fase de referencia)",
  nombre_y = "MAAP",
  prefijo = "FASE_AE33_REFERENCIA_vs_MAAP"
)

resultado_fase_ae36 <- analizar_par(
  df = datos_fase_ae36,
  x_var = "AE36",
  y_var = "MAAP",
  nombre_x = "AE36 (fase de referencia)",
  nombre_y = "MAAP",
  prefijo = "FASE_AE36_REFERENCIA_vs_MAAP"
)

resultado_solapamiento <- analizar_par(
  df = datos_solapamiento_ae33_ae36,
  x_var = "AE33",
  y_var = "AE36",
  nombre_x = "AE33 (solapamiento)",
  nombre_y = "AE36 (solapamiento)",
  prefijo = "SOLAPAMIENTO_AE33_vs_AE36"
)

resultados_referencia_cambiante <- list(
  referencia_oficial = resultado_referencia_oficial,
  fase_ae33 = resultado_fase_ae33,
  fase_ae36 = resultado_fase_ae36,
  solapamiento = resultado_solapamiento
)

resultados_referencia_cambiante <- resultados_referencia_cambiante[
  !vapply(resultados_referencia_cambiante, is.null, logical(1))
]

tabla_referencia_cambiante <- dplyr::bind_rows(
  purrr::imap(
    resultados_referencia_cambiante,
    function(resultado, tipo) {
      resultado$resumen |>
        dplyr::mutate(tipo_analisis = tipo, .before = 1)
    }
  )
)

readr::write_csv(
  tabla_referencia_cambiante,
  file.path(DIR_TABLAS, "resultados_referencia_cambiante_y_solapamiento.csv")
)

# ---------------------------------------------------------------------------
# Gráfico conjunto: MAAP vs referencia oficial, distinguiendo la fase
# ---------------------------------------------------------------------------

pares_ref_fase <- datos_referencia |>
  dplyr::filter(
    is.finite(REFERENCIA_OFICIAL),
    is.finite(MAAP)
  )

p_ref_fases <- ggplot(
  pares_ref_fase,
  aes(
    x = REFERENCIA_OFICIAL,
    y = MAAP,
    color = instrumento_referencia
  )
) +
  geom_point(alpha = 0.35, size = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    linewidth = 0.8,
    color = "black"
  ) +
  geom_smooth(
    aes(group = instrumento_referencia),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.9
  ) +
  labs(
    title = "MAAP vs referencia oficial, distinguiendo la fase",
    subtitle = "AE33 antes del cambio y AE36 después del cambio",
    x = "Referencia oficial",
    y = "MAAP",
    color = "Referencia vigente"
  )

guardar_plot(
  p_ref_fases,
  "REFERENCIA_CAMBIANTE_MAAP_por_fase",
  width = 9,
  height = 7
)

# ---------------------------------------------------------------------------
# Bland-Altman por fase de referencia
# Diferencia = referencia - MAAP, consistente con analizar_par(x=referencia,y=MAAP)
# ---------------------------------------------------------------------------

ba_fases <- pares_ref_fase |>
  dplyr::mutate(
    diferencia = REFERENCIA_OFICIAL - MAAP,
    promedio = (REFERENCIA_OFICIAL + MAAP) / 2
  )

estad_ba_fases <- ba_fases |>
  dplyr::group_by(instrumento_referencia) |>
  dplyr::summarise(
    n = dplyr::n(),
    sesgo = mean(diferencia),
    sd_diferencias = sd(diferencia),
    LoA_inferior_95 = sesgo - 1.96 * sd_diferencias,
    LoA_superior_95 = sesgo + 1.96 * sd_diferencias,
    .groups = "drop"
  )

p_ba_fases <- ggplot(
  ba_fases,
  aes(x = promedio, y = diferencia)
) +
  geom_point(alpha = 0.35, size = 0.9, color = "#2B5C8F") +
  facet_wrap(~ instrumento_referencia, scales = "free") +
  geom_hline(
    data = estad_ba_fases,
    aes(yintercept = sesgo),
    color = "blue",
    linewidth = 0.8
  ) +
  geom_hline(
    data = estad_ba_fases,
    aes(yintercept = LoA_inferior_95),
    color = "red",
    linetype = "dashed"
  ) +
  geom_hline(
    data = estad_ba_fases,
    aes(yintercept = LoA_superior_95),
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Bland-Altman de MAAP por fase de referencia",
    subtitle = "Diferencia = referencia vigente - MAAP",
    x = "Promedio de las dos mediciones",
    y = "Referencia - MAAP"
  )

guardar_plot(
  p_ba_fases,
  "REFERENCIA_CAMBIANTE_bland_altman_por_fase",
  width = 11,
  height = 6
)

readr::write_csv(
  estad_ba_fases,
  file.path(DIR_TABLAS, "bland_altman_resumen_por_fase_referencia.csv")
)


# =============================================================================
# 10. GUARDAR TODO EN UN EXCEL DE RESULTADOS
# =============================================================================

lista_excel <- list(
  descriptivos = descriptivos,
  comparaciones_principales = tabla_comparaciones,
  matriz_correlacion = tabla_cor,
  cambio_referencia = tabla_cambio_referencia,
  disponibilidad_fases = tabla_disponibilidad_fases,
  referencia_cambiante = tabla_referencia_cambiante,
  bland_altman_por_fase = estad_ba_fases,
  serie_referencia_oficial = datos_referencia |>
    dplyr::select(
      date,
      AE33,
      AE36,
      REFERENCIA_OFICIAL,
      instrumento_referencia,
      fase_referencia,
      MAAP
    )
)

if (!is.null(resultado_crudo)) {
  lista_excel$comparacion_crudos <- resultado_crudo$resumen
}

configuracion <- tibble::tibble(
  parametro = c(
    "archivos", "version_AE33", "version_AE36", "PREFERIR_ONA",
    "CONVERTIR_MAAP_UG_A_NG", "FACTOR_MAAP", "UNIDAD_COMUN",
    "ELIMINAR_NEGATIVOS", "LIMITE_SUPERIOR", "C_TOLERANCIA_usuario",
    "FECHA_CAMBIO_REFERENCIA_MANUAL",
    "FECHA_CAMBIO_REFERENCIA_USADA",
    "FUENTE_FECHA_CAMBIO"
  ),
  valor = c(
    paste(basename(archivos), collapse = " ; "),
    COL_AE33, COL_AE36, as.character(PREFERIR_ONA),
    as.character(CONVERTIR_MAAP_UG_A_NG), as.character(FACTOR_MAAP), UNIDAD_COMUN,
    as.character(ELIMINAR_NEGATIVOS), as.character(LIMITE_SUPERIOR), as.character(C_TOLERANCIA),
    as.character(FECHA_CAMBIO_REFERENCIA_MANUAL),
    format(FECHA_CAMBIO_REFERENCIA, "%Y-%m-%d %H:%M:%S"),
    FUENTE_FECHA_CAMBIO
  )
)

lista_excel$configuracion <- configuracion

writexl::write_xlsx(
  lista_excel,
  path = file.path(DIR_SALIDA, "resultados_numericos_concordancia.xlsx")
)


# =============================================================================
# 11. RESUMEN FINAL EN CONSOLA
# =============================================================================

cat("\n============================================================\n")
cat("ANÁLISIS FINALIZADO EXITOSAMENTE\n")
cat("============================================================\n")
cat("Se conservaron todos los análisis originales.\n")
cat("Además se agregó:\n")
cat("  - análisis temporal del cambio AE33 -> AE36;\n")
cat("  - serie única de referencia oficial AE33->AE36;\n")
cat("  - MAAP vs referencia oficial global;\n")
cat("  - MAAP vs AE33 restringido a la fase AE33;\n")
cat("  - MAAP vs AE36 restringido a la fase AE36;\n")
cat("  - AE33 vs AE36 restringido al período de solapamiento.\n")
cat(
  "Fecha de cambio utilizada: ",
  format(FECHA_CAMBIO_REFERENCIA, "%Y-%m-%d %H:%M:%S"),
  "\n",
  sep = ""
)
cat("Fuente de la fecha: ", FUENTE_FECHA_CAMBIO, "\n", sep = "")
cat("============================================================\n")
