###############################################################################
# CONCORDANCIA ENTRE INSTRUMENTOS DE MATERIAL PARTICULADO
# CETAM = instrumento de referencia
# MT    = instrumento a comparar
#
# Variables:
#   - PM1.0
#   - PM2.5
#   - PM10
#
# ENTRADA:
#   datos_suavizados_PM/CETAM_MT_suavizados_alineados.csv
#
# Este script NO vuelve a suavizar los datos.
# Trabaja directamente con la base de 5 minutos ya suavizada y alineada.
#
# Para cada fraccion genera:
#   - datos individuales
#   - histogramas
#   - boxplot comparativo
#   - serie temporal
#   - nube de puntos
#   - nube + y=x
#   - nube + regresion
#   - diferencias MT-CETAM
#   - Bland-Altman
#   - QQ-plots
#   - residuos
#   - zona de acuerdo
#   - curva PA(c)
#
# Calcula:
#   - descriptivos
#   - Pearson
#   - CCC de Lin
#   - sesgo medio
#   - limites de concordancia
#   - regresion
#   - RMSE y MAE
#   - PA empirica y normal
###############################################################################

# =============================================================================
# 0. PAQUETES
# =============================================================================

paquetes <- c(
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
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
  install.packages(faltan, dependencies = TRUE)
}

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(writexl)

# =============================================================================
# 1. CONFIGURACION
# =============================================================================

ARCHIVO_DATOS <- file.path(
  "datos_suavizados_PM",
  "CETAM_MT_suavizados_alineados.csv"
)

INSTRUMENTO_REFERENCIA <- "CETAM"
INSTRUMENTO_COMPARADO <- "MT"

UNIDAD <- "ug/m3"

# Tolerancias para Probability of Agreement.
# Si quedan en NA, el codigo usa SD(CETAM) solo como valor exploratorio.
C_PM1_0 <- NA_real_
C_PM2_5 <- NA_real_
C_PM10  <- NA_real_

CARPETA_SALIDA <- "resultados_concordancia_PM"
CARPETA_GRAFICOS <- file.path(CARPETA_SALIDA, "graficos")
CARPETA_TABLAS <- file.path(CARPETA_SALIDA, "tablas")

dir.create(CARPETA_SALIDA, showWarnings = FALSE, recursive = TRUE)
dir.create(CARPETA_GRAFICOS, showWarnings = FALSE, recursive = TRUE)
dir.create(CARPETA_TABLAS, showWarnings = FALSE, recursive = TRUE)

theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

# =============================================================================
# 2. FUNCIONES AUXILIARES
# =============================================================================

guardar_grafico <- function(grafico, nombre, ancho = 8, alto = 6) {
  
  ggsave(
    filename = file.path(
      CARPETA_GRAFICOS,
      paste0(nombre, ".png")
    ),
    plot = grafico,
    width = ancho,
    height = alto,
    dpi = 300,
    bg = "white"
  )
}


ccc_lin <- function(x, y) {
  
  ok <- is.finite(x) & is.finite(y)
  
  x <- x[ok]
  y <- y[ok]
  
  if (length(x) < 3) {
    return(NA_real_)
  }
  
  2 * cov(x, y) /
    (
      var(x) +
        var(y) +
        (mean(x) - mean(y))^2
    )
}


pa_empirica <- function(referencia, comparado, c) {
  
  ok <- is.finite(referencia) & is.finite(comparado)
  
  d <- comparado[ok] - referencia[ok]
  
  if (length(d) == 0 || !is.finite(c)) {
    return(NA_real_)
  }
  
  mean(abs(d) <= c)
}


pa_normal <- function(referencia, comparado, c) {
  
  ok <- is.finite(referencia) & is.finite(comparado)
  
  d <- comparado[ok] - referencia[ok]
  
  if (length(d) < 2 || !is.finite(c)) {
    return(NA_real_)
  }
  
  mu_d <- mean(d)
  sd_d <- sd(d)
  
  if (!is.finite(sd_d) || sd_d == 0) {
    return(as.numeric(abs(mu_d) <= c))
  }
  
  pnorm((c - mu_d) / sd_d) -
    pnorm((-c - mu_d) / sd_d)
}


descriptivos_variable <- function(x, instrumento, fraccion) {
  
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(
      data.frame(
        fraccion = fraccion,
        instrumento = instrumento,
        n = 0,
        media = NA_real_,
        sd = NA_real_,
        mediana = NA_real_,
        q25 = NA_real_,
        q75 = NA_real_,
        minimo = NA_real_,
        maximo = NA_real_
      )
    )
  }
  
  data.frame(
    fraccion = fraccion,
    instrumento = instrumento,
    n = length(x),
    media = mean(x),
    sd = sd(x),
    mediana = median(x),
    q25 = as.numeric(quantile(x, 0.25)),
    q75 = as.numeric(quantile(x, 0.75)),
    minimo = min(x),
    maximo = max(x)
  )
}

# =============================================================================
# 3. LEER LA BASE SUAVIZADA
# =============================================================================

if (!file.exists(ARCHIVO_DATOS)) {
  
  cat(
    "\nNo encontre automaticamente:\n",
    ARCHIVO_DATOS,
    "\n\nSelecciona manualmente CETAM_MT_suavizados_alineados.csv\n\n",
    sep = ""
  )
  
  ARCHIVO_DATOS <- file.choose()
}

datos <- readr::read_csv(
  ARCHIVO_DATOS,
  show_col_types = FALSE
)

cat("\nArchivo utilizado:\n")
cat(ARCHIVO_DATOS, "\n\n")

cat("Columnas disponibles:\n")
print(names(datos))
cat("\n")

# =============================================================================
# 4. COMPROBAR COLUMNAS
# =============================================================================

columnas_necesarias <- c(
  "bloque_tiempo",
  "CETAM_PM1_0",
  "CETAM_PM2_5",
  "CETAM_PM10",
  "MT_PM1_0",
  "MT_PM2_5",
  "MT_PM10"
)

faltan_columnas <- setdiff(
  columnas_necesarias,
  names(datos)
)

if (length(faltan_columnas) > 0) {
  
  stop(
    paste0(
      "Faltan columnas necesarias: ",
      paste(faltan_columnas, collapse = ", ")
    )
  )
}

# Guardamos primero la columna original de tiempo.
tiempo_original <- datos$bloque_tiempo

# Intento 1: YYYY-MM-DD HH:MM:SS
tiempo_parseado <- lubridate::ymd_hms(
  tiempo_original,
  tz = "America/Santiago",
  quiet = TRUE
)

# Intento 2: YYYY-MM-DD HH:MM
if (all(is.na(tiempo_parseado))) {
  
  tiempo_parseado <- lubridate::ymd_hm(
    tiempo_original,
    tz = "America/Santiago",
    quiet = TRUE
  )
}

if (all(is.na(tiempo_parseado))) {
  stop("No pude interpretar la columna bloque_tiempo como fecha/hora.")
}

datos$bloque_tiempo <- tiempo_parseado

datos <- datos |>
  arrange(bloque_tiempo)

# =============================================================================
# 5. FUNCION PRINCIPAL PARA CADA FRACCION
# =============================================================================

analizar_fraccion <- function(
    datos,
    columna_ref,
    columna_mt,
    nombre_pm,
    prefijo,
    c_usuario = NA_real_
) {
  
  df <- data.frame(
    tiempo = datos$bloque_tiempo,
    CETAM = datos[[columna_ref]],
    MT = datos[[columna_mt]]
  )
  
  df <- df[
    is.finite(df$CETAM) &
      is.finite(df$MT),
  ]
  
  df <- df[
    order(df$tiempo),
  ]
  
  if (nrow(df) < 3) {
    
    warning(
      paste(
        "Muy pocos datos para",
        nombre_pm
      )
    )
    
    return(NULL)
  }
  
  df$orden <- seq_len(nrow(df))
  df$diferencia <- df$MT - df$CETAM
  df$promedio <- (df$MT + df$CETAM) / 2
  
  # ---------------------------------------------------------------------------
  # Estadisticos
  # ---------------------------------------------------------------------------
  
  pearson <- cor(
    df$CETAM,
    df$MT,
    method = "pearson"
  )
  
  ccc <- ccc_lin(
    df$CETAM,
    df$MT
  )
  
  sesgo <- mean(
    df$diferencia
  )
  
  sd_dif <- sd(
    df$diferencia
  )
  
  loa_inf <- sesgo - 1.96 * sd_dif
  loa_sup <- sesgo + 1.96 * sd_dif
  
  modelo <- lm(
    MT ~ CETAM,
    data = df
  )
  
  df$ajustado <- fitted(modelo)
  df$residuo <- residuals(modelo)
  
  intercepto <- unname(coef(modelo)[1])
  pendiente <- unname(coef(modelo)[2])
  r2 <- summary(modelo)$r.squared
  
  rmse <- sqrt(
    mean(
      (df$MT - df$CETAM)^2
    )
  )
  
  mae <- mean(
    abs(
      df$MT - df$CETAM
    )
  )
  
  # ---------------------------------------------------------------------------
  # Tolerancia c
  # ---------------------------------------------------------------------------
  
  if (is.finite(c_usuario)) {
    
    c_usado <- c_usuario
    fuente_c <- "definida_por_usuario"
    
  } else {
    
    c_usado <- sd(
      df$CETAM
    )
    
    fuente_c <- "exploratoria_SD_CETAM"
  }
  
  if (!is.finite(c_usado) || c_usado <= 0) {
    c_usado <- 1
    fuente_c <- "valor_auxiliar_1"
  }
  
  pa_e <- pa_empirica(
    df$CETAM,
    df$MT,
    c_usado
  )
  
  pa_n <- pa_normal(
    df$CETAM,
    df$MT,
    c_usado
  )
  
  # ---------------------------------------------------------------------------
  # Tabla resumen
  # ---------------------------------------------------------------------------
  
  resumen <- data.frame(
    fraccion = nombre_pm,
    referencia = "CETAM",
    comparado = "MT",
    n_pares = nrow(df),
    Pearson = pearson,
    CCC_Lin = ccc,
    sesgo_MT_menos_CETAM = sesgo,
    SD_diferencias = sd_dif,
    LoA_inferior_95 = loa_inf,
    LoA_superior_95 = loa_sup,
    regresion_intercepto = intercepto,
    regresion_pendiente = pendiente,
    R2 = r2,
    RMSE = rmse,
    MAE = mae,
    c_tolerancia = c_usado,
    fuente_c = fuente_c,
    PA_empirica = pa_e,
    PA_normal = pa_n
  )
  
  # ---------------------------------------------------------------------------
  # 1. Datos individuales
  # ---------------------------------------------------------------------------
  
  df_ind <- rbind(
    data.frame(
      orden = df$orden,
      valor = df$CETAM,
      instrumento = "CETAM"
    ),
    data.frame(
      orden = df$orden,
      valor = df$MT,
      instrumento = "MT"
    )
  )
  
  p_individual <- ggplot(
    df_ind,
    aes(
      x = orden,
      y = valor,
      shape = instrumento
    )
  ) +
    geom_point(
      alpha = 0.60,
      size = 1.5
    ) +
    labs(
      title = paste(
        "Datos individuales -",
        nombre_pm
      ),
      x = "Orden de observacion",
      y = paste0(
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      shape = "Instrumento"
    )
  
  guardar_grafico(
    p_individual,
    paste0(
      prefijo,
      "_01_datos_individuales"
    ),
    9,
    5
  )
  
  # ---------------------------------------------------------------------------
  # 2. Histograma CETAM
  # ---------------------------------------------------------------------------
  
  p_hist_ref <- ggplot(
    df,
    aes(x = CETAM)
  ) +
    geom_histogram(
      bins = 20
    ) +
    labs(
      title = paste(
        "Histograma CETAM -",
        nombre_pm
      ),
      x = paste0(
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      y = "Frecuencia"
    )
  
  guardar_grafico(
    p_hist_ref,
    paste0(
      prefijo,
      "_02_histograma_CETAM"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 3. Histograma MT
  # ---------------------------------------------------------------------------
  
  p_hist_mt <- ggplot(
    df,
    aes(x = MT)
  ) +
    geom_histogram(
      bins = 20
    ) +
    labs(
      title = paste(
        "Histograma MT -",
        nombre_pm
      ),
      x = paste0(
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      y = "Frecuencia"
    )
  
  guardar_grafico(
    p_hist_mt,
    paste0(
      prefijo,
      "_03_histograma_MT"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 4. Boxplot
  # ---------------------------------------------------------------------------
  
  df_box <- rbind(
    data.frame(
      instrumento = "CETAM",
      valor = df$CETAM
    ),
    data.frame(
      instrumento = "MT",
      valor = df$MT
    )
  )
  
  p_box <- ggplot(
    df_box,
    aes(
      x = instrumento,
      y = valor
    )
  ) +
    geom_boxplot(
      outlier.alpha = 0.35
    ) +
    labs(
      title = paste(
        "Boxplot comparativo -",
        nombre_pm
      ),
      x = NULL,
      y = paste0(
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      )
    )
  
  guardar_grafico(
    p_box,
    paste0(
      prefijo,
      "_04_boxplot"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 5. Serie temporal
  # ---------------------------------------------------------------------------
  
  df_series <- rbind(
    data.frame(
      tiempo = df$tiempo,
      valor = df$CETAM,
      instrumento = "CETAM"
    ),
    data.frame(
      tiempo = df$tiempo,
      valor = df$MT,
      instrumento = "MT"
    )
  )
  
  p_series <- ggplot(
    df_series,
    aes(
      x = tiempo,
      y = valor,
      linetype = instrumento,
      group = instrumento
    )
  ) +
    geom_line(
      linewidth = 0.55
    ) +
    geom_point(
      size = 1,
      alpha = 0.65
    ) +
    labs(
      title = paste(
        "Serie temporal CETAM vs MT -",
        nombre_pm
      ),
      x = "Tiempo",
      y = paste0(
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      linetype = "Instrumento"
    )
  
  guardar_grafico(
    p_series,
    paste0(
      prefijo,
      "_05_serie_temporal"
    ),
    11,
    6
  )
  
  # ---------------------------------------------------------------------------
  # 6. Nube de puntos
  # ---------------------------------------------------------------------------
  
  p_nube <- ggplot(
    df,
    aes(
      x = CETAM,
      y = MT
    )
  ) +
    geom_point(
      alpha = 0.65,
      size = 1.8
    ) +
    labs(
      title = paste(
        "Nube de puntos CETAM vs MT -",
        nombre_pm
      ),
      x = paste0(
        "CETAM ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      y = paste0(
        "MT ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      )
    )
  
  guardar_grafico(
    p_nube,
    paste0(
      prefijo,
      "_06_nube"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 7. Nube + identidad y=x
  # ---------------------------------------------------------------------------
  
  p_identidad <- ggplot(
    df,
    aes(
      x = CETAM,
      y = MT
    )
  ) +
    geom_point(
      alpha = 0.65,
      size = 1.8
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 0.9
    ) +
    coord_equal() +
    labs(
      title = paste(
        "Concordancia visual CETAM vs MT -",
        nombre_pm
      ),
      subtitle = "Linea discontinua: acuerdo perfecto y = x",
      x = paste0(
        "CETAM ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      y = paste0(
        "MT ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      )
    )
  
  guardar_grafico(
    p_identidad,
    paste0(
      prefijo,
      "_07_identidad"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 8. Nube + regresion + identidad
  # ---------------------------------------------------------------------------
  
  p_regresion <- ggplot(
    df,
    aes(
      x = CETAM,
      y = MT
    )
  ) +
    geom_point(
      alpha = 0.65,
      size = 1.8
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.9
    ) +
    labs(
      title = paste(
        "Regresion CETAM vs MT -",
        nombre_pm
      ),
      subtitle = paste0(
        "Pearson = ",
        round(pearson, 3),
        " | CCC = ",
        round(ccc, 3),
        " | MT = ",
        round(intercepto, 3),
        " + ",
        round(pendiente, 3),
        " CETAM"
      ),
      x = paste0(
        "CETAM ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      y = paste0(
        "MT ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      )
    )
  
  guardar_grafico(
    p_regresion,
    paste0(
      prefijo,
      "_08_regresion"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 9. Diferencias en el tiempo
  # ---------------------------------------------------------------------------
  
  p_diferencias <- ggplot(
    df,
    aes(
      x = tiempo,
      y = diferencia
    )
  ) +
    geom_point(
      alpha = 0.75,
      size = 1.5
    ) +
    geom_line(
      linewidth = 0.35,
      alpha = 0.6
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = sesgo,
      linewidth = 0.8
    ) +
    labs(
      title = paste(
        "Diferencias MT - CETAM -",
        nombre_pm
      ),
      subtitle = paste0(
        "Sesgo medio = ",
        round(sesgo, 3),
        " ",
        UNIDAD
      ),
      x = "Tiempo",
      y = paste0(
        "MT - CETAM (",
        UNIDAD,
        ")"
      )
    )
  
  guardar_grafico(
    p_diferencias,
    paste0(
      prefijo,
      "_09_diferencias_tiempo"
    ),
    11,
    5
  )
  
  # ---------------------------------------------------------------------------
  # 10. Bland-Altman
  # ---------------------------------------------------------------------------
  
  p_ba <- ggplot(
    df,
    aes(
      x = promedio,
      y = diferencia
    )
  ) +
    geom_point(
      alpha = 0.70,
      size = 1.7
    ) +
    geom_hline(
      yintercept = sesgo,
      linewidth = 0.9
    ) +
    geom_hline(
      yintercept = loa_inf,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    geom_hline(
      yintercept = loa_sup,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    labs(
      title = paste(
        "Bland-Altman -",
        nombre_pm
      ),
      subtitle = paste0(
        "Diferencia = MT - CETAM | sesgo = ",
        round(sesgo, 3),
        " | LoA 95% = [",
        round(loa_inf, 3),
        ", ",
        round(loa_sup, 3),
        "]"
      ),
      x = paste0(
        "Promedio CETAM-MT (",
        UNIDAD,
        ")"
      ),
      y = paste0(
        "MT - CETAM (",
        UNIDAD,
        ")"
      )
    )
  
  guardar_grafico(
    p_ba,
    paste0(
      prefijo,
      "_10_bland_altman"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 11. QQ CETAM
  # ---------------------------------------------------------------------------
  
  p_qq_cetam <- ggplot(
    df,
    aes(
      sample = CETAM
    )
  ) +
    stat_qq(
      alpha = 0.65
    ) +
    stat_qq_line() +
    labs(
      title = paste(
        "QQ-plot CETAM -",
        nombre_pm
      ),
      x = "Cuantiles teoricos",
      y = "Cuantiles observados"
    )
  
  guardar_grafico(
    p_qq_cetam,
    paste0(
      prefijo,
      "_11_QQ_CETAM"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 12. QQ MT
  # ---------------------------------------------------------------------------
  
  p_qq_mt <- ggplot(
    df,
    aes(
      sample = MT
    )
  ) +
    stat_qq(
      alpha = 0.65
    ) +
    stat_qq_line() +
    labs(
      title = paste(
        "QQ-plot MT -",
        nombre_pm
      ),
      x = "Cuantiles teoricos",
      y = "Cuantiles observados"
    )
  
  guardar_grafico(
    p_qq_mt,
    paste0(
      prefijo,
      "_12_QQ_MT"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 13. QQ diferencias
  # ---------------------------------------------------------------------------
  
  p_qq_dif <- ggplot(
    df,
    aes(
      sample = diferencia
    )
  ) +
    stat_qq(
      alpha = 0.65
    ) +
    stat_qq_line() +
    labs(
      title = paste(
        "QQ-plot de las diferencias -",
        nombre_pm
      ),
      subtitle = "Variable analizada: MT - CETAM",
      x = "Cuantiles teoricos",
      y = "Cuantiles observados"
    )
  
  guardar_grafico(
    p_qq_dif,
    paste0(
      prefijo,
      "_13_QQ_diferencias"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 14. Residuos
  # ---------------------------------------------------------------------------
  
  p_residuos <- ggplot(
    df,
    aes(
      x = ajustado,
      y = residuo
    )
  ) +
    geom_point(
      alpha = 0.70,
      size = 1.6
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    labs(
      title = paste(
        "Residuos de la regresion -",
        nombre_pm
      ),
      x = "Valores ajustados",
      y = "Residuos"
    )
  
  guardar_grafico(
    p_residuos,
    paste0(
      prefijo,
      "_14_residuos"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 15. Zona de acuerdo
  # ---------------------------------------------------------------------------
  
  xmin <- min(df$CETAM)
  xmax <- max(df$CETAM)
  
  banda <- data.frame(
    CETAM = seq(
      xmin,
      xmax,
      length.out = 300
    )
  )
  
  banda$inferior <- banda$CETAM - c_usado
  banda$superior <- banda$CETAM + c_usado
  
  p_zona <- ggplot(
    df,
    aes(
      x = CETAM,
      y = MT
    )
  ) +
    geom_ribbon(
      data = banda,
      aes(
        x = CETAM,
        ymin = inferior,
        ymax = superior
      ),
      inherit.aes = FALSE,
      alpha = 0.18
    ) +
    geom_point(
      alpha = 0.65,
      size = 1.7
    ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed"
    ) +
    labs(
      title = paste(
        "Zona de acuerdo -",
        nombre_pm
      ),
      subtitle = paste0(
        "|MT - CETAM| <= ",
        round(c_usado, 3),
        " ",
        UNIDAD,
        " | PA empirica = ",
        round(pa_e, 3)
      ),
      x = paste0(
        "CETAM ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      ),
      y = paste0(
        "MT ",
        nombre_pm,
        " (",
        UNIDAD,
        ")"
      )
    )
  
  guardar_grafico(
    p_zona,
    paste0(
      prefijo,
      "_15_zona_acuerdo"
    )
  )
  
  # ---------------------------------------------------------------------------
  # 16. Curva PA(c)
  # ---------------------------------------------------------------------------
  
  abs_d <- abs(df$diferencia)
  
  c_max <- as.numeric(
    quantile(
      abs_d,
      0.99,
      na.rm = TRUE
    )
  )
  
  if (!is.finite(c_max) || c_max <= 0) {
    c_max <- max(abs_d, na.rm = TRUE)
  }
  
  if (!is.finite(c_max) || c_max <= 0) {
    c_max <- 1
  }
  
  c_max <- max(
    c_max,
    1.20 * c_usado
  )
  
  c_grid <- seq(
    0,
    c_max,
    length.out = 250
  )
  
  curva_pa <- data.frame(
    c = c_grid,
    PA_empirica = sapply(
      c_grid,
      function(cc) {
        pa_empirica(
          df$CETAM,
          df$MT,
          cc
        )
      }
    ),
    PA_normal = sapply(
      c_grid,
      function(cc) {
        pa_normal(
          df$CETAM,
          df$MT,
          cc
        )
      }
    )
  )
  
  curva_long <- rbind(
    data.frame(
      c = curva_pa$c,
      PA = curva_pa$PA_empirica,
      metodo = "Empirica"
    ),
    data.frame(
      c = curva_pa$c,
      PA = curva_pa$PA_normal,
      metodo = "Normal"
    )
  )
  
  p_pa <- ggplot(
    curva_long,
    aes(
      x = c,
      y = PA,
      linetype = metodo
    )
  ) +
    geom_line(
      linewidth = 0.9
    ) +
    geom_hline(
      yintercept = 0.95,
      linetype = "dotted"
    ) +
    geom_vline(
      xintercept = c_usado,
      linetype = "dashed"
    ) +
    coord_cartesian(
      ylim = c(0, 1)
    ) +
    labs(
      title = paste(
        "Probability of Agreement PA(c) -",
        nombre_pm
      ),
      subtitle = paste0(
        "Linea vertical: c = ",
        round(c_usado, 3),
        " ",
        UNIDAD
      ),
      x = paste0(
        "Tolerancia c (",
        UNIDAD,
        ")"
      ),
      y = "PA(c)",
      linetype = "Estimacion"
    )
  
  guardar_grafico(
    p_pa,
    paste0(
      prefijo,
      "_16_curva_PA"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Guardar tablas de esta fraccion
  # ---------------------------------------------------------------------------
  
  write.csv(
    df,
    file.path(
      CARPETA_TABLAS,
      paste0(
        prefijo,
        "_datos_pareados.csv"
      )
    ),
    row.names = FALSE
  )
  
  write.csv(
    curva_pa,
    file.path(
      CARPETA_TABLAS,
      paste0(
        prefijo,
        "_curva_PA.csv"
      )
    ),
    row.names = FALSE
  )
  
  desc <- rbind(
    descriptivos_variable(
      df$CETAM,
      "CETAM",
      nombre_pm
    ),
    descriptivos_variable(
      df$MT,
      "MT",
      nombre_pm
    )
  )
  
  list(
    resumen = resumen,
    descriptivos = desc,
    curva_pa = curva_pa,
    datos = df
  )
}

# =============================================================================
# 6. ANALISIS PM1.0
# =============================================================================

resultado_PM1 <- analizar_fraccion(
  datos = datos,
  columna_ref = "CETAM_PM1_0",
  columna_mt = "MT_PM1_0",
  nombre_pm = "PM1.0",
  prefijo = "PM1_0",
  c_usuario = C_PM1_0
)

# =============================================================================
# 7. ANALISIS PM2.5
# =============================================================================

resultado_PM2_5 <- analizar_fraccion(
  datos = datos,
  columna_ref = "CETAM_PM2_5",
  columna_mt = "MT_PM2_5",
  nombre_pm = "PM2.5",
  prefijo = "PM2_5",
  c_usuario = C_PM2_5
)

# =============================================================================
# 8. ANALISIS PM10
# =============================================================================

resultado_PM10 <- analizar_fraccion(
  datos = datos,
  columna_ref = "CETAM_PM10",
  columna_mt = "MT_PM10",
  nombre_pm = "PM10",
  prefijo = "PM10",
  c_usuario = C_PM10
)

# =============================================================================
# 9. TABLAS RESUMEN
# =============================================================================

resultados_validos <- list(
  resultado_PM1,
  resultado_PM2_5,
  resultado_PM10
)

resultados_validos <- resultados_validos[
  !vapply(
    resultados_validos,
    is.null,
    logical(1)
  )
]

tabla_resultados <- do.call(
  rbind,
  lapply(
    resultados_validos,
    function(x) x$resumen
  )
)

tabla_descriptivos <- do.call(
  rbind,
  lapply(
    resultados_validos,
    function(x) x$descriptivos
  )
)

write.csv(
  tabla_resultados,
  file.path(
    CARPETA_TABLAS,
    "resumen_concordancia_PM.csv"
  ),
  row.names = FALSE
)

write.csv(
  tabla_descriptivos,
  file.path(
    CARPETA_TABLAS,
    "descriptivos_PM.csv"
  ),
  row.names = FALSE
)

# =============================================================================
# 10. GRAFICOS COMBINADOS DE PM1.0, PM2.5 Y PM10
# =============================================================================

series_todas <- rbind(
  data.frame(
    tiempo = datos$bloque_tiempo,
    valor = datos$CETAM_PM1_0,
    instrumento = "CETAM",
    fraccion = "PM1.0"
  ),
  data.frame(
    tiempo = datos$bloque_tiempo,
    valor = datos$MT_PM1_0,
    instrumento = "MT",
    fraccion = "PM1.0"
  ),
  data.frame(
    tiempo = datos$bloque_tiempo,
    valor = datos$CETAM_PM2_5,
    instrumento = "CETAM",
    fraccion = "PM2.5"
  ),
  data.frame(
    tiempo = datos$bloque_tiempo,
    valor = datos$MT_PM2_5,
    instrumento = "MT",
    fraccion = "PM2.5"
  ),
  data.frame(
    tiempo = datos$bloque_tiempo,
    valor = datos$CETAM_PM10,
    instrumento = "CETAM",
    fraccion = "PM10"
  ),
  data.frame(
    tiempo = datos$bloque_tiempo,
    valor = datos$MT_PM10,
    instrumento = "MT",
    fraccion = "PM10"
  )
)

series_todas <- series_todas[
  is.finite(series_todas$valor),
]

p_series_panel <- ggplot(
  series_todas,
  aes(
    x = tiempo,
    y = valor,
    linetype = instrumento,
    group = instrumento
  )
) +
  geom_line(
    linewidth = 0.55
  ) +
  geom_point(
    size = 0.9,
    alpha = 0.65
  ) +
  facet_wrap(
    ~ fraccion,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title = "Series temporales CETAM vs MT",
    subtitle = "Datos suavizados y alineados en bloques de 5 minutos",
    x = "Tiempo",
    y = paste0(
      "Concentracion (",
      UNIDAD,
      ")"
    ),
    linetype = "Instrumento"
  )

guardar_grafico(
  p_series_panel,
  "COMBINADO_01_series_PM1_PM25_PM10",
  11,
  9
)

scatter_todos <- rbind(
  data.frame(
    CETAM = datos$CETAM_PM1_0,
    MT = datos$MT_PM1_0,
    fraccion = "PM1.0"
  ),
  data.frame(
    CETAM = datos$CETAM_PM2_5,
    MT = datos$MT_PM2_5,
    fraccion = "PM2.5"
  ),
  data.frame(
    CETAM = datos$CETAM_PM10,
    MT = datos$MT_PM10,
    fraccion = "PM10"
  )
)

scatter_todos <- scatter_todos[
  is.finite(scatter_todos$CETAM) &
    is.finite(scatter_todos$MT),
]

p_scatter_panel <- ggplot(
  scatter_todos,
  aes(
    x = CETAM,
    y = MT
  )
) +
  geom_point(
    alpha = 0.65,
    size = 1.6
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = FALSE
  ) +
  facet_wrap(
    ~ fraccion,
    scales = "free",
    nrow = 1
  ) +
  labs(
    title = "CETAM vs MT para las tres fracciones",
    subtitle = "Linea discontinua: identidad y = x",
    x = paste0(
      "CETAM (",
      UNIDAD,
      ")"
    ),
    y = paste0(
      "MT (",
      UNIDAD,
      ")"
    )
  )

guardar_grafico(
  p_scatter_panel,
  "COMBINADO_02_concordancia_PM1_PM25_PM10",
  12,
  5
)

coeficientes <- tabla_resultados |>
  select(
    fraccion,
    Pearson,
    CCC_Lin
  ) |>
  pivot_longer(
    cols = c(
      Pearson,
      CCC_Lin
    ),
    names_to = "coeficiente",
    values_to = "valor"
  )

p_coef <- ggplot(
  coeficientes,
  aes(
    x = fraccion,
    y = valor,
    group = coeficiente,
    linetype = coeficiente
  )
) +
  geom_point(
    size = 2
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  coord_cartesian(
    ylim = c(
      -1,
      1
    )
  ) +
  labs(
    title = "Pearson y CCC de Lin por fraccion",
    subtitle = "Pearson mide asociacion; CCC mide concordancia",
    x = "Fraccion",
    y = "Coeficiente",
    linetype = NULL
  )

guardar_grafico(
  p_coef,
  "COMBINADO_03_Pearson_CCC",
  8,
  5
)

p_sesgo <- ggplot(
  tabla_resultados,
  aes(
    x = fraccion,
    y = sesgo_MT_menos_CETAM
  )
) +
  geom_point(
    size = 2.5
  ) +
  geom_errorbar(
    aes(
      ymin = LoA_inferior_95,
      ymax = LoA_superior_95
    ),
    width = 0.15
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Sesgo y limites de concordancia por fraccion",
    subtitle = "Punto = sesgo medio MT-CETAM; barra = LoA 95%",
    x = "Fraccion",
    y = paste0(
      "Diferencia (",
      UNIDAD,
      ")"
    )
  )

guardar_grafico(
  p_sesgo,
  "COMBINADO_04_sesgo_LoA",
  8,
  5
)

# =============================================================================
# 11. GUARDAR EXCEL FINAL
# =============================================================================

configuracion <- data.frame(
  parametro = c(
    "archivo_entrada",
    "instrumento_referencia",
    "instrumento_comparado",
    "unidad",
    "C_PM1_0_usuario",
    "C_PM2_5_usuario",
    "C_PM10_usuario"
  ),
  valor = c(
    ARCHIVO_DATOS,
    "CETAM",
    "MT",
    UNIDAD,
    as.character(C_PM1_0),
    as.character(C_PM2_5),
    as.character(C_PM10)
  )
)

salida_excel <- list(
  configuracion = configuracion,
  descriptivos = tabla_descriptivos,
  concordancia = tabla_resultados
)

writexl::write_xlsx(
  salida_excel,
  path = file.path(
    CARPETA_SALIDA,
    "resultados_concordancia_CETAM_MT.xlsx"
  )
)

# =============================================================================
# 12. FIN
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("ANALISIS DE CONCORDANCIA TERMINADO CORRECTAMENTE\n")
cat("============================================================\n")
cat("Referencia: CETAM\n")
cat("Instrumento comparado: MT\n")

cat(
  "Datos de entrada: ",
  ARCHIVO_DATOS,
  "\n",
  sep = ""
)

cat(
  "Carpeta de salida: ",
  CARPETA_SALIDA,
  "\n",
  sep = ""
)

cat(
  "Graficos: ",
  CARPETA_GRAFICOS,
  "\n\n",
  sep = ""
)

cat("Resumen numerico:\n\n")

print(
  tabla_resultados
)

cat("\n")
cat("INTERPRETACION DE SIGNO:\n")
cat("- diferencia = MT - CETAM\n")
cat("- sesgo positivo: MT mide mas que CETAM en promedio\n")
cat("- sesgo negativo: MT mide menos que CETAM en promedio\n")
cat("- Pearson alto no implica necesariamente concordancia alta\n")
cat("- Si c no fue definido por ustedes, PA es solo exploratoria\n")
cat("============================================================\n")
