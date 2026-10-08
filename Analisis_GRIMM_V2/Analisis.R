# ==============================================================================
# PROCESAMIENTO Y COMPARACIÓN DE EQUIPOS GRIMM
# CETAM vs MT
#
# V7 - OUTLIERS CONFIGURABLES + MÉTRICAS + GRÁFICOS INTERACTIVOS CON AVISO
#
# Incluye:
#   - Variable editable para controlar la rigurosidad de exclusión de outliers
#   - Cálculo del % de datos descartados
#   - Aviso en los gráficos del % descartado
#   - Promedios por minuto, FULL JOIN de las series
#   - Pearson, CCC de Lin, Bland-Altman, R², Probability of Agreement
#   - Autoescalado dinámico del eje Y
# ==============================================================================


# ==============================================================================
# 1. PAQUETES
# ==============================================================================

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  readxl,
  lubridate,
  plotly,
  htmlwidgets
)


# ==============================================================================
# 2. CONFIGURACIÓN DE OUTLIERS (EDITABLE POR EL USUARIO)
# ==============================================================================

# Multiplicador IQR para descartar outliers (Límite = Q3 + Multiplicador * IQR).
#   - Valores bajos (ej. 1.5 a 3): Eliminan más datos (más estricto).
#   - Valores altos (ej. 5 a 8): Eliminan menos datos, solo picos muy irracionales.
MULTIPLICADOR_OUTLIERS <- 5

# Variable global para almacenar los porcentajes descartados
outliers_stats <- list()


# ==============================================================================
# 3. CONFIGURAR DIRECTORIO Y ARCHIVOS DE ENTRADA
# ==============================================================================

output_dir <- "Analisis_GRIMM_V2"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

file_cetam <- "GRIMM 11D Cetam 22052025.xlsx"
file_mt <- "GRIMM 11D MT 22052025.xlsx"


# ==============================================================================
# 4. FUNCIONES AUXILIARES
# ==============================================================================


# ------------------------------------------------------------------------------
# 4.1 Promedio robusto frente a NA
# ------------------------------------------------------------------------------

mean_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}


# ------------------------------------------------------------------------------
# 4.2 Eliminación de valores extremos superiores
# ------------------------------------------------------------------------------

remove_extreme_outliers <- function(x, multiplier = MULTIPLICADOR_OUTLIERS) {
  
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  
  limite_superior <- qnt[2] + multiplier * iqr
  
  x[x > limite_superior] <- NA
  
  return(x)
}


# ------------------------------------------------------------------------------
# 4.3 Generador de texto de outliers para los gráficos
# ------------------------------------------------------------------------------

get_outlier_text <- function(poll) {
  
  # Asegurar que el formato coincida con los nombres de columnas (ej. "PM2.5" a "PM25")
  poll_key <- gsub("\\.", "", poll) 
  
  pct_c <- outliers_stats[["Cetam"]][[poll_key]]
  pct_m <- outliers_stats[["MT"]][[poll_key]]
  
  if (is.null(pct_c)) pct_c <- 0
  if (is.null(pct_m)) pct_m <- 0
  
  texto <- paste0(
    "<b>Outliers excluidos:</b><br>",
    "Cetam: ", round(pct_c, 2), " %<br>",
    "MT: ", round(pct_m, 2), " %"
  )
  
  return(texto)
}


# ==============================================================================
# 5. CARGA, LIMPIEZA DE DATOS Y CÁLCULO DE % DE OUTLIERS
# ==============================================================================


load_grimm_mass <- function(filepath, equipo_label) {
  
  # Cargar datos crudos
  df_raw <- read_excel(filepath, sheet = "Mass values", skip = 4) %>%
    rename(
      datetime_raw = `date&time`,
      PM10 = `PM10 [ug/m3]`,
      PM25 = `PM2,5 [ug/m3]`,
      PM1 = `PM1 [ug/m3]`
    ) %>%
    filter(!is.na(datetime_raw)) %>%
    mutate(
      datetime = dmy_hms(datetime_raw),
      across(c(PM10, PM25, PM1), as.numeric)
    )
  
  # 1. Contar observaciones válidas ANTES de limpiar
  n_antes <- list(
    PM10 = sum(!is.na(df_raw$PM10)),
    PM25 = sum(!is.na(df_raw$PM25)),
    PM1  = sum(!is.na(df_raw$PM1))
  )
  
  # 2. Aplicar filtro de outliers
  df_clean <- df_raw %>%
    mutate(
      across(
        c(PM10, PM25, PM1),
        ~ remove_extreme_outliers(.x, multiplier = MULTIPLICADOR_OUTLIERS)
      )
    )
  
  # 3. Contar observaciones válidas DESPUÉS y calcular porcentaje
  pct_eliminado <- list(
    PM10 = (n_antes$PM10 - sum(!is.na(df_clean$PM10))) / n_antes$PM10 * 100,
    PM25 = (n_antes$PM25 - sum(!is.na(df_clean$PM25))) / n_antes$PM25 * 100,
    PM1  = (n_antes$PM1  - sum(!is.na(df_clean$PM1)))  / n_antes$PM1  * 100
  )
  
  # Guardar porcentajes globalmente para usarlos en los gráficos
  outliers_stats[[equipo_label]] <<- pct_eliminado
  
  # Continuar el pipeline de limpieza temporal
  df_clean %>%
    filter(
      !is.na(datetime),
      datetime >= ymd("2025-01-01")
    ) %>%
    mutate(
      datetime_1m = floor_date(datetime, unit = "1 minute")
    )
}


# ==============================================================================
# 6. CARGAR Y PROMEDIAR CETAM
# ==============================================================================

df_c <- load_grimm_mass(file_cetam, "Cetam") %>%
  group_by(datetime_1m) %>%
  summarise(
    PM1_C = mean_na(PM1),
    PM25_C = mean_na(PM25),
    PM10_C = mean_na(PM10),
    .groups = "drop"
  )


# ==============================================================================
# 7. CARGAR Y PROMEDIAR MT
# ==============================================================================

df_m <- load_grimm_mass(file_mt, "MT") %>%
  group_by(datetime_1m) %>%
  summarise(
    PM1_M = mean_na(PM1),
    PM25_M = mean_na(PM25),
    PM10_M = mean_na(PM10),
    .groups = "drop"
  )


# ==============================================================================
# 8. UNIR SERIES TEMPORALES
# ==============================================================================

df_merged <- full_join(df_c, df_m, by = "datetime_1m") %>%
  rename(datetime = datetime_1m) %>%
  arrange(datetime)


# ==============================================================================
# 9. MÉTRICAS AVANZADAS
# ==============================================================================

calc_metrics <- function(df, c_col, m_col, label) {
  
  x <- df[[c_col]]
  y <- df[[m_col]]
  
  valid_idx <- is.finite(x) & is.finite(y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  if (length(x) < 2) {
    return(data.frame(
      Parametro = label, N = length(x), Media_Cetam = NA_real_, Media_MT = NA_real_,
      Pearson = NA_real_, R2 = NA_real_, CCC_Lin = NA_real_,
      Bias_Mean_Diff = NA_real_, LoA_Lower = NA_real_, LoA_Upper = NA_real_
    ))
  }
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  if (sd(x) == 0 || sd(y) == 0) {
    r_pearson <- NA_real_
  } else {
    r_pearson <- cor(x, y, method = "pearson")
  }
  
  if (sd(x) == 0) {
    r2 <- NA_real_
  } else {
    fit <- lm(y ~ x)
    r2 <- summary(fit)$r.squared
  }
  
  var_x <- var(x)
  var_y <- var(y)
  cov_xy <- cov(x, y)
  denominador_ccc <- var_x + var_y + (mean_x - mean_y)^2
  
  if (denominador_ccc == 0) {
    ccc <- NA_real_
  } else {
    ccc <- 2 * cov_xy / denominador_ccc
  }
  
  diff <- y - x
  bias <- mean(diff)
  sd_diff <- sd(diff)
  loa_upper <- bias + 1.96 * sd_diff
  loa_lower <- bias - 1.96 * sd_diff
  
  data.frame(
    Parametro = label, N = length(x), Media_Cetam = mean_x, Media_MT = mean_y,
    Pearson = r_pearson, R2 = r2, CCC_Lin = ccc,
    Bias_Mean_Diff = bias, LoA_Lower = loa_lower, LoA_Upper = loa_upper
  )
}


# ==============================================================================
# 10 Y 11. CALCULAR Y GUARDAR MÉTRICAS
# ==============================================================================

resumen <- bind_rows(
  calc_metrics(df_merged, "PM1_C", "PM1_M", "PM1"),
  calc_metrics(df_merged, "PM25_C", "PM25_M", "PM2.5"),
  calc_metrics(df_merged, "PM10_C", "PM10_M", "PM10")
)

write.csv(resumen, file.path(output_dir, "Metricas_Avanzadas_SinOutliers.csv"), row.names = FALSE)
print(resumen)


# ==============================================================================
# 12. SERIE DE TIEMPO INTERACTIVA (ANOTACIÓN UBICADA CORRECTAMENTE DENTRO DEL ÁREA)
# ==============================================================================

plot_ts <- function(df, poll, c_col, m_col) {
  
  y_cetam <- df[[c_col]]
  y_mt <- df[[m_col]]
  
  p <- plot_ly(df, x = ~datetime) %>%
    add_lines(y = y_cetam, name = paste("Cetam", poll), line = list(color = "#1f77b4", width = 1)) %>%
    add_lines(y = y_mt, name = paste("MT", poll), line = list(color = "#ff7f0e", width = 1)) %>%
    layout(
      title = list(text = paste("Serie de Tiempo Limpia -", poll), y = 0.97),
      margin = list(t = 60), # Margen superior para evitar solapamiento con el título
      xaxis = list(
        title = "Fecha", type = "date",
        rangeslider = list(visible = TRUE, thickness = 0.10),
        rangeselector = list(
          buttons = list(
            list(count = 1, label = "1 día", step = "day", stepmode = "backward"),
            list(count = 7, label = "1 semana", step = "day", stepmode = "backward"),
            list(count = 1, label = "1 mes", step = "month", stepmode = "backward"),
            list(step = "all", label = "Todo")
          )
        )
      ),
      yaxis = list(title = "µg/m³", autorange = TRUE, fixedrange = FALSE),
      dragmode = "zoom", hovermode = "x unified",
      # ANOTACIÓN CORREGIDA: Ubicada en la esquina superior derecha DENTRO del gráfico
      annotations = list(
        list(
          x = 0.98, y = 0.98, 
          xref = "paper", yref = "paper",
          text = get_outlier_text(poll),
          showarrow = FALSE, 
          xanchor = "right", yanchor = "top", 
          align = "right",
          bgcolor = "rgba(255,255,255,0.88)", 
          bordercolor = "black", 
          borderwidth = 1, 
          borderpad = 5
        )
      )
    )
  
  # AUTOESCALADO JAVASCRIPT
  p <- htmlwidgets::onRender(
    p,
    "
    function(el, x) {
      var plot = el;
      plot.on('plotly_relayout', function(eventdata) {
        if (eventdata['xaxis.range[0]'] !== undefined || eventdata['xaxis.range[1]'] !== undefined || eventdata['xaxis.autorange'] !== undefined) {
          var x0 = eventdata['xaxis.range[0]'];
          var x1 = eventdata['xaxis.range[1]'];

          if (eventdata['xaxis.autorange'] === true) {
            Plotly.relayout(plot, {'yaxis.autorange': true}); return;
          }
          if (x0 === undefined || x1 === undefined) { return; }

          var xmin = new Date(x0).getTime();
          var xmax = new Date(x1).getTime();
          var valores = [];

          plot.data.forEach(function(trace) {
            if (trace.x && trace.y) {
              for (var i = 0; i < trace.x.length; i++) {
                var xi = new Date(trace.x[i]).getTime();
                var yi = trace.y[i];
                if (xi >= xmin && xi <= xmax && yi !== null && yi !== undefined && isFinite(yi)) {
                  valores.push(yi);
                }
              }
            }
          });

          if (valores.length > 0) {
            var ymin = Math.min.apply(null, valores);
            var ymax = Math.max.apply(null, valores);
            if (ymin === ymax) {
              var margen = Math.abs(ymin) * 0.05;
              if (margen === 0) margen = 1;
              ymin -= margen; ymax += margen;
            } else {
              var margen = (ymax - ymin) * 0.05;
              ymin -= margen; ymax += margen;
            }
            Plotly.relayout(plot, {'yaxis.range': [ymin, ymax], 'yaxis.autorange': false});
          }
        }
      });
    }
    "
  )
  return(p)
}

# ==============================================================================
# 13. BLAND-ALTMAN
# ==============================================================================

plot_ba <- function(df, poll, c_col, m_col) {
  
  datos <- tibble(x = df[[c_col]], y = df[[m_col]]) %>%
    filter(is.finite(x), is.finite(y))
  
  if (nrow(datos) < 2) return(plot_ly() %>% layout(title = paste("Datos insuficientes -", poll)))
  
  media_xy <- (datos$x + datos$y) / 2
  diferencia <- datos$y - datos$x
  bias <- mean(diferencia)
  sd_diff <- sd(diferencia)
  loa_u <- bias + 1.96 * sd_diff
  loa_l <- bias - 1.96 * sd_diff
  rango_x <- range(media_xy, na.rm = TRUE)
  
  plot_ly(x = media_xy, y = diferencia, type = "scatter", mode = "markers",
          marker = list(color = "rgba(44, 160, 44, 0.4)", size = 4), name = "Medición") %>%
    add_lines(x = rango_x, y = c(bias, bias), name = "Sesgo (Media)", line = list(color = "red", width = 2)) %>%
    add_lines(x = rango_x, y = c(loa_u, loa_u), name = "Límite Sup (+1.96 SD)", line = list(color = "blue", dash = "dash")) %>%
    add_lines(x = rango_x, y = c(loa_l, loa_l), name = "Límite Inf (-1.96 SD)", line = list(color = "blue", dash = "dash")) %>%
    layout(
      title = paste("Bland-Altman (Sin Outliers) -", poll),
      xaxis = list(title = "Media de ambos equipos [(MT + Cetam)/2] µg/m³"),
      yaxis = list(title = "Diferencia (MT - Cetam) µg/m³"),
      # ANOTACIÓN: % DE OUTLIERS
      annotations = list(
        list(
          x = 0.98, y = 0.95, xref = "paper", yref = "paper",
          text = get_outlier_text(poll),
          showarrow = FALSE, xanchor = "right", yanchor = "top", align = "right",
          bgcolor = "rgba(255,255,255,0.85)", bordercolor = "black", borderwidth = 1, borderpad = 4
        )
      )
    )
}


# ==============================================================================
# 14. DISPERSIÓN + REGRESIÓN OLS (CORREGIDO)
# ==============================================================================

plot_scatter_limpio <- function(df, poll, c_col, m_col) {
  
  datos <- data.frame(Cetam = df[[c_col]], MT = df[[m_col]]) %>%
    filter(is.finite(Cetam), is.finite(MT))
  
  if (nrow(datos) < 2) return(plot_ly() %>% layout(title = paste("Datos insuficientes -", poll)))
  
  fit <- lm(MT ~ Cetam, data = datos)
  pendiente <- coef(fit)[2]
  intercepto <- coef(fit)[1]
  
  # CONDICIÓN DE SVIACIÓN ESTÁNDAR CORREGIDA (SE USAN BARRAS VERTICALES SIMPLES ||)
  if (sd(datos$Cetam) == 0 || sd(datos$MT) == 0) { 
    r <- NA_real_ 
  } else { 
    r <- cor(datos$Cetam, datos$MT, method = "pearson") 
  }
  
  r2 <- summary(fit)$r.squared
  mean_c <- mean(datos$Cetam); mean_m <- mean(datos$MT)
  var_c <- var(datos$Cetam); var_m <- var(datos$MT); cov_cm <- cov(datos$Cetam, datos$MT)
  denominador_ccc <- var_c + var_m + (mean_c - mean_m)^2
  
  if (denominador_ccc == 0) { 
    ccc <- NA_real_ 
  } else { 
    ccc <- 2 * cov_cm / denominador_ccc 
  }
  
  n <- nrow(datos)
  ecuacion <- paste0("y = ", round(pendiente, 3), "x ", ifelse(intercepto >= 0, "+ ", "- "), round(abs(intercepto), 3))
  x_reg <- seq(min(datos$Cetam), max(datos$Cetam), length.out = 100)
  y_reg <- predict(fit, newdata = data.frame(Cetam = x_reg))
  limite <- max(datos$Cetam, datos$MT, na.rm = TRUE)
  
  # CONSTRUIR TEXTO DEL CUADRO ESTADÍSTICO + AVISO DE OUTLIERS
  texto_stats <- paste0(
    "<b>Pearson r:</b> ", round(r, 4),
    "<br><b>R²:</b> ", round(r2, 4),
    "<br><b>CCC Lin:</b> ", round(ccc, 4),
    "<br><b>", ecuacion, "</b>",
    "<br><b>n:</b> ", n,
    "<br><br>", get_outlier_text(poll)
  )
  
  plot_ly() %>%
    add_markers(
      x = datos$Cetam, y = datos$MT, name = "Mediciones",
      marker = list(size = 5, opacity = 0.6),
      hovertemplate = paste("Cetam: %{x:.2f} µg/m³", "<br>MT: %{y:.2f} µg/m³", "<extra></extra>")
    ) %>%
    add_lines(x = x_reg, y = y_reg, name = "Regresión OLS", line = list(width = 2)) %>%
    add_lines(x = c(0, limite), y = c(0, limite), name = "y = x", line = list(dash = "dash", width = 2)) %>%
    layout(
      title = list(text = paste("Comparación Cetam vs MT -", poll)),
      xaxis = list(title = "Cetam (µg/m³)", zeroline = FALSE),
      yaxis = list(title = "MT (µg/m³)", zeroline = FALSE),
      hovermode = "closest",
      annotations = list(
        list(
          x = 0.02, y = 0.98, xref = "paper", yref = "paper",
          xanchor = "left", yanchor = "top",
          text = texto_stats, showarrow = FALSE, align = "left",
          bgcolor = "rgba(255,255,255,0.85)", bordercolor = "black", borderwidth = 1, borderpad = 6
        )
      )
    )
}


# ==============================================================================
# 15. PROBABILITY OF AGREEMENT
# ==============================================================================

plot_probability_agreement <- function(df, poll, c_col, m_col) {
  
  datos <- data.frame(Cetam = df[[c_col]], MT = df[[m_col]]) %>% filter(is.finite(Cetam), is.finite(MT))
  if (nrow(datos) < 2) return(plot_ly() %>% layout(title = paste("Datos insuficientes -", poll)))
  
  datos <- datos %>% mutate(diferencia = abs(MT - Cetam))
  tolerancia_max <- as.numeric(quantile(datos$diferencia, 0.99, na.rm = TRUE))
  if (tolerancia_max <= 0 || !is.finite(tolerancia_max)) { tolerancia_max <- 1 }
  
  tolerancias <- seq(0, tolerancia_max, length.out = 200)
  prob_agreement <- sapply(tolerancias, function(tol) { mean(datos$diferencia <= tol) })
  datos_poa <- data.frame(tolerancia = tolerancias, probabilidad = prob_agreement)
  
  plot_ly(datos_poa, x = ~tolerancia, y = ~probabilidad, type = "scatter", mode = "lines",
          line = list(width = 3), name = "Probability of Agreement",
          hovertemplate = paste("Tolerancia: %{x:.2f} µg/m³", "<br>Probabilidad: %{y:.2%}", "<extra></extra>")) %>%
    add_lines(x = c(0, tolerancia_max), y = c(0.50, 0.50), name = "50 %", line = list(dash = "dash"), hoverinfo = "skip") %>%
    add_lines(x = c(0, tolerancia_max), y = c(0.90, 0.90), name = "90 %", line = list(dash = "dash"), hoverinfo = "skip") %>%
    add_lines(x = c(0, tolerancia_max), y = c(0.95, 0.95), name = "95 %", line = list(dash = "dash"), hoverinfo = "skip") %>%
    layout(
      title = list(text = paste("Probability of Agreement -", poll)),
      xaxis = list(title = "Tolerancia absoluta |MT - Cetam| (µg/m³)"),
      yaxis = list(title = "Probabilidad de acuerdo", tickformat = ".0%", range = c(0, 1)),
      hovermode = "x unified",
      # ANOTACIÓN: % DE OUTLIERS
      annotations = list(
        list(
          x = 0.98, y = 0.05, xref = "paper", yref = "paper",
          text = get_outlier_text(poll),
          showarrow = FALSE, xanchor = "right", yanchor = "bottom", align = "right",
          bgcolor = "rgba(255,255,255,0.85)", bordercolor = "black", borderwidth = 1, borderpad = 4
        )
      )
    )
}


# ==============================================================================
# 16. GUARDAR TODOS LOS GRÁFICOS
# ==============================================================================

guardar_grafico <- function(grafico, nombre) {
  htmlwidgets::saveWidget(grafico, file.path(output_dir, nombre), selfcontained = TRUE)
}

# SCATTER
guardar_grafico(plot_scatter_limpio(df_merged, "PM1", "PM1_C", "PM1_M"), "Scatter_PM1_Limpio.html")
guardar_grafico(plot_scatter_limpio(df_merged, "PM2.5", "PM25_C", "PM25_M"), "Scatter_PM25_Limpio.html")
guardar_grafico(plot_scatter_limpio(df_merged, "PM10", "PM10_C", "PM10_M"), "Scatter_PM10_Limpio.html")

# TIME SERIES
guardar_grafico(plot_ts(df_merged, "PM1", "PM1_C", "PM1_M"), "TS_PM1_Limpio.html")
guardar_grafico(plot_ts(df_merged, "PM2.5", "PM25_C", "PM25_M"), "TS_PM25_Limpio.html")
guardar_grafico(plot_ts(df_merged, "PM10", "PM10_C", "PM10_M"), "TS_PM10_Limpio.html")

# BLAND ALTMAN
guardar_grafico(plot_ba(df_merged, "PM1", "PM1_C", "PM1_M"), "BlandAltman_PM1_Limpio.html")
guardar_grafico(plot_ba(df_merged, "PM2.5", "PM25_C", "PM25_M"), "BlandAltman_PM25_Limpio.html")
guardar_grafico(plot_ba(df_merged, "PM10", "PM10_C", "PM10_M"), "BlandAltman_PM10_Limpio.html")

# PROBABILITY OF AGREEMENT
guardar_grafico(plot_probability_agreement(df_merged, "PM1", "PM1_C", "PM1_M"), "Probability_of_Agreement_PM1.html")
guardar_grafico(plot_probability_agreement(df_merged, "PM2.5", "PM25_C", "PM25_M"), "Probability_of_Agreement_PM25.html")
guardar_grafico(plot_probability_agreement(df_merged, "PM10", "PM10_C", "PM10_M"), "Probability_of_Agreement_PM10.html")


# ==============================================================================
# 17. FIN DEL SCRIPT
# ==============================================================================

cat("\n============================================================\n")
cat(" ANÁLISIS GRIMM FINALIZADO\n")
cat("============================================================\n")
cat(paste(" Nivel estricto de Outliers aplicado (Multiplicador IQR):", MULTIPLICADOR_OUTLIERS, "\n"))
cat(" Resultados e interactivos guardados en la carpeta:", output_dir, "\n")
cat("============================================================\n")