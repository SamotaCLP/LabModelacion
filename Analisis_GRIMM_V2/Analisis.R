# ==============================================================================
# PROCESAMIENTO Y COMPARACIÓN DE EQUIPOS GRIMM (CETAM vs MT) - V4 (OUTLIERS)
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, lubridate, plotly, htmlwidgets)

# 1. Configurar directorio
output_dir <- "Analisis_GRIMM_V2"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Archivos
file_cetam <- "GRIMM 11D Cetam 22052025.xlsx"
file_mt    <- "GRIMM 11D MT 22052025.xlsx"

# --- NUEVO: FUNCIÓN PARA TRATAMIENTO DE OUTLIERS ---
# Utiliza el Rango Intercuartílico para detectar y eliminar picos espurios.
remove_extreme_outliers <- function(x, multiplier = 5) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  
  # Límite superior estricto (Q3 + 5 * IQR)
  limite_superior <- qnt[2] + multiplier * iqr
  
  # Reemplazar valores irreales con NA
  x[x > limite_superior] <- NA
  return(x)
}
# ---------------------------------------------------

# 3. Función de carga modificada para aplicar limpieza
load_grimm_mass <- function(filepath, equipo_label) {
  read_excel(filepath, sheet = "Mass values", skip = 4) %>%
    rename(
      datetime_raw = `date&time`,
      PM10 = `PM10 [ug/m3]`,
      PM25 = `PM2,5 [ug/m3]`,
      PM1  = `PM1 [ug/m3]`
    ) %>%
    filter(!is.na(datetime_raw)) %>%
    mutate(
      datetime = dmy_hms(datetime_raw),
      across(c(PM10, PM25, PM1), as.numeric),
      # APLICAR TRATAMIENTO DE OUTLIERS a PM1, PM2.5 y PM10
      across(c(PM10, PM25, PM1), ~remove_extreme_outliers(.x, multiplier = 5))
    ) %>%
    filter(!is.na(datetime), datetime >= ymd("2025-01-01")) %>%
    mutate(
      datetime_1m = floor_date(datetime, "1 minute"),
      Equipo = equipo_label
    )
}


#------ funcion nan
mean_na <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}

# 4. Leer y promediar a 1 min (los NAs generados por outliers son ignorados al promediar)
df_c <- load_grimm_mass(file_cetam, "Cetam") %>%
  group_by(datetime_1m) %>%
  summarise(PM1_C = mean(PM1, na.rm=T), PM25_C = mean(PM25, na.rm=T), PM10_C = mean(PM10, na.rm=T), .groups="drop")

df_m <- load_grimm_mass(file_mt, "MT") %>%
  group_by(datetime_1m) %>%
  summarise(PM1_M = mean(PM1, na.rm=T), PM25_M = mean(PM25, na.rm=T), PM10_M = mean(PM10, na.rm=T), .groups="drop")

df_merged <- inner_join(df_c, df_m, by = "datetime_1m") %>% rename(datetime = datetime_1m)

# ==============================================================================
# 5. MÉTRICAS AVANZADAS (INCLUYENDO CCC DE LIN)
# ==============================================================================

calc_metrics <- function(df, c_col, m_col, label) {
  x <- df[[c_col]]
  y <- df[[m_col]]
  
  # Remover NAs (incluyendo los outliers que convertimos en NA) para el cálculo
  valid_idx <- !is.na(x) & !is.na(y)
  x <- x[valid_idx]
  y <- y[valid_idx]
  
  # Pearson
  r_pearson <- cor(x, y, method = "pearson")
  
  # Concordance Correlation Coefficient (Lin)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  ccc <- (2 * r_pearson * sd(x) * sd(y)) / (var_x + var_y + (mean_x - mean_y)^2)
  
  # Bland Altman Stats
  diff <- y - x
  bias <- mean(diff)
  sd_diff <- sd(diff)
  loa_upper <- bias + 1.96 * sd_diff
  loa_lower <- bias - 1.96 * sd_diff
  
  data.frame(
    Parametro = label,
    Media_Cetam = mean_x, Media_MT = mean_y,
    Pearson = r_pearson,
    CCC_Lin = ccc,
    Bias_Mean_Diff = bias,
    LoA_Lower = loa_lower,
    LoA_Upper = loa_upper
  )
}

resumen <- bind_rows(
  calc_metrics(df_merged, "PM1_C", "PM1_M", "PM1"),
  calc_metrics(df_merged, "PM25_C", "PM25_M", "PM2.5"),
  calc_metrics(df_merged, "PM10_C", "PM10_M", "PM10")
)

write.csv(resumen, file.path(output_dir, "Metricas_Avanzadas_SinOutliers.csv"), row.names = FALSE)
print(resumen)

# ==============================================================================
# 6. GRÁFICOS INTERACTIVOS (SERIES CON AUTOESCALADO + BLAND ALTMAN)
# ==============================================================================

# ==============================================================================
# A) SERIES DE TIEMPO INTERACTIVAS
#    - Selector de rango temporal debajo del gráfico
#    - Autoescala del eje Y según el intervalo seleccionado
# ==============================================================================

plot_ts <- function(df, poll, c_col, m_col) {
  
  # Datos para Cetam
  y_cetam <- df[[c_col]]
  
  # Datos para MT
  y_mt <- df[[m_col]]
  
  p <- plot_ly(df, x = ~datetime) %>%
    
    add_lines(
      y = y_cetam,
      name = paste("Cetam", poll),
      line = list(
        color = "#1f77b4",
        width = 1
      )
    ) %>%
    
    add_lines(
      y = y_mt,
      name = paste("MT", poll),
      line = list(
        color = "#ff7f0e",
        width = 1
      )
    ) %>%
    
    layout(
      
      title = list(
        text = paste("Serie de Tiempo Limpia -", poll)
      ),
      
      xaxis = list(
        title = "Fecha",
        type = "date",
        
        # Selector de intervalo debajo del gráfico
        rangeslider = list(
          visible = TRUE,
          thickness = 0.10
        ),
        
        # Permite seleccionar intervalos predefinidos
        rangeselector = list(
          buttons = list(
            list(
              count = 1,
              label = "1 día",
              step = "day",
              stepmode = "backward"
            ),
            list(
              count = 7,
              label = "1 semana",
              step = "day",
              stepmode = "backward"
            ),
            list(
              count = 1,
              label = "1 mes",
              step = "month",
              stepmode = "backward"
            ),
            list(
              step = "all",
              label = "Todo"
            )
          )
        )
      ),
      
      yaxis = list(
        title = "µg/m³",
        autorange = TRUE,
        fixedrange = FALSE
      ),
      
      dragmode = "zoom",
      hovermode = "x unified"
    )
  
  # --------------------------------------------------------------------------
  # IMPORTANTE:
  # Cuando cambia el intervalo temporal, recalcular automáticamente
  # el rango del eje Y usando solamente los datos visibles.
  # --------------------------------------------------------------------------
  
  p <- htmlwidgets::onRender(
    p,
    "
    function(el, x) {

      var plot = el;

      plot.on('plotly_relayout', function(eventdata) {

        // Detectar cambios en el rango del eje X
        if (
          eventdata['xaxis.range[0]'] !== undefined ||
          eventdata['xaxis.range[1]'] !== undefined ||
          eventdata['xaxis.autorange'] !== undefined
        ) {

          var x0 = eventdata['xaxis.range[0]'];
          var x1 = eventdata['xaxis.range[1]'];

          // Si se seleccionó 'Todo', volver a autorange
          if (eventdata['xaxis.autorange'] === true) {

            Plotly.relayout(plot, {
              'yaxis.autorange': true
            });

            return;
          }

          // Si no tenemos ambos extremos, no hacer nada
          if (x0 === undefined || x1 === undefined) {
            return;
          }

          var xmin = new Date(x0).getTime();
          var xmax = new Date(x1).getTime();

          var valores = [];

          // Recorrer las dos series
          plot.data.forEach(function(trace) {

            if (trace.x && trace.y) {

              for (var i = 0; i < trace.x.length; i++) {

                var xi = new Date(trace.x[i]).getTime();
                var yi = trace.y[i];

                if (
                  xi >= xmin &&
                  xi <= xmax &&
                  yi !== null &&
                  yi !== undefined &&
                  isFinite(yi)
                ) {
                  valores.push(yi);
                }
              }
            }
          });

          // Si encontramos datos visibles, calcular rango Y
          if (valores.length > 0) {

            var ymin = Math.min.apply(null, valores);
            var ymax = Math.max.apply(null, valores);

            // Evitar que una serie constante produzca un eje degenerado
            if (ymin === ymax) {
              var margen = Math.abs(ymin) * 0.05;

              if (margen === 0) {
                margen = 1;
              }

              ymin -= margen;
              ymax += margen;
            } else {

              // Agregar 5% de margen arriba y abajo
              var margen = (ymax - ymin) * 0.05;

              ymin -= margen;
              ymax += margen;
            }

            Plotly.relayout(plot, {
              'yaxis.range': [ymin, ymax],
              'yaxis.autorange': false
            });
          }
        }
      });
    }
    "
  )
  
  return(p)
}

# B) Bland-Altman
plot_ba <- function(df, poll, c_col, m_col) {
  x <- df[[c_col]]
  y <- df[[m_col]]
  media_xy <- (x + y) / 2
  diferencia <- y - x
  
  bias <- mean(diferencia, na.rm=T)
  sd_diff <- sd(diferencia, na.rm=T)
  loa_u <- bias + 1.96 * sd_diff
  loa_l <- bias - 1.96 * sd_diff
  
  plot_ly(x = ~media_xy, y = ~diferencia, type = "scatter", mode = "markers",
          marker = list(color = "rgba(44, 160, 44, 0.4)", size = 4),
          name = "Medición") %>%
    add_lines(x = range(media_xy, na.rm=T), y = c(bias, bias), name = "Sesgo (Media)", line = list(color="red", width=2)) %>%
    add_lines(x = range(media_xy, na.rm=T), y = c(loa_u, loa_u), name = "Límite Sup (+1.96 SD)", line = list(color="blue", dash="dash")) %>%
    add_lines(x = range(media_xy, na.rm=T), y = c(loa_l, loa_l), name = "Límite Inf (-1.96 SD)", line = list(color="blue", dash="dash")) %>%
    layout(
      title = paste("Bland-Altman (Sin Outliers) -", poll),
      xaxis = list(title = paste("Media de ambos equipos [(MT + Cetam)/2] µg/m³")),
      yaxis = list(title = paste("Diferencia (MT - Cetam) µg/m³"))
    )
}

# --- C) Gráficos de Dispersión (Scatter) Sin Outliers ---
plot_scatter_limpio <- function(df, pollutant_name, c_col, m_col) {
  x_val <- df[[c_col]]
  y_val <- df[[m_col]]
  
  # Regresión lineal usando datos limpios
  fit <- lm(y_val ~ x_val)
  max_lim <- max(c(max(x_val, na.rm=TRUE), max(y_val, na.rm=TRUE)))
  
  plot_ly() %>%
    add_markers(
      x = x_val, y = y_val, name = "Mediciones 1-min",
      marker = list(color = "#2ca02c", opacity = 0.35, size = 4),
      hovertemplate = paste("<b>Cetam</b>: %{x:.2f}<br><b>MT</b>: %{y:.2f}<extra></extra>")
    ) %>%
    add_lines(
      x = c(0, max_lim), y = c(0, max_lim), name = "Identidad 1:1",
      line = list(color = "black", dash = "dash")
    ) %>%
    add_lines(
      x = x_val, y = fitted(fit), name = "Regresión (OLS)",
      line = list(color = "red", width = 2)
    ) %>%
    layout(
      title = list(text = paste("Dispersión y Regresión (Sin Outliers) -", pollutant_name)),
      xaxis = list(title = paste(pollutant_name, "Cetam (µg/m³)")),
      yaxis = list(title = paste(pollutant_name, "MT (µg/m³)")),
      showlegend = TRUE
    )
}

# Generar y guardar los Scatter limpios
saveWidget(plot_scatter_limpio(df_merged, "PM1", "PM1_C", "PM1_M"), file.path(output_dir, "Scatter_PM1_Limpio.html"))
saveWidget(plot_scatter_limpio(df_merged, "PM2.5", "PM25_C", "PM25_M"), file.path(output_dir, "Scatter_PM25_Limpio.html"))
saveWidget(plot_scatter_limpio(df_merged, "PM10", "PM10_C", "PM10_M"), file.path(output_dir, "Scatter_PM10_Limpio.html"))


# Generar y guardar
saveWidget(plot_ts(df_merged, "PM1", "PM1_C", "PM1_M"), file.path(output_dir, "TS_PM1_Limpio.html"))
saveWidget(plot_ts(df_merged, "PM2.5", "PM25_C", "PM25_M"), file.path(output_dir, "TS_PM25_Limpio.html"))
saveWidget(plot_ts(df_merged, "PM10", "PM10_C", "PM10_M"), file.path(output_dir, "TS_PM10_Limpio.html"))

saveWidget(plot_ba(df_merged, "PM1", "PM1_C", "PM1_M"), file.path(output_dir, "BlandAltman_PM1_Limpio.html"))
saveWidget(plot_ba(df_merged, "PM2.5", "PM25_C", "PM25_M"), file.path(output_dir, "BlandAltman_PM25_Limpio.html"))
saveWidget(plot_ba(df_merged, "PM10", "PM10_C", "PM10_M"), file.path(output_dir, "BlandAltman_PM10_Limpio.html"))