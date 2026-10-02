# ==============================================================================
# PROCESAMIENTO Y COMPARACIÓN DE EQUIPOS GRIMM
# CETAM vs MT
#
# V5 - OUTLIERS + MÉTRICAS + GRÁFICOS INTERACTIVOS
#
# Correcciones incluidas:
#   - Autoescalado dinámico del eje Y en series temporales
#   - Rangeslider para seleccionar intervalo temporal
#   - Botones de selección temporal
#   - full_join() para conservar períodos sin datos de un equipo
#   - Manejo correcto de NA / NaN
#   - Regresión OLS robusta ante NA
#   - Bland-Altman usando solamente pares válidos
#   - Orden temporal explícito
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
# 2. CONFIGURAR DIRECTORIO DE SALIDA
# ==============================================================================

output_dir <- "Analisis_GRIMM_V2"

if (!dir.exists(output_dir)) {
  dir.create(
    output_dir,
    recursive = TRUE
  )
}


# ==============================================================================
# 3. ARCHIVOS DE ENTRADA
# ==============================================================================

file_cetam <- "GRIMM 11D Cetam 22052025.xlsx"

file_mt <- "GRIMM 11D MT 22052025.xlsx"


# ==============================================================================
# 4. FUNCIONES AUXILIARES
# ==============================================================================


# ------------------------------------------------------------------------------
# 4.1 Promedio robusto frente a NA
#
# Si todos los valores de un minuto son NA, devuelve NA en vez de NaN.
# ------------------------------------------------------------------------------

mean_na <- function(x) {
  
  if (all(is.na(x))) {
    
    return(NA_real_)
    
  } else {
    
    return(
      mean(
        x,
        na.rm = TRUE
      )
    )
  }
}


# ------------------------------------------------------------------------------
# 4.2 Eliminación de valores extremos superiores
#
# Se utiliza:
#
#       límite = Q3 + 5 * IQR
#
# IMPORTANTE:
# Solo se eliminan valores extremos superiores.
# No se eliminan valores extremos inferiores.
# ------------------------------------------------------------------------------

remove_extreme_outliers <- function(
    x,
    multiplier = 5
) {
  
  qnt <- quantile(
    x,
    probs = c(
      0.25,
      0.75
    ),
    na.rm = TRUE
  )
  
  iqr <- IQR(
    x,
    na.rm = TRUE
  )
  
  limite_superior <-
    qnt[2] +
    multiplier * iqr
  
  x[
    x > limite_superior
  ] <- NA
  
  return(x)
}


# ==============================================================================
# 5. CARGA Y LIMPIEZA DE DATOS
# ==============================================================================


load_grimm_mass <- function(
    filepath,
    equipo_label
) {
  
  read_excel(
    filepath,
    sheet = "Mass values",
    skip = 4
  ) %>%
    
    # --------------------------------------------------------------------------
  # Renombrar columnas
  # --------------------------------------------------------------------------
  
  rename(
    
    datetime_raw = `date&time`,
    
    PM10 = `PM10 [ug/m3]`,
    
    PM25 = `PM2,5 [ug/m3]`,
    
    PM1 = `PM1 [ug/m3]`
    
  ) %>%
    
    # --------------------------------------------------------------------------
  # Eliminar filas sin fecha
  # --------------------------------------------------------------------------
  
  filter(
    !is.na(datetime_raw)
  ) %>%
    
    # --------------------------------------------------------------------------
  # Conversión de variables
  # --------------------------------------------------------------------------
  
  mutate(
    
    datetime =
      dmy_hms(
        datetime_raw
      ),
    
    across(
      c(
        PM10,
        PM25,
        PM1
      ),
      as.numeric
    ),
    
    # ------------------------------------------------------------------------
    # Eliminación de outliers superiores
    # ------------------------------------------------------------------------
    
    across(
      c(
        PM10,
        PM25,
        PM1
      ),
      ~ remove_extreme_outliers(
        .x,
        multiplier = 5
      )
    )
    
  ) %>%
    
    # --------------------------------------------------------------------------
  # Filtrar fechas válidas
  # --------------------------------------------------------------------------
  
  filter(
    
    !is.na(datetime),
    
    datetime >=
      ymd(
        "2025-01-01"
      )
    
  ) %>%
    
    # --------------------------------------------------------------------------
  # Redondear a minuto
  # --------------------------------------------------------------------------
  
  mutate(
    
    datetime_1m =
      floor_date(
        datetime,
        unit = "1 minute"
      )
    
  )
}


# ==============================================================================
# 6. CARGAR Y PROMEDIAR CETAM
# ==============================================================================


df_c <- load_grimm_mass(
  file_cetam,
  "Cetam"
) %>%
  
  group_by(
    datetime_1m
  ) %>%
  
  summarise(
    
    PM1_C =
      mean_na(PM1),
    
    PM25_C =
      mean_na(PM25),
    
    PM10_C =
      mean_na(PM10),
    
    .groups = "drop"
    
  )


# ==============================================================================
# 7. CARGAR Y PROMEDIAR MT
# ==============================================================================


df_m <- load_grimm_mass(
  file_mt,
  "MT"
) %>%
  
  group_by(
    datetime_1m
  ) %>%
  
  summarise(
    
    PM1_M =
      mean_na(PM1),
    
    PM25_M =
      mean_na(PM25),
    
    PM10_M =
      mean_na(PM10),
    
    .groups = "drop"
    
  )


# ==============================================================================
# 8. UNIR SERIES TEMPORALES
#
# Se utiliza FULL JOIN en lugar de INNER JOIN.
#
# Esto permite conservar:
#
#   - minutos donde ambos equipos tienen datos
#   - minutos donde solo Cetam tiene datos
#   - minutos donde solo MT tiene datos
#
# Para las métricas se volverán a utilizar únicamente los pares completos.
# ==============================================================================


df_merged <- full_join(
  
  df_c,
  
  df_m,
  
  by = "datetime_1m"
  
) %>%
  
  rename(
    datetime = datetime_1m
  ) %>%
  
  arrange(
    datetime
  )


# ==============================================================================
# 9. MÉTRICAS AVANZADAS
# ==============================================================================


calc_metrics <- function(
    df,
    c_col,
    m_col,
    label
) {
  
  # --------------------------------------------------------------------------
  # Extraer variables
  # --------------------------------------------------------------------------
  
  x <- df[[c_col]]
  
  y <- df[[m_col]]
  
  
  # --------------------------------------------------------------------------
  # Mantener solamente pares válidos
  # --------------------------------------------------------------------------
  
  valid_idx <-
    is.finite(x) &
    is.finite(y)
  
  x <- x[valid_idx]
  
  y <- y[valid_idx]
  
  
  # --------------------------------------------------------------------------
  # Verificar cantidad de observaciones
  # --------------------------------------------------------------------------
  
  if (length(x) < 2) {
    
    return(
      data.frame(
        
        Parametro = label,
        
        Media_Cetam = NA_real_,
        
        Media_MT = NA_real_,
        
        Pearson = NA_real_,
        
        CCC_Lin = NA_real_,
        
        Bias_Mean_Diff = NA_real_,
        
        LoA_Lower = NA_real_,
        
        LoA_Upper = NA_real_
        
      )
    )
  }
  
  
  # ============================================================================
  # Pearson
  # ============================================================================
  
  r_pearson <- cor(
    x,
    y,
    method = "pearson"
  )
  
  
  # ============================================================================
  # CCC DE LIN
  # ============================================================================
  
  mean_x <- mean(x)
  
  mean_y <- mean(y)
  
  var_x <- var(x)
  
  var_y <- var(y)
  
  ccc <-
    (
      2 *
        r_pearson *
        sd(x) *
        sd(y)
    ) /
    (
      var_x +
        var_y +
        (mean_x - mean_y)^2
    )
  
  
  # ============================================================================
  # BLAND-ALTMAN
  # ============================================================================
  
  diff <-
    y - x
  
  bias <-
    mean(diff)
  
  sd_diff <-
    sd(diff)
  
  loa_upper <-
    bias +
    1.96 * sd_diff
  
  loa_lower <-
    bias -
    1.96 * sd_diff
  
  
  # ============================================================================
  # Resultado
  # ============================================================================
  
  data.frame(
    
    Parametro = label,
    
    Media_Cetam =
      mean_x,
    
    Media_MT =
      mean_y,
    
    Pearson =
      r_pearson,
    
    CCC_Lin =
      ccc,
    
    Bias_Mean_Diff =
      bias,
    
    LoA_Lower =
      loa_lower,
    
    LoA_Upper =
      loa_upper
    
  )
}


# ==============================================================================
# 10. CALCULAR MÉTRICAS
# ==============================================================================


resumen <- bind_rows(
  
  calc_metrics(
    df_merged,
    "PM1_C",
    "PM1_M",
    "PM1"
  ),
  
  calc_metrics(
    df_merged,
    "PM25_C",
    "PM25_M",
    "PM2.5"
  ),
  
  calc_metrics(
    df_merged,
    "PM10_C",
    "PM10_M",
    "PM10"
  )
  
)


# ==============================================================================
# 11. GUARDAR MÉTRICAS
# ==============================================================================


write.csv(
  
  resumen,
  
  file.path(
    output_dir,
    "Metricas_Avanzadas_SinOutliers.csv"
  ),
  
  row.names = FALSE
  
)


print(resumen)


# ==============================================================================
# 12. SERIE DE TIEMPO INTERACTIVA
#
# Características:
#
#   - Rangeslider debajo del gráfico
#   - Selección temporal
#   - Botones 1 día / 1 semana / 1 mes / Todo
#   - Autoescala dinámica del eje Y
#   - Zoom manual
#   - Hover unificado
# ==============================================================================


plot_ts <- function(
    df,
    poll,
    c_col,
    m_col
) {
  
  # --------------------------------------------------------------------------
  # Extraer columnas
  # --------------------------------------------------------------------------
  
  y_cetam <- df[[c_col]]
  
  y_mt <- df[[m_col]]
  
  
  # --------------------------------------------------------------------------
  # Crear gráfico
  # --------------------------------------------------------------------------
  
  p <- plot_ly(
    
    df,
    
    x = ~datetime
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # Cetam
  # ------------------------------------------------------------------------
  
  add_lines(
    
    y = y_cetam,
    
    name =
      paste(
        "Cetam",
        poll
      ),
    
    line = list(
      
      color = "#1f77b4",
      
      width = 1
      
    )
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # MT
  # ------------------------------------------------------------------------
  
  add_lines(
    
    y = y_mt,
    
    name =
      paste(
        "MT",
        poll
      ),
    
    line = list(
      
      color = "#ff7f0e",
      
      width = 1
      
    )
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # Layout
  # ------------------------------------------------------------------------
  
  layout(
    
    title = list(
      
      text =
        paste(
          "Serie de Tiempo Limpia -",
          poll
        )
      
    ),
    
    # ----------------------------------------------------------------------
    # Eje X
    # ----------------------------------------------------------------------
    
    xaxis = list(
      
      title = "Fecha",
      
      type = "date",
      
      # Selector visual debajo del gráfico
      rangeslider = list(
        
        visible = TRUE,
        
        thickness = 0.10
        
      ),
      
      # Botones de selección temporal
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
    
    # ----------------------------------------------------------------------
    # Eje Y
    # ----------------------------------------------------------------------
    
    yaxis = list(
      
      title = "µg/m³",
      
      autorange = TRUE,
      
      fixedrange = FALSE
      
    ),
    
    dragmode = "zoom",
    
    hovermode = "x unified"
    
  )
  
  
  # ==========================================================================
  # JAVASCRIPT PARA AUTOESCALADO DEL EJE Y
  #
  # Cada vez que el usuario modifica el intervalo temporal:
  #
  #   1. Se obtiene el nuevo rango de X.
  #   2. Se buscan los datos dentro de ese rango.
  #   3. Se obtiene min(Y) y max(Y).
  #   4. Se agrega 5% de margen.
  #   5. Se actualiza automáticamente el eje Y.
  # ==========================================================================
  
  
  p <- htmlwidgets::onRender(
    
    p,
    
    "
    function(el, x) {

      var plot = el;

      plot.on(
        'plotly_relayout',
        function(eventdata) {

          // --------------------------------------------------------------
          // Detectar modificaciones del eje X
          // --------------------------------------------------------------

          if (

            eventdata['xaxis.range[0]'] !== undefined ||

            eventdata['xaxis.range[1]'] !== undefined ||

            eventdata['xaxis.autorange'] !== undefined

          ) {

            var x0 =
              eventdata['xaxis.range[0]'];

            var x1 =
              eventdata['xaxis.range[1]'];


            // ------------------------------------------------------------
            // Si se seleccionó 'Todo'
            // ------------------------------------------------------------

            if (
              eventdata['xaxis.autorange'] === true
            ) {

              Plotly.relayout(

                plot,

                {
                  'yaxis.autorange': true
                }

              );

              return;
            }


            // ------------------------------------------------------------
            // Ambos extremos son necesarios
            // ------------------------------------------------------------

            if (
              x0 === undefined ||
              x1 === undefined
            ) {

              return;

            }


            // ------------------------------------------------------------
            // Convertir fechas a milisegundos
            // ------------------------------------------------------------

            var xmin =
              new Date(x0).getTime();

            var xmax =
              new Date(x1).getTime();


            // ------------------------------------------------------------
            // Vector para guardar valores visibles
            // ------------------------------------------------------------

            var valores = [];


            // ------------------------------------------------------------
            // Recorrer las series
            // ------------------------------------------------------------

            plot.data.forEach(

              function(trace) {

                if (
                  trace.x &&
                  trace.y
                ) {

                  for (
                    var i = 0;
                    i < trace.x.length;
                    i++
                  ) {

                    var xi =
                      new Date(
                        trace.x[i]
                      ).getTime();

                    var yi =
                      trace.y[i];


                    // --------------------------------------------------
                    // Conservar solamente datos visibles y válidos
                    // --------------------------------------------------

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

              }

            );


            // ------------------------------------------------------------
            // Si existen datos visibles
            // ------------------------------------------------------------

            if (
              valores.length > 0
            ) {

              var ymin =
                Math.min.apply(
                  null,
                  valores
                );

              var ymax =
                Math.max.apply(
                  null,
                  valores
                );


              // ----------------------------------------------------------
              // Caso de serie constante
              // ----------------------------------------------------------

              if (
                ymin === ymax
              ) {

                var margen =
                  Math.abs(ymin) * 0.05;


                if (
                  margen === 0
                ) {

                  margen = 1;

                }


                ymin -= margen;

                ymax += margen;

              }


              // ----------------------------------------------------------
              // Caso normal
              // ----------------------------------------------------------

              else {

                var margen =
                  (ymax - ymin) * 0.05;

                ymin -= margen;

                ymax += margen;

              }


              // ----------------------------------------------------------
              // Actualizar eje Y
              // ----------------------------------------------------------

              Plotly.relayout(

                plot,

                {

                  'yaxis.range':
                    [ymin, ymax],

                  'yaxis.autorange':
                    false

                }

              );

            }

          }

        }

      );

    }
    "
  )
  
  
  return(p)
}


# ==============================================================================
# 13. BLAND-ALTMAN
# ==============================================================================


plot_ba <- function(
    df,
    poll,
    c_col,
    m_col
) {
  
  # --------------------------------------------------------------------------
  # Crear tabla con pares válidos
  # --------------------------------------------------------------------------
  
  datos <- tibble(
    
    x = df[[c_col]],
    
    y = df[[m_col]]
    
  ) %>%
    
    filter(
      
      is.finite(x),
      
      is.finite(y)
      
    )
  
  
  # --------------------------------------------------------------------------
  # Media de ambos equipos
  # --------------------------------------------------------------------------
  
  media_xy <-
    (
      datos$x +
        datos$y
    ) / 2
  
  
  # --------------------------------------------------------------------------
  # Diferencia
  # --------------------------------------------------------------------------
  
  diferencia <-
    datos$y -
    datos$x
  
  
  # --------------------------------------------------------------------------
  # Estadísticas
  # --------------------------------------------------------------------------
  
  bias <-
    mean(
      diferencia
    )
  
  sd_diff <-
    sd(
      diferencia
    )
  
  loa_u <-
    bias +
    1.96 * sd_diff
  
  loa_l <-
    bias -
    1.96 * sd_diff
  
  
  # --------------------------------------------------------------------------
  # Rango X
  # --------------------------------------------------------------------------
  
  rango_x <-
    range(
      media_xy,
      na.rm = TRUE
    )
  
  
  # --------------------------------------------------------------------------
  # Gráfico
  # --------------------------------------------------------------------------
  
  plot_ly(
    
    x = media_xy,
    
    y = diferencia,
    
    type = "scatter",
    
    mode = "markers",
    
    marker = list(
      
      color =
        "rgba(44, 160, 44, 0.4)",
      
      size = 4
      
    ),
    
    name = "Medición"
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # Sesgo
  # ------------------------------------------------------------------------
  
  add_lines(
    
    x = rango_x,
    
    y = c(
      bias,
      bias
    ),
    
    name =
      "Sesgo (Media)",
    
    line = list(
      
      color = "red",
      
      width = 2
      
    )
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # Límite superior
  # ------------------------------------------------------------------------
  
  add_lines(
    
    x = rango_x,
    
    y = c(
      loa_u,
      loa_u
    ),
    
    name =
      "Límite Sup (+1.96 SD)",
    
    line = list(
      
      color = "blue",
      
      dash = "dash"
      
    )
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # Límite inferior
  # ------------------------------------------------------------------------
  
  add_lines(
    
    x = rango_x,
    
    y = c(
      loa_l,
      loa_l
    ),
    
    name =
      "Límite Inf (-1.96 SD)",
    
    line = list(
      
      color = "blue",
      
      dash = "dash"
      
    )
    
  ) %>%
    
    layout(
      
      title =
        paste(
          "Bland-Altman (Sin Outliers) -",
          poll
        ),
      
      xaxis = list(
        
        title =
          "Media de ambos equipos [(MT + Cetam)/2] µg/m³"
        
      ),
      
      yaxis = list(
        
        title =
          "Diferencia (MT - Cetam) µg/m³"
        
      )
      
    )
}


# ==============================================================================
# 14. DISPERSIÓN + REGRESIÓN OLS
# ==============================================================================


plot_scatter_limpio <- function(df, poll, c_col, m_col) {
  
  # ------------------------------------------------------------
  # 1. Seleccionar datos válidos
  # ------------------------------------------------------------
  
  datos <- data.frame(
    Cetam = df[[c_col]],
    MT = df[[m_col]]
  ) %>%
    filter(
      is.finite(Cetam),
      is.finite(MT)
    )
  
  # Verificar que existan suficientes datos
  if (nrow(datos) < 2) {
    return(
      plot_ly() %>%
        layout(
          title = paste("Datos insuficientes -", poll)
        )
    )
  }
  
  # ------------------------------------------------------------
  # 2. Regresión lineal
  # ------------------------------------------------------------
  
  fit <- lm(MT ~ Cetam, data = datos)
  
  pendiente <- coef(fit)[2]
  intercepto <- coef(fit)[1]
  
  r <- cor(
    datos$Cetam,
    datos$MT,
    method = "pearson"
  )
  
  r2 <- summary(fit)$r.squared
  
  n <- nrow(datos)
  
  # ------------------------------------------------------------
  # 3. Ecuación de la recta
  # ------------------------------------------------------------
  
  ecuacion <- paste0(
    "y = ",
    round(pendiente, 3),
    "x ",
    ifelse(intercepto >= 0, "+ ", "- "),
    round(abs(intercepto), 3)
  )
  
  # ------------------------------------------------------------
  # 4. Valores para dibujar la regresión
  # ------------------------------------------------------------
  
  x_reg <- seq(
    min(datos$Cetam),
    max(datos$Cetam),
    length.out = 100
  )
  
  y_reg <- predict(
    fit,
    newdata = data.frame(Cetam = x_reg)
  )
  
  # ------------------------------------------------------------
  # 5. Límites para la línea y = x
  # ------------------------------------------------------------
  
  limite <- max(
    datos$Cetam,
    datos$MT,
    na.rm = TRUE
  )
  
  # ------------------------------------------------------------
  # 6. Crear gráfico
  # ------------------------------------------------------------
  
  p <- plot_ly() %>%
    
    # Puntos
    add_markers(
      x = datos$Cetam,
      y = datos$MT,
      name = "Mediciones",
      marker = list(
        size = 5,
        opacity = 0.6
      ),
      hovertemplate =
        paste(
          "Cetam: %{x:.2f} µg/m³",
          "<br>MT: %{y:.2f} µg/m³",
          "<extra></extra>"
        )
    ) %>%
    
    # Recta de regresión
    add_lines(
      x = x_reg,
      y = y_reg,
      name = "Regresión OLS",
      line = list(
        width = 2
      )
    ) %>%
    
    # Línea de identidad y = x
    add_lines(
      x = c(0, limite),
      y = c(0, limite),
      name = "y = x",
      line = list(
        dash = "dash",
        width = 2
      )
    ) %>%
    
    # ----------------------------------------------------------
  # 7. Layout
  # ----------------------------------------------------------
  
  layout(
    
    title = list(
      text = paste(
        "Comparación Cetam vs MT -",
        poll
      )
    ),
    
    xaxis = list(
      title = "Cetam (µg/m³)",
      zeroline = FALSE
    ),
    
    yaxis = list(
      title = "MT (µg/m³)",
      zeroline = FALSE
    ),
    
    hovermode = "closest",
    
    # --------------------------------------------------------
    # 8. Información estadística dentro del gráfico
    # --------------------------------------------------------
    
    annotations = list(
      list(
        x = 0.02,
        y = 0.98,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "top",
        
        text = paste0(
          "<b>Pearson r:</b> ", round(r, 4),
          "<br><b>R²:</b> ", round(r2, 4),
          "<br><b>", ecuacion, "</b>",
          "<br><b>n:</b> ", n
        ),
        
        showarrow = FALSE,
        
        align = "left",
        
        bgcolor = "rgba(255,255,255,0.85)",
        
        bordercolor = "black",
        
        borderwidth = 1,
        
        borderpad = 6
      )
    )
  )
  
  return(p)
}

# ==============================================================================
# 15. GENERAR GRÁFICOS DE DISPERSIÓN
# ==============================================================================


saveWidget(
  
  plot_scatter_limpio(
    
    df_merged,
    
    "PM1",
    
    "PM1_C",
    
    "PM1_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "Scatter_PM1_Limpio.html"
    
  )
  
)


saveWidget(
  
  plot_scatter_limpio(
    
    df_merged,
    
    "PM2.5",
    
    "PM25_C",
    
    "PM25_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "Scatter_PM25_Limpio.html"
    
  )
  
)


saveWidget(
  
  plot_scatter_limpio(
    
    df_merged,
    
    "PM10",
    
    "PM10_C",
    
    "PM10_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "Scatter_PM10_Limpio.html"
    
  )
  
)


# ==============================================================================
# 16. GENERAR SERIES TEMPORALES
# ==============================================================================


saveWidget(
  
  plot_ts(
    
    df_merged,
    
    "PM1",
    
    "PM1_C",
    
    "PM1_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "TS_PM1_Limpio.html"
    
  )
  
)


saveWidget(
  
  plot_ts(
    
    df_merged,
    
    "PM2.5",
    
    "PM25_C",
    
    "PM25_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "TS_PM25_Limpio.html"
    
  )
  
)


saveWidget(
  
  plot_ts(
    
    df_merged,
    
    "PM10",
    
    "PM10_C",
    
    "PM10_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "TS_PM10_Limpio.html"
    
  )
  
)


# ==============================================================================
# 17. GENERAR BLAND-ALTMAN
# ==============================================================================


saveWidget(
  
  plot_ba(
    
    df_merged,
    
    "PM1",
    
    "PM1_C",
    
    "PM1_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "BlandAltman_PM1_Limpio.html"
    
  )
  
)


saveWidget(
  
  plot_ba(
    
    df_merged,
    
    "PM2.5",
    
    "PM25_C",
    
    "PM25_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "BlandAltman_PM25_Limpio.html"
    
  )
  
)


saveWidget(
  
  plot_ba(
    
    df_merged,
    
    "PM10",
    
    "PM10_C",
    
    "PM10_M"
    
  ),
  
  file.path(
    
    output_dir,
    
    "BlandAltman_PM10_Limpio.html"
    
  )
  
)


# ==============================================================================
# FIN DEL SCRIPT
# ==============================================================================

cat(
  "\n============================================================\n",
  "ANÁLISIS GRIMM FINALIZADO\n",
  "============================================================\n",
  "Resultados guardados en:\n",
  output_dir,
  "\n============================================================\n"
)

