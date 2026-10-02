# ==============================================================================
# PROCESAMIENTO Y COMPARACIÓN DE EQUIPOS GRIMM
# CETAM vs MT
#
# V6 - OUTLIERS + MÉTRICAS + GRÁFICOS INTERACTIVOS
#
# Incluye:
#   - Eliminación de outliers superiores mediante Q3 + 5*IQR
#   - Promedios por minuto
#   - FULL JOIN de las series
#   - Pearson
#   - CCC de Lin
#   - Bias y límites de concordancia
#   - Bland-Altman
#   - Scatter + regresión OLS
#   - Línea de identidad y = x
#   - R²
#   - Probability of Agreement
#   - Rangeslider
#   - Rangeselector
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
# límite = Q3 + 5 * IQR
#
# Solo se eliminan valores extremos superiores.
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
    
    rename(
      
      datetime_raw = `date&time`,
      
      PM10 = `PM10 [ug/m3]`,
      
      PM25 = `PM2,5 [ug/m3]`,
      
      PM1 = `PM1 [ug/m3]`
      
    ) %>%
    
    filter(
      !is.na(datetime_raw)
    ) %>%
    
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
    
    filter(
      
      !is.na(datetime),
      
      datetime >=
        ymd(
          "2025-01-01"
        )
      
    ) %>%
    
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
# FULL JOIN:
#
#   - conserva minutos con ambos equipos
#   - conserva minutos solo de Cetam
#   - conserva minutos solo de MT
#
# Las métricas utilizan únicamente pares completos.
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
        
        N = length(x),
        
        Media_Cetam = NA_real_,
        
        Media_MT = NA_real_,
        
        Pearson = NA_real_,
        
        R2 = NA_real_,
        
        CCC_Lin = NA_real_,
        
        Bias_Mean_Diff = NA_real_,
        
        LoA_Lower = NA_real_,
        
        LoA_Upper = NA_real_
        
      )
    )
  }
  
  
  # ============================================================================
  # Medias
  # ============================================================================
  
  mean_x <- mean(x)
  
  mean_y <- mean(y)
  
  
  # ============================================================================
  # Pearson
  # ============================================================================
  
  if (
    sd(x) == 0 ||
    sd(y) == 0
  ) {
    
    r_pearson <- NA_real_
    
  } else {
    
    r_pearson <- cor(
      x,
      y,
      method = "pearson"
    )
  }
  
  
  # ============================================================================
  # REGRESIÓN OLS
  # ============================================================================
  
  if (
    sd(x) == 0
  ) {
    
    r2 <- NA_real_
    
  } else {
    
    fit <- lm(
      y ~ x
    )
    
    r2 <- summary(fit)$r.squared
  }
  
  
  # ============================================================================
  # CCC DE LIN
  #
  # CCC = 2 * cov(x,y) /
  #       [var(x) + var(y) + (mean(x)-mean(y))^2]
  # ============================================================================
  
  var_x <- var(x)
  
  var_y <- var(y)
  
  cov_xy <- cov(
    x,
    y
  )
  
  denominador_ccc <-
    var_x +
    var_y +
    (mean_x - mean_y)^2
  
  if (
    denominador_ccc == 0
  ) {
    
    ccc <- NA_real_
    
  } else {
    
    ccc <-
      2 * cov_xy /
      denominador_ccc
  }
  
  
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
    
    N = length(x),
    
    Media_Cetam =
      mean_x,
    
    Media_MT =
      mean_y,
    
    Pearson =
      r_pearson,
    
    R2 =
      r2,
    
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
    
    layout(
      
      title = list(
        
        text =
          paste(
            "Serie de Tiempo Limpia -",
            poll
          )
        
      ),
      
      xaxis = list(
        
        title = "Fecha",
        
        type = "date",
        
        rangeslider = list(
          
          visible = TRUE,
          
          thickness = 0.10
          
        ),
        
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
  
  
  # ============================================================================
  # JAVASCRIPT - AUTOESCALADO DEL EJE Y
  # ============================================================================
  
  p <- htmlwidgets::onRender(
    
    p,
    
    "
    function(el, x) {

      var plot = el;

      plot.on(
        'plotly_relayout',
        function(eventdata) {

          if (

            eventdata['xaxis.range[0]'] !== undefined ||

            eventdata['xaxis.range[1]'] !== undefined ||

            eventdata['xaxis.autorange'] !== undefined

          ) {

            var x0 =
              eventdata['xaxis.range[0]'];

            var x1 =
              eventdata['xaxis.range[1]'];


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


            if (
              x0 === undefined ||
              x1 === undefined
            ) {

              return;

            }


            var xmin =
              new Date(x0).getTime();

            var xmax =
              new Date(x1).getTime();


            var valores = [];


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

              } else {

                var margen =
                  (ymax - ymin) * 0.05;

                ymin -= margen;

                ymax += margen;

              }


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
  
  datos <- tibble(
    
    x = df[[c_col]],
    
    y = df[[m_col]]
    
  ) %>%
    
    filter(
      
      is.finite(x),
      
      is.finite(y)
      
    )
  
  
  # --------------------------------------------------------------------------
  # Datos insuficientes
  # --------------------------------------------------------------------------
  
  if (
    nrow(datos) < 2
  ) {
    
    return(
      plot_ly() %>%
        layout(
          title =
            paste(
              "Datos insuficientes -",
              poll
            )
        )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Media y diferencia
  # --------------------------------------------------------------------------
  
  media_xy <-
    (
      datos$x +
        datos$y
    ) / 2
  
  diferencia <-
    datos$y -
    datos$x
  
  
  # --------------------------------------------------------------------------
  # Estadísticas
  # --------------------------------------------------------------------------
  
  bias <-
    mean(diferencia)
  
  sd_diff <-
    sd(diferencia)
  
  loa_u <-
    bias +
    1.96 * sd_diff
  
  loa_l <-
    bias -
    1.96 * sd_diff
  
  
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


plot_scatter_limpio <- function(
    df,
    poll,
    c_col,
    m_col
) {
  
  # --------------------------------------------------------------------------
  # Seleccionar pares válidos
  # --------------------------------------------------------------------------
  
  datos <- data.frame(
    
    Cetam = df[[c_col]],
    
    MT = df[[m_col]]
    
  ) %>%
    
    filter(
      
      is.finite(Cetam),
      
      is.finite(MT)
      
    )
  
  
  # --------------------------------------------------------------------------
  # Verificar datos suficientes
  # --------------------------------------------------------------------------
  
  if (
    nrow(datos) < 2
  ) {
    
    return(
      plot_ly() %>%
        layout(
          title =
            paste(
              "Datos insuficientes -",
              poll
            )
        )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Regresión lineal
  # --------------------------------------------------------------------------
  
  fit <-
    lm(
      MT ~ Cetam,
      data = datos
    )
  
  
  pendiente <-
    coef(fit)[2]
  
  intercepto <-
    coef(fit)[1]
  
  
  # --------------------------------------------------------------------------
  # Pearson
  # --------------------------------------------------------------------------
  
  if (
    sd(datos$Cetam) == 0 ||
    sd(datos$MT) == 0
  ) {
    
    r <- NA_real_
    
  } else {
    
    r <-
      cor(
        datos$Cetam,
        datos$MT,
        method = "pearson"
      )
  }
  
  
  # --------------------------------------------------------------------------
  # R²
  # --------------------------------------------------------------------------
  
  r2 <-
    summary(fit)$r.squared
  
  
  # --------------------------------------------------------------------------
  # CCC de Lin
  # --------------------------------------------------------------------------
  
  mean_c <-
    mean(datos$Cetam)
  
  mean_m <-
    mean(datos$MT)
  
  var_c <-
    var(datos$Cetam)
  
  var_m <-
    var(datos$MT)
  
  cov_cm <-
    cov(
      datos$Cetam,
      datos$MT
    )
  
  denominador_ccc <-
    var_c +
    var_m +
    (mean_c - mean_m)^2
  
  if (
    denominador_ccc == 0
  ) {
    
    ccc <- NA_real_
    
  } else {
    
    ccc <-
      2 * cov_cm /
      denominador_ccc
  }
  
  
  # --------------------------------------------------------------------------
  # n
  # --------------------------------------------------------------------------
  
  n <-
    nrow(datos)
  
  
  # --------------------------------------------------------------------------
  # Ecuación
  # --------------------------------------------------------------------------
  
  ecuacion <- paste0(
    
    "y = ",
    
    round(
      pendiente,
      3
    ),
    
    "x ",
    
    ifelse(
      intercepto >= 0,
      "+ ",
      "- "
    ),
    
    round(
      abs(intercepto),
      3
    )
    
  )
  
  
  # --------------------------------------------------------------------------
  # Valores para regresión
  # --------------------------------------------------------------------------
  
  x_reg <- seq(
    
    min(
      datos$Cetam
    ),
    
    max(
      datos$Cetam
    ),
    
    length.out = 100
    
  )
  
  
  y_reg <-
    predict(
      
      fit,
      
      newdata =
        data.frame(
          Cetam = x_reg
        )
      
    )
  
  
  # --------------------------------------------------------------------------
  # Línea y = x
  # --------------------------------------------------------------------------
  
  limite <- max(
    
    datos$Cetam,
    
    datos$MT,
    
    na.rm = TRUE
    
  )
  
  
  # --------------------------------------------------------------------------
  # Gráfico
  # --------------------------------------------------------------------------
  
  p <- plot_ly() %>%
    
    add_markers(
      
      x = datos$Cetam,
      
      y = datos$MT,
      
      name = "Mediciones",
      
      marker = list(
        
        size = 5,
        
        opacity = 0.6
        
      ),
      
      hovertemplate = paste(
        
        "Cetam: %{x:.2f} µg/m³",
        
        "<br>MT: %{y:.2f} µg/m³",
        
        "<extra></extra>"
        
      )
      
    ) %>%
    
    add_lines(
      
      x = x_reg,
      
      y = y_reg,
      
      name = "Regresión OLS",
      
      line = list(
        
        width = 2
        
      )
      
    ) %>%
    
    add_lines(
      
      x = c(
        0,
        limite
      ),
      
      y = c(
        0,
        limite
      ),
      
      name = "y = x",
      
      line = list(
        
        dash = "dash",
        
        width = 2
        
      )
      
    ) %>%
    
    layout(
      
      title = list(
        
        text = paste(
          
          "Comparación Cetam vs MT -",
          
          poll
          
        )
        
      ),
      
      xaxis = list(
        
        title =
          "Cetam (µg/m³)",
        
        zeroline = FALSE
        
      ),
      
      yaxis = list(
        
        title =
          "MT (µg/m³)",
        
        zeroline = FALSE
        
      ),
      
      hovermode = "closest",
      
      annotations = list(
        
        list(
          
          x = 0.02,
          
          y = 0.98,
          
          xref = "paper",
          
          yref = "paper",
          
          xanchor = "left",
          
          yanchor = "top",
          
          text = paste0(
            
            "<b>Pearson r:</b> ",
            round(
              r,
              4
            ),
            
            "<br><b>R²:</b> ",
            round(
              r2,
              4
            ),
            
            "<br><b>CCC Lin:</b> ",
            round(
              ccc,
              4
            ),
            
            "<br><b>",
            ecuacion,
            "</b>",
            
            "<br><b>n:</b> ",
            n
            
          ),
          
          showarrow = FALSE,
          
          align = "left",
          
          bgcolor =
            "rgba(255,255,255,0.85)",
          
          bordercolor = "black",
          
          borderwidth = 1,
          
          borderpad = 6
          
        )
        
      )
      
    )
  
  
  return(p)
}


# ==============================================================================
# 15. PROBABILITY OF AGREEMENT
#
# Se calcula:
#
#       P(|MT - Cetam| <= tolerancia)
#
# Para cada tolerancia posible.
#
# Ejemplo:
#
#       Si P = 0.90 en tolerancia = 10,
#
#       significa que el 90% de los pares tienen una diferencia
#       absoluta menor o igual a 10 µg/m³.
# ==============================================================================


plot_probability_agreement <- function(
    df,
    poll,
    c_col,
    m_col
) {
  
  # --------------------------------------------------------------------------
  # Seleccionar pares válidos
  # --------------------------------------------------------------------------
  
  datos <- data.frame(
    
    Cetam = df[[c_col]],
    
    MT = df[[m_col]]
    
  ) %>%
    
    filter(
      
      is.finite(Cetam),
      
      is.finite(MT)
      
    )
  
  
  # --------------------------------------------------------------------------
  # Verificar datos suficientes
  # --------------------------------------------------------------------------
  
  if (
    nrow(datos) < 2
  ) {
    
    return(
      plot_ly() %>%
        layout(
          title =
            paste(
              "Datos insuficientes -",
              poll
            )
        )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Diferencia absoluta
  # --------------------------------------------------------------------------
  
  datos <- datos %>%
    
    mutate(
      
      diferencia =
        abs(
          MT - Cetam
        )
      
    )
  
  
  # --------------------------------------------------------------------------
  # Máxima tolerancia mostrada
  #
  # Se utiliza el percentil 99 para evitar que unos pocos valores
  # extremos compriman visualmente el gráfico.
  # --------------------------------------------------------------------------
  
  tolerancia_max <-
    as.numeric(
      quantile(
        datos$diferencia,
        0.99,
        na.rm = TRUE
      )
    )
  
  
  # Evitar un rango cero
  
  if (
    tolerancia_max <= 0 ||
    !is.finite(tolerancia_max)
  ) {
    
    tolerancia_max <- 1
    
  }
  
  
  # --------------------------------------------------------------------------
  # Tolerancias
  # --------------------------------------------------------------------------
  
  tolerancias <- seq(
    
    0,
    
    tolerancia_max,
    
    length.out = 200
    
  )
  
  
  # --------------------------------------------------------------------------
  # Probability of Agreement
  # --------------------------------------------------------------------------
  
  prob_agreement <- sapply(
    
    tolerancias,
    
    function(tol) {
      
      mean(
        datos$diferencia <= tol
      )
      
    }
    
  )
  
  
  datos_poa <- data.frame(
    
    tolerancia =
      tolerancias,
    
    probabilidad =
      prob_agreement
    
  )
  
  
  # --------------------------------------------------------------------------
  # Crear gráfico
  # --------------------------------------------------------------------------
  
  p <- plot_ly(
    
    datos_poa,
    
    x = ~tolerancia,
    
    y = ~probabilidad,
    
    type = "scatter",
    
    mode = "lines",
    
    line = list(
      
      width = 3
      
    ),
    
    name = "Probability of Agreement",
    
    hovertemplate = paste(
      
      "Tolerancia: %{x:.2f} µg/m³",
      
      "<br>Probabilidad: %{y:.2%}",
      
      "<extra></extra>"
      
    )
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # 50 %
  # ------------------------------------------------------------------------
  
  add_lines(
    
    x = c(
      0,
      tolerancia_max
    ),
    
    y = c(
      0.50,
      0.50
    ),
    
    name = "50 %",
    
    line = list(
      
      dash = "dash"
      
    ),
    
    hoverinfo = "skip"
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # 90 %
  # ------------------------------------------------------------------------
  
  add_lines(
    
    x = c(
      0,
      tolerancia_max
    ),
    
    y = c(
      0.90,
      0.90
    ),
    
    name = "90 %",
    
    line = list(
      
      dash = "dash"
      
    ),
    
    hoverinfo = "skip"
    
  ) %>%
    
    # ------------------------------------------------------------------------
  # 95 %
  # ------------------------------------------------------------------------
  
  add_lines(
    
    x = c(
      0,
      tolerancia_max
    ),
    
    y = c(
      0.95,
      0.95
    ),
    
    name = "95 %",
    
    line = list(
      
      dash = "dash"
      
    ),
    
    hoverinfo = "skip"
    
  ) %>%
    
    layout(
      
      title = list(
        
        text = paste(
          
          "Probability of Agreement -",
          
          poll
          
        )
        
      ),
      
      xaxis = list(
        
        title =
          "Tolerancia absoluta |MT - Cetam| (µg/m³)"
        
      ),
      
      yaxis = list(
        
        title =
          "Probabilidad de acuerdo",
        
        tickformat = ".0%",
        
        range =
          c(
            0,
            1
          )
        
      ),
      
      hovermode =
        "x unified"
      
    )
  
  
  return(p)
}


# ==============================================================================
# 16. FUNCIÓN PARA GUARDAR GRÁFICOS
#
# Evita repetir htmlwidgets::saveWidget() muchas veces.
# ==============================================================================


guardar_grafico <- function(
    grafico,
    nombre
) {
  
  htmlwidgets::saveWidget(
    
    grafico,
    
    file.path(
      output_dir,
      nombre
    ),
    
    selfcontained = TRUE
    
  )
}


# ==============================================================================
# 17. GENERAR GRÁFICOS DE DISPERSIÓN
# ==============================================================================


guardar_grafico(
  
  plot_scatter_limpio(
    
    df_merged,
    
    "PM1",
    
    "PM1_C",
    
    "PM1_M"
    
  ),
  
  "Scatter_PM1_Limpio.html"
  
)


guardar_grafico(
  
  plot_scatter_limpio(
    
    df_merged,
    
    "PM2.5",
    
    "PM25_C",
    
    "PM25_M"
    
  ),
  
  "Scatter_PM25_Limpio.html"
  
)


guardar_grafico(
  
  plot_scatter_limpio(
    
    df_merged,
    
    "PM10",
    
    "PM10_C",
    
    "PM10_M"
    
  ),
  
  "Scatter_PM10_Limpio.html"
  
)


# ==============================================================================
# 18. GENERAR SERIES TEMPORALES
# ==============================================================================


guardar_grafico(
  
  plot_ts(
    
    df_merged,
    
    "PM1",
    
    "PM1_C",
    
    "PM1_M"
    
  ),
  
  "TS_PM1_Limpio.html"
  
)


guardar_grafico(
  
  plot_ts(
    
    df_merged,
    
    "PM2.5",
    
    "PM25_C",
    
    "PM25_M"
    
  ),
  
  "TS_PM25_Limpio.html"
  
)


guardar_grafico(
  
  plot_ts(
    
    df_merged,
    
    "PM10",
    
    "PM10_C",
    
    "PM10_M"
    
  ),
  
  "TS_PM10_Limpio.html"
  
)


# ==============================================================================
# 19. GENERAR BLAND-ALTMAN
# ==============================================================================


guardar_grafico(
  
  plot_ba(
    
    df_merged,
    
    "PM1",
    
    "PM1_C",
    
    "PM1_M"
    
  ),
  
  "BlandAltman_PM1_Limpio.html"
  
)


guardar_grafico(
  
  plot_ba(
    
    df_merged,
    
    "PM2.5",
    
    "PM25_C",
    
    "PM25_M"
    
  ),
  
  "BlandAltman_PM25_Limpio.html"
  
)


guardar_grafico(
  
  plot_ba(
    
    df_merged,
    
    "PM10",
    
    "PM10_C",
    
    "PM10_M"
    
  ),
  
  "BlandAltman_PM10_Limpio.html"
  
)


# ==============================================================================
# 20. GENERAR PROBABILITY OF AGREEMENT
# ==============================================================================


guardar_grafico(
  
  plot_probability_agreement(
    
    df_merged,
    
    "PM1",
    
    "PM1_C",
    
    "PM1_M"
    
  ),
  
  "Probability_of_Agreement_PM1.html"
  
)


guardar_grafico(
  
  plot_probability_agreement(
    
    df_merged,
    
    "PM2.5",
    
    "PM25_C",
    
    "PM25_M"
    
  ),
  
  "Probability_of_Agreement_PM25.html"
  
)


guardar_grafico(
  
  plot_probability_agreement(
    
    df_merged,
    
    "PM10",
    
    "PM10_C",
    
    "PM10_M"
    
  ),
  
  "Probability_of_Agreement_PM10.html"
  
)


# ==============================================================================
# 21. FIN DEL SCRIPT
# ==============================================================================


cat(
  
  "\n============================================================\n",
  
  "ANÁLISIS GRIMM FINALIZADO\n",
  
  "============================================================\n",
  
  "Resultados guardados en:\n",
  
  output_dir,
  
  "\n\n",
  
  "Archivos generados:\n",
  
  "  - Metricas_Avanzadas_SinOutliers.csv\n",
  
  "  - Scatter_PM1_Limpio.html\n",
  
  "  - Scatter_PM25_Limpio.html\n",
  
  "  - Scatter_PM10_Limpio.html\n",
  
  "  - TS_PM1_Limpio.html\n",
  
  "  - TS_PM25_Limpio.html\n",
  
  "  - TS_PM10_Limpio.html\n",
  
  "  - BlandAltman_PM1_Limpio.html\n",
  
  "  - BlandAltman_PM25_Limpio.html\n",
  
  "  - BlandAltman_PM10_Limpio.html\n",
  
  "  - Probability_of_Agreement_PM1.html\n",
  
  "  - Probability_of_Agreement_PM25.html\n",
  
  "  - Probability_of_Agreement_PM10.html\n",
  
  "============================================================\n"
  
)