# =============================================================================
# GRIMM CETAM vs MT — EDA interactivo, normalidad, concordancia y boxplots
# Versión Optimizada: WebGL (scattergl), Max 15k puntos visuales, Diseño UI
# Dashboard Maestro Unificado (Pestañas HTML) + Series Temporales
# =============================================================================

pkgs <- c("tidyverse", "readxl", "lubridate", "plotly", "htmlwidgets",
          "htmltools", "here", "jsonlite", "scales", "rmarkdown") 
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!requireNamespace("robustbase", quietly = TRUE)) {
  install.packages("robustbase")
}

# Rutas
dir_grimm <- here::here("Analisis_GRIMM_V2")
dir_datos <- file.path(dir_grimm, "Datos")
dir_resultados <- file.path(dir_grimm, "Resultados", "Resultados Grimm-Cetam-MT")
dir_html <- file.path(dir_resultados, "HTML")
dir_tablas <- file.path(dir_resultados, "tablas")
dir.create(dir_html, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tablas, recursive = TRUE, showWarnings = FALSE)

file_cetam <- file.path(dir_datos, "GRIMM 11D Cetam 22052025.xlsx")
file_mt <- file.path(dir_datos, "GRIMM 11D MT 22052025.xlsx")
stopifnot(file.exists(file_cetam), file.exists(file_mt))

# =============================================================================
# CONFIGURACIÓN GENERAL Y OUTLIERS DE FILTRADO
# =============================================================================

METODO_OUTLIERS <- "IQR"
MULTIPLICADOR_OUTLIERS <- 5
CORTE_SUPERIOR <- 5000
TOLERANCIA_INICIAL <- 2
MAX_PUNTOS_VISUALES <- 15000 

mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

parse_grimm_datetime <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x))
  if (inherits(x, "Date")) return(as.POSIXct(x))
  if (is.numeric(x)) return(as.POSIXct(x * 86400, origin = "1899-12-30"))
  z <- suppressWarnings(lubridate::dmy_hms(as.character(x)))
  i <- is.na(z) & !is.na(x)
  if (any(i)) z[i] <- suppressWarnings(lubridate::ymd_hms(as.character(x[i])))
  z
}

remove_upper_outliers <- function(x, metodo = METODO_OUTLIERS, k = MULTIPLICADOR_OUTLIERS, corte = CORTE_SUPERIOR) {
  metodo <- match.arg(metodo, c("IQR", "CORTE", "NINGUNO"))
  if (metodo == "NINGUNO") return(x)
  ok <- is.finite(x)
  if (metodo == "IQR") {
    if (sum(ok) < 4) return(x)
    q <- quantile(x[ok], probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    limite <- q[2] + k * (q[2] - q[1])
    x[ok & x > limite] <- NA_real_
  } else if (metodo == "CORTE") {
    x[ok & x > corte] <- NA_real_
  }
  return(x)
}

outlier_method_label <- function() {
  if (METODO_OUTLIERS == "IQR") {
    paste0("Q3 + ", MULTIPLICADOR_OUTLIERS, " × IQR")
  } else if (METODO_OUTLIERS == "CORTE") {
    paste0("corte absoluto > ", CORTE_SUPERIOR, " µg/m³")
  } else {
    "sin eliminación de outliers"
  }
}

# =============================================================================
# LECTURA Y PROCESAMIENTO DE DATOS
# =============================================================================

load_grimm <- function(path, instrument) {
  raw <- readxl::read_excel(path, sheet = "Mass values", skip = 4) |>
    rename(datetime_raw = `date&time`,
           TSP  = `TSP [ug/m3]`,
           PM10 = `PM10 [ug/m3]`,
           PM4  = `PM4 [ug/m3]`,
           PM25 = `PM2,5 [ug/m3]`,
           PM1  = `PM1 [ug/m3]`) |>
    mutate(datetime = parse_grimm_datetime(datetime_raw),
           across(c(TSP, PM10, PM4, PM25, PM1), ~ suppressWarnings(as.numeric(.x)))) |>
    filter(!is.na(datetime), lubridate::year(datetime) == 2025) |>
    arrange(datetime)
  
  if (!nrow(raw)) stop("No se encontraron fechas de 2025 en: ", path)
  
  raw_minute <- raw |>
    mutate(datetime_1m = floor_date(datetime, "minute")) |>
    group_by(datetime_1m) |>
    summarise(TSP = mean_na(TSP), PM10 = mean_na(PM10), PM4 = mean_na(PM4), 
              PM25 = mean_na(PM25), PM1 = mean_na(PM1),
              n_registros = n(), .groups = "drop")
  
  fracs_cols <- c("TSP", "PM10", "PM4", "PM25", "PM1")
  n0 <- sapply(raw[fracs_cols], \(x) sum(is.finite(x)))
  clean <- raw |> mutate(across(all_of(fracs_cols), remove_upper_outliers))
  n1 <- sapply(clean[fracs_cols], \(x) sum(is.finite(x)))
  
  out <- tibble(instrumento = instrument, 
                fraccion = c("TSP", "PM10", "PM4", "PM2.5", "PM1"),
                n_original = as.integer(n0[fracs_cols]),
                n_excluido = as.integer(n0[fracs_cols] - n1[fracs_cols])) |>
    mutate(pct_excluido = if_else(n_original > 0, 100 * n_excluido / n_original, NA_real_))
  
  clean_minute <- clean |>
    mutate(datetime_1m = floor_date(datetime, "minute")) |>
    group_by(datetime_1m) |>
    summarise(TSP = mean_na(TSP), PM10 = mean_na(PM10), PM4 = mean_na(PM4), 
              PM25 = mean_na(PM25), PM1 = mean_na(PM1),
              n_registros = n(), .groups = "drop")
  
  list(data = clean_minute, raw_data = raw_minute, outliers = out)
}

rc <- load_grimm(file_cetam, "Cetam")
rm <- load_grimm(file_mt, "MT")

dc <- rc$data |> rename(TSP_C = TSP, PM10_C = PM10, PM4_C = PM4, PM25_C = PM25, PM1_C = PM1, n_Cetam = n_registros)
dm <- rm$data |> rename(TSP_M = TSP, PM10_M = PM10, PM4_M = PM4, PM25_M = PM25, PM1_M = PM1, n_MT = n_registros)
merged <- full_join(dc, dm, by = "datetime_1m") |> rename(datetime = datetime_1m) |> arrange(datetime)

dcr <- rc$raw_data |> rename(TSP_C = TSP, PM10_C = PM10, PM4_C = PM4, PM25_C = PM25, PM1_C = PM1, n_Cetam = n_registros)
dmr <- rm$raw_data |> rename(TSP_M = TSP, PM10_M = PM10, PM4_M = PM4, PM25_M = PM25, PM1_M = PM1, n_MT = n_registros)
merged_raw <- full_join(dcr, dmr, by = "datetime_1m") |> rename(datetime = datetime_1m) |> arrange(datetime)

outliers <- bind_rows(rc$outliers, rm$outliers)
write_csv(outliers, file.path(dir_tablas, "00_outliers_excluidos.csv"))
saveRDS(merged, file.path(dir_resultados, "datos_minuto.rds"))

cols <- list("TSP"   = c("TSP_C", "TSP_M"),
             "PM10"  = c("PM10_C", "PM10_M"),
             "PM4"   = c("PM4_C", "PM4_M"),
             "PM2.5" = c("PM25_C", "PM25_M"),
             "PM1"   = c("PM1_C", "PM1_M"))

pairs_for <- function(fr) {
  cc <- cols[[fr]]
  tibble(datetime = merged$datetime, Cetam = merged[[cc[1]]], MT = merged[[cc[2]]]) |>
    filter(is.finite(Cetam), is.finite(MT)) |>
    mutate(promedio = (Cetam + MT)/2, diferencia = MT - Cetam, abs_diferencia = abs(diferencia))
}

# =============================================================================
# WIDGET INTERACTIVO DE BOXPLOTS CON SLIDERS Y REGLA DE BIGOTES
# =============================================================================

calc_medcouple_subsample <- function(x, max_n = 2000, seed = 123) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(0)
  if (length(x) > max_n) {
    set.seed(seed)
    x <- sample(x, max_n)
  }
  if (requireNamespace("robustbase", quietly = TRUE)) {
    return(robustbase::mc(x))
  } else {
    med <- median(x)
    z <- x - med
    z_pos <- z[z >= 0]; z_neg <- z[z <= 0]
    h_vals <- outer(z_pos, z_neg, function(a, b) ifelse((a - b) == 0, 0, (a + b) / (a - b)))
    return(median(h_vals))
  }
}

widget_boxplots_interactivos <- function(fr) {
  cc <- cols[[fr]]
  c_vals <- merged_raw[[cc[1]]][is.finite(merged_raw[[cc[1]]])]
  m_vals <- merged_raw[[cc[2]]][is.finite(merged_raw[[cc[2]]])]
  
  mc_c <- calc_medcouple_subsample(c_vals)
  mc_m <- calc_medcouple_subsample(m_vals)
  
  prefix <- paste0("bp_", gsub("[^A-Za-z0-9]", "", fr))
  slider_qinf_id <- paste0(prefix, "_qinf")
  slider_qsup_id <- paste0(prefix, "_qsup")
  val_qinf_id <- paste0(prefix, "_val_qinf")
  val_qsup_id <- paste0(prefix, "_val_qsup")
  select_rule_id <- paste0(prefix, "_rule")
  info_box_id <- paste0(prefix, "_info")
  
  df_c <- tibble(Valor = c_vals, Instrumento = "Cetam")
  df_m <- tibble(Valor = m_vals, Instrumento = "MT")
  df_comb <- bind_rows(df_c, df_m)
  
  p <- plot_ly(df_comb, x = ~Instrumento, y = ~Valor, color = ~Instrumento,
               colors = c(Cetam = "#3b82f6", MT = "#f97316"),
               type = "box", boxmean = TRUE, marker = list(size = 3, opacity = 0.4)) |>
    layout(
      yaxis = list(title = "Concentración Cruda [µg/m³]"),
      xaxis = list(title = ""),
      showlegend = FALSE,
      margin = list(t = 10)
    )
  
  js_code <- sprintf(
    "function(el, x){
      var c_raw = %s, m_raw = %s;
      var mc_c = %f, mc_m = %f;
      
      var s_inf = document.getElementById('%s');
      var s_sup = document.getElementById('%s');
      var v_inf = document.getElementById('%s');
      var v_sup = document.getElementById('%s');
      var sel_rule = document.getElementById('%s');
      var info = document.getElementById('%s');
      
      function quantile(arr, q) {
        var sorted = arr.slice().sort(function(a,b){return a-b;});
        var pos = (sorted.length - 1) * q;
        var base = Math.floor(pos);
        var rest = pos - base;
        if (sorted[base+1] !== undefined) {
          return sorted[base] + rest * (sorted[base+1] - sorted[base]);
        } else {
          return sorted[base];
        }
      }

      function updateBoxplots(){
        var qinf_p = parseFloat(s_inf.value);
        var qsup_p = parseFloat(s_sup.value);
        var rule = sel_rule.value;
        
        v_inf.textContent = (qinf_p * 100).toFixed(1) + '%%';
        v_sup.textContent = (qsup_p * 100).toFixed(1) + '%%';
        
        var qinf_c = quantile(c_raw, qinf_p), qsup_c = quantile(c_raw, qsup_p);
        var qinf_m = quantile(m_raw, qinf_p), qsup_m = quantile(m_raw, qsup_p);
        
        var c_filt = c_raw.filter(function(v){ return v >= qinf_c && v <= qsup_c; });
        var m_filt = m_raw.filter(function(v){ return v >= qinf_m && v <= qsup_m; });
        
        function getOutliers(data, mc) {
          if(data.length < 4) return {tukey:0, hv:0};
          var q25 = quantile(data, 0.25), q75 = quantile(data, 0.75);
          var iqr = q75 - q25;
          var t_low = q25 - 1.5*iqr, t_high = q75 + 1.5*iqr;
          var hv_low = mc >= 0 ? q25 - 1.5*Math.exp(-4*mc)*iqr : q25 - 1.5*Math.exp(-3*mc)*iqr;
          var hv_high = mc >= 0 ? q75 + 1.5*Math.exp(3*mc)*iqr : q75 + 1.5*Math.exp(4*mc)*iqr;
          
          var n_t = data.filter(function(v){ return v < t_low || v > t_high; }).length;
          var n_hv = data.filter(function(v){ return v < hv_low || v > hv_high; }).length;
          return {tukey: n_t, hv: n_hv};
        }
        
        var c_out = getOutliers(c_filt, mc_c);
        var m_out = getOutliers(m_filt, mc_m);
        
        Plotly.restyle(el, {y: [c_filt, m_filt]});
        
        info.innerHTML = '<div style=\"display:flex; justify-content:space-between; flex-wrap:wrap; gap:15px;\">' +
          '<div><b>Eliminados (cuantiles):</b><br>• Cetam: ' + (c_raw.length - c_filt.length) + '<br>• MT: ' + (m_raw.length - m_filt.length) + '</div>' +
          '<div><b>Outliers Cetam:</b><br>Tukey: ' + (rule==='tukey'?'<b>'+c_out.tukey+'</b>':c_out.tukey) + '<br>H-V: ' + (rule==='hv'?'<b>'+c_out.hv+'</b>':c_out.hv) + '</div>' +
          '<div><b>Outliers MT:</b><br>Tukey: ' + (rule==='tukey'?'<b>'+m_out.tukey+'</b>':m_out.tukey) + '<br>H-V: ' + (rule==='hv'?'<b>'+m_out.hv+'</b>':m_out.hv) + '</div></div>';
      }
      
      s_inf.addEventListener('input', updateBoxplots);
      s_sup.addEventListener('input', updateBoxplots);
      sel_rule.addEventListener('change', updateBoxplots);
      updateBoxplots();
    }",
    jsonlite::toJSON(c_vals), jsonlite::toJSON(m_vals),
    mc_c, mc_m,
    slider_qinf_id, slider_qsup_id, val_qinf_id, val_qsup_id, select_rule_id, info_box_id
  )
  
  p <- htmlwidgets::onRender(p, js_code)
  
  ctrl_html <- tagList(
    tags$div(class = "control-panel",
             tags$div(class = "control-group",
                      tags$label(`for` = slider_qinf_id, "Recorte Inferior: "),
                      tags$input(id = slider_qinf_id, type = "range", min = 0, max = 0.10, step = 0.005, value = 0),
                      tags$span(id = val_qinf_id, class = "fw-bold", "0.0%")
             ),
             tags$div(class = "control-group",
                      tags$label(`for` = slider_qsup_id, "Recorte Superior: "),
                      tags$input(id = slider_qsup_id, type = "range", min = 0.90, max = 1.00, step = 0.005, value = 1.00),
                      tags$span(id = val_qsup_id, class = "fw-bold", "100.0%")
             ),
             tags$div(class = "control-group",
                      tags$label(`for` = select_rule_id, "Regla de Asimetría: "),
                      tags$select(id = select_rule_id, class = "ui-select",
                                  tags$option(value = "hv", "Hubert-Vandervieren (Medcouple)"),
                                  tags$option(value = "tukey", "Tukey Estándar"))
             )
    ),
    tags$div(id = info_box_id, class = "info-panel")
  )
  
  tagList(ctrl_html, p)
}

# =============================================================================
# TABLA DE ESTADÍSTICOS DESCRIPTIVOS
# =============================================================================

calc_stats_vec <- function(x) {
  x <- x[is.finite(x)]
  n_obs <- length(x)
  if (n_obs == 0) return(rep(NA_real_, 9))
  q <- quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  c(n = n_obs, media = mean(x, na.rm = TRUE), mediana = unname(q[2]),
    sd = sd(x, na.rm = TRUE), iqr = unname(q[3] - q[1]), min = min(x, na.rm = TRUE),
    q25 = unname(q[1]), q75 = unname(q[3]), max = max(x, na.rm = TRUE))
}

summary_stats_html <- function(d) {
  s_c <- calc_stats_vec(d$Cetam)
  s_m <- calc_stats_vec(d$MT)
  s_d <- calc_stats_vec(d$diferencia)
  
  df_summary <- tibble(
    Variable = c("Cetam", "MT", "Diferencia (MT − Cetam)"),
    `N obs` = c(s_c["n"], s_m["n"], s_d["n"]),
    `Media` = sprintf("%.3f", c(s_c["media"], s_m["media"], s_d["media"])),
    `Mediana (Q2)` = sprintf("%.3f", c(s_c["mediana"], s_m["mediana"], s_d["mediana"])),
    `Desv. Est. (SD)` = sprintf("%.3f", c(s_c["sd"], s_m["sd"], s_d["sd"])),
    `Rango Interc. (IQR)` = sprintf("%.3f", c(s_c["iqr"], s_m["iqr"], s_d["iqr"])),
    `Mínimo` = sprintf("%.3f", c(s_c["min"], s_m["min"], s_d["min"])),
    `Cuantil 25% (Q1)` = sprintf("%.3f", c(s_c["q25"], s_m["q25"], s_d["q25"])),
    `Cuantil 75% (Q3)` = sprintf("%.3f", c(s_c["q75"], s_m["q75"], s_d["q75"])),
    `Máximo` = sprintf("%.3f", c(s_c["max"], s_m["max"], s_d["max"]))
  )
  
  rows_html <- apply(df_summary, 1, function(row) {
    paste0(
      "<tr>",
      "<td class='fw-bold'>", row[["Variable"]], "</td>",
      "<td>", row[["N obs"]], "</td>",
      "<td>", row[["Media"]], "</td>",
      "<td>", row[["Mediana (Q2)"]], "</td>",
      "<td>", row[["Desv. Est. (SD)"]], "</td>",
      "<td>", row[["Rango Interc. (IQR)"]], "</td>",
      "<td>", row[["Mínimo"]], "</td>",
      "<td>", row[["Cuantil 25% (Q1)"]], "</td>",
      "<td>", row[["Cuantil 75% (Q3)"]], "</td>",
      "<td>", row[["Máximo"]], "</td>",
      "</tr>"
    )
  })
  
  paste0(
    "<div class='table-responsive'>",
    "<table>",
    "<thead><tr>",
    "<th>Variable</th>",
    "<th>N</th>",
    "<th>Media</th>",
    "<th>Mediana</th>",
    "<th>SD</th>",
    "<th>IQR</th>",
    "<th>Mínimo</th>",
    "<th>Q1 (25%)</th>",
    "<th>Q3 (75%)</th>",
    "<th>Máximo</th>",
    "</tr></thead><tbody>",
    paste0(rows_html, collapse = ""),
    "</tbody></table>",
    "</div>"
  )
}

# =============================================================================
# MÉTRICAS Y GRÁFICOS (WEBGL & MUESTREO VISUAL)
# =============================================================================

metrics_for <- function(d, fr) {
  n <- nrow(d)
  if (n < 2) return(tibble(fraccion = fr, n = n))
  den <- var(d$Cetam) + var(d$MT) + (mean(d$Cetam) - mean(d$MT))^2
  fit <- if (sd(d$Cetam) > 0) lm(MT ~ Cetam, d) else NULL
  bias <- mean(d$diferencia); sdd <- sd(d$diferencia)
  
  tibble(fraccion = fr, n = n, 
         media_Cetam = mean(d$Cetam), media_MT = mean(d$MT),
         Pearson = if(sd(d$Cetam) > 0 && sd(d$MT) > 0) cor(d$Cetam, d$MT) else NA_real_,
         R2_OLS = if(!is.null(fit)) summary(fit)$r.squared else NA_real_,
         CCC_Lin = if(den > 0) 2 * cov(d$Cetam, d$MT) / den else NA_real_,
         sesgo_MT_menos_Cetam = bias, 
         LoA_inferior = bias - 1.96 * sdd,
         LoA_superior = bias + 1.96 * sdd, 
         MAE = mean(abs(d$diferencia)),
         RMSE = sqrt(mean(d$diferencia^2)),
         beta1 = if(!is.null(fit)) coef(fit)[2] else NA_real_, 
         beta0 = if(!is.null(fit)) coef(fit)[1] else NA_real_)
}

metrics <- bind_rows(lapply(names(cols), \(fr) metrics_for(pairs_for(fr), fr)))
write_csv(metrics, file.path(dir_tablas, "01_metricas_concordancia.csv"))

plot_ba <- function(d) {
  if(nrow(d) < 2) return(plot_ly() |> layout(title = "Datos insuficientes"))
  
  bias <- mean(d$diferencia)
  s <- sd(d$diferencia)
  xr <- range(d$promedio)
  
  # Muestreo visual para WebGL
  d_plot <- if (nrow(d) > MAX_PUNTOS_VISUALES) d[sample(nrow(d), MAX_PUNTOS_VISUALES), ] else d
  
  plot_ly(d_plot, x = ~promedio, y = ~diferencia, type = "scattergl", mode = "markers",
          hovertemplate = "Promedio: %{x:.2f}<br>Diferencia: %{y:.2f}<extra></extra>",
          marker = list(size = 4, opacity = 0.5, color = "#64748b")) |>
    add_segments(x = xr[1], xend = xr[2], y = bias, yend = bias, name = "Sesgo medio", line = list(color = "#dc2626", width = 2)) |>
    add_segments(x = xr[1], xend = xr[2], y = bias + 1.96*s, yend = bias + 1.96*s, name = "Lím. Sup. 95%", line = list(color = "#3b82f6", dash = "dash")) |>
    add_segments(x = xr[1], xend = xr[2], y = bias - 1.96*s, yend = bias - 1.96*s, name = "Lím. Inf. 95%", line = list(color = "#3b82f6", dash = "dash")) |>
    layout(xaxis = list(title = "Promedio (Cetam + MT)/2 [µg/m³]"), 
           yaxis = list(title = "Diferencia MT − Cetam [µg/m³]"), 
           showlegend = TRUE, margin = list(t = 10))
}

plot_scatter <- function(d, fr) {
  if (nrow(d) < 2) return(plot_ly() |> layout(title = "Datos insuficientes"))
  
  m <- metrics_for(d, fr)
  lim <- max(c(d$Cetam, d$MT), na.rm = TRUE)
  
  # Muestreo visual (WebGL)
  d_plot <- if (nrow(d) > MAX_PUNTOS_VISUALES) d[sample(nrow(d), MAX_PUNTOS_VISUALES), ] else d
  
  # Línea OLS calculada con TODA la base
  xreg <- c(0, max(d$Cetam))
  yreg <- m$beta0 + m$beta1 * xreg
  
  note <- paste0(
    "<b>N Total:</b> ", scales::comma(nrow(d)), "<br>",
    "<b>R² OLS:</b> ", signif(m$R2_OLS, 4), "<br>",
    "<b>CCC Lin:</b> ", signif(m$CCC_Lin, 4), "<br>",
    "<b>MAE:</b> ", signif(m$MAE, 4), "<br>",
    "<b>RMSE:</b> ", signif(m$RMSE, 4)
  )
  
  if(nrow(d) > MAX_PUNTOS_VISUALES) {
    note <- paste0(note, "<br><br><i>(Visualizando ", scales::comma(MAX_PUNTOS_VISUALES), " pts)</i>")
  }
  
  plot_ly(d_plot, x = ~Cetam, y = ~MT, type = "scattergl", mode = "markers",
          hovertemplate = "Cetam: %{x:.2f}<br>MT: %{y:.2f}<extra></extra>",
          marker = list(size = 4, opacity = 0.5, color = "#64748b")) |>
    add_lines(x = xreg, y = yreg, name = "Regresión OLS", line = list(color = "#dc2626", width = 2), hoverinfo = "skip") |>
    add_lines(x = c(0, lim), y = c(0, lim), name = "Identidad y=x", line = list(color = "#1e293b", dash = "dash"), hoverinfo = "skip") |>
    layout(xaxis = list(title = "Cetam [µg/m³]", rangemode = "tozero"), 
           yaxis = list(title = "MT [µg/m³]", rangemode = "tozero"),
           showlegend = FALSE, margin = list(t = 10),
           annotations = list(list(
             x = 0.05, y = 0.95, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top",
             text = note, showarrow = FALSE, align = "left", 
             bgcolor = "rgba(255,255,255,0.9)", bordercolor = "#cbd5e1", borderwidth = 1, borderpad = 8
           )))
}

plot_poa <- function(d, fr) {
  if(nrow(d) < 2) return(plot_ly() |> layout(title = "Datos insuficientes"))
  dd <- d$abs_diferencia
  xmax <- max(TOLERANCIA_INICIAL, as.numeric(quantile(dd, .99, na.rm = TRUE)), 1)
  
  # Resolución suave para WebGL
  ts <- seq(0, xmax, length.out = 150) 
  ps <- vapply(ts, function(t) mean(dd <= t), numeric(1))
  tol0 <- min(TOLERANCIA_INICIAL, xmax)
  
  p <- plot_ly(x = ts, y = ps, type = "scatter", mode = "lines", name = "PoA",
               line = list(color = "#3b82f6", width = 3),
               hovertemplate = "Tolerancia: ±%{x:.2f} µg/m³<br>PoA: %{y:.2%}<extra></extra>") |>
    layout(xaxis = list(title = "Tolerancia absoluta |MT − Cetam| [µg/m³]", range = c(0, xmax)),
           yaxis = list(title = "Probabilidad empírica", tickformat = ".0%", range = c(0, 1)),
           margin = list(t = 10),
           shapes = list(list(type = "line", xref = "x", yref = "paper", x0 = tol0, x1 = tol0, y0 = 0, y1 = 1,
                              line = list(color = "#dc2626", width = 2, dash = "dash"))))
  
  sid <- paste0("tol_", gsub("[^A-Za-z0-9]", "", fr))
  vid <- paste0(sid, "_value")
  
  # Inyección JavaScript para el slider
  p <- htmlwidgets::onRender(p, sprintf(
    "function(el,x){
      var s=document.getElementById('%s'), v=document.getElementById('%s');
      if(!s) return;
      var ts=%s, ps=%s;
      function upd(){
        var t=+s.value, i=0;
        while(i+1<ts.length && ts[i+1]<=t) i++;
        Plotly.relayout(el,{'shapes[0].x0':t, 'shapes[0].x1':t});
        v.textContent='±'+t.toFixed(2)+' µg/m³; PoA = '+(100*ps[i]).toFixed(2)+'%%';
      }
      s.addEventListener('input', upd);
      upd();
    }", sid, vid, jsonlite::toJSON(ts, auto_unbox = TRUE), jsonlite::toJSON(ps, auto_unbox = TRUE)))
  
  # Guardar el HTML del slider como atributo
  attr(p, "slider") <- tagList(
    tags$div(class = "control-panel", style = "margin-bottom: 10px; padding: 12px;",
             tags$div(class = "control-group",
                      tags$label(`for` = sid, "Tolerancia elegida: "),
                      tags$input(id = sid, type = "range", min = 0, max = xmax, step = max(xmax/300, 0.01), value = tol0),
                      tags$span(id = vid, class = "fw-bold", style = "color: #0369a1; margin-left: 10px;")
             )
    )
  )
  return(p)
}

# =============================================================================
# GENERACIÓN DE DASHBOARDS HTML (DISEÑO UI PREMIUM + RMARKDOWN AUTOCONTENIDO)
# =============================================================================

css_styles <- HTML("
  :root { --primary: #2563eb; --bg: #f8fafc; --card-bg: #ffffff; --text: #0f172a; --border: #e2e8f0; }
  body { font-family: 'Inter', system-ui, -apple-system, sans-serif; background: var(--bg); color: var(--text); padding: 40px 20px; line-height: 1.5; }
  .dashboard-container { max-width: 1400px; margin: 0 auto; }
  
  .header { padding-bottom: 24px; margin-bottom: 32px; border-bottom: 2px solid var(--border); }
  .header h1, .header h2 { color: var(--text); font-size: 2.2rem; font-weight: 800; margin: 0 0 8px 0; letter-spacing: -0.5px; }
  .header p { color: #64748b; margin: 0; font-size: 1.1rem; line-height: 1.6; }
  
  .card { background: var(--card-bg); border-radius: 16px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05), 0 10px 15px -3px rgba(0,0,0,0.05); padding: 24px; margin-bottom: 24px; border: 1px solid var(--border); }
  .card-title { font-size: 1.25rem; font-weight: 700; color: var(--text); margin-bottom: 20px; border-bottom: 1px solid var(--border); padding-bottom: 12px; }
  
  .grid-2 { display: grid; grid-template-columns: minmax(0, 1fr); gap: 24px; }
  @media(min-width: 1024px) { .grid-2 { grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); } }
  .grid-2 > .card { min-width: 0; overflow: hidden; }
  .card .html-widget, .card .plotly, .card .js-plotly-plot { max-width: 100%; }
  
  .table-responsive { overflow-x: auto; }
  table { width: 100%; border-collapse: collapse; font-size: 14px; text-align: left; }
  th { background: #f8fafc; padding: 14px 16px; font-weight: 600; border-bottom: 2px solid var(--border); color: #475569; }
  td { padding: 14px 16px; border-bottom: 1px solid var(--border); }
  tr:hover { background: #f1f5f9; }
  .fw-bold { font-weight: 600; }
  
  .control-panel { background: #f8fafc; padding: 16px 20px; border-radius: 12px; margin-bottom: 20px; border: 1px solid #e2e8f0; display: flex; flex-wrap: wrap; gap: 24px; align-items: center; }
  .control-group { display: flex; align-items: center; gap: 12px; font-weight: 500; color: #475569; font-size: 14px; }
  .ui-select { padding: 8px 16px; border-radius: 8px; border: 1px solid #cbd5e1; background: white; outline: none; font-weight: 500; cursor: pointer; }
  .info-panel { font-size: 14px; background: #eff6ff; padding: 16px; border-radius: 12px; border: 1px solid #bfdbfe; color: #1e3a8a; line-height: 1.6; }
  
  /* Pestañas UI Modernas (Estilo iOS / Segmented Control) */
  .tab-container { display: inline-flex; background: #e2e8f0; padding: 6px; border-radius: 12px; gap: 4px; margin-bottom: 32px; }
  .tab-btn { background: transparent; color: #64748b; border: none; padding: 10px 28px; font-size: 1.05rem; font-weight: 600; border-radius: 8px; cursor: pointer; transition: all 0.2s ease; }
  .tab-btn:hover { color: #0f172a; }
  .tab-btn.active { background: #ffffff; color: var(--primary); box-shadow: 0 2px 4px rgba(0,0,0,0.06); }
  .tab-content { display: none; animation: fadeIn 0.4s cubic-bezier(0.4, 0, 0.2, 1); }
  .tab-content.active { display: block; }
  @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
")

js_tabs <- HTML("
  <script>
    function resizeTabPlots(tabId) {
      var tab = document.getElementById(tabId);
      if (!tab || typeof Plotly === 'undefined') return;
      tab.querySelectorAll('.js-plotly-plot').forEach(function(gd) {
        Plotly.relayout(gd, {autosize: true});
        Plotly.Plots.resize(gd);
      });
    }

    function openTab(evt, tabId) {
      var contents = document.getElementsByClassName('tab-content');
      for (var i = 0; i < contents.length; i++) {
        contents[i].style.display = 'none';
        contents[i].classList.remove('active');
      }
      var links = document.getElementsByClassName('tab-btn');
      for (var i = 0; i < links.length; i++) {
        links[i].classList.remove('active');
      }
      var current = document.getElementById(tabId);
      current.style.display = 'block';
      current.classList.add('active');
      evt.currentTarget.classList.add('active');

      // Redimensionar los gráficos de la pestaña recién visible
      requestAnimationFrame(function() {
        resizeTabPlots(tabId);
        setTimeout(function() { resizeTabPlots(tabId); }, 100);
      });
    }
  </script>
")

# 1. Crear los botones de las pestañas
tabs_buttons <- tags$div(class = "tab-container",
                         lapply(names(cols), function(fr) {
                           tags$button(class = paste("tab-btn", if(fr == "TSP") "active" else ""),
                                       onclick = sprintf("openTab(event, 'tab_%s')", gsub("\\.", "", fr)),
                                       fr)
                         })
)

# 2. Generar el contenido (plots y tablas) para cada fracción
cat("\nGenerando pestañas para Dashboard Maestro...\n")
tab_contents <- lapply(names(cols), function(fr) {
  d <- pairs_for(fr)
  
  z <- outliers |> filter(fraccion == fr)
  n_c <- if(length(z$n_excluido[z$instrumento == "Cetam"])) z$n_excluido[z$instrumento == "Cetam"] else 0
  pct_c <- if(length(z$pct_excluido[z$instrumento == "Cetam"])) z$pct_excluido[z$instrumento == "Cetam"] else 0
  n_m <- if(length(z$n_excluido[z$instrumento == "MT"])) z$n_excluido[z$instrumento == "MT"] else 0
  pct_m <- if(length(z$pct_excluido[z$instrumento == "MT"])) z$pct_excluido[z$instrumento == "MT"] else 0
  
  poa_plot <- plot_poa(d, fr)
  poa_slider <- attr(poa_plot, "slider")
  
  tags$div(id = paste0("tab_", gsub("\\.", "", fr)), 
           class = paste("tab-content", if(fr == "TSP") "active" else ""),
           style = if(fr != "TSP") "display:none;" else "",
           
           tags$div(class = "header",
                    tags$h2(paste("Análisis:", fr)),
                    tags$p(HTML(paste0(
                      "Cetam vs MT • Límite outliers: ", outlier_method_label(), " • Pares Completos: ", scales::comma(nrow(d)), "<br>",
                      "<span style='color:#dc2626;'><b>Excluidos:</b> Cetam = ", n_c, " (", sprintf("%.2f", pct_c), "%), MT = ", n_m, " (", sprintf("%.2f", pct_m), "%)</span>"
                    )))
           ),
           
           tags$div(class = "grid-2",
                    tags$div(class = "card",
                             tags$div(class = "card-title", "Estadísticas Descriptivas"),
                             HTML(summary_stats_html(d))
                    ),
                    tags$div(class = "card",
                             tags$div(class = "card-title", "Boxplots y Asimetría"),
                             widget_boxplots_interactivos(fr)
                    )
           ),
           
           tags$div(class = "grid-2",
                    tags$div(class = "card",
                             tags$div(class = "card-title", "Bland-Altman"),
                             plot_ba(d)
                    ),
                    tags$div(class = "card",
                             tags$div(class = "card-title", "Dispersión OLS"),
                             plot_scatter(d, fr)
                    )
           ),
           
           tags$div(class = "card",
                    tags$div(class = "card-title", "Probability of Agreement (PoA)"),
                    poa_slider,
                    poa_plot
           )
  )
})

# 3. Ensamblar la UI principal
ui_maestro <- tagList(
  tags$style(css_styles),
  js_tabs,
  tags$div(class = "dashboard-container",
           tabs_buttons,
           tagList(tab_contents)
  )
)

# 4. Exportar el Dashboard Maestro
archivo_dashboard <- file.path(dir_html, "Dashboard_Maestro_Concordancia.html")
temp_rds <- file.path(dir_html, "temp_maestro.rds")
rmd_file <- file.path(dir_html, "temp_maestro.Rmd")

saveRDS(ui_maestro, temp_rds)
writeLines(c(
  "---", 
  "title: 'Dashboard Maestro GRIMM'",
  "output:",
  "  html_document:",
  "    self_contained: true",
  "    theme: null",
  "    mathjax: null",
  "params:",
  "  rds_path: ''", 
  "---",
  "```{r, echo=FALSE, warning=FALSE, message=FALSE}",
  "library(htmltools); library(plotly); readRDS(params$rds_path)", 
  "```"
), rmd_file)
rmarkdown::render(rmd_file, output_file = archivo_dashboard, output_dir = dir_html, params = list(rds_path = temp_rds), quiet = TRUE)
unlink(rmd_file); unlink(temp_rds)

cat("Dashboard Maestro generado con éxito.\n")


# =============================================================================
# DASHBOARDS DE SERIES TEMPORALES (Envueltos en diseño UI)
# =============================================================================


exportar_serie_temporal_ui <- function(d_limpia, d_cruda, tipo, filename) {
  
  d_plot <- if(tipo == "limpia") d_limpia else d_cruda
  
  fracciones <- c("TSP", "PM10", "PM4", "PM2.5", "PM1")
  cols_c <- c("TSP_C", "PM10_C", "PM4_C", "PM25_C", "PM1_C")
  cols_m <- c("TSP_M", "PM10_M", "PM4_M", "PM25_M", "PM1_M")
  
  p <- plot_ly(d_plot, x = ~datetime)
  
  for(i in 1:length(fracciones)) {
    vis <- if(i == 1) TRUE else FALSE
    p <- p |> 
      add_trace(
        y = as.formula(paste0("~", cols_c[i])), 
        name = paste("Cetam", fracciones[i]), 
        type = "scatter", mode = "lines", visible = vis,
        line = list(color = "#3b82f6", width = 1.5)
      ) |>
      add_trace(
        y = as.formula(paste0("~", cols_m[i])), 
        name = paste("MT", fracciones[i]), 
        type = "scatter", mode = "lines", visible = vis,
        line = list(color = "#f97316", width = 1.5)
      )
  }
  
  # Botones originales (sin cambios)
  botones <- lapply(1:length(fracciones), function(i) {
    vis_array <- rep(FALSE, length(fracciones) * 2)
    vis_array[c(i*2 - 1, i*2)] <- TRUE 
    list(method = "restyle", args = list("visible", vis_array), label = fracciones[i])
  })
  
  p <- p |> layout(
    autosize = TRUE,
    height = NULL,
    xaxis = list(
      title = "Fecha",
      type = "date",
      rangeslider = list(visible = TRUE),
      rangeselector = list(
        buttons = list(
          list(count = 1, label = "1 día", step = "day", stepmode = "backward"),
          list(count = 7, label = "1 semana", step = "day", stepmode = "backward"),
          list(step = "all", label = "Todo")
        )
      )
    ),
    yaxis = list(title = "Concentración [µg/m³]", automargin = TRUE),
    hovermode = "x unified",
    margin = list(l = 75, r = 30, t = 90, b = 65),
    updatemenus = list(
      list(
        type = "buttons", direction = "right",
        x = 0, y = 1.15, xanchor = "left", yanchor = "bottom",
        pad = list(r = 10, t = 10), showactive = TRUE,
        buttons = botones,
        bgcolor = "#ffffff", bordercolor = "#cbd5e1",
        font = list(color = "#0f172a")
      )
    )
  ) |> config(responsive = TRUE)
  
  # ---------------------------------------------------------------------------
  # INFO DE OUTLIERS POR FRACCIÓN (se actualiza al cambiar de botón)
  # ---------------------------------------------------------------------------
  info_list <- lapply(fracciones, function(fr) {
    z <- outliers |> filter(fraccion == fr)
    g <- function(inst, col) {
      v <- z[[col]][z$instrumento == inst]
      if (length(v)) as.numeric(v[1]) else NA_real_
    }
    list(c_n = g("Cetam", "n_excluido"), c_pct = g("Cetam", "pct_excluido"), c_tot = g("Cetam", "n_original"),
         m_n = g("MT", "n_excluido"),    m_pct = g("MT", "pct_excluido"),    m_tot = g("MT", "n_original"))
  })
  names(info_list) <- fracciones
  
  js_info <- paste0(
    "function(el, x){
      var info = ", jsonlite::toJSON(info_list, auto_unbox = TRUE, na = "null"), ";
      var tipo = '", tipo, "';
      var crit = ", jsonlite::toJSON(outlier_method_label(), auto_unbox = TRUE), ";
      var box = document.getElementById('ts-info');
      if (!box) return;
      
      function fmt(n){ return (n === null || n === undefined) ? '–' : Number(n).toLocaleString('es-CL'); }
      function pct(v){ return (v === null || v === undefined) ? '–' : Number(v).toFixed(2) + ' %'; }
      
      function render(fr){
        var d = info[fr]; if (!d) return;
        var titulo = (tipo === 'limpia')
          ? 'Outliers eliminados · ' + fr
          : 'Datos crudos (sin eliminar) · ' + fr + ' — outliers que se excluirían al limpiar';
        box.innerHTML =
          '<div class=\"ts-info-title\">' + titulo + '</div>' +
          '<div class=\"ts-info-item\"><span class=\"dot\" style=\"background:#3b82f6\"></span><b>Cetam:</b> ' +
            fmt(d.c_n) + ' de ' + fmt(d.c_tot) + ' registros (' + pct(d.c_pct) + ')</div>' +
          '<div class=\"ts-info-item\"><span class=\"dot\" style=\"background:#f97316\"></span><b>MT:</b> ' +
            fmt(d.m_n) + ' de ' + fmt(d.m_tot) + ' registros (' + pct(d.m_pct) + ')</div>' +
          '<div class=\"ts-info-crit\">Criterio: ' + crit + '</div>';
      }
      
      render('TSP');
      el.on('plotly_buttonclicked', function(e){
        if (e && e.button && e.button.label) render(e.button.label);
      });
    }"
  )
  p <- htmlwidgets::onRender(p, js_info)
  
  # ---------------------------------------------------------------------------
  # HTML: barra de info arriba + gráfico ocupando el resto de la pantalla
  # ---------------------------------------------------------------------------
  ui_ts <- tagList(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$style(HTML("
        html, body { width: 100%; height: 100%; margin: 0 !important; padding: 0 !important; overflow: hidden; }
        body { font-family: Arial, sans-serif; background: white; }
        
        .ts-root {
          position: fixed; inset: 0;
          display: flex; flex-direction: column;
          background: white;
        }
        
        .ts-info {
          flex: 0 0 auto;
          display: flex; flex-wrap: wrap; align-items: center; gap: 8px 28px;
          padding: 10px 20px;
          background: #eff6ff; border-bottom: 1px solid #bfdbfe;
          color: #1e3a8a; font-size: 14px;
        }
        .ts-info-title { font-weight: 700; }
        .ts-info-item { display: flex; align-items: center; gap: 6px; }
        .ts-info-item .dot { width: 10px; height: 10px; border-radius: 50%; display: inline-block; }
        .ts-info-crit { color: #475569; font-size: 13px; margin-left: auto; }
        
        .ts-plot { flex: 1 1 auto; min-height: 0; position: relative; }
        .ts-plot .html-widget,
        .ts-plot .js-plotly-plot,
        .ts-plot .plotly {
          position: absolute; top: 0; left: 0;
          width: 100% !important; height: 100% !important;
        }
      "))
    ),
    tags$div(
      class = "ts-root",
      tags$div(id = "ts-info", class = "ts-info"),
      tags$div(class = "ts-plot", p)
    )
  )
  
  # Exportación autocontenida (sin cambios)
  temp_rds <- file.path(dir_html, paste0("temp_ts_", tipo, ".rds"))
  rmd_file <- file.path(dir_html, paste0("temp_ts_", tipo, ".Rmd"))
  
  saveRDS(ui_ts, temp_rds)
  
  writeLines(c(
    "---",
    "output:",
    "  html_document:",
    "    self_contained: true",
    "    theme: null",
    "    mathjax: null",
    "    includes:",
    "      in_header: null",
    "params:",
    "  rds_path: ''",
    "---",
    "```{r, echo=FALSE, warning=FALSE, message=FALSE}",
    "library(htmltools); library(plotly); readRDS(params$rds_path)",
    "```"
  ), rmd_file)
  
  rmarkdown::render(
    rmd_file,
    output_file = filename,
    output_dir = dir_html,
    params = list(rds_path = temp_rds),
    quiet = TRUE
  )
  
  unlink(rmd_file)
  unlink(temp_rds)
}


cat("Generando dashboards de series temporales hermosos...\n")
exportar_serie_temporal_ui(merged, merged_raw, "limpia", "Series_Temporales_Limpias_Todas.html")
exportar_serie_temporal_ui(merged, merged_raw, "cruda", "Series_Temporales_Crudas_Todas.html")

cat("\nEjecución finalizada con éxito.\n")