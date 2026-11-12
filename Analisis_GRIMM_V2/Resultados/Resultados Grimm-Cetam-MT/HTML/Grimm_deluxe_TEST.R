# =============================================================================
# GRIMM CETAM vs MT — EDA interactivo, normalidad, concordancia y boxplots
# Versión Optimizada: WebGL (scattergl), Max 15k puntos visuales, Diseño UI
# Dashboard Maestro Unificado (Pestañas HTML) + Series Temporales
# + Múltiples pares de archivos (detección automática) + Intervalo de tiempo
# =============================================================================

library(beepr)


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

# =============================================================================
# CONFIGURACIÓN GENERAL Y OUTLIERS DE FILTRADO
# =============================================================================

METODO_OUTLIERS <- "IQR"
MULTIPLICADOR_OUTLIERS <- 5
CORTE_SUPERIOR <- 5000
TOLERANCIA_INICIAL <- 2
MAX_PUNTOS_VISUALES <- 15000 

# ---- Selección de datos ----
FECHAS_ARCHIVOS  <- NULL   # NULL = todos los pares encontrados; o c("22052025", "19062025")
INTERVALO_INICIO <- NULL   # NULL = sin límite. Ej: "2025-05-22" o "2025-05-22 08:00:00"
INTERVALO_FIN    <- "2025-05-18"   # Ej: "2025-06-19" (si es solo fecha, incluye el día completo)
ANIOS_VALIDOS    <- 2025   # años aceptados (antes estaba fijo en 2025)

# ---- Reporte Markdown ----
UMBRAL_HUECO_MIN <- 60     # se reportan intervalos sin datos mayores a este valor [minutos]

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

# Devuelve el umbral superior que usa remove_upper_outliers (NA si no aplica); solo para el reporte
limite_outlier <- function(x, metodo = METODO_OUTLIERS, k = MULTIPLICADOR_OUTLIERS, corte = CORTE_SUPERIOR) {
  ok <- is.finite(x)
  if (metodo == "IQR") {
    if (sum(ok) < 4) return(NA_real_)
    q <- quantile(x[ok], probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    return(q[2] + k * (q[2] - q[1]))
  }
  if (metodo == "CORTE") return(corte)
  NA_real_
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
# DETECCIÓN DE PARES DE ARCHIVOS
# =============================================================================

buscar_pares <- function(dir, fechas = NULL) {
  f_c <- list.files(dir, pattern = "^GRIMM 11D Cetam \\d{8}\\.xlsx$", full.names = TRUE)
  f_m <- list.files(dir, pattern = "^GRIMM 11D MT \\d{8}\\.xlsx$",    full.names = TRUE)
  tag <- function(f) sub(".* (\\d{8})\\.xlsx$", "\\1", basename(f))
  tc <- tibble(fecha = tag(f_c), cetam = f_c)
  tm <- tibble(fecha = tag(f_m), mt = f_m)
  pares <- inner_join(tc, tm, by = "fecha")
  
  sin_par <- setdiff(union(tc$fecha, tm$fecha), pares$fecha)
  if (length(sin_par)) warning("Fechas sin par Cetam/MT (ignoradas): ", paste(sin_par, collapse = ", "))
  
  if (!is.null(fechas)) pares <- filter(pares, fecha %in% fechas)
  if (!nrow(pares)) stop("No se encontraron pares de archivos en: ", dir)
  arrange(pares, as.Date(fecha, "%d%m%Y"))
}

pares <- buscar_pares(dir_datos, FECHAS_ARCHIVOS)
cat("Pares de archivos a procesar:", paste(pares$fecha, collapse = ", "), "\n")

# =============================================================================
# LECTURA Y PROCESAMIENTO DE DATOS
# =============================================================================

parse_intervalo <- function(x, tz, fin = FALSE) {
  if (is.null(x) || is.na(x)) return(NULL)
  z <- lubridate::parse_date_time(x, orders = c("Ymd HMS", "Ymd HM", "Ymd"), tz = tz, quiet = TRUE)
  if (is.na(z)) stop("Formato de intervalo no válido: ", x)
  if (fin && nchar(trimws(x)) <= 10) z <- z + lubridate::days(1) - 1  # hasta 23:59:59
  z
}

read_grimm_raw <- function(path) {
  readxl::read_excel(path, sheet = "Mass values", skip = 4) |>
    rename(datetime_raw = `date&time`,
           TSP  = `TSP [ug/m3]`,
           PM10 = `PM10 [ug/m3]`,
           PM4  = `PM4 [ug/m3]`,
           PM25 = `PM2,5 [ug/m3]`,
           PM1  = `PM1 [ug/m3]`) |>
    mutate(datetime = parse_grimm_datetime(datetime_raw),
           across(c(TSP, PM10, PM4, PM25, PM1), ~ suppressWarnings(as.numeric(.x)))) |>
    filter(!is.na(datetime), lubridate::year(datetime) %in% ANIOS_VALIDOS)
}

load_grimm <- function(paths, instrument) {
  raw <- purrr::map_dfr(paths, read_grimm_raw) |> arrange(datetime)
  
  # Filtro de intervalo (aplica a TODO lo que sigue)
  tz_dat <- lubridate::tz(raw$datetime)
  ini <- parse_intervalo(INTERVALO_INICIO, tz_dat)
  fin <- parse_intervalo(INTERVALO_FIN, tz_dat, fin = TRUE)
  if (!is.null(ini)) raw <- filter(raw, datetime >= ini)
  if (!is.null(fin)) raw <- filter(raw, datetime <= fin)
  
  if (!nrow(raw)) stop("No hay datos de ", instrument, " en el intervalo/fechas elegidos.")
  
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
  
  # ----- Descripción de la data (para el reporte Markdown) -----
  etiquetas <- c(TSP = "TSP", PM10 = "PM10", PM4 = "PM4", PM25 = "PM2.5", PM1 = "PM1")
  desc <- purrr::map_dfr(fracs_cols, function(cn) {
    x <- raw[[cn]]
    ok <- is.finite(x)
    lim <- limite_outlier(x)
    of <- if (!is.na(lim)) ok & x > lim else rep(FALSE, length(x))
    tibble(instrumento = instrument, fraccion = etiquetas[[cn]],
           n_registros = length(x), n_validos = sum(ok), n_na = sum(!ok),
           n_negativos = sum(x[ok] < 0), n_cero = sum(x[ok] == 0),
           limite_superior = lim, n_outliers = sum(of),
           minimo = if (any(ok)) min(x[ok]) else NA_real_,
           media = if (any(ok)) mean(x[ok]) else NA_real_,
           mediana = if (any(ok)) median(x[ok]) else NA_real_,
           maximo = if (any(ok)) max(x[ok]) else NA_real_,
           max_outlier = if (any(of)) max(x[of]) else NA_real_)
  })
  tiempo <- tibble(instrumento = instrument,
                   n_registros = nrow(raw), n_minutos = nrow(raw_minute),
                   muestreo_seg = if (nrow(raw) > 1) median(as.numeric(diff(raw$datetime), units = "secs")) else NA_real_)
  
  list(data = clean_minute, raw_data = raw_minute, outliers = out, desc = desc, tiempo = tiempo)
}

rc <- load_grimm(pares$cetam, "Cetam")
rm <- load_grimm(pares$mt,    "MT")

dc <- rc$data |> rename(TSP_C = TSP, PM10_C = PM10, PM4_C = PM4, PM25_C = PM25, PM1_C = PM1, n_Cetam = n_registros)
dm <- rm$data |> rename(TSP_M = TSP, PM10_M = PM10, PM4_M = PM4, PM25_M = PM25, PM1_M = PM1, n_MT = n_registros)
merged <- full_join(dc, dm, by = "datetime_1m") |> rename(datetime = datetime_1m) |> arrange(datetime)

dcr <- rc$raw_data |> rename(TSP_C = TSP, PM10_C = PM10, PM4_C = PM4, PM25_C = PM25, PM1_C = PM1, n_Cetam = n_registros)
dmr <- rm$raw_data |> rename(TSP_M = TSP, PM10_M = PM10, PM4_M = PM4, PM25_M = PM25, PM1_M = PM1, n_MT = n_registros)
merged_raw <- full_join(dcr, dmr, by = "datetime_1m") |> rename(datetime = datetime_1m) |> arrange(datetime)

cat("Rango de datos analizado:", format(min(merged$datetime)), "->", format(max(merged$datetime)), "\n")

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
    jsonlite::toJSON(round(c_vals, 2)), jsonlite::toJSON(round(m_vals, 2)),
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



# -----------------------------------------------------------
# Verifica que el HTML sea realmente autocontenido y reporta su tamaño
verificar_html_autocontenido <- function(ruta) {
  stopifnot(file.exists(ruta))
  txt <- paste(readLines(ruta, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  
  # Referencias a archivos locales (rutas de Windows / carpeta _files) o recursos remotos
  patron_local  <- "(src|href)=\"(file:|[A-Za-z]:[/\\\\]|[^\"]*_files/)[^\"]*\""
  patron_remoto <- "(src|href)=\"https?://[^\"]*\\.(js|css)[^\"]*\""
  loc <- regmatches(txt, gregexpr(patron_local,  txt, perl = TRUE))[[1]]
  rem <- regmatches(txt, gregexpr(patron_remoto, txt, perl = TRUE))[[1]]
  
  mb <- file.size(ruta) / 1024^2
  cat(sprintf("• %s: %.1f MB | refs locales: %d | refs remotas: %d\n",
              basename(ruta), mb, length(loc), length(rem)))
  if (length(loc)) warning("Este HTML apunta a archivos locales; en otro PC se verá vacío: ",
                           paste(utils::head(loc, 3), collapse = " | "))
  if (mb > 50) warning(sprintf("%s pesa %.0f MB; los visores web suelen fallar sobre ~50 MB.", basename(ruta), mb))
  invisible(list(local = loc, remoto = rem, mb = mb))
}

# =============================================================================
# DASHBOARDS DE SERIES TEMPORALES (Envueltos en diseño UI)
# =============================================================================

# Inserta una fila NA en cada hueco largo (p. ej. entre días distintos) para que
# plotly NO una con una línea recta el último punto de un día con el primero del siguiente.
rellenar_huecos <- function(d, max_gap_min = 10) {
  d <- arrange(d, datetime)
  idx <- which(c(NA, as.numeric(diff(d$datetime), units = "mins")) > max_gap_min)
  if (!length(idx)) return(d)
  na_rows <- d[idx, ] |> mutate(across(-datetime, ~ NA_real_), datetime = datetime - 1)
  bind_rows(d, na_rows) |> arrange(datetime)
}

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
exportar_serie_temporal_ui(rellenar_huecos(merged), rellenar_huecos(merged_raw), "limpia", "Series_Temporales_Limpias_Todas.html")
exportar_serie_temporal_ui(rellenar_huecos(merged), rellenar_huecos(merged_raw), "cruda",  "Series_Temporales_Crudas_Todas.html")

cat("\nVerificando HTMLs generados...\n")
for (f in c("Dashboard_Maestro_Concordancia.html",
            "Series_Temporales_Limpias_Todas.html",
            "Series_Temporales_Crudas_Todas.html")) {
  verificar_html_autocontenido(file.path(dir_html, f))
}

# =============================================================================
# REPORTE EN MARKDOWN
# =============================================================================

generar_reporte_md <- function(ruta) {
  
  # ---- Helpers de formato ----
  num  <- function(x, d = 2) ifelse(is.na(x), "–", formatC(as.numeric(x), format = "f", digits = d, big.mark = ","))
  pct  <- function(x, d = 2) ifelse(is.na(x), "–", paste0(formatC(as.numeric(x), format = "f", digits = d), " %"))
  fh   <- function(x) format(x, "%Y-%m-%d %H:%M")
  dur  <- function(horas) {
    d <- floor(horas / 24); h <- round(horas - 24 * d)
    ifelse(d > 0, paste0(d, " d ", h, " h"), sprintf("%.1f h", horas))
  }
  tabla <- function(df) paste(knitr::kable(df, format = "pipe", align = "l"), collapse = "\n")
  
  md <- character()
  add <- function(...) md <<- c(md, ...)
  
  # ---- Series de tiempo (minutos con datos) ----
  t_c <- rc$raw_data$datetime_1m
  t_m <- rm$raw_data$datetime_1m
  series <- list(Cetam = t_c, MT = t_m, `Simultáneo (ambos)` = t_c[t_c %in% t_m])
  
  detectar_huecos <- function(t, umbral_min) {
    t <- sort(unique(t))
    if (length(t) < 2) return(tibble(desde = as.POSIXct(character()), hasta = as.POSIXct(character()), horas = numeric()))
    dif <- as.numeric(diff(t), units = "mins")
    i <- which(dif > umbral_min)
    tibble(desde = t[i], hasta = t[i + 1], horas = dif[i] / 60)
  }
  
  # ===========================================================================
  add("# Reporte de análisis GRIMM: Cetam vs MT", "")
  
  # ---- 1. Datos de entrada y configuración ----
  add("## 1. Datos de entrada y configuración", "", "**Pares de archivos utilizados:**", "")
  add(tabla(tibble(Fecha = as.character(as.Date(pares$fecha, "%d%m%Y")),
                   `Archivo Cetam` = basename(pares$cetam),
                   `Archivo MT` = basename(pares$mt))), "")
  add("**Configuración del análisis:**", "",
      paste0("- Intervalo solicitado: ", if (is.null(INTERVALO_INICIO)) "sin límite inferior" else INTERVALO_INICIO,
             " → ", if (is.null(INTERVALO_FIN)) "sin límite superior" else INTERVALO_FIN),
      paste0("- Años aceptados: ", paste(ANIOS_VALIDOS, collapse = ", ")),
      paste0("- Criterio de outliers: ", outlier_method_label(), " (solo valores superiores)"),
      "- Resolución del análisis: promedios por minuto",
      paste0("- Umbral para reportar intervalos sin datos: ", UMBRAL_HUECO_MIN, " minutos"),
      "")
  
  # ---- 2. Rango de tiempo y cobertura ----
  add("## 2. Rango de tiempo y cobertura", "")
  tab_t <- bind_rows(lapply(names(series), function(nm) {
    t <- unique(series[[nm]])
    if (!length(t)) return(NULL)
    ini <- min(t); fin <- max(t)
    n_min <- length(t)
    rng <- as.numeric(difftime(fin, ini, units = "mins")) + 1
    tibble(Serie = nm, Inicio = fh(ini), Fin = fh(fin),
           `Duración del rango` = dur(as.numeric(difftime(fin, ini, units = "hours"))),
           `Minutos con datos` = num(n_min, 0),
           `Cobertura del rango` = pct(100 * n_min / rng))
  }))
  add(tabla(tab_t), "")
  
  add("### Intervalos sin datos", "")
  huecos <- bind_rows(lapply(names(series), function(nm) {
    detectar_huecos(series[[nm]], UMBRAL_HUECO_MIN) |> mutate(Serie = nm)
  }))
  
  if (nrow(huecos) == 0) {
    add(paste0("No se detectaron intervalos sin datos mayores a ", UMBRAL_HUECO_MIN, " minutos."), "")
  } else {
    res <- huecos |> group_by(Serie) |>
      summarise(n = n(), total = sum(horas), mayor = max(horas), .groups = "drop") |>
      arrange(match(Serie, names(series)))
    add(tabla(transmute(res, Serie,
                        `N° de intervalos` = num(n, 0),
                        `Tiempo total sin datos` = dur(total),
                        `Intervalo más largo` = dur(mayor))), "")
    
    max_filas <- 50
    det <- huecos |> arrange(desc(horas))
    if (nrow(det) > max_filas) det <- slice_head(det, n = max_filas)
    det <- det |> arrange(match(Serie, names(series)), desde)
    add(paste0("Detalle (mayores a ", UMBRAL_HUECO_MIN, " min",
               if (nrow(huecos) > max_filas) paste0("; se muestran los ", max_filas, " más largos") else "", "):"), "")
    add(tabla(transmute(det, Serie,
                        `Último dato antes del hueco` = fh(desde),
                        `Primer dato después` = fh(hasta),
                        `Duración` = dur(horas))), "")
  }
  
  # ---- 3. Descripción de la data ----
  add("## 3. Descripción de la data", "")
  desc <- bind_rows(rc$desc, rm$desc)
  tiempo <- bind_rows(rc$tiempo, rm$tiempo)
  
  add("**Observación por instrumento:**", "")
  add(tabla(transmute(tiempo, Instrumento = instrumento,
                      `Registros originales` = num(n_registros, 0),
                      `Minutos con datos` = num(n_minutos, 0),
                      `Intervalo de muestreo mediano` = ifelse(is.na(muestreo_seg), "–", paste0(num(muestreo_seg, 0), " s")))), "")
  
  add("**Conteo de registros por fracción** (antes de eliminar outliers; % sobre registros válidos):", "")
  add(tabla(transmute(desc, Instrumento = instrumento, `Fracción` = fraccion,
                      Registros = num(n_registros, 0),
                      `Válidos` = num(n_validos, 0),
                      `Vacíos / NA` = num(n_na, 0),
                      Negativos = paste0(num(n_negativos, 0), " (", pct(100 * n_negativos / n_validos), ")"),
                      Ceros = paste0(num(n_cero, 0), " (", pct(100 * n_cero / n_validos), ")"),
                      Outliers = paste0(num(n_outliers, 0), " (", pct(100 * n_outliers / n_validos), ")"))), "")
  
  add("**Estadísticos de la data original** [µg/m³]:", "")
  add(tabla(transmute(desc, Instrumento = instrumento, `Fracción` = fraccion,
                      `Mínimo` = num(minimo), Media = num(media), Mediana = num(mediana),
                      `Máximo` = num(maximo),
                      `Mayor valor outlier` = num(max_outlier))), "")
  
  # ---- 4. Criterios ----
  add("## 4. Criterios de outliers", "",
      paste0("- **Método:** ", outlier_method_label(), "."),
      "- Solo se eliminan valores **superiores** al límite; los valores negativos y los ceros **no** se eliminan, solo se reportan en la sección 3.",
      "- Los valores eliminados se reemplazan por NA antes de promediar por minuto.",
      "")
  lc <- filter(desc, instrumento == "Cetam"); lm_ <- filter(desc, instrumento == "MT")
  add("**Límite superior aplicado** [µg/m³]:", "")
  add(tabla(tibble(`Fracción` = lc$fraccion,
                   `Límite Cetam` = num(lc$limite_superior),
                   `Límite MT` = num(lm_$limite_superior[match(lc$fraccion, lm_$fraccion)]))), "")
  
  # ---- 5. Outliers eliminados ----
  add("## 5. Outliers eliminados", "")
  oc <- filter(outliers, instrumento == "Cetam"); om <- filter(outliers, instrumento == "MT")
  tab5 <- left_join(oc, om, by = "fraccion", suffix = c("_c", "_m"))
  if (sum(outliers$n_excluido, na.rm = TRUE) == 0) {
    add("No se eliminó ningún outlier en este intervalo.", "")
  }
  add(tabla(transmute(tab5, `Fracción` = fraccion,
                      `Cetam: válidos originales` = num(n_original_c, 0),
                      `Cetam: eliminados` = num(n_excluido_c, 0),
                      `Cetam: %` = pct(pct_excluido_c),
                      `MT: válidos originales` = num(n_original_m, 0),
                      `MT: eliminados` = num(n_excluido_m, 0),
                      `MT: %` = pct(pct_excluido_m))), "")
  
  # ---- 6. Concordancia ----
  add("## 6. Resultados del análisis de concordancia", "",
      "Calculados con los pares de minutos en que ambos instrumentos tienen dato válido, después de eliminar outliers. Diferencia = MT − Cetam.",
      "")
  
  cat_ccc <- function(x) dplyr::case_when(is.na(x) ~ "–", x > 0.99 ~ "Casi perfecta",
                                          x > 0.95 ~ "Sustancial", x > 0.90 ~ "Moderada", TRUE ~ "Pobre")
  poa <- vapply(names(cols), function(fr) {
    d <- pairs_for(fr)
    if (nrow(d)) mean(d$abs_diferencia <= TOLERANCIA_INICIAL) else NA_real_
  }, numeric(1))
  
  add("**Acuerdo y correlación:**", "")
  add(tabla(transmute(metrics, `Fracción` = fraccion, `N° de pares` = num(n, 0),
                      `Media Cetam` = num(media_Cetam), `Media MT` = num(media_MT),
                      Pearson = num(Pearson, 3), `R² (OLS)` = num(R2_OLS, 3),
                      `CCC de Lin` = num(CCC_Lin, 3))), "")
  
  add("**Sesgo, errores y regresión** (MT = β0 + β1 · Cetam):", "")
  add(tabla(transmute(metrics, `Fracción` = fraccion,
                      `Sesgo (MT − Cetam)` = num(sesgo_MT_menos_Cetam, 3),
                      `Sesgo relativo` = pct(100 * sesgo_MT_menos_Cetam / media_Cetam),
                      `LoA inferior` = num(LoA_inferior), `LoA superior` = num(LoA_superior),
                      MAE = num(MAE, 3), RMSE = num(RMSE, 3),
                      β0 = num(beta0, 3), β1 = num(beta1, 3),
                      `PoA (±tolerancia)` = pct(100 * unname(poa[fraccion])))), "")
  add(paste0("*PoA = proporción de pares con |MT − Cetam| ≤ ", TOLERANCIA_INICIAL, " µg/m³.*"), "")
  
  add("**Estadísticos de la diferencia (MT − Cetam)** [µg/m³]:", "")
  tab_dif <- bind_rows(lapply(names(cols), function(fr) {
    s <- calc_stats_vec(pairs_for(fr)$diferencia)
    tibble(`Fracción` = fr, Media = num(s["media"], 3), Mediana = num(s["mediana"], 3),
           SD = num(s["sd"], 3), `Mínimo` = num(s["min"], 3), Q1 = num(s["q25"], 3),
           Q3 = num(s["q75"], 3), `Máximo` = num(s["max"], 3))
  }))
  add(tabla(tab_dif), "")
  
  writeLines(enc2utf8(md), ruta, useBytes = TRUE)
  invisible(ruta)
}

cat("\nGenerando reporte Markdown...\n")
archivo_md <- file.path(dir_resultados, "Reporte_Analisis_GRIMM.md")
generar_reporte_md(archivo_md)
cat("Reporte Markdown generado:", archivo_md, "\n")

cat("\nEjecución finalizada con éxito.\n")

beep()