# =============================================================================
# GRIMM CETAM vs MT — EDA interactivo, normalidad, concordancia y boxplots
# Incluye fracciones: TSP, PM10, PM4, PM2.5 y PM1.
# Módulo: Boxplots interactivos con sliders de cuantiles y regla Tukey vs Medcouple.
# Conserva here() global en LabModelacion y trabaja dentro de Analisis_GRIMM_V2.
# =============================================================================

pkgs <- c("tidyverse", "readxl", "lubridate", "plotly", "htmlwidgets",
          "htmltools", "here", "jsonlite", "scales")
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
  div_plot_id <- paste0(prefix, "_plot")
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
               colors = c(Cetam = "#1f77b4", MT = "#ff7f0e"),
               type = "box", boxmean = TRUE, marker = list(size = 3, opacity = 0.5)) |>
    layout(
      title = paste("Boxplots de mediciones crudas —", fr),
      yaxis = list(title = "Concentración Cruda [µg/m³]"),
      xaxis = list(title = "Instrumento"),
      showlegend = FALSE
    )
  
  # JS dinámico para recortar cuantiles y actualizar contadores
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
        
        var c_rem_low = c_raw.filter(function(v){ return v < qinf_c; }).length;
        var c_rem_high = c_raw.filter(function(v){ return v > qsup_c; }).length;
        var m_rem_low = m_raw.filter(function(v){ return v < qinf_m; }).length;
        var m_rem_high = m_raw.filter(function(v){ return v > qsup_m; }).length;
        
        // Función para contar outliers
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
        
        var t_bold_c = rule === 'tukey' ? '<b>' + c_out.tukey + '</b>' : c_out.tukey;
        var hv_bold_c = rule === 'hv' ? '<b>' + c_out.hv + '</b>' : c_out.hv;
        var t_bold_m = rule === 'tukey' ? '<b>' + m_out.tukey + '</b>' : m_out.tukey;
        var hv_bold_m = rule === 'hv' ? '<b>' + m_out.hv + '</b>' : m_out.hv;
        
        info.innerHTML = '<b>Eliminados por recortes de cuantiles:</b><br>' +
          '• Cetam: ' + c_rem_low + ' (cola inf), ' + c_rem_high + ' (cola sup)<br>' +
          '• MT: ' + m_rem_low + ' (cola inf), ' + m_rem_high + ' (cola sup)<br><br>' +
          '<b>Conteo de Outliers (Sobre datos filtrados):</b><br>' +
          '<b>Cetam</b> (MC = ' + mc_c.toFixed(3) + '): Tukey = ' + t_bold_c + ' | Hubert-Vandervieren = ' + hv_bold_c + '<br>' +
          '<b>MT</b> (MC = ' + mc_m.toFixed(3) + '): Tukey = ' + t_bold_m + ' | Hubert-Vandervieren = ' + hv_bold_m;
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
    tags$div(
      style = "background:#f8f9fa;padding:15px;border-radius:8px;border:1px solid #ddd;margin-bottom:15px;font-size:13px;",
      tags$div(
        style = "display:flex;gap:20px;flex-wrap:wrap;align-items:center;",
        tags$div(
          tags$label(`for` = slider_qinf_id, "Cuantil inferior (q_inf): "),
          tags$input(id = slider_qinf_id, type = "range", min = 0, max = 0.10, step = 0.005, value = 0, style = "width:150px;vertical-align:middle;"),
          tags$span(id = val_qinf_id, style = "font-weight:bold;margin-left:5px;", "0.0%")
        ),
        tags$div(
          tags$label(`for` = slider_qsup_id, "Cuantil superior (q_sup): "),
          tags$input(id = slider_qsup_id, type = "range", min = 0.90, max = 1.00, step = 0.005, value = 1.00, style = "width:150px;vertical-align:middle;"),
          tags$span(id = val_qsup_id, style = "font-weight:bold;margin-left:5px;", "100.0%")
        ),
        tags$div(
          tags$label(`for` = select_rule_id, "Regla de Bigotes Activa: "),
          tags$select(id = select_rule_id, style = "padding:3px 8px;border-radius:4px;border:1px solid #ccc;",
                      tags$option(value = "hv", "Ajustada (Hubert–Vandervieren con Medcouple MC)"),
                      tags$option(value = "tukey", "Tukey Estándar (1.5 × IQR)"))
        )
      ),
      tags$hr(style = "margin:10px 0;border:0;border-top:1px solid #eee;"),
      tags$div(id = info_box_id, style = "color:#333;")
    )
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

summary_stats_html <- function(d, fr) {
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
      "<td style='padding:8px;border-bottom:1px solid #eee;font-weight:bold;'>", row[["Variable"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["N obs"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Media"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Mediana (Q2)"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Desv. Est. (SD)"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Rango Interc. (IQR)"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Mínimo"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Cuantil 25% (Q1)"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Cuantil 75% (Q3)"]], "</td>",
      "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["Máximo"]], "</td>",
      "</tr>"
    )
  })
  
  paste0(
    "<table style='border-collapse:collapse;width:100%;font-size:13px;margin-top:10px;'>",
    "<thead><tr style='background-color:#f4f6f8;'>",
    "<th style='text-align:left;padding:8px;border-bottom:2px solid #ccc;'>Variable</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>n</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>Media</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>Mediana</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>SD</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>IQR</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>Mín</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>Q1 (25%)</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>Q3 (75%)</th>",
    "<th style='text-align:right;padding:8px;border-bottom:2px solid #ccc;'>Máx</th>",
    "</tr></thead><tbody>",
    paste0(rows_html, collapse = ""),
    "</tbody>mtable>"
  )
}

# =============================================================================
# MÉTRICAS Y NORMALIDAD
# =============================================================================

metrics_for <- function(d, fr) {
  n <- nrow(d)
  if (n < 2) return(tibble(fraccion = fr, n = n))
  den <- var(d$Cetam) + var(d$MT) + (mean(d$Cetam) - mean(d$MT))^2
  fit <- if (sd(d$Cetam) > 0) lm(MT ~ Cetam, d) else NULL
  bias <- mean(d$diferencia); sdd <- sd(d$diferencia)
  tibble(fraccion = fr, n = n, media_Cetam = mean(d$Cetam), media_MT = mean(d$MT),
         Pearson = if(sd(d$Cetam) > 0 && sd(d$MT) > 0) cor(d$Cetam, d$MT) else NA_real_,
         R2_OLS = if(!is.null(fit)) summary(fit)$r.squared else NA_real_,
         CCC_Lin = if(den > 0) 2 * cov(d$Cetam, d$MT) / den else NA_real_,
         sesgo_MT_menos_Cetam = bias, LoA_inferior = bias - 1.96 * sdd,
         LoA_superior = bias + 1.96 * sdd, MAE = mean(abs(d$diferencia)),
         RMSE = sqrt(mean(d$diferencia^2)),
         PoA_1 = mean(d$abs_diferencia <= 1), PoA_2 = mean(d$abs_diferencia <= 2),
         PoA_5 = mean(d$abs_diferencia <= 5))
}
metrics <- bind_rows(lapply(names(cols), \(fr) metrics_for(pairs_for(fr), fr)))
write_csv(metrics, file.path(dir_tablas, "01_metricas_concordancia.csv"))

shapiro_safe <- function(x, max_n = 5000, seed = 123) {
  x <- x[is.finite(x)]
  n_total <- length(x)
  if (n_total < 3 || length(unique(x)) < 2) {
    return(tibble(n = n_total, n_test = NA_integer_, W_Shapiro = NA_real_, p_Shapiro = NA_real_, resultado_05 = "No evaluable"))
  }
  x_test <- if (n_total > max_n) { set.seed(seed); sample(x, max_n) } else x
  test <- shapiro.test(x_test)
  tibble(n = n_total, n_test = length(x_test), W_Shapiro = unname(test$statistic), p_Shapiro = test$p.value,
         resultado_05 = if_else(test$p.value < 0.05, "Se rechaza normalidad", "No se rechaza normalidad"))
}

normality_data <- function(fr) {
  d <- pairs_for(fr)
  bind_rows(
    tibble(fraccion = fr, variable = "Cetam", valor = d$Cetam),
    tibble(fraccion = fr, variable = "MT", valor = d$MT),
    tibble(fraccion = fr, variable = "Diferencia (MT - Cetam)", valor = d$diferencia)
  ) |> group_by(fraccion, variable) |> group_modify(\(.x, .y) shapiro_safe(.x$valor)) |> ungroup()
}

normality_results <- bind_rows(lapply(names(cols), normality_data))
write_csv(normality_results, file.path(dir_tablas, "02_analisis_normalidad.csv"))

# =============================================================================
# FUNCIONES DE GRÁFICOS INTERACTIVOS (PLOTLY)
# =============================================================================

plot_normality_hist <- function(d, fr) {
  if (nrow(d) < 3) return(plot_ly() |> layout(title = paste("Datos insuficientes:", fr)))
  plot_ly(d, x = ~diferencia, type = "histogram", nbinsx = 40,
          marker = list(color = "#4472C4", line = list(color = "white", width = 1)),
          hovertemplate = "Diferencia: %{x:.4f} µg/m³<br>Frecuencia: %{y}<extra></extra>") |>
    layout(title = paste("Distribución de las diferencias —", fr),
           xaxis = list(title = "Diferencia MT − Cetam [µg/m³]"), yaxis = list(title = "Frecuencia"), bargap = 0.08)
}

plot_normality_qq <- function(d, fr) {
  x <- d$diferencia[is.finite(d$diferencia)]
  if (length(x) < 3 || length(unique(x)) < 2) return(plot_ly() |> layout(title = paste("Datos insuficientes:", fr)))
  qq <- qqnorm(x, plot.it = FALSE)
  y_quartiles <- unname(quantile(x, c(0.25, 0.75), na.rm = TRUE, type = 7))
  x_quartiles <- qnorm(c(0.25, 0.75))
  pendiente <- diff(y_quartiles) / diff(x_quartiles)
  intercepto <- y_quartiles[1] - pendiente * x_quartiles[1]
  x_linea <- range(qq$x, finite = TRUE)
  qq_line <- list(x = x_linea, y = intercepto + pendiente * x_linea)
  
  plot_ly(x = qq$x, y = qq$y, type = "scatter", mode = "markers", name = "Cuantiles observados",
          marker = list(size = 5, opacity = 0.6),
          hovertemplate = "Cuantil teórico: %{x:.3f}<br>Cuantil observado: %{y:.3f}<extra></extra>") |>
    add_lines(x = qq_line$x, y = qq_line$y, name = "Referencia normal", line = list(color = "#d62728", width = 2), inherit = FALSE, hoverinfo = "skip") |>
    layout(title = paste("Gráfico Q-Q de las diferencias —", fr), xaxis = list(title = "Cuantiles teóricos normales"), yaxis = list(title = "Cuantiles observados [µg/m³]"))
}

outlier_note <- function(fr) {
  z <- outliers |> filter(fraccion == fr)
  a <- z$pct_excluido[z$instrumento == "Cetam"]
  b <- z$pct_excluido[z$instrumento == "MT"]
  paste0("Método: ", outlier_method_label(),
         "<br>Cetam: ", round(ifelse(length(a), a[1], NA_real_), 2), "% excluido",
         "<br>MT: ", round(ifelse(length(b), b[1], NA_real_), 2), "% excluido")
}

plot_ba <- function(d, fr) {
  if(nrow(d) < 2) return(plot_ly() |> layout(title = paste("Datos insuficientes:", fr)))
  bias <- mean(d$diferencia); s <- sd(d$diferencia)
  xr <- range(d$promedio)
  plot_ly(d, x = ~promedio, y = ~diferencia, type = "scatter", mode = "markers",
          text = ~paste0("Fecha: ", datetime, "<br>Cetam: ", signif(Cetam, 4), "<br>MT: ", signif(MT, 4), "<br>MT − Cetam: ", signif(diferencia, 4)),
          hoverinfo = "text", marker = list(size = 5, opacity = .55)) |>
    add_segments(x = xr[1], xend = xr[2], y = bias, yend = bias, name = "Sesgo medio", line = list(color = "#d62728", width = 2)) |>
    add_segments(x = xr[1], xend = xr[2], y = bias + 1.96*s, yend = bias + 1.96*s, name = "Límite superior 95%", line = list(color = "#1f77b4", dash = "dash")) |>
    add_segments(x = xr[1], xend = xr[2], y = bias - 1.96*s, yend = bias - 1.96*s, name = "Límite inferior 95%", line = list(color = "#1f77b4", dash = "dash")) |>
    layout(title = paste("Bland–Altman —", fr), xaxis = list(title = "Promedio (Cetam + MT)/2 [µg/m³]"), yaxis = list(title = "Diferencia MT − Cetam [µg/m³]"),
           annotations = list(list(x = .99, y = .99, xref = "paper", yref = "paper", xanchor = "right", yanchor = "top",
                                   text = outlier_note(fr), showarrow = FALSE, align = "right", bgcolor = "rgba(255,255,255,.9)", bordercolor = "#999", borderwidth = 1, borderpad = 4)))
}

plot_scatter <- function(d, fr) {
  if (nrow(d) < 2) return(plot_ly() |> layout(title = paste("Datos insuficientes:", fr)))
  fit <- lm(MT ~ Cetam, d)
  xreg <- seq(min(d$Cetam), max(d$Cetam), length.out = 100)
  yreg <- predict(fit, newdata = data.frame(Cetam = xreg))
  m <- metrics_for(d, fr)
  lim <- max(c(d$Cetam, d$MT), na.rm = TRUE)
  note <- paste0("n = ", nrow(d), "<br>Pearson = ", signif(m$Pearson, 4),
                 "<br>CCC Lin = ", signif(m$CCC_Lin, 4), "<br>R² = ", signif(m$R2_OLS, 4),
                 "<br>MAE = ", signif(m$MAE, 4), "<br>RMSE = ", signif(m$RMSE, 4),
                 "<br>MT = ", signif(coef(fit)[2], 4), " × Cetam ",
                 ifelse(coef(fit)[1] >= 0, "+ ", "− "), signif(abs(coef(fit)[1]), 4))
  
  plot_ly(d, x = ~Cetam, y = ~MT, type = "scatter", mode = "markers",
          text = ~paste0("Fecha: ", datetime, "<br>Cetam: ", signif(Cetam, 4), "<br>MT: ", signif(MT, 4)),
          hoverinfo = "text", marker = list(size = 5, opacity = .55)) |>
    add_lines(x = xreg, y = yreg, inherit = FALSE, name = "Regresión OLS", line = list(color = "#d62728", width = 2), hoverinfo = "skip") |>
    add_lines(x = c(0, lim), y = c(0, lim), inherit = FALSE, name = "Identidad y=x", line = list(color = "#222", dash = "dash"), hoverinfo = "skip") |>
    layout(title = paste("Scatter Cetam vs. MT —", fr), xaxis = list(title = "Cetam [µg/m³]", rangemode = "tozero"), yaxis = list(title = "MT [µg/m³]", rangemode = "tozero"),
           annotations = list(list(x = .02, y = .98, xref = "paper", yref = "paper", xanchor = "left", yanchor = "top", text = note,
                                   showarrow = FALSE, align = "left", bgcolor = "rgba(255,255,255,.9)", bordercolor = "#999", borderwidth = 1, borderpad = 5)))
}

plot_poa <- function(d, fr) {
  if(nrow(d) < 2) return(plot_ly() |> layout(title = paste("Datos insuficientes:", fr)))
  dd <- d$abs_diferencia
  xmax <- max(TOLERANCIA_INICIAL, as.numeric(quantile(dd, .99, na.rm = TRUE)), 1)
  ts <- seq(0, xmax, length.out = 301)
  ps <- vapply(ts, \(t) mean(dd <= t), numeric(1))
  tol0 <- min(TOLERANCIA_INICIAL, xmax)
  
  p <- plot_ly(x = ts, y = ps, type = "scatter", mode = "lines", name = "PoA",
               line = list(color = "#1f77b4", width = 3),
               hovertemplate = "Tolerancia: %{x:.3f} µg/m³<br>PoA: %{y:.2%}<extra></extra>") |>
    layout(title = paste("Probability of Agreement —", fr),
           xaxis = list(title = "Tolerancia absoluta |MT − Cetam| [µg/m³]", range = c(0, xmax)),
           yaxis = list(title = "Probabilidad empírica", tickformat = ".0%", range = c(0, 1)),
           shapes = list(list(type = "line", xref = "x", yref = "paper", x0 = tol0, x1 = tol0, y0 = 0, y1 = 1,
                              line = list(color = "#d62728", width = 2, dash = "dash"))))
  
  sid <- paste0("tol_", gsub("[^A-Za-z0-9]", "", fr))
  vid <- paste0(sid, "_value")
  p <- onRender(p, sprintf(
    "function(el,x){
      var s=document.getElementById('%s'),v=document.getElementById('%s');
      if(!s)return;
      var ts=%s,ps=%s;
      function upd(){var t=+s.value,i=0;while(i+1<ts.length&&ts[i+1]<=t)i++;
        Plotly.relayout(el,{'shapes[0].x0':t,'shapes[0].x1':t});
        v.textContent='±'+t.toFixed(2)+' µg/m³; PoA = '+(100*ps[i]).toFixed(2)+'%%';}
      s.addEventListener('input',upd);upd();
    }", sid, vid, jsonlite::toJSON(ts, auto_unbox = TRUE), jsonlite::toJSON(ps, auto_unbox = TRUE)))
  
  attr(p, "slider") <- tagList(tags$label(`for` = sid, paste0("Tolerancia para ", fr, ": ")),
                               tags$input(id = sid, type = "range", min = 0, max = xmax, step = max(xmax/500, .01), value = tol0, style = "width:70%;"),
                               tags$span(id = vid, style = "margin-left:12px;font-weight:bold;"))
  p
}

plot_ts <- function(fr, tipo = c("limpia", "cruda")) {
  tipo <- match.arg(tipo)
  base <- if (tipo == "cruda") merged_raw else merged
  cc <- cols[[fr]]
  d <- base |> select(datetime, Cetam = all_of(cc[1]), MT = all_of(cc[2])) |>
    pivot_longer(c(Cetam, MT), names_to = "instrumento", values_to = "valor") |> filter(!is.na(datetime))
  
  subtitulo <- if (tipo == "cruda") "Datos crudos: sin eliminación de outliers; promedio por minuto" else paste0("Datos limpios; método: ", outlier_method_label(), "; promedio por minuto")
  
  anotaciones <- list()
  if (tipo == "limpia") {
    z <- outliers |> filter(fraccion == fr)
    pct_c <- z$pct_excluido[z$instrumento == "Cetam"]
    pct_m <- z$pct_excluido[z$instrumento == "MT"]
    n_c <- z$n_excluido[z$instrumento == "Cetam"]
    n_m <- z$n_excluido[z$instrumento == "MT"]
    anotaciones <- list(list(
      x = 0.99, y = 0.99, xref = "paper", yref = "paper", xanchor = "right", yanchor = "top", showarrow = FALSE, align = "left",
      text = paste0("<b>Datos excluidos</b><br>", "Método: ", outlier_method_label(), "<br>",
                    "Cetam: ", if (length(pct_c) && is.finite(pct_c[1])) sprintf("%.2f%%", pct_c[1]) else "N/D", " (", if (length(n_c)) n_c[1] else "N/D", " med.)<br>",
                    "MT: ", if (length(pct_m) && is.finite(pct_m[1])) sprintf("%.2f%%", pct_m[1]) else "N/D", " (", if (length(n_m)) n_m[1] else "N/D", " med.)"),
      bgcolor = "rgba(255,255,255,0.92)", bordercolor = "#777", borderwidth = 1, borderpad = 7, font = list(size = 12, color = "#222")
    ))
  }
  
  plot_ly(d, x = ~datetime, y = ~valor, color = ~instrumento, colors = c(Cetam = "#1f77b4", MT = "#ff7f0e"), type = "scatter", mode = "lines", connectgaps = FALSE) |>
    layout(title = list(text = paste0("Serie temporal ", tipo, " — ", fr, "<br><sup>", subtitulo, "</sup>")),
           xaxis = list(title = "Fecha", type = "date", rangeslider = list(visible = TRUE),
                        rangeselector = list(buttons = list(
                          list(count = 1, label = "1 día", step = "day", stepmode = "backward"),
                          list(count = 7, label = "1 semana", step = "day", stepmode = "backward"),
                          list(count = 1, label = "1 mes", step = "month", stepmode = "backward"),
                          list(step = "all", label = "Todo")))),
           yaxis = list(title = "Concentración [µg/m³]", autorange = TRUE), annotations = anotaciones, hovermode = "x unified")
}

# =============================================================================
# GENERACIÓN DE DASHBOARDS HTML Y SERIES TEMPORALES
# =============================================================================

for (fr in names(cols)) {
  d <- pairs_for(fr)
  ba <- plot_ba(d, fr)
  sc <- plot_scatter(d, fr)
  poa <- plot_poa(d, fr)
  hist_normalidad <- plot_normality_hist(d, fr)
  qq_normalidad <- plot_normality_qq(d, fr)
  normalidad_fr <- normality_results |> filter(fraccion == fr)
  
  table_summary_html <- summary_stats_html(d, fr)
  widget_bp <- widget_boxplots_interactivos(fr)
  
  dashboard_fr <- tagList(
    tags$head(tags$meta(charset = "utf-8"), tags$title(paste("GRIMM Cetam vs MT —", fr)),
              tags$style(HTML("body{font-family:Arial,sans-serif;max-width:1450px;margin:24px auto;padding:0 18px;color:#222}.plotly{width:100%!important}input[type=range]{accent-color:#d62728}"))),
    tags$h1(paste("Comparación de instrumentos GRIMM: Cetam vs MT —", fr)),
    tags$p(paste0("Mass Value: ", fr, " · método de outliers: ", outlier_method_label(), " · diferencia = MT − Cetam")),
    tags$p(paste("Pares completos disponibles:", nrow(d))),
    
    # SECCIÓN 1: ESTADÍSTICOS DESCRIPTIVOS
    tags$section(
      style = "margin:24px 0;padding:18px;border:1px solid #ddd;border-radius:10px;",
      tags$h2("Estadísticos Descriptivos y Resumen de la Muestra"),
      tags$p("Tendencia central (Media, Mediana), Variabilidad (SD, IQR), Rango (Mín, Máx) y Cuartiles (Q1, Q3) expresados en [µg/m³]."),
      htmltools::HTML(table_summary_html)
    ),
    
    # SECCIÓN 2: BOXPLOTS INTERACTIVOS CON SLIDERS DE CUANTILES
    tags$section(
      style = "margin:24px 0;padding:18px;border:1px solid #ddd;border-radius:10px;",
      tags$h2("Boxplots de mediciones crudas · Cetam vs MT (Controles Dinámicos)"),
      tags$p("Usa los deslizadores para ajustar los recortes de cuantiles (q_inf y q_sup) y selecciona la regla de bigotes para evaluar asimetría y outliers."),
      widget_bp
    ),
    
    # SECCIÓN 3: NORMALIDAD
    tags$section(
      style = "margin:24px 0;padding:18px;border:1px solid #ddd;border-radius:10px;",
      tags$h2("Análisis de normalidad"),
      tags$p("Prueba de Shapiro–Wilk para Cetam, MT y las diferencias pareadas (α = 0,05)."),
      htmltools::HTML(paste0(
        "<table style='border-collapse:collapse;width:100%;'>",
        "<thead><tr>",
        "<th style='text-align:left;padding:8px;border-bottom:1px solid #ccc;'>Variable</th>",
        "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>n</th>",
        "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>n evaluado</th>",
        "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>W</th>",
        "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>p-valor</th>",
        "<th style='text-align:left;padding:8px;border-bottom:1px solid #ccc;'>Conclusión (α = 0,05)</th>",
        "</tr></thead><tbody>",
        paste0(apply(normalidad_fr, 1, function(row) {
          paste0("<tr><td style='padding:8px;border-bottom:1px solid #eee;'>", row[["variable"]], "</td>",
                 "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["n"]], "</td>",
                 "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["n_test"]], "</td>",
                 "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", ifelse(is.na(as.numeric(row[["W_Shapiro"]])), "NA", signif(as.numeric(row[["W_Shapiro"]]), 5)), "</td>",
                 "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", ifelse(is.na(as.numeric(row[["p_Shapiro"]])), "NA", format.pval(as.numeric(row[["p_Shapiro"]]), digits = 4)), "</td>",
                 "<td style='padding:8px;border-bottom:1px solid #eee;'>", row[["resultado_05"]], "</td></tr>")
        }), collapse = ""),
        "</tbody></table>"
      )),
      tags$p("Revisa también el histograma y el gráfico Q-Q para complementar el p-valor."),
      hist_normalidad, qq_normalidad
    ),
    
    # SECCIÓN 4: CONCORDANCIA
    tags$section(
      style = "margin:24px 0;padding:18px;border:1px solid #ddd;border-radius:10px;",
      tags$h2("Bland–Altman"), ba,
      tags$h2("Probability of Agreement"), attr(poa, "slider"), poa,
      tags$h2("Scatter plot"), sc
    )
  )
  
  nombre_fr <- gsub("\\.", "", fr)
  archivo_dashboard <- file.path(dir_html, paste0("Dashboard_Concordancia_", nombre_fr, ".html"))
  carpeta_lib <- file.path(dir_html, paste0("Dashboard_Concordancia_", nombre_fr, "_lib"))
  
  htmltools::save_html(dashboard_fr, file = archivo_dashboard, libdir = carpeta_lib)
  
  htmlwidgets::saveWidget(plot_ts(fr, "limpia"), file.path(dir_html, paste0("Serie_Temporal_Limpia_", nombre_fr, ".html")), selfcontained = TRUE)
  htmlwidgets::saveWidget(plot_ts(fr, "cruda"), file.path(dir_html, paste0("Serie_Temporal_Cruda_", nombre_fr, ".html")), selfcontained = TRUE)
}

cat("\nDashboards generados con sliders interactivos de cuantiles y regla de bigotes para: TSP, PM10, PM4, PM2.5 y PM1.\nUbicación HTML: ", dir_html, "\n", sep = "")