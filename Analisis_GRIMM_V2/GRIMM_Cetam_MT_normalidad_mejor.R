# =============================================================================
# GRIMM CETAM vs MT — EDA interactivo y análisis de normalidad
# Objetivo: comparar concordancia por fracción y evaluar normalidad.
# Conserva here() global en LabModelacion y trabaja dentro de Analisis_GRIMM_V2.
# Genera dashboards HTML, series temporales y tablas CSV.
# =============================================================================

pkgs <- c("tidyverse", "readxl", "lubridate", "plotly", "htmlwidgets",
          "htmltools", "here", "jsonlite", "scales")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
invisible(lapply(pkgs, library, character.only = TRUE))

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
# CONFIGURACIÓN DE OUTLIERS
# =============================================================================

# Método activo:
#   "IQR"       : Q3 + MULTIPLICADOR_OUTLIERS * IQR
#   "CORTE"     : elimina valores mayores que CORTE_SUPERIOR
#   "NINGUNO"   : no elimina valores atípicos
METODO_OUTLIERS <- "IQR"

# Parámetros del método IQR (método original)
MULTIPLICADOR_OUTLIERS <- 5

# Parámetro del método de corte absoluto
CORTE_SUPERIOR <- 5000

# Tolerancia inicial del gráfico PoA
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

remove_upper_outliers <- function(
    x,
    metodo = METODO_OUTLIERS,
    k = MULTIPLICADOR_OUTLIERS,
    corte = CORTE_SUPERIOR
) {
  
  # Validar el método seleccionado
  metodo <- match.arg(metodo, c("IQR", "CORTE", "NINGUNO"))
  
  # No modificar los datos
  if (metodo == "NINGUNO") {
    return(x)
  }
  
  # Identificar valores finitos
  ok <- is.finite(x)
  
  # Método 1: criterio estadístico IQR (método original)
  if (metodo == "IQR") {
    
    if (sum(ok) < 4) {
      return(x)
    }
    
    q <- quantile(
      x[ok],
      probs = c(0.25, 0.75),
      na.rm = TRUE,
      names = FALSE
    )
    
    limite <- q[2] + k * (q[2] - q[1])
    
    x[ok & x > limite] <- NA_real_
    
  }
  
  # Método 2: corte absoluto
  else if (metodo == "CORTE") {
    
    x[ok & x > corte] <- NA_real_
    
  }
  
  return(x)
}

# Descripción del método activo para los gráficos y el dashboard
outlier_method_label <- function() {
  
  if (METODO_OUTLIERS == "IQR") {
    paste0(
      "Q3 + ", MULTIPLICADOR_OUTLIERS, " × IQR"
    )
    
  } else if (METODO_OUTLIERS == "CORTE") {
    paste0(
      "corte absoluto > ", CORTE_SUPERIOR, " µg/m³"
    )
    
  } else {
    "sin eliminación de outliers"
  }
}

load_grimm <- function(path, instrument) {
  raw <- readxl::read_excel(path, sheet = "Mass values", skip = 4) |>
    rename(datetime_raw = `date&time`, PM10 = `PM10 [ug/m3]`,
           PM25 = `PM2,5 [ug/m3]`, PM1 = `PM1 [ug/m3]`) |>
    mutate(datetime = parse_grimm_datetime(datetime_raw),
           across(c(PM1, PM25, PM10), ~ suppressWarnings(as.numeric(.x)))) |>
    filter(!is.na(datetime), lubridate::year(datetime) == 2025) |>
    arrange(datetime)
  if (!nrow(raw)) stop("No se encontraron fechas de 2025 en: ", path)
  
  n0 <- sapply(raw[c("PM1", "PM25", "PM10")], \(x) sum(is.finite(x)))
  clean <- raw |>
    mutate(across(c(PM1, PM25, PM10), remove_upper_outliers))
  n1 <- sapply(clean[c("PM1", "PM25", "PM10")], \(x) sum(is.finite(x)))
  out <- tibble(instrumento = instrument, fraccion = c("PM1", "PM2.5", "PM10"),
                n_original = as.integer(n0[c("PM1", "PM25", "PM10")]),
                n_excluido = as.integer(n0[c("PM1", "PM25", "PM10")] -
                                          n1[c("PM1", "PM25", "PM10")])) |>
    mutate(pct_excluido = if_else(n_original > 0, 100*n_excluido/n_original, NA_real_))
  
  clean |>
    mutate(datetime_1m = floor_date(datetime, "minute")) |>
    group_by(datetime_1m) |>
    summarise(PM1 = mean_na(PM1), PM25 = mean_na(PM25), PM10 = mean_na(PM10),
              n_registros = n(), .groups = "drop") |>
    list(data = _, outliers = out)
}

rc <- load_grimm(file_cetam, "Cetam")
rm <- load_grimm(file_mt, "MT")
dc <- rc$data |> rename(PM1_C = PM1, PM25_C = PM25, PM10_C = PM10,
                        n_Cetam = n_registros)
dm <- rm$data |> rename(PM1_M = PM1, PM25_M = PM25, PM10_M = PM10,
                        n_MT = n_registros)
merged <- full_join(dc, dm, by = "datetime_1m") |>
  rename(datetime = datetime_1m) |> arrange(datetime)
outliers <- bind_rows(rc$outliers, rm$outliers)
write_csv(outliers, file.path(dir_tablas, "00_outliers_excluidos.csv"))
saveRDS(merged, file.path(dir_resultados, "datos_minuto.rds"))

cols <- list("PM1" = c("PM1_C", "PM1_M"),
             "PM2.5" = c("PM25_C", "PM25_M"),
             "PM10" = c("PM10_C", "PM10_M"))

pairs_for <- function(fr) {
  cc <- cols[[fr]]
  tibble(datetime = merged$datetime, Cetam = merged[[cc[1]]],
         MT = merged[[cc[2]]]) |>
    filter(is.finite(Cetam), is.finite(MT)) |>
    mutate(promedio = (Cetam + MT)/2, diferencia = MT-Cetam,
           abs_diferencia = abs(diferencia))
}

metrics_for <- function(d, fr) {
  n <- nrow(d)
  if (n < 2) return(tibble(fraccion=fr, n=n))
  den <- var(d$Cetam)+var(d$MT)+(mean(d$Cetam)-mean(d$MT))^2
  fit <- if (sd(d$Cetam)>0) lm(MT~Cetam, d) else NULL
  bias <- mean(d$diferencia); sdd <- sd(d$diferencia)
  tibble(fraccion=fr, n=n, media_Cetam=mean(d$Cetam), media_MT=mean(d$MT),
         Pearson=if(sd(d$Cetam)>0 && sd(d$MT)>0) cor(d$Cetam,d$MT) else NA_real_,
         R2_OLS=if(!is.null(fit)) summary(fit)$r.squared else NA_real_,
         CCC_Lin=if(den>0) 2*cov(d$Cetam,d$MT)/den else NA_real_,
         sesgo_MT_menos_Cetam=bias, LoA_inferior=bias-1.96*sdd,
         LoA_superior=bias+1.96*sdd, MAE=mean(abs(d$diferencia)),
         RMSE=sqrt(mean(d$diferencia^2)),
         PoA_1=mean(d$abs_diferencia<=1), PoA_2=mean(d$abs_diferencia<=2),
         PoA_5=mean(d$abs_diferencia<=5))
}
metrics <- bind_rows(lapply(names(cols), \(fr) metrics_for(pairs_for(fr), fr)))
write_csv(metrics, file.path(dir_tablas, "01_metricas_concordancia.csv"))

# -----------------------------------------------------------------------------
# NORMALIDAD: Cetam, MT y diferencias pareadas (MT - Cetam)
# -----------------------------------------------------------------------------
# Shapiro-Wilk admite entre 3 y 5000 observaciones. Si hay más, se toma una
# muestra reproducible de 5000 datos. Con n grande, el test detecta desviaciones
# pequeñas; por eso debe interpretarse junto con el histograma y el gráfico Q-Q.
shapiro_safe <- function(x, max_n = 5000, seed = 123) {
  x <- x[is.finite(x)]
  n_total <- length(x)
  
  if (n_total < 3 || length(unique(x)) < 2) {
    return(tibble(
      n = n_total, n_test = NA_integer_,
      W_Shapiro = NA_real_, p_Shapiro = NA_real_,
      resultado_05 = "No evaluable"
    ))
  }
  
  if (n_total > max_n) {
    set.seed(seed)
    x_test <- sample(x, max_n)
  } else {
    x_test <- x
  }
  
  test <- shapiro.test(x_test)
  
  tibble(
    n = n_total,
    n_test = length(x_test),
    W_Shapiro = unname(test$statistic),
    p_Shapiro = test$p.value,
    resultado_05 = if_else(
      test$p.value < 0.05,
      "Se rechaza normalidad",
      "No se rechaza normalidad"
    )
  )
}

normality_data <- function(fr) {
  d <- pairs_for(fr)
  
  bind_rows(
    tibble(fraccion = fr, variable = "Cetam", valor = d$Cetam),
    tibble(fraccion = fr, variable = "MT", valor = d$MT),
    tibble(fraccion = fr, variable = "Diferencia (MT - Cetam)",
           valor = d$diferencia)
  ) |>
    group_by(fraccion, variable) |>
    group_modify(\(.x, .y) shapiro_safe(.x$valor)) |>
    ungroup()
}

normality_results <- bind_rows(lapply(names(cols), normality_data))
write_csv(
  normality_results,
  file.path(dir_tablas, "02_analisis_normalidad.csv")
)

plot_normality_hist <- function(d, fr) {
  if (nrow(d) < 3) {
    return(plot_ly() |> layout(title = paste("Datos insuficientes:", fr)))
  }
  
  plot_ly(
    d, x = ~diferencia,
    type = "histogram",
    nbinsx = 40,
    marker = list(color = "#4472C4", line = list(color = "white", width = 1)),
    hovertemplate = "Diferencia: %{x:.4f} µg/m³<br>Frecuencia: %{y}<extra></extra>"
  ) |>
    layout(
      title = paste("Distribución de las diferencias —", fr),
      xaxis = list(title = "Diferencia MT − Cetam [µg/m³]"),
      yaxis = list(title = "Frecuencia"),
      bargap = 0.08
    )
}

plot_normality_qq <- function(d, fr) {
  x <- d$diferencia[is.finite(d$diferencia)]
  
  if (length(x) < 3 || length(unique(x)) < 2) {
    return(plot_ly() |> layout(title = paste("Datos insuficientes:", fr)))
  }
  
  qq <- qqnorm(x, plot.it = FALSE)
  
  # Recta de referencia normal (método de qqline: cuartiles 25 % y 75 %)
  y_quartiles <- unname(quantile(x, c(0.25, 0.75), na.rm = TRUE, type = 7))
  x_quartiles <- qnorm(c(0.25, 0.75))
  pendiente <- diff(y_quartiles) / diff(x_quartiles)
  intercepto <- y_quartiles[1] - pendiente * x_quartiles[1]
  x_linea <- range(qq$x, finite = TRUE)
  qq_line <- list(
    x = x_linea,
    y = intercepto + pendiente * x_linea
  )
  
  plot_ly(
    x = qq$x, y = qq$y,
    type = "scatter", mode = "markers",
    name = "Cuantiles observados",
    marker = list(size = 5, opacity = 0.6),
    hovertemplate = "Cuantil teórico: %{x:.3f}<br>Cuantil observado: %{y:.3f}<extra></extra>"
  ) |>
    add_lines(
      x = qq_line$x, y = qq_line$y,
      name = "Referencia normal",
      line = list(color = "#d62728", width = 2),
      inherit = FALSE,
      hoverinfo = "skip"
    ) |>
    layout(
      title = paste("Gráfico Q-Q de las diferencias —", fr),
      xaxis = list(title = "Cuantiles teóricos normales"),
      yaxis = list(title = "Cuantiles observados de MT − Cetam [µg/m³]")
    )
}


outlier_note <- function(fr) {
  
  z <- outliers |> filter(fraccion == fr)
  
  a <- z$pct_excluido[z$instrumento == "Cetam"]
  b <- z$pct_excluido[z$instrumento == "MT"]
  
  paste0(
    "Método: ", outlier_method_label(),
    "<br>Cetam: ",
    round(ifelse(length(a), a[1], NA_real_), 2),
    "% excluido",
    "<br>MT: ",
    round(ifelse(length(b), b[1], NA_real_), 2),
    "% excluido"
  )
}

plot_ba <- function(d, fr) {
  if(nrow(d)<2) return(plot_ly() |> layout(title=paste("Datos insuficientes:",fr)))
  bias <- mean(d$diferencia); s <- sd(d$diferencia)
  xr <- range(d$promedio)
  plot_ly(d, x=~promedio, y=~diferencia, type="scatter", mode="markers",
          text=~paste0("Fecha: ",datetime,"<br>Cetam: ",signif(Cetam,4),
                       "<br>MT: ",signif(MT,4),"<br>MT − Cetam: ",signif(diferencia,4)),
          hoverinfo="text", marker=list(size=5,opacity=.55)) |>
    add_segments(x=xr[1],xend=xr[2],y=bias,yend=bias,name="Sesgo medio",
                 line=list(color="#d62728",width=2)) |>
    add_segments(x=xr[1],xend=xr[2],y=bias+1.96*s,yend=bias+1.96*s,
                 name="Límite superior 95%",line=list(color="#1f77b4",dash="dash")) |>
    add_segments(x=xr[1],xend=xr[2],y=bias-1.96*s,yend=bias-1.96*s,
                 name="Límite inferior 95%",line=list(color="#1f77b4",dash="dash")) |>
    layout(title=paste("Bland–Altman —",fr),
           xaxis=list(title="Promedio (Cetam + MT)/2 [µg/m³]"),
           yaxis=list(title="Diferencia MT − Cetam [µg/m³]"),
           annotations=list(list(x=.99,y=.99,xref="paper",yref="paper",xanchor="right",
                                 yanchor="top",text=outlier_note(fr),showarrow=FALSE,align="right",
                                 bgcolor="rgba(255,255,255,.9)",bordercolor="#999",borderwidth=1,borderpad=4)))
}

# Gráfico de dispersión con regresión OLS y recta de identidad
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
          text = ~paste0("Fecha: ", datetime, "<br>Cetam: ", signif(Cetam, 4),
                         "<br>MT: ", signif(MT, 4)), 
          hoverinfo = "text",
          marker = list(size = 5, opacity = .55)) |>
    add_lines(x = xreg, y = yreg, inherit = FALSE,
              name = "Regresión OLS", line = list(color = "#d62728", width = 2),
              hoverinfo = "skip") |>
    add_lines(x = c(0, lim), y = c(0, lim), inherit = FALSE,
              name = "Identidad y=x", line = list(color = "#222", dash = "dash"),
              hoverinfo = "skip") |>
    layout(title = paste("Scatter Cetam vs. MT —", fr),
           xaxis = list(title = "Cetam [µg/m³]", rangemode = "tozero"),
           yaxis = list(title = "MT [µg/m³]", rangemode = "tozero"),
           annotations = list(list(x = .02, y = .98, xref = "paper", yref = "paper", 
                                   xanchor = "left", yanchor = "top", text = note, 
                                   showarrow = FALSE, align = "left",
                                   bgcolor = "rgba(255,255,255,.9)", 
                                   bordercolor = "#999", borderwidth = 1, borderpad = 5)))
}
# -----------------------------------------------------------------------------

plot_poa <- function(d, fr) {
  if(nrow(d)<2) return(plot_ly() |> layout(title=paste("Datos insuficientes:",fr)))
  dd <- d$abs_diferencia
  xmax <- max(TOLERANCIA_INICIAL,as.numeric(quantile(dd,.99,na.rm=TRUE)),1)
  ts <- seq(0,xmax,length.out=301)
  ps <- vapply(ts,\(t) mean(dd<=t),numeric(1))
  tol0 <- min(TOLERANCIA_INICIAL,xmax)
  p <- plot_ly(x=ts,y=ps,type="scatter",mode="lines",name="PoA",
               line=list(color="#1f77b4",width=3),
               hovertemplate="Tolerancia: %{x:.3f} µg/m³<br>PoA: %{y:.2%}<extra></extra>") |>
    layout(title=paste("Probability of Agreement —",fr),
           xaxis=list(title="Tolerancia absoluta |MT − Cetam| [µg/m³]",range=c(0,xmax)),
           yaxis=list(title="Probabilidad empírica",tickformat=".0%",range=c(0,1)),
           shapes=list(list(type="line",xref="x",yref="paper",x0=tol0,x1=tol0,y0=0,y1=1,
                            line=list(color="#d62728",width=2,dash="dash"))))
  sid <- paste0("tol_",gsub("[^A-Za-z0-9]","",fr))
  vid <- paste0(sid,"_value")
  p <- onRender(p,sprintf(
    "function(el,x){
      var s=document.getElementById('%s'),v=document.getElementById('%s');
      if(!s)return;
      var ts=%s,ps=%s;
      function upd(){var t=+s.value,i=0;while(i+1<ts.length&&ts[i+1]<=t)i++;
        Plotly.relayout(el,{'shapes[0].x0':t,'shapes[0].x1':t});
        v.textContent='±'+t.toFixed(2)+' µg/m³; PoA = '+(100*ps[i]).toFixed(2)+'%%';}
      s.addEventListener('input',upd);upd();
    }",sid,vid,jsonlite::toJSON(ts,auto_unbox=TRUE),
    jsonlite::toJSON(ps,auto_unbox=TRUE)))
  attr(p,"slider") <- tagList(tags$label(`for`=sid,paste0("Tolerancia para ",fr,": ")),
                              tags$input(id=sid,type="range",min=0,max=xmax,step=max(xmax/500,.01),
                                         value=tol0,style="width:70%;"),
                              tags$span(id=vid,style="margin-left:12px;font-weight:bold;"))
  p
}

plot_ts <- function(fr) {
  cc <- cols[[fr]]
  d <- merged |> select(datetime, Cetam=all_of(cc[1]), MT=all_of(cc[2])) |>
    pivot_longer(c(Cetam,MT),names_to="instrumento",values_to="valor") |>
    filter(!is.na(datetime))
  plot_ly(d,x=~datetime,y=~valor,color=~instrumento,
          colors=c(Cetam="#1f77b4",MT="#ff7f0e"),type="scatter",mode="lines") |>
    layout(title=paste("Serie temporal —",fr),
           xaxis=list(title="Fecha",type="date",rangeslider=list(visible=TRUE),
                      rangeselector=list(buttons=list(
                        list(count=1,label="1 día",step="day",stepmode="backward"),
                        list(count=7,label="1 semana",step="day",stepmode="backward"),
                        list(count=1,label="1 mes",step="month",stepmode="backward"),
                        list(step="all",label="Todo")))),
           yaxis=list(title="Concentración [µg/m³]",autorange=TRUE),hovermode="x unified")
}

# HTML independientes para cada Mass Value y para cada serie temporal.
# Cada dashboard contiene únicamente Bland–Altman, PoA y Scatter de una fracción.
for (fr in names(cols)) {
  d <- pairs_for(fr)
  ba <- plot_ba(d, fr)
  sc <- plot_scatter(d, fr)
  poa <- plot_poa(d, fr)
  hist_normalidad <- plot_normality_hist(d, fr)
  qq_normalidad <- plot_normality_qq(d, fr)
  normalidad_fr <- normality_results |> filter(fraccion == fr)
  
  dashboard_fr <- tagList(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title(paste("GRIMM Cetam vs MT —", fr)),
      tags$style(HTML(
        "body{font-family:Arial,sans-serif;max-width:1450px;margin:24px auto;padding:0 18px;color:#222}.plotly{width:100%!important}input[type=range]{accent-color:#d62728}"
      ))
    ),
    tags$h1(paste("Comparación de instrumentos GRIMM: Cetam vs MT —", fr)),
    tags$p(paste0(
      "Mass Value: ", fr,
      " · método de outliers: ", outlier_method_label(),
      " · diferencia = MT − Cetam"
    )),
    tags$p(paste("Pares completos disponibles:", nrow(d))),
    tags$p(paste(
      "PoA es la proporción empírica de pares con |MT − Cetam| menor o igual que la tolerancia seleccionada.",
      "La autocorrelación temporal puede afectar la inferencia estadística."
    )),
    tags$section(
      style = "margin:24px 0;padding:18px;border:1px solid #ddd;border-radius:10px;",
      tags$h2("Análisis de normalidad"),
      tags$p(paste(
        "Prueba de Shapiro–Wilk para Cetam, MT y las diferencias pareadas.",
        "Nivel de significación: 0,05. Un p-valor menor que 0,05 aporta evidencia contra la normalidad."
      )),
      htmltools::HTML(
        paste0(
          "<table style='border-collapse:collapse;width:100%;'>",
          "<thead><tr>",
          "<th style='text-align:left;padding:8px;border-bottom:1px solid #ccc;'>Variable</th>",
          "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>n</th>",
          "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>n evaluado</th>",
          "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>W</th>",
          "<th style='text-align:right;padding:8px;border-bottom:1px solid #ccc;'>p-valor</th>",
          "<th style='text-align:left;padding:8px;border-bottom:1px solid #ccc;'>Conclusión (α = 0,05)</th>",
          "</tr></thead><tbody>",
          paste0(
            apply(normalidad_fr, 1, function(row) {
              paste0(
                "<tr><td style='padding:8px;border-bottom:1px solid #eee;'>", row[["variable"]], "</td>",
                "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["n"]], "</td>",
                "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>", row[["n_test"]], "</td>",
                "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>",
                ifelse(is.na(as.numeric(row[["W_Shapiro"]])), "NA", signif(as.numeric(row[["W_Shapiro"]]), 5)), "</td>",
                "<td style='text-align:right;padding:8px;border-bottom:1px solid #eee;'>",
                ifelse(is.na(as.numeric(row[["p_Shapiro"]])), "NA", format.pval(as.numeric(row[["p_Shapiro"]]), digits = 4)), "</td>",
                "<td style='padding:8px;border-bottom:1px solid #eee;'>", row[["resultado_05"]], "</td></tr>"
              )
            }),
            collapse = ""
          ),
          "</tbody></table>"
        )
      ),
      tags$p(
        "La decisión no debe basarse solo en el p-valor: revisa también el histograma y el gráfico Q-Q. ",
        "La prueba de Shapiro–Wilk no evalúa independencia temporal."
      ),
      hist_normalidad,
      qq_normalidad
    ),
    tags$section(
      style = "margin:24px 0;padding:18px;border:1px solid #ddd;border-radius:10px;",
      tags$h2("Bland–Altman"), ba,
      tags$h2("Probability of Agreement"),
      attr(poa, "slider"), poa,
      tags$h2("Scatter plot"), sc
    )
  )
  
  nombre_fr <- gsub("\\.", "", fr)
  archivo_dashboard <- file.path(
    dir_html, paste0("Dashboard_Concordancia_", nombre_fr, ".html")
  )
  carpeta_lib <- file.path(
    dir_html, paste0("Dashboard_Concordancia_", nombre_fr, "_lib")
  )
  
  htmltools::save_html(
    dashboard_fr,
    file = archivo_dashboard,
    libdir = carpeta_lib
  )
  
  # Se conserva además un HTML separado para la serie temporal de cada fracción.
  htmlwidgets::saveWidget(
    plot_ts(fr),
    file.path(dir_html, paste0("Serie_Temporal_", nombre_fr, ".html")),
    selfcontained = TRUE
  )
}

cat(
  "\nListo.\nDashboards de concordancia (uno por Mass Value):\n",
  paste0(" - ", file.path(dir_html, "Dashboard_Concordancia_PM1.html"), "\n"),
  paste0(" - ", file.path(dir_html, "Dashboard_Concordancia_PM25.html"), "\n"),
  paste0(" - ", file.path(dir_html, "Dashboard_Concordancia_PM10.html"), "\n"),
  "Series temporales: ", dir_html,
  "\nTablas: ", dir_tablas, "\n",
  sep = ""
)

