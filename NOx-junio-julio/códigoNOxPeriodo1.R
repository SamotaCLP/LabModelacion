# 1. Periodo 1 y lectura de NOx total -----------------------------------------
# Ejecutar desde la carpeta de la campaña. Solo requiere R base.
# 42i con sonda y 42iQ directo. Se excluye el 25/07 (hora de rotación incierta).
# UTC conserva las marcas de los relojes sin aplicar cambios de hora local.
inicio <- as.POSIXct("2025-05-22 00:00:00", tz = "UTC")
fin <- as.POSIXct("2025-07-25 00:00:00", tz = "UTC")

# La descarga de julio del 42iQ contiene los registros de mayo y junio.
archivo_Q <- "12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv"
archivos_i <- c("42i 053025 1425_Sonda.dat", "NOx_42i_Portillo_junio_2025_Sonda.dat",
                "NOx_42i_julio_Portillo_Sonda.dat")
if (!all(file.exists(c(archivo_Q, archivos_i)))) {
  stop("Selecciona como directorio de trabajo la carpeta con los archivos de campaña.")
}

# El CSV tiene una coma final en los registros que falta en el encabezado.
campos_Q <- strsplit(readLines(archivo_Q, n = 1), ",", fixed = TRUE)[[1]]
csv_Q <- read.csv(archivo_Q, skip = 1, header = FALSE, check.names = FALSE,
                  col.names = c(campos_Q, "Campo_final"))
serie_Q <- data.frame(
  minuto = as.POSIXct(csv_Q[["Time Stamp"]], format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
  NOx_Q = csv_Q[["NOx Concentration (ppb or ug/m3)"]], alarma = csv_Q[["General Alarm"]])
leer_i <- function(archivo) {
  z <- read.table(archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
  data.frame(minuto = as.POSIXct(paste(z$Date, z$Time), format = "%m-%d-%y %H:%M", tz = "UTC"),
             NOx_i = z$nox, bandera = z$Flags)
}
serie_i <- do.call(rbind, lapply(archivos_i, leer_i))

# 2. Ordenar y verificar las series del periodo ----
ordenar_periodo <- function(z) {
  if (anyNA(z$minuto)) stop("Revisar marcas de tiempo que no pudieron leerse.")
  z <- unique(subset(z, minuto >= inicio & minuto < fin))
  if (anyDuplicated(z$minuto)) stop("Hay lecturas distintas para el mismo instrumento y minuto.")
  z[order(z$minuto), ]
}
serie_Q <- ordenar_periodo(serie_Q)
serie_i <- ordenar_periodo(serie_i)

# 3. Filtrar el 42iQ y emparejar los minutos válidos ----
# Detectar constantes antes de convertir negativos a cero. Una brecha corta la racha.
repetido <- c(FALSE, diff(as.numeric(serie_Q$minuto)) == 60 & diff(serie_Q$NOx_Q) == 0)
repetido[is.na(repetido)] <- FALSE
racha <- cumsum(!repetido)
serie_Q$constante <- tabulate(racha)[racha] >= 4L
pares <- merge(serie_i, serie_Q, by = "minuto")
pares <- subset(pares, bandera == "--------" & alarma == 0 & !constante &
                  is.finite(NOx_i) & is.finite(NOx_Q) & NOx_Q >= -3)
pares$NOx_Q_filtrado <- pmax(0, pares$NOx_Q)

# 4. Construir las medias de 60 minutos en las mismas horas ----
# Ventanas evaluadas al cierre de cada hora de reloj. Ambos equipos usan
# exactamente los mismos minutos. Se exigen al menos 45 de los 60 posibles.
pares$hora <- as.POSIXct(trunc(pares$minuto, "hours"), tz = "UTC")
promedios <- aggregate(cbind(NOx_i, NOx_Q_filtrado) ~ hora, pares, mean)
conteos <- aggregate(minuto ~ hora, pares, length)
names(conteos)[2] <- "minutos_validos"
horas_periodo1 <- subset(merge(promedios, conteos, by = "hora"), minutos_validos >= 45)

# 5. Ajustar el cero del 42i con el criterio P25 de George ----
# Análisis descriptivo del periodo completo. En una escala min-max, restar
# el P25 y volver a la amplitud original equivale a restar el P25 original.
# Así se conserva la unidad para comparar con el 42iQ. Bajo el nuevo cero:
# hora descartada para los dos equipos. El P25 es un cero provisional.
cero_i <- unname(quantile(horas_periodo1$NOx_i, 0.25))
horas_periodo1$NOx_i_ajustado <- horas_periodo1$NOx_i - cero_i
comparacion_periodo1 <- subset(horas_periodo1, NOx_i_ajustado >= 0)
cat(sprintf("Cero P25 del 42i: %.4f | Horas retenidas: %d de %d (%.1f%%)\n",
            cero_i, nrow(comparacion_periodo1), nrow(horas_periodo1),
            100 * nrow(comparacion_periodo1) / nrow(horas_periodo1)))

# 6. Calcular Pearson, CCC de Lin y límites de Bland-Altman ----
x <- comparacion_periodo1$NOx_i_ajustado
y <- comparacion_periodo1$NOx_Q_filtrado
d <- y - x
media_par <- (x + y) / 2
vx <- mean((x - mean(x))^2)
vy <- mean((y - mean(y))^2)
ccc <- 2 * mean((x - mean(x)) * (y - mean(y))) / (vx + vy + mean(d)^2)
limites_BA <- mean(d) + c(-1, 1) * 1.96 * sd(d)
# Los percentiles complementan los límites clásicos cuando hay asimetría.
resumen_periodo1 <- data.frame(horas = length(d), Pearson = cor(x, y), CCC = ccc,
                              sesgo_Q_menos_i = mean(d), BA_inferior = limites_BA[1],
                              BA_superior = limites_BA[2], P025 = unname(quantile(d, 0.025)),
                              P975 = unname(quantile(d, 0.975)))
print(resumen_periodo1, digits = 5, row.names = FALSE)

# 7. Estimar la probabilidad empírica de acuerdo ----
# Márgenes ilustrativos en la escala original. Cada hora retenida pesa igual.
tolerancias <- c(5, 10, 20)
acuerdo_periodo1 <- data.frame(tolerancia = tolerancias,
                               horas_dentro = vapply(tolerancias, function(t) sum(abs(d) <= t), integer(1)))
acuerdo_periodo1$porcentaje <- 100 * acuerdo_periodo1$horas_dentro / length(d)
print(acuerdo_periodo1, digits = 4, row.names = FALSE)

# 8. Graficar las series simultáneas y el acuerdo ----
graficar_periodo1 <- function() {
  ajustes <- par(no.readonly = TRUE)
  on.exit(par(ajustes))
  layout(matrix(c(1, 1, 2, 3), 2, byrow = TRUE))
  par(mar = c(4.2, 4.5, 2.8, 1))
  reloj <- seq(min(horas_periodo1$hora), max(horas_periodo1$hora), by = "hour")
  pos <- match(reloj, comparacion_periodo1$hora)
  azul <- "#15658B"
  naranja <- "#A74523"
  plot(reloj, y[pos], type = "o", pch = 16, cex = 0.25, col = azul, ylim = range(x, y),
       main = "Periodo 1 | NOx horario en los minutos compartidos",
       xlab = "Fecha de 2025", ylab = "NOx en escala original")
  lines(reloj, x[pos], type = "o", pch = 16, cex = 0.25, col = naranja)
  legend("topright", c("42iQ directo, filtrado", "42i con sonda, cero P25"),
         col = c(azul, naranja), lty = 1, bty = "n")
  plot(media_par, d, pch = 16, cex = 0.65, col = "#15658B80",
       main = "Bland-Altman", xlab = "Promedio de ambos equipos",
       ylab = "42iQ filtrado − 42i ajustado", ylim = range(d, limites_BA, 0))
  abline(h = 0, col = "grey65")
  abline(h = mean(d), col = naranja, lwd = 2)
  abline(h = limites_BA, col = naranja, lty = 2)
  plot(ecdf(abs(d)), do.points = FALSE, verticals = TRUE, col = azul, lwd = 2,
       main = "Probabilidad empírica de acuerdo", xlab = "Tolerancia absoluta",
       ylab = "Fracción de horas", sub = "", xlim = c(0, max(abs(d))))
  points(tolerancias, acuerdo_periodo1$porcentaje / 100, pch = 16, col = naranja)
}
if (interactive()) graficar_periodo1()
dir.create("Gráficas", showWarnings = FALSE)
png(file.path("Gráficas", "Periodo1_NOx_ajustado.png"), width = 1750, height = 1050, res = 145)
tryCatch(graficar_periodo1(), finally = dev.off())

# 9. Localizar un cambio sostenido de amplitud en el 42i sin ajuste ----------
# Ventanas de reloj de 6 horas con >= 5 medias horarias disponibles.
# La amplitud se resume por el rango dentro de la ventana. Elegimos un corte
# que minimiza las desviaciones absolutas respecto a las medianas de los dos
# tramos en log(1 + rango). Se exige una semana de bloques observados por lado.
buscar_corte <- function(ancho) {
  bloque <- floor(as.numeric(horas_periodo1$hora) / (3600 * ancho))
  grupos <- split(horas_periodo1, bloque)
  grupos <- grupos[lengths(lapply(grupos, "[[", "hora")) >= ceiling(0.75 * ancho)]
  bloques <- do.call(rbind, lapply(grupos, function(z) {
    data.frame(desde = min(z$hora), hasta = max(z$hora), rango = diff(range(z$NOx_i)))
  }))
  bloques <- bloques[order(bloques$desde), ]
  v <- log1p(bloques$rango)
  minimo <- ceiling(168 / ancho)
  candidatos <- seq.int(minimo, length(v) - minimo)
  perdida <- function(x) sum(abs(x - median(x)))
  costo <- vapply(candidatos, function(i) perdida(v[1:i]) + perdida(v[-(1:i)]), numeric(1))
  i <- candidatos[which.min(costo)]
  data.frame(ventana_h = ancho, ultima_hora_previa = bloques$hasta[i],
             primera_hora_posterior = bloques$desde[i + 1L],
             rango_mediano_previo = median(bloques$rango[1:i]),
             rango_mediano_posterior = median(bloques$rango[-(1:i)]))
}
cortes_42i <- do.call(rbind, lapply(c(3, 6, 12), buscar_corte))
print(cortes_42i, row.names = FALSE)
corte_42i <- cortes_42i$primera_hora_posterior[cortes_42i$ventana_h == 6]

# 10. Comparar 1-1 y 1-2 con los mismos filtros y el mismo cero -------------
# Un único P25 conserva la escala de comparación. Se informa su retención en
# cada régimen y la correlación previa al P25 para hacer visible la selección.
horas_periodo1$tramo <- ifelse(horas_periodo1$hora < corte_42i, "1-1", "1-2")
resumir_tramo <- function(z) {
  ok <- z$NOx_i_ajustado >= 0
  x <- z$NOx_i_ajustado[ok]
  y <- z$NOx_Q_filtrado[ok]
  d <- y - x
  data.frame(tramo = z$tramo[1], disponibles = nrow(z), retenidas = length(x),
             r_antes_P25 = cor(z$NOx_i, z$NOx_Q_filtrado), Pearson = cor(x,y),
             CCC = 2 * mean((x-mean(x))*(y-mean(y))) /
               (mean((x-mean(x))^2) + mean((y-mean(y))^2) + mean(d)^2),
             sesgo = mean(d), BA_inf = mean(d)-1.96*sd(d), BA_sup = mean(d)+1.96*sd(d),
             acuerdo_5 = mean(abs(d)<=5), acuerdo_10 = mean(abs(d)<=10),
             acuerdo_20 = mean(abs(d)<=20))
}
tramos_42i <- split(horas_periodo1, horas_periodo1$tramo)
resumen_subperiodos <- do.call(rbind, lapply(tramos_42i, resumir_tramo))
print(resumen_subperiodos, digits = 4, row.names = FALSE)

# 11. Visualizar las dos configuraciones temporales del mismo montaje ----
graficar_subperiodos <- function() {
  ajustes <- par(mfrow = c(2,1), mar = c(4,4,3,1))
  on.exit(par(ajustes))
  reloj <- seq(min(horas_periodo1$hora), max(horas_periodo1$hora), by = "hour")
  idx <- match(reloj, horas_periodo1$hora)
  plot(reloj, horas_periodo1$NOx_i[idx], type = "l", col = "#A74523",
       xlab = "", ylab = "NOx original", main = "Detección | 42i antes del ajuste P25")
  abline(v = as.numeric(corte_42i), lty = 2)
  k <- match(reloj, comparacion_periodo1$hora)
  plot(reloj, y[k], type = "l", col = "#15658B", ylim = range(x,y),
       xlab = "Fecha de 2025", ylab = "NOx ajustado / filtrado",
       main = "Comparación | 1-1 a la izquierda del corte y 1-2 a la derecha")
  lines(reloj, x[k], col = "#A74523")
  abline(v = as.numeric(corte_42i), lty = 2)
  legend("topright", c("42iQ filtrado","42i ajustado"), col=c("#15658B","#A74523"),lty=1,bty="n")
}
if (interactive()) graficar_subperiodos()
png(file.path("Gráficas", "Periodo1_subperiodos.png"), width=1600, height=900, res=135)
tryCatch(graficar_subperiodos(), finally = dev.off())

# 12. Aislar 1-1 y congelar el cero antes de la validación -------------------
# Corte operativo establecido en la sección 9. Este bloque también puede
# ejecutarse directamente después de la sección 4, sin procesar 1-2 en el modelo.
fin_11 <- as.POSIXct("2025-06-21 00:00:00", tz = "UTC")
horas_11 <- subset(horas_periodo1, hora >= inicio & hora < fin_11,
                   select = c(hora, NOx_i, NOx_Q_filtrado, minutos_validos))
horas_11 <- horas_11[order(horas_11$hora), ]
dia_11 <- as.Date(horas_11$hora, tz = "UTC")
dias_11 <- sort(unique(dia_11))
# Primer 70 % de días disponibles (redondeando hacia abajo); días enteros.
inicio_prueba_11 <- dias_11[floor(0.7 * length(dias_11)) + 1L]
horas_11$muestra <- ifelse(dia_11 < inicio_prueba_11, "Ajuste", "Prueba")
cero_11 <- unname(quantile(horas_11$NOx_i[horas_11$muestra == "Ajuste"], 0.25))
horas_11$NOx_i_ajustado <- horas_11$NOx_i - cero_11
pares_11 <- subset(horas_11, NOx_i_ajustado >= 0)

# 13. Predecir el 42iQ a partir del 42i y evaluar el error temporal ----
ajuste_11 <- subset(pares_11, muestra == "Ajuste")
prueba_11 <- subset(pares_11, muestra == "Prueba")
modelo_11 <- lm(NOx_Q_filtrado ~ NOx_i_ajustado, data = ajuste_11)
error_11 <- predict(modelo_11, newdata = prueba_11) - prueba_11$NOx_Q_filtrado
mae_11 <- mean(abs(error_11))
rmse_11 <- sqrt(mean(error_11^2))
mae_base_11 <- mean(abs(median(ajuste_11$NOx_Q_filtrado) - prueba_11$NOx_Q_filtrado))
dif_11 <- pares_11$NOx_Q_filtrado - pares_11$NOx_i_ajustado
limites_11 <- mean(dif_11) + c(-1, 1) * 1.96 * sd(dif_11)
print(coef(modelo_11))
print(data.frame(P25 = cero_11, horas = nrow(horas_11), ajuste = nrow(ajuste_11),
                 prueba = nrow(prueba_11), inicio_prueba = inicio_prueba_11,
                 R2_ajuste = summary(modelo_11)$r.squared, MAE_prueba = mae_11,
                 RMSE_prueba = rmse_11, MAE_mediana = mae_base_11), row.names = FALSE)

# 14. Estudiar la forma de las curvas en el subperiodo 1-2 ----
# Explorar todas las horas válidas, antes del recorte P25. El min-max de
# cada instrumento se calcula en este tramo y solo se usa para visualizar.
horas_12 <- subset(horas_periodo1, hora >= fin_11 & hora < fin)
normalizar_forma <- function(v) {
  amplitud <- diff(range(v))
  if (amplitud == 0) return(rep(NA_real_, length(v)))
  (v - min(v)) / amplitud
}
horas_12$forma_i <- normalizar_forma(horas_12$NOx_i)
horas_12$forma_Q <- normalizar_forma(horas_12$NOx_Q_filtrado)
r_forma_12 <- with(horas_12, cor(NOx_i, NOx_Q_filtrado))

# 15. Repetir la comparación y validar una recta propia del 1-2 ----
dia_12 <- as.Date(horas_12$hora, tz = "UTC")
dias_12 <- sort(unique(dia_12))
inicio_prueba_12 <- dias_12[floor(0.7 * length(dias_12)) + 1L]
horas_12$muestra <- ifelse(dia_12 < inicio_prueba_12, "Ajuste", "Prueba")
cero_12 <- unname(quantile(horas_12$NOx_i[horas_12$muestra == "Ajuste"], 0.25))
horas_12$NOx_i_ajustado <- horas_12$NOx_i - cero_12
pares_12 <- subset(horas_12, NOx_i_ajustado >= 0)
ajuste_12 <- subset(pares_12, muestra == "Ajuste")
prueba_12 <- subset(pares_12, muestra == "Prueba")
modelo_12 <- lm(NOx_Q_filtrado ~ NOx_i_ajustado, data = ajuste_12)
error_12 <- predict(modelo_12, newdata = prueba_12) - prueba_12$NOx_Q_filtrado

acuerdo_tratado <- function(z) {
  i <- z$NOx_i_ajustado
  q <- z$NOx_Q_filtrado
  delta <- q - i
  vi <- mean((i - mean(i))^2)
  vq <- mean((q - mean(q))^2)
  list(pearson = cor(i, q), ccc = 2 * mean((i - mean(i)) * (q - mean(q))) /
         (vi + vq + mean(delta)^2), sesgo = mean(delta),
       limites = mean(delta) + c(-1, 1) * 1.96 * sd(delta),
       tolerancias = c(5, 10, 20),
       acuerdo = vapply(c(5, 10, 20), function(t) mean(abs(delta) <= t), numeric(1)))
}
diag_11 <- acuerdo_tratado(pares_11)
diag_12 <- acuerdo_tratado(pares_12)
panel_12 <- list(
  serie = data.frame(t = format(horas_12$hora, "%Y-%m-%d %H:%M", tz = "UTC"),
                     i = horas_12$NOx_i, q = horas_12$NOx_Q_filtrado,
                     fi = horas_12$forma_i, fq = horas_12$forma_Q),
  r_forma = r_forma_12, amplitud_i = diff(range(horas_12$NOx_i)),
  amplitud_q = diff(range(horas_12$NOx_Q_filtrado)), acuerdo = diag_12,
  entrenamiento = nrow(ajuste_12), prueba = nrow(prueba_12), cero = cero_12,
  inicio_prueba = format(inicio_prueba_12, "%d/%m/%Y"), r2 = summary(modelo_12)$r.squared,
  mae = mean(abs(error_12)), rmse = sqrt(mean(error_12^2)),
  base = mean(abs(median(ajuste_12$NOx_Q_filtrado) - prueba_12$NOx_Q_filtrado)))
print(panel_12[names(panel_12) != "serie"])

# 15.1 Sensibilidad a horas extremas del 42i en 1-2 ----
# Regla bilateral de una sola pasada sobre las horas válidas antes del P25.
# Se conserva el cero previamente estimado para aislar el efecto de retirar pares.
limites_sd <- mean(horas_12$NOx_i) + c(-3, 3) * sd(horas_12$NOx_i)
extrema <- horas_12$NOx_i < limites_sd[1] | horas_12$NOx_i > limites_sd[2]
sin_extremas <- horas_12[!extrema, ]
panel_12$serie$extrema <- extrema
panel_12$serie$fi_sin <- panel_12$serie$fq_sin <- NA_real_
panel_12$serie$fi_sin[!extrema] <- normalizar_forma(sin_extremas$NOx_i)
panel_12$serie$fq_sin[!extrema] <- normalizar_forma(sin_extremas$NOx_Q_filtrado)
retenidas_sd <- subset(sin_extremas, NOx_i_ajustado >= 0)
panel_12$sensibilidad <- list(limites = limites_sd, amplitud = diff(range(sin_extremas$NOx_i)),
  r = with(sin_extremas, cor(NOx_i, NOx_Q_filtrado)), n = nrow(retenidas_sd),
  acuerdo = acuerdo_tratado(retenidas_sd))
print(horas_12[extrema, c("hora", "NOx_i", "NOx_Q_filtrado")])
print(panel_12$sensibilidad)

# 15.2 Evaluar el ascenso del nivel bajo del 42i por día completo ----
# Corrección retrospectiva y aditiva. El nivel de referencia de cada método
# es la mediana de sus niveles bajos durante los días de ajuste.
por_dia <- split(horas_12, dia_12)
por_dia <- Filter(function(z) length(unique(z$hora)) == 24L, por_dia)
niveles <- data.frame(dia = names(por_dia),
  minimo = vapply(por_dia, function(z) min(z$NOx_i), numeric(1)),
  p10 = vapply(por_dia, function(z) unname(quantile(z$NOx_i, .10)), numeric(1)))
dia_indice <- match(as.character(dia_12), niveles$dia)
dias_desde_inicio <- as.numeric(as.Date(niveles$dia) - min(as.Date(niveles$dia)))
ajustar_nivel <- function(nivel) {
  referencia <- median(nivel[as.Date(niveles$dia) < inicio_prueba_12])
  horas_12$NOx_i - nivel[dia_indice] + referencia
}
panel_12$serie$i_p10 <- ajustar_nivel(niveles$p10)
panel_12$serie$i_min <- ajustar_nivel(niveles$minimo)
# Comparar sobre los mismos pares del análisis tratado. Mantener el P25 y
# conservar incluso los valores que queden negativos tras la corrección.
comunes <- !is.na(dia_indice) & horas_12$NOx_i_ajustado >= 0
comparar_nivel <- function(i) {
  z <- horas_12[comunes, ]
  z$NOx_i_ajustado <- i[comunes] - cero_12
  acuerdo_tratado(z)
}
panel_12$nivel <- list(dias = niveles, n = sum(comunes),
  pendientes = vapply(niveles[c("minimo", "p10")], function(v)
    unname(coef(lm(v ~ dias_desde_inicio))[2]), numeric(1)),
  original = comparar_nivel(horas_12$NOx_i),
  minimo = comparar_nivel(panel_12$serie$i_min), p10 = comparar_nivel(panel_12$serie$i_p10))

# 16. Generar el dashboard HTML autocontenido del avance ----
# Requiere los paquetes plotly, jsonlite y base64enc. La plantilla contiene
# solo la presentación. Los datos, límites, recta y errores se obtienen en R.
js_plotly <- system.file("htmlwidgets/lib/plotlyjs/plotly-latest.min.js", package = "plotly")
if (!nzchar(js_plotly) || !requireNamespace("jsonlite", quietly = TRUE) ||
    !requireNamespace("base64enc", quietly = TRUE)) {
  stop("Para generar el HTML instala en RStudio: install.packages(c('plotly','jsonlite','base64enc'))")
}
grilla_11 <- seq(inicio, fin_11 - 3600, by = "hour")
filas_11 <- match(grilla_11, pares_11$hora)

# 16.1 Comparar tres tratamientos dentro de cada tramo ----
ciclos_html <- jsonlite::read_json(file.path("plantillas", "ciclos_diarios_NOx.json"),
                                   simplifyVector = FALSE)
comparar_tramo <- function(z) {
  dia <- as.Date(substr(z$t, 1, 10))
  dias <- sort(unique(dia))
  corte <- dias[floor(length(dias) * .7) + 1L]
  ajuste <- dia < corte
  cero <- unname(quantile(z$i[ajuste], .25))
  umbral <- mean(z$i) + 3 * sd(z$i)
  filtrado <- replace(z$i, z$i > umbral, NA_real_)
  # Usar días completos antes del filtro. Después se toma el mínimo de las
  # horas conservadas. Las horas excluidas siguen siendo huecos.
  completos <- ave(z$i, dia, FUN = length) == 24
  bajos <- tapply(filtrado[completos], as.character(dia[completos]), min, na.rm = TRUE)
  bajos[!is.finite(bajos)] <- NA_real_
  por_dia <- split(z$i[completos], as.character(dia[completos]))
  min_original <- vapply(por_dia, min, numeric(1))
  p10 <- vapply(por_dia, quantile, numeric(1), probs=.10, names=FALSE)
  ref <- median(bajos[as.Date(names(bajos)) < corte], na.rm = TRUE)
  z$filtro <- filtrado
  z$combinado <- filtrado - as.numeric(bajos[as.character(dia)]) + ref
  z$retenida <- z$i >= cero
  z$comun <- z$retenida & is.finite(z$combinado)
  medir <- function(x, q) acuerdo_tratado(data.frame(NOx_i_ajustado=x, NOx_Q_filtrado=q))
  diagnostico_bajo <- function(nivel) {
    referencia <- median(nivel[as.Date(names(nivel)) < corte])
    x <- z$i - nivel[as.character(dia)] + referencia - cero
    usar <- completos & z$retenida
    medir(x[usar], z$q[usar])
  }
  disponibles <- lapply(z[c("i","filtro","combinado")], function(v) {
    ok <- z$retenida & is.finite(v)
    list(n=sum(ok), acuerdo=medir(v[ok]-cero,z$q[ok]))
  })
  comun <- lapply(z[c("i","filtro","combinado")], function(v)
    medir(v[z$comun] - cero, z$q[z$comun]))

  # 16.2 Validación posterior del desplazamiento constante y la recta ----
  # Estimar con el 42i original tras P25, sin información de días futuros.
  a <- z[ajuste & z$retenida, ]
  b <- z[!ajuste & z$retenida, ]
  a$x <- a$i - cero
  b$x <- b$i - cero
  desplazamiento <- mean(a$q - a$x)
  recta <- lm(q ~ x, a)
  pred <- predict(recta, b)
  list(serie=z, corte=format(corte), cero=cero, umbral=umbral, excluidas=sum(z$i > umbral),
       bajos=data.frame(dia=names(bajos), minimo=as.numeric(bajos), p10=unname(p10)),
       solo_min=diagnostico_bajo(min_original), solo_p10=diagnostico_bajo(p10),
       disponibles=disponibles,
       ref=ref, comun=comun, n_comun=sum(z$comun), ajuste=nrow(a), prueba=nrow(b),
       offset=desplazamiento, antes=medir(b$x,b$q), despues=medir(b$x+desplazamiento,b$q),
       coef=unname(coef(recta)), r2=summary(recta)$r.squared, mae=mean(abs(pred-b$q)),
       rmse=sqrt(mean((pred-b$q)^2)), base=mean(abs(b$q-median(a$q))))
}
tramos_html <- list("1-2"=comparar_tramo(panel_12$serie[c("t","i","q")]))
for (nombre in c("2-1","2-2")) {
  z <- do.call(rbind, lapply(ciclos_html[[nombre]]$serie, as.data.frame))
  tramos_html[[nombre]] <- comparar_tramo(z)
}
# 16.3 Mostrar el periodo 1 completo y auditar las banderas junto al corte ----
reloj_1 <- seq(inicio, fin - 3600, by = "hour")
indice_1 <- match(reloj_1, horas_periodo1$hora)
ventana_evento <- subset(serie_i, minuto >= corte_42i - 86400 & minuto < corte_42i + 86400)
vista_1 <- list(serie=data.frame(t=format(reloj_1, "%Y-%m-%d %H:%M", tz="UTC"),
  i=horas_periodo1$NOx_i[indice_1], q=horas_periodo1$NOx_Q_filtrado[indice_1]),
  corte=format(corte_42i, "%Y-%m-%d %H:%M", tz="UTC"),
  amplitudes=unlist(cortes_42i[cortes_42i$ventana_h == 6,
    c("rango_mediano_previo","rango_mediano_posterior")], use.names=FALSE),
  minutos=nrow(ventana_evento), marcados=sum(ventana_evento$bandera != "--------"))
datos_html_11 <- list(periodo1=vista_1,
  ciclos = ciclos_html, tramos = tramos_html,
  concordancia = diag_11, periodo12 = panel_12,
  serie = data.frame(t = format(grilla_11, "%Y-%m-%d %H:%M", tz = "UTC"),
                     x = pares_11$NOx_i_ajustado[filas_11], y = pares_11$NOx_Q_filtrado[filas_11],
                     grupo = pares_11$muestra[filas_11]),
  horas = nrow(horas_11), entrenamiento = nrow(ajuste_11), prueba = nrow(prueba_11),
  corte = format(inicio_prueba_11, "%d/%m/%Y"), cero = cero_11,
  intercepto = unname(coef(modelo_11)[1]), pendiente = unname(coef(modelo_11)[2]),
  rango = range(ajuste_11$NOx_i_ajustado), r2 = summary(modelo_11)$r.squared,
  mae = mae_11, rmse = rmse_11, mae_base = mae_base_11,
  sesgo = mean(dif_11), limites = limites_11)
plantilla_11 <- readLines(file.path("plantillas", "dashboard_NOx.html"), encoding = "UTF-8")
plantilla_11[plantilla_11 == "<!-- PLOTLY -->"] <- paste0(
  '<script src="data:text/javascript;base64,', base64enc::base64encode(js_plotly), '"></script>')
plantilla_11[plantilla_11 == "<!-- DATOS -->"] <- paste0(
  '<script id="datos" type="application/json">',
  jsonlite::toJSON(datos_html_11, auto_unbox = TRUE, digits = 10, na = "null"), '</script>')
writeLines(enc2utf8(plantilla_11), "Dashboard_NOx_Portillo.html", useBytes = TRUE)
