# PASO 1. Cobertura diaria de NOx total ----------------------------------------
# Ejecutar desde la carpeta de los 12 archivos de campaña.
# Los conteos describen registros crudos, incluidos los que tengan alarma.

# 1.1 Leer los archivos de ambos equipos ----
archivos_42iQ <- list.files(pattern = "^12218618650_UserData42iQ_2025-.*\\.csv$")
archivos_42i <- list.files(pattern = "^(42i |NOx_42i_).*\\.dat$", ignore.case = TRUE)
if (length(archivos_42iQ) != 6L || length(archivos_42i) != 6L) {
  stop("Revisa el directorio de trabajo: se esperan 6 CSV del 42iQ y 6 DAT del 42i.")
}

# UTC sirve aquí para conservar la hora escrita en ambos relojes, sin aplicar
# cambios de horario de verano. La zona real de los instrumentos está sin confirmar.
leer_minutos_42iQ <- function(archivo) {
  # Las filas del CSV tienen una coma final que el encabezado no tiene.
  encabezado <- strsplit(readLines(archivo, n = 1L), ",", fixed = TRUE)[[1]]
  datos <- read.csv(archivo, skip = 1, header = FALSE, check.names = FALSE,
                    col.names = c(encabezado, "Campo_final"))
  data.frame(
    minuto = as.POSIXct(datos[["Time Stamp"]], format = "%d/%m/%Y %H:%M:%S", tz = "UTC"),
    NOx_42iQ = datos[["NOx Concentration (ppb or ug/m3)"]],
    alarma_42iQ = datos[["General Alarm"]]
  )
}

leer_minutos_42i <- function(archivo) {
  datos <- read.table(archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
  data.frame(
    minuto = as.POSIXct(paste(datos$Date, datos$Time), format = "%m-%d-%y %H:%M", tz = "UTC"),
    NOx_42i = datos$nox,
    flag_42i = datos$Flags
  )
}

# Las listas conservan el origen de cada descarga para revisar los solapamientos.
descargas_42iQ <- lapply(setNames(archivos_42iQ, archivos_42iQ), leer_minutos_42iQ)
descargas_42i <- lapply(setNames(archivos_42i, archivos_42i), leer_minutos_42i)

# 1.2 Unir las descargas y verificar duplicados ----
unir_minutos_NOx <- function(descargas, equipo) {
  datos <- unique(do.call(rbind, unname(descargas)))
  if (anyNA(datos$minuto) || any(as.numeric(datos$minuto) %% 60 != 0)) {
    stop("Revisar las fechas de ", equipo, ": hay registros sin minuto exacto.")
  }
  conflicto <- anyDuplicated(datos$minuto)
  if (conflicto > 0L) {
    stop("Descargas de ", equipo, " con distinto NOx o bandera en ",
         format(datos$minuto[conflicto], tz = "UTC"))
  }
  datos[order(datos$minuto), ]
}

minutos_42iQ <- unir_minutos_NOx(descargas_42iQ, "42iQ")
minutos_42i  <- unir_minutos_NOx(descargas_42i, "42i")
minutos_NOx <- merge(minutos_42i, minutos_42iQ, by = "minuto", all = TRUE)
minutos_NOx$fecha <- as.Date(minutos_NOx$minuto, tz = "UTC")

# 1.3 Asignar la sonda según la bitácora y contar minutos por día ----
# Se incluyen todos los días entre el primer y último registro, incluso los vacíos.
dias_campana <- seq(min(minutos_NOx$fecha), max(minutos_NOx$fecha), by = "day")
dia_rotacion <- as.Date("2025-07-25")
equipo_con_sonda <- rep("Transición", length(dias_campana))
equipo_con_sonda[dias_campana < dia_rotacion] <- "42i"
equipo_con_sonda[dias_campana > dia_rotacion] <- "42iQ"
indice_dia <- match(minutos_NOx$fecha, dias_campana)
minutos_NOx$equipo_con_sonda <- equipo_con_sonda[indice_dia]

hay_42i  <- !is.na(minutos_NOx$NOx_42i)
hay_42iQ <- !is.na(minutos_NOx$NOx_42iQ)
contar_por_dia <- function(hay_dato) {
  tabulate(indice_dia[hay_dato], nbins = length(dias_campana))
}

cobertura_diaria_NOx <- data.frame(
  fecha = dias_campana,
  equipo_con_sonda = equipo_con_sonda,
  minutos_42i = contar_por_dia(hay_42i),
  minutos_42iQ = contar_por_dia(hay_42iQ),
  minutos_compartidos = contar_por_dia(hay_42i & hay_42iQ)
)

# Resultado del paso 1. Para recorrer la tabla en RStudio: View(cobertura_diaria_NOx)
print(head(cobertura_diaria_NOx))

# PASO 2. Comparación horaria de NOx total ------------------------------------
# Ejecutar después del paso 1. Se conserva la escala numérica de las descargas.
# La comparación supone que ambos relojes y unidades son compatibles.

# 2.1 Seleccionar los minutos que se usarán en ambos promedios ----
# Se excluye el día del intercambio porque se desconoce la hora de la visita.
# George identificó H como anómalo. Se usan minutos sin bandera en el 42i
# y, como criterio de trabajo para el 42iQ, con alarma general igual a cero.
# Los valores negativos y los picos sin alarma se conservan.
pares_NOx <- subset(minutos_NOx,
                    fecha != dia_rotacion &
                    is.finite(NOx_42i) & is.finite(NOx_42iQ) &
                    flag_42i == "--------" & alarma_42iQ == 0)
pares_NOx$hora <- as.POSIXct(trunc(pares_NOx$minuto, "hours"), tz = "UTC")

# 2.2 Calcular promedios por hora de reloj ----
# Ejemplo: la hora 10:00 usa los minutos 10:00 a 10:59 de los dos equipos.
# El 75 % de cobertura equivale a 45 de los 60 minutos posibles.
promedios_NOx <- aggregate(cbind(NOx_42i, NOx_42iQ) ~ hora + equipo_con_sonda,
                           data = pares_NOx, FUN = mean)
conteos_NOx <- aggregate(minuto ~ hora, data = pares_NOx, FUN = length)
names(conteos_NOx)[2] <- "minutos_pareados"
horas_NOx <- merge(promedios_NOx, conteos_NOx, by = "hora")
horas_NOx <- subset(horas_NOx, minutos_pareados >= 45L)
horas_NOx$diferencia <- horas_NOx$NOx_42iQ - horas_NOx$NOx_42i
horas_NOx$promedio_equipos <- (horas_NOx$NOx_42iQ + horas_NOx$NOx_42i) / 2

# 2.3 Resumir las diferencias por configuración ----
# Cada hora aporta un par. Una diferencia positiva indica que el 42iQ leyó más.
resumir_horas_NOx <- function(datos) {
  data.frame(equipo_con_sonda = datos$equipo_con_sonda[1],
             horas_comparadas = nrow(datos),
             diferencia_media = mean(datos$diferencia),
             diferencia_absoluta_media = mean(abs(datos$diferencia)),
             desviacion_diferencias = sd(datos$diferencia),
             correlacion_Pearson = cor(datos$NOx_42i, datos$NOx_42iQ))
}
por_sonda_NOx <- split(horas_NOx, horas_NOx$equipo_con_sonda)
resumen_horario_NOx <- do.call(rbind, unname(lapply(por_sonda_NOx, resumir_horas_NOx)))
print(resumen_horario_NOx, digits = 4, row.names = FALSE)

# 2.4 Graficar los pares horarios por configuración ----
# En el panel izquierdo la diagonal indica igualdad. En el derecho, la línea
# horizontal en cero permite ver cómo cambia la diferencia con la concentración.
graficar_horas_NOx <- function(equipo) {
  datos <- por_sonda_NOx[[equipo]]
  configuracion <- paste("Sonda en", equipo)
  limites <- range(horas_NOx$NOx_42i, horas_NOx$NOx_42iQ)
  ajustes <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  on.exit(par(ajustes))
  plot(datos$NOx_42i, datos$NOx_42iQ, xlim = limites, ylim = limites,
       xlab = "NOx horario del 42i", ylab = "NOx horario del 42iQ",
       main = configuracion, pch = 16, cex = 0.55, col = "#235C8B65")
  abline(a = 0, b = 1, lty = 2)
  plot(datos$promedio_equipos, datos$diferencia,
       xlim = range(horas_NOx$promedio_equipos), ylim = range(0, horas_NOx$diferencia),
       xlab = "Promedio de los dos equipos", ylab = "NOx horario (42iQ - 42i)",
       main = configuracion, pch = 16, cex = 0.55, col = "#235C8B65")
  abline(h = 0, lty = 2)
}
graficar_horas_NOx("42i")
graficar_horas_NOx("42iQ")

# PASO 3. Evaluación del ajuste de cero y de la homologación de NOx ------------
# Se usa el 42iQ como destino de la predicción para evaluar la corrección
# sugerida para el 42i. Esto no establece cuál instrumento es más exacto.

# 3.1 Separar ajuste y prueba por fecha ----
# En cada configuración se reservan los últimos 14 días calendario con respecto
# al último día observado. Ninguna lectura de prueba determina los parámetros.
evaluar_ajustes_NOx <- function(horas) {
  horas <- horas[order(horas$hora), ]
  corte <- max(as.Date(horas$hora, tz = "UTC")) - 13
  es_prueba <- as.Date(horas$hora, tz = "UTC") >= corte
  ajuste <- horas[!es_prueba, ]
  prueba <- horas[es_prueba, ]

  # Media móvil de 60 min evaluada al cierre de cada hora (10:00 a 10:59,
  # 11:00 a 11:59, etc.). Son los promedios pareados del paso 2, con >=45 min.
  # Evaluar una vez por hora evita contar 60 ventanas casi idénticas.
  rango <- range(ajuste$NOx_42i)
  amplitud <- diff(rango)
  if (amplitud <= 0) stop("El 42i no varía en el tramo de ajuste.")
  normalizado <- (ajuste$NOx_42i - rango[1]) / amplitud
  cero_relativo <- as.numeric(quantile(normalizado, 0.25))
  prueba$NOx_relativo <- (prueba$NOx_42i - rango[1]) / amplitud - cero_relativo
  prueba$conservada_P25 <- prueba$NOx_relativo >= 0

  # Volver a la amplitud original conservando el nuevo cero equivale a
  # restar el P25 del 42i. Así se puede comparar la diferencia con el 42iQ.
  prueba$ajuste_P25 <- prueba$NOx_relativo * amplitud
  prueba$ajuste_P25[!prueba$conservada_P25] <- NA_real_
  prueba$sin_correccion <- prueba$NOx_42i
  # Punto de comparación: predecir siempre la mediana histórica del 42iQ
  # permite medir cuánto aporta disponer de la lectura horaria del 42i.
  prueba$mediana_historica_42iQ <- median(ajuste$NOx_42iQ)
  prueba$ajuste_constante <- prueba$NOx_42i + mean(ajuste$diferencia)
  modelo <- lm(NOx_42iQ ~ NOx_42i, data = ajuste)
  prueba$ajuste_lineal <- as.numeric(predict(modelo, newdata = prueba))
  prueba
}
prueba_NOx <- do.call(rbind, unname(lapply(por_sonda_NOx, evaluar_ajustes_NOx)))

# 3.2 Comparar errores en las horas reservadas ----
# Error = predicción a partir del 42i menos lectura del 42iQ.
# Se muestran todas las horas para los métodos de cobertura completa y,
# aparte, exactamente las mismas horas retenidas por P25 para los cuatro.
resumir_prueba_NOx <- function(datos) {
  filas <- list()
  for (grupo in c("Todas", "Retenidas por P25")) {
    comparables <- if (grupo == "Todas") datos else datos[datos$conservada_P25, ]
    metodos <- c("sin_correccion", "mediana_historica_42iQ", "ajuste_constante", "ajuste_lineal")
    if (grupo == "Retenidas por P25") metodos <- c(metodos, "ajuste_P25")
    for (metodo in metodos) {
      error <- comparables[[metodo]] - comparables$NOx_42iQ
      filas[[length(filas) + 1L]] <- data.frame(
        sonda_en = datos$equipo_con_sonda[1], grupo = grupo, metodo = metodo,
        horas = length(error), cobertura_pct = 100 * length(error) / nrow(datos),
        error_medio = mean(error), error_absoluto_medio = mean(abs(error)))
    }
  }
  do.call(rbind, filas)
}
resumen_prueba_NOx <- do.call(rbind, unname(lapply(
  split(prueba_NOx, prueba_NOx$equipo_con_sonda), resumir_prueba_NOx)))
print(resumen_prueba_NOx, digits = 4, row.names = FALSE)

# PASO 4. Cambio observado alrededor de la rotación de sonda -----------------
# Se vuelve a las horas originales del paso 2. La diferencia siempre es
# 42iQ menos 42i, incluso cuando cambia el equipo conectado a la sonda.

# 4.1 Comparar horarios equivalentes antes y después de una fecha ----
comparar_rotacion_NOx <- function(fecha_corte, dias) {
  distancia <- as.integer(as.Date(horas_NOx$hora, tz = "UTC") - fecha_corte)
  dentro <- abs(distancia) <= dias & distancia != 0
  datos <- horas_NOx[dentro, c("hora", "NOx_42i", "NOx_42iQ", "diferencia")]
  distancia <- distancia[dentro]
  # Primera semana a cada lado del corte, segunda con segunda, etc.
  datos$semana <- (abs(distancia) - 1L) %/% 7L + 1L
  datos$horario <- format(datos$hora, "%u-%H", tz = "UTC")
  pares <- merge(datos[distancia < 0, ], datos[distancia > 0, ],
                 by = c("semana", "horario"), suffixes = c("_antes", "_despues"))
  cambio <- pares$diferencia_despues - pares$diferencia_antes
  data.frame(corte = fecha_corte, dias_por_lado = dias,
             pares_horarios = nrow(pares),
             brecha_antes = mean(pares$diferencia_antes),
             brecha_despues = mean(pares$diferencia_despues),
             cambio_medio = mean(cambio), cambio_mediano = median(cambio),
             cambio_42i = mean(pares$NOx_42i_despues - pares$NOx_42i_antes),
             cambio_42iQ = mean(pares$NOx_42iQ_despues - pares$NOx_42iQ_antes))
}

# 4.2 Revisar si el cambio depende de las semanas elegidas ----
# Análisis principal: dos semanas por lado. Sensibilidad: una y cuatro.
rotacion_NOx <- do.call(rbind, lapply(c(7L, 14L, 28L), comparar_rotacion_NOx,
                                      fecha_corte = dia_rotacion))
print(rotacion_NOx, digits = 4, row.names = FALSE)

# 4.3 Repetir la comparación en fechas sin intercambio documentado ----
# A 15 días del intercambio, cada ventana de 14 días queda íntegramente en
# una configuración. Estos contrastes describen la variación de fondo.
cortes_control <- c(dia_rotacion - 15L, dia_rotacion + 15L)
control_rotacion_NOx <- rbind(comparar_rotacion_NOx(cortes_control[1], 14L),
                              comparar_rotacion_NOx(cortes_control[2], 14L))
print(control_rotacion_NOx, digits = 4, row.names = FALSE)

# PASO 5. Comprobar un posible desfase entre los relojes --------------------
# Se contrasta el 42i en el minuto t con el 42iQ en el minuto t + k.
# k positivo busca la lectura del 42iQ más tarde. Se usan los valores crudos
# con las banderas del paso 2. Un máximo de correlación no prueba un error de reloj.

# 5.1 Construir la curva de correlación para un tramo ----
perfil_desfase_NOx <- function(datos, desfases) {
  # Grilla regular: los huecos de campaña quedan como NA y nunca se comprimen.
  indice <- as.integer(difftime(datos$minuto, min(datos$minuto), units = "mins")) + 1L
  serie_Q <- rep(NA_real_, max(indice))
  ok_Q <- which(datos$alarma_42iQ == 0 & is.finite(datos$NOx_42iQ))
  serie_Q[indice[ok_Q]] <- datos$NOx_42iQ[ok_Q]
  ok_i <- which(datos$flag_42i == "--------" & is.finite(datos$NOx_42i))
  filas <- ok_i[indice[ok_i] + min(desfases) >= 1L &
                 indice[ok_i] + max(desfases) <= length(serie_Q)]
  # Las mismas lecturas del 42i participan en todos los desplazamientos.
  comunes <- rep(TRUE, length(filas))
  for (k in desfases) comunes <- comunes & is.finite(serie_Q[indice[filas] + k])
  filas <- filas[comunes]
  if (length(filas) < 2L) stop("Cobertura insuficiente para comparar desfases.")
  r <- vapply(desfases, function(k) {
    cor(datos$NOx_42i[filas], serie_Q[indice[filas] + k])
  }, numeric(1))
  data.frame(desfase_min = desfases, minutos = length(filas), r = r)
}

# 5.2 Elegir el desfase en ajuste y evaluarlo en los 14 días reservados ----
probar_desfase_NOx <- function(equipo) {
  datos <- subset(minutos_NOx, equipo_con_sonda == equipo)
  # Usar el mismo corte calendario de prueba que en el paso 3.
  corte <- min(as.Date(prueba_NOx$hora[prueba_NOx$equipo_con_sonda == equipo], tz = "UTC"))
  # Ante un empate exacto, se prefiere el desplazamiento de menor magnitud.
  desfases <- -60:60
  ajuste <- perfil_desfase_NOx(datos[datos$fecha < corte, ], desfases)
  prueba <- perfil_desfase_NOx(datos[datos$fecha >= corte, ], desfases)
  elegido <- order(-ajuste$r, abs(desfases))[1]
  data.frame(sonda_en = equipo, desfase_min = desfases[elegido],
             minutos_ajuste = ajuste$minutos[elegido], minutos_prueba = prueba$minutos[elegido],
             r_ajuste_sin = ajuste$r[desfases == 0], r_ajuste_con = ajuste$r[elegido],
             r_prueba_sin = prueba$r[desfases == 0], r_prueba_con = prueba$r[elegido])
}
resumen_desfase_NOx <- rbind(probar_desfase_NOx("42i"), probar_desfase_NOx("42iQ"))
print(resumen_desfase_NOx, digits = 4, row.names = FALSE)

# PASO 6. Concordancia horaria de NOx según Lin (1989) ------------------------
# Fuente: A Concordance Correlation Coefficient to Evaluate Reproducibility,
# Biometrics 45, 255-268. DOI: 10.2307/2532051.
# Usar las horas pareadas del paso 2 en su escala original y sin desfase.

# 6.1 Calcular concordancia y sus componentes ----
concordancia_Lin_NOx <- function(datos) {
  x <- datos$NOx_42i
  y <- datos$NOx_42iQ
  media_x <- mean(x)
  media_y <- mean(y)
  # Momentos empíricos con divisor n, el mismo para varianzas y covarianza.
  var_x <- mean((x - media_x)^2)
  var_y <- mean((y - media_y)^2)
  denominador <- var_x + var_y + (media_y - media_x)^2
  data.frame(horas = length(x), Pearson = cor(x, y),
             factor_nivel_escala = 2 * sqrt(var_x * var_y) / denominador,
             CCC = 2 * mean((x - media_x) * (y - media_y)) / denominador,
             diferencia_media = media_y - media_x,
             dispersion_42i = sqrt(var_x), dispersion_42iQ = sqrt(var_y))
}

# 6.2 Recalcular omitiendo una semana calendario cada vez ----
# El rango es un diagnóstico de sensibilidad descriptivo, no un intervalo de confianza.
resumir_Lin_NOx <- function(datos) {
  semana <- as.Date(cut(as.Date(datos$hora, tz = "UTC"), "week", start.on.monday = TRUE))
  por_omision <- vapply(unique(semana), function(omitida) {
    concordancia_Lin_NOx(datos[semana != omitida, ])$CCC
  }, numeric(1))
  data.frame(sonda_en = datos$equipo_con_sonda[1], concordancia_Lin_NOx(datos),
             minimo_sin_una_semana = min(por_omision),
             maximo_sin_una_semana = max(por_omision))
}
resumen_Lin_NOx <- do.call(rbind, unname(lapply(por_sonda_NOx, resumir_Lin_NOx)))
print(resumen_Lin_NOx, digits = 5, row.names = FALSE)

# PASO 7. Sensibilidad al filtrado del 42iQ y acuerdo horario ----------------
# Ejecutar después de los pasos 1, 2 y 6. Se mantienen los criterios del 42i.

# 7.1 Marcar rachas en la serie original y tratar los negativos ----
# Detectar repeticiones antes de convertir negativos a cero. Los minutos
# ausentes interrumpen la racha. Se descarta toda racha de longitud >= 4.
q <- minutos_42iQ$NOx_42iQ
repetido <- c(FALSE, diff(as.numeric(minutos_42iQ$minuto)) == 60 & diff(q) == 0)
repetido[is.na(repetido)] <- FALSE
grupo_racha <- cumsum(!repetido)
racha_Q <- tabulate(grupo_racha)[grupo_racha] >= 4L
sens_min <- pares_NOx
sens_min$racha <- racha_Q[match(sens_min$minuto, minutos_42iQ$minuto)]
sens_min$extremo <- sens_min$NOx_42iQ < -3
sens_min$a_cero <- with(sens_min, !racha & !extremo & NOx_42iQ < 0)
print(aggregate(cbind(racha, extremo, a_cero) ~ equipo_con_sonda, sens_min, sum))
sens_min <- subset(sens_min, !racha & !extremo)
sens_min$Q_filtrado <- pmax(0, sens_min$NOx_42iQ)

# 7.2 Reconstruir los promedios usando exactamente los mismos minutos ----
sens_min$n <- 1L
llaves <- sens_min[c("hora", "equipo_con_sonda")]
sens_h <- aggregate(sens_min[c("NOx_42i", "NOx_42iQ", "Q_filtrado")], llaves, mean)
sens_h <- merge(sens_h, aggregate(sens_min["n"], llaves, sum), by = names(llaves))
sens_h <- subset(sens_h, n >= 45)
variantes_NOx <- list(base = horas_NOx, comunes = sens_h,
                      filtrada = transform(sens_h, NOx_42iQ = Q_filtrado))

# 7.3 Concordancia, límites de Bland-Altman y acuerdo dentro de tolerancia ----
# Los límites clásicos describen aproximadamente el 95 % si las diferencias
# son normales y de dispersión estable. Se añaden percentiles observados.
resumir_acuerdo_NOx <- function(z) {
  lin <- concordancia_Lin_NOx(z)
  d <- z$NOx_42iQ - z$NOx_42i
  data.frame(sonda_en = z$equipo_con_sonda[1], lin[c("horas", "Pearson", "CCC")],
             sesgo = mean(d), limite_inf = mean(d) - 1.96 * sd(d),
             limite_sup = mean(d) + 1.96 * sd(d),
             P025 = unname(quantile(d, 0.025)), P975 = unname(quantile(d, 0.975)),
             acuerdo_5 = mean(abs(d) <= 5), acuerdo_10 = mean(abs(d) <= 10),
             acuerdo_20 = mean(abs(d) <= 20))
}
resumen_filtros_NOx <- do.call(rbind, lapply(names(variantes_NOx), function(nombre) {
  z <- variantes_NOx[[nombre]]
  tabla <- do.call(rbind, lapply(split(z, z$equipo_con_sonda), resumir_acuerdo_NOx))
  data.frame(variante = nombre, tabla, row.names = NULL)
}))
print(resumen_filtros_NOx, digits = 4, row.names = FALSE)

# 7.4 Figura de comparación para RStudio y PNG ----
graficar_acuerdo_NOx <- function() {
  ajustes <- par(mfrow = c(2, 3), mar = c(4.2, 4.3, 3, 1), las = 1)
  on.exit(par(ajustes))
  xlim_xy <- range(sens_h$NOx_42i, sens_h$Q_filtrado)
  rango_media <- range((sens_h$NOx_42i + sens_h$Q_filtrado) / 2)
  limites <- subset(resumen_filtros_NOx, variante == "filtrada")
  rango_d <- range(sens_h$Q_filtrado - sens_h$NOx_42i, limites$limite_inf,
                   limites$limite_sup, 0)
  for (equipo in c("42i", "42iQ")) {
    z <- subset(sens_h, equipo_con_sonda == equipo)
    titulo <- paste("Sonda en", equipo)
    d <- z$Q_filtrado - z$NOx_42i
    plot(z$NOx_42i, z$Q_filtrado, pch = 16, cex = 0.6, col = "#21668170",
         xlim = xlim_xy, ylim = xlim_xy, xlab = "42i", ylab = "42iQ filtrado",
         main = paste(titulo, "| Pares horarios"))
    abline(0, 1, lty = 2)
    plot((z$Q_filtrado + z$NOx_42i) / 2, d, pch = 16, cex = 0.6, col = "#21668170",
         xlim = rango_media, ylim = rango_d, main = paste(titulo, "| Bland-Altman"),
         xlab = "Promedio de ambos equipos", ylab = "Diferencia (42iQ - 42i)")
    abline(h = 0, col = "grey60")
    abline(h = mean(d), col = "#983D22", lwd = 2)
    abline(h = mean(d) + c(-1, 1) * 1.96 * sd(d), col = "#983D22", lty = 2)
    plot(ecdf(abs(z$NOx_42iQ - z$NOx_42i)), xlim = c(0, max(abs(rango_d))),
         ylim = c(0, 1), verticals = TRUE, do.points = FALSE, col = "#983D22",
         main = paste(titulo, "| Acuerdo"), xlab = "Tolerancia absoluta",
         ylab = "Fracción de horas", sub = "")
    lines(ecdf(abs(d)), verticals = TRUE, do.points = FALSE, col = "#12577B", lty = 2)
    legend("bottomright", c("Sin recorte a cero", "Filtrado"), lty = c(1, 2),
           col = c("#983D22", "#12577B"), bty = "n", cex = 0.8)
  }
}
if (interactive()) graficar_acuerdo_NOx()
dir.create("Gráficas", showWarnings = FALSE)
png(file.path("Gráficas", "Paso7_acuerdo_NOx.png"), width = 1800, height = 1000, res = 135)
tryCatch(graficar_acuerdo_NOx(), finally = dev.off())

# PASO 8. Medias horarias en el tiempo y ciclo diario de NOx -----------------
# Lamsal et al. (2015), sección 5 y figura 5: comparación por hora del día.
# Aquí se estudia NOx total en Portillo. Usamos el reloj registrado en campaña.

# 8.1 Preparar las series filtradas y los días completos ----
series_p8 <- sens_h[order(sens_h$hora), ]
series_p8$dia <- as.Date(series_p8$hora, tz = "UTC")
series_p8$hora_dia <- as.integer(format(series_p8$hora, "%H", tz = "UTC"))
series_p8$dia_completo <- ave(series_p8$hora_dia, series_p8$dia, FUN = length) == 24L
# Cada media del ciclo usa los mismos días a las 00, 01, ..., 23 horas.
perfil_diario_NOx <- function(z) {
  aggregate(cbind(NOx_42i, Q_filtrado) ~ hora_dia, z, mean)
}
resumir_ciclo_NOx <- function(z, tramo) {
  perfil <- perfil_diario_NOx(z)
  data.frame(sonda_en = z$equipo_con_sonda[1], tramo = tramo, dias = length(unique(z$dia)),
             max_h_42i = perfil$hora_dia[which.max(perfil$NOx_42i)],
             max_h_42iQ = perfil$hora_dia[which.max(perfil$Q_filtrado)],
             amplitud_42i = diff(range(perfil$NOx_42i)),
             amplitud_42iQ = diff(range(perfil$Q_filtrado)),
             r_perfil = cor(perfil$NOx_42i, perfil$Q_filtrado))
}
por_periodo_p8 <- split(series_p8, series_p8$equipo_con_sonda)
resumen_ciclo_NOx <- do.call(rbind, lapply(por_periodo_p8, function(z) {
  mitad <- min(z$dia) + floor(as.numeric(max(z$dia) - min(z$dia)) / 2)
  completos <- subset(z, dia_completo)
  rbind(resumir_ciclo_NOx(completos, "Total"),
        resumir_ciclo_NOx(subset(completos, dia <= mitad), "Primera mitad"),
        resumir_ciclo_NOx(subset(completos, dia > mitad), "Segunda mitad"))
}))
print(resumen_ciclo_NOx, digits = 4, row.names = FALSE)

# 8.2 Graficar cronología y perfil de 24 horas en escala original ----
graficar_medias_NOx <- function() {
  ajustes <- par(mfrow = c(2, 2), mar = c(4.1, 4.4, 3, 0.9))
  on.exit(par(ajustes))
  colores <- c("#AA4926", "#15658B")
  perfiles <- lapply(por_periodo_p8, function(z) perfil_diario_NOx(subset(z, dia_completo)))
  limites_series <- range(series_p8$NOx_42i, series_p8$Q_filtrado)
  limites_ciclos <- range(lapply(perfiles, function(z) unlist(z[c("NOx_42i", "Q_filtrado")])))
  for (equipo in names(por_periodo_p8)) {
    z <- por_periodo_p8[[equipo]]
    reloj <- seq(min(z$hora), max(z$hora), by = "hour")
    grilla <- z[match(reloj, z$hora), ]
    plot(reloj, grilla$Q_filtrado, type = "l", col = colores[2], ylim = limites_series,
         main = paste("Serie horaria | Sonda en", equipo), xlab = "Fecha de 2025",
         ylab = "NOx en escala original")
    lines(reloj, grilla$NOx_42i, col = colores[1])
    legend("topright", c("42i", "42iQ filtrado"), col = colores, lty = 1, bty = "n")
    p <- perfiles[[equipo]]
    matplot(p$hora_dia, p[c("NOx_42i", "Q_filtrado")], type = "o", pch = c(16, 1),
            lty = 1, col = colores, ylim = limites_ciclos, xlab = "Hora del reloj",
            ylab = "NOx medio de los días completos", main = paste("Ciclo diario | Sonda en", equipo))
  }
}
if (interactive()) graficar_medias_NOx()
png(file.path("Gráficas", "Paso8_medias_horarias_NOx.png"), width = 1800, height = 980, res = 140)
tryCatch(graficar_medias_NOx(), finally = dev.off())

# PASO 9. Ciclo diario y repetición temporal en 1-2 y 2 ----------------------
# Ejecutar después del paso 8. Usar el 42i antes del P25 y el 42iQ filtrado.
# Días completos: los 24 promedios de reloj se calculan con los mismos días.
# El 25/07 sigue excluido. No se interpola entre visitas ni entre días.
ciclo_datos <- subset(series_p8, dia_completo & dia >= as.Date("2025-06-21"))
ciclo_datos$periodo <- ifelse(ciclo_datos$dia < dia_rotacion, "1-2", "2")

# 9.1 Promediar por hora y solo entonces normalizar cada perfil ----
normalizar_perfil <- function(v) {
  rango <- range(v)
  if (diff(rango) == 0) return(rep(NA_real_, length(v)))
  (v - rango[1]) / diff(rango)
}

# 9.2 Medir repetición a retardos de 1 a 72 horas reales ----
analizar_reloj <- function(z) {
  z <- z[order(z$hora), ]
  perfil <- perfil_diario_NOx(z)
  perfil$forma_i <- normalizar_perfil(perfil$NOx_42i)
  perfil$forma_Q <- normalizar_perfil(perfil$Q_filtrado)
  # Quitar el promedio de cada día para estudiar cambios intradiarios.
  # Esto reduce la influencia de cambios de nivel entre semanas.
  i <- z$NOx_42i - ave(z$NOx_42i, z$dia, FUN = mean)
  q <- z$Q_filtrado - ave(z$Q_filtrado, z$dia, FUN = mean)
  tiempos <- as.numeric(z$hora)
  retardos <- do.call(rbind, lapply(1:72, function(k) {
    anterior <- match(tiempos - k * 3600, tiempos)
    ok <- !is.na(anterior)
    data.frame(k = k, pares = sum(ok),
               i = cor(i[ok], i[anterior[ok]]), q = cor(q[ok], q[anterior[ok]]))
  }))

  # 9.3 Comprobar si la fase del ciclo se mantiene entre semanas ----
  # Hipótesis de referencia: cada bloque de siete días calendario podría
  # tener su propio origen horario. Un desplazamiento circular común a
  # todo el bloque preserva sus formas intradiarias y la relación i/Q.
  # Estadístico: variación entre las 24 medias. Se compara con 1999
  # reasignaciones de fase; no es una prueba de causalidad ni de calibración.
  bloque <- as.integer(z$dia - min(z$dia)) %/% 7L
  sumas <- lapply(split(seq_len(nrow(z)), bloque), function(filas) {
    rowsum(cbind(i = i[filas], q = q[filas]), z$hora_dia[filas], reorder = TRUE)
  })
  dias <- length(unique(z$dia))
  fuerza <- function(suma) colMeans((suma / dias)^2)
  observada <- fuerza(Reduce("+", sumas))
  simulada <- replicate(1999, {
    rotadas <- lapply(sumas, function(m) m[(0:23 + sample.int(24, 1) - 1L) %% 24 + 1L, , drop = FALSE])
    fuerza(Reduce("+", rotadas))
  })
  p <- (1 + rowSums(simulada >= observada)) / (ncol(simulada) + 1)
  list(perfil = perfil, retardos = retardos, dias = dias, bloques = length(sumas),
       amplitud = c(diff(range(perfil$NOx_42i)), diff(range(perfil$Q_filtrado))),
       maximo = perfil$hora_dia[c(which.max(perfil$NOx_42i), which.max(perfil$Q_filtrado))],
       r_perfil = cor(perfil$NOx_42i, perfil$Q_filtrado),
       r_horas = cor(z$NOx_42i, z$Q_filtrado), p = unname(p))
}
set.seed(20250621)
ciclos_NOx <- lapply(split(ciclo_datos, ciclo_datos$periodo), analizar_reloj)
# Cuatro contrastes: dos equipos por dos periodos.
p_ciclos <- matrix(p.adjust(unlist(lapply(ciclos_NOx, "[[", "p")), method = "holm"), nrow = 2)
for (j in seq_along(ciclos_NOx)) ciclos_NOx[[j]]$p_holm <- p_ciclos[, j]
for (periodo in names(ciclos_NOx)) {
  info <- ciclos_NOx[[periodo]]
  cat("\nPeriodo", periodo, "| días completos:", info$dias, "| bloques:", info$bloques, "\n")
  print(data.frame(equipo = c("42i","42iQ"), amplitud = info$amplitud,
                   hora_maxima = info$maximo, p_holm = info$p_holm))
  print(subset(info$retardos, k %in% c(12,24,48,72)))
}
# La sección 16 del script de dashboard incorpora estos resultados sin leer
# ni mezclar crudos del periodo 2 en los modelos de calibración del periodo 1.
horas_panel_2 <- subset(series_p8, dia > dia_rotacion)
ciclos_NOx[["2"]]$serie <- with(horas_panel_2, data.frame(
  t = format(hora, "%Y-%m-%d %H:%M", tz = "UTC"), i = NOx_42i, q = Q_filtrado))
# 9.3.1 Separar los tramos observados a ambos lados del mayor vacío ----
horas_panel_2 <- horas_panel_2[order(horas_panel_2$hora), ]
reinicio <- horas_panel_2$hora[which.max(diff(as.numeric(horas_panel_2$hora))) + 1L]
tramo <- ifelse(horas_panel_2$hora < reinicio, "2-1", "2-2")
subciclos <- lapply(split(horas_panel_2, tramo), function(z) {
  info <- analizar_reloj(subset(z, dia_completo))
  info$serie <- with(z, data.frame(t = format(hora, "%Y-%m-%d %H:%M", tz = "UTC"),
                                   i = NOx_42i, q = Q_filtrado))
  info
})
p_sub <- matrix(p.adjust(unlist(lapply(subciclos, "[[", "p")), "holm"), nrow = 2)
for (j in seq_along(subciclos)) subciclos[[j]]$p_holm <- p_sub[, j]
jsonlite::write_json(c(ciclos_NOx, subciclos), file.path("plantillas", "ciclos_diarios_NOx.json"),
                     auto_unbox = TRUE, digits = 9, na = "null")

# 9.4 Figura comparativa de los dos periodos para el informe ----
graficar_periodicidad <- function() {
  ajustes <- par(mfrow = c(2, 2), mar = c(4.3, 4.3, 3.5, 1))
  on.exit(par(ajustes))
  colores <- c("#A74523", "#15658B")
  for (periodo in names(ciclos_NOx)) {
    z <- ciclos_NOx[[periodo]]
    titulo <- paste("Periodo", periodo)
    matplot(z$perfil$hora_dia, z$perfil[c("forma_i", "forma_Q")], type = "o",
            pch = c(16, 1), lty = 1, col = colores, main = paste(titulo, "| Ciclo diario"),
            xlab = "Hora del reloj", ylab = "Perfil medio normalizado")
    legend("topleft", c("42i", "42iQ filtrado"), col = colores, lty = 1, bty = "n")
    matplot(z$retardos$k, z$retardos[c("i", "q")], type = "l", lwd = 2,
            lty = 1, col = colores, ylim = c(-1, 1), main = paste(titulo, "| Repetición temporal"),
            xlab = "Horas de retardo", ylab = "Correlación con la propia serie")
    abline(h = 0, v = c(24, 48, 72), col = "grey65", lty = 3)
  }
}
png(file.path("Gráficas", "Periodos_12_2_ciclo_diario.png"), width = 1500, height = 920, res = 130)
tryCatch(graficar_periodicidad(), finally = dev.off())

# 42iQ sin sonda -----------------------####################################

#lectura 3 bases ----
archivos_csv <- c("12218618650_UserData42iQ_2025-05-30_15-24-10_Sinsonda.csv", 
                  "12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv", 
                  "12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv")

# 2. Función de lectura con corrección de row.names
leer_csv_42iQ <- function(archivo) {
  # row.names = NULL mantiene cada columna en su posición original
  datos <- read.csv(archivo, stringsAsFactors = FALSE, row.names = NULL)
  
  # Posiciones reales: 1 (Time Stamp), 8 (NO), 9 (NO2), 10 (NOx)
  datos_limpios <- datos[, c(1, 8, 9, 10)]
  
  colnames(datos_limpios) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
  datos_limpios$Fecha_y_Hora <- as.POSIXct(datos_limpios$Fecha_y_Hora, format="%d/%m/%Y %H:%M:%S")
  
  return(datos_limpios)
}

# 3. Unir las 3 bases
lista_csv <- lapply(archivos_csv, leer_csv_42iQ)
iQ_sinsonda_datos_totales <- do.call(rbind, lista_csv)

# 4. Ordenar cronológicamente y eliminar solapamientos exactos
iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[order(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]
iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[!duplicated(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]


#contención de bases ----
base1 <- leer_csv_42iQ(archivos_csv[1])
base2 <- leer_csv_42iQ(archivos_csv[2])
base3 <- leer_csv_42iQ(archivos_csv[3])

# 2. Extraer fechas de inicio y fin de cada archivo
rango1 <- range(base1$Fecha_y_Hora, na.rm = TRUE)
rango2 <- range(base2$Fecha_y_Hora, na.rm = TRUE)
rango3 <- range(base3$Fecha_y_Hora, na.rm = TRUE)

# 3. Imprimir la radiografía temporal para inspección visual
cat("\n--- Análisis de Periodos por Archivo ---\n")
cat("Base 1 (Mayo) :", format(rango1[1]), " al ", format(rango1[2]), "\n")
cat("Base 2 (Junio):", format(rango2[1]), " al ", format(rango2[2]), "\n")
cat("Base 3 (Julio):", format(rango3[1]), " al ", format(rango3[2]), "\n")

# 4. Comprobación lógica y matemática de contención
# ¿La base 3 empieza antes o igual y termina después o igual que las otras dos?
contenida_1 <- (rango3[1] <= rango1[1]) & (rango3[2] >= rango1[2])
contenida_2 <- (rango3[1] <= rango2[1]) & (rango3[2] >= rango2[2])

cat("\n--- Conclusión del Análisis de Solapamiento ---\n")
if(contenida_1 & contenida_2) {
  cat("RESULTADO: La Base 1 y Base 2 están temporalmente contenidas al 100% en la Base 3.\n")
  cat("ACCIÓN: Se descartan las dos primeras bases para evitar redundancia de procesamiento.\n")
  
  # Redefinimos la base total usando ÚNICAMENTE la tercera base
  iQ_sinsonda_datos_totales <- base3
  
} else {
  cat("RESULTADO: Las bases aportan datos en periodos distintos.\n")
  cat("ACCIÓN: Es necesario unirlas (rbind) y usar unique() para los traslapes parciales.\n")
  
  # Se mantiene la lógica de unión si resultan no estar contenidas
  lista_csv <- list(base1, base2, base3)
  iQ_sinsonda_datos_totales <- do.call(rbind, lista_csv)
  iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[order(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]
  iQ_sinsonda_datos_totales <- iQ_sinsonda_datos_totales[!duplicated(iQ_sinsonda_datos_totales$Fecha_y_Hora), ]
}


#medidas de resumen crudas ----
# Función de resumen estadístico ampliada (42iQ: NO, NO2, NOx)
generar_resumen_estadistico <- function(df, nombre_base) {
  gases <- c("Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
  
  resumen <- data.frame(
    Gas           = c("NO", "NO2", "NOx"),
    Minimo        = sapply(df[gases], min, na.rm = TRUE),
    Q1            = sapply(df[gases], quantile, probs = 0.25, na.rm = TRUE),
    Mediana       = sapply(df[gases], median, na.rm = TRUE),
    Promedio      = sapply(df[gases], mean, na.rm = TRUE),
    Q3            = sapply(df[gases], quantile, probs = 0.75, na.rm = TRUE),
    Maximo        = sapply(df[gases], max, na.rm = TRUE),
    IQR           = sapply(df[gases], IQR, na.rm = TRUE),
    Desv_Estandar = sapply(df[gases], sd, na.rm = TRUE)
  )
  rownames(resumen) <- NULL
  
  cat("\n========================================================================================\n")
  cat("ESTADÍSTICAS:", nombre_base, "\n")
  cat("========================================================================================\n")
  print(format(resumen, digits = 4, nsmall = 4))
  cat("========================================================================================\n")
}

# Función de resumen estadístico ampliada (42i: NO, NOx)
generar_resumen_estadistico_42i <- function(df, nombre_base) {
  gases <- c("Concentracion_NO", "Concentracion_NOx")
  
  resumen <- data.frame(
    Gas           = c("NO", "NOx"),
    Minimo        = sapply(df[gases], min, na.rm = TRUE),
    Q1            = sapply(df[gases], quantile, probs = 0.25, na.rm = TRUE),
    Mediana       = sapply(df[gases], median, na.rm = TRUE),
    Promedio      = sapply(df[gases], mean, na.rm = TRUE),
    Q3            = sapply(df[gases], quantile, probs = 0.75, na.rm = TRUE),
    Maximo        = sapply(df[gases], max, na.rm = TRUE),
    IQR           = sapply(df[gases], IQR, na.rm = TRUE),
    Desv_Estandar = sapply(df[gases], sd, na.rm = TRUE)
  )
  rownames(resumen) <- NULL
  
  cat("\n========================================================================================\n")
  cat("ESTADÍSTICAS:", nombre_base, "\n")
  cat("========================================================================================\n")
  print(format(resumen, digits = 4, nsmall = 4))
  cat("========================================================================================\n")
}

# 2. Ejecutar la función para la base cruda sin sonda
generar_resumen_estadistico(iQ_sinsonda_datos_totales, "42iQ SIN SONDA (Datos Crudos)")
#analisis valores constantes ----
analizar_falla <- function(valores, limite = 4) {
  valores <- na.omit(valores)
  if(length(valores) == 0) return()
  
  rachas <- rle(valores)
  fallas <- rachas$lengths >= limite
  
  if(any(fallas)) {
    cat("Advertencia: Se detectaron valores constantes.\n")
    print(data.frame(Valor_Pegado = rachas$values[fallas], 
                     Repeticiones = rachas$lengths[fallas]))
  } else {
    cat("Estado normal: No hay valores constantes por 4 o más mediciones.\n")
  }
}

cat("\n--- Análisis de Mediciones constantes (42iQ) ---\n")
cat("NO:\n"); analizar_falla(iQ_sinsonda_datos_totales$Concentracion_NO)
cat("\nNO2:\n"); analizar_falla(iQ_sinsonda_datos_totales$Concentracion_NO2)
cat("\nNOx:\n"); analizar_falla(iQ_sinsonda_datos_totales$Concentracion_NOx)
#total de registros unicos y NAs iniciales ----
total_crudos <- nrow(iQ_sinsonda_datos_totales)

na_crudos_NO  <- sum(is.na(iQ_sinsonda_datos_totales$Concentracion_NO))
na_crudos_NO2 <- sum(is.na(iQ_sinsonda_datos_totales$Concentracion_NO2))
na_crudos_NOx <- sum(is.na(iQ_sinsonda_datos_totales$Concentracion_NOx))

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE CONTINUIDAD (BASE CRUDA)\n")
cat("==============================================\n")
cat(sprintf("Total de registros crudos: %d\n", total_crudos))
cat(sprintf("Valores NA iniciales -> NO: %d | NO2: %d | NOx: %d\n", 
            na_crudos_NO, na_crudos_NO2, na_crudos_NOx))
cat("==============================================\n")
#eliminacion de datos en calibración ----
# 1. Crear copia para el primer filtro (preservando la base cruda intacta)
datos_filtro1_sinsonda <- iQ_sinsonda_datos_totales

# 2. Función limpia para convertir rachas >= 4 en NA
filtrar_rachas <- function(vector_gases, limite = 4) {
  rachas <- rle(vector_gases)
  indices_pegados <- rep(rachas$lengths >= limite, rachas$lengths)
  vector_gases[indices_pegados] <- NA
  return(vector_gases)
}

# 3. Aplicar primer filtro a cada contaminante
datos_filtro1_sinsonda$Concentracion_NO  <- filtrar_rachas(datos_filtro1_sinsonda$Concentracion_NO, limite = 4)
datos_filtro1_sinsonda$Concentracion_NO2 <- filtrar_rachas(datos_filtro1_sinsonda$Concentracion_NO2, limite = 4)
datos_filtro1_sinsonda$Concentracion_NOx <- filtrar_rachas(datos_filtro1_sinsonda$Concentracion_NOx, limite = 4)

# 4. Contabilizar datos eliminados (nuevos NA) y su porcentaje respecto al total crudo
elim_f1_NO  <- sum(is.na(datos_filtro1_sinsonda$Concentracion_NO)) - na_crudos_NO
elim_f1_NO2 <- sum(is.na(datos_filtro1_sinsonda$Concentracion_NO2)) - na_crudos_NO2
elim_f1_NOx <- sum(is.na(datos_filtro1_sinsonda$Concentracion_NOx)) - na_crudos_NOx

# 5. Eliminar físicamente de la tabla las filas de calibración
filas_vacias_sin <- is.na(datos_filtro1_sinsonda$Concentracion_NO) & 
  is.na(datos_filtro1_sinsonda$Concentracion_NO2) & 
  is.na(datos_filtro1_sinsonda$Concentracion_NOx)
datos_filtro1_sinsonda <- datos_filtro1_sinsonda[!filas_vacias_sin, ]

cat("\n==============================================\n")
cat("REPORTE PRIMER FILTRO: VALORES CONSTANTES (>= 4)\n")
cat("==============================================\n")
cat(sprintf("Total de datos crudos evaluados: %d registros\n\n", total_crudos))
cat("Datos convertidos a NA (periodos de estabilización/calentamiento):\n")
cat(sprintf("- NO  : %d registros eliminados (%.4f%% del total)\n", elim_f1_NO, (elim_f1_NO / total_crudos) * 100))
cat(sprintf("- NO2 : %d registros eliminados (%.4f%% del total)\n", elim_f1_NO2, (elim_f1_NO2 / total_crudos) * 100))
cat(sprintf("- NOx : %d registros eliminados (%.4f%% del total)\n", elim_f1_NOx, (elim_f1_NOx / total_crudos) * 100))
cat("==============================================\n")
#filtrado de negativos ----
# 1. Definir el total válido (post-filtro 1) como base de cálculo
total_valido <- nrow(datos_filtro1_sinsonda)

# 2. Conteo de negativos ANTES de aplicar la limpieza
neg_NO  <- sum(datos_filtro1_sinsonda$Concentracion_NO < 0, na.rm = TRUE)
neg_NO2 <- sum(datos_filtro1_sinsonda$Concentracion_NO2 < 0, na.rm = TRUE)
neg_NOx <- sum(datos_filtro1_sinsonda$Concentracion_NOx < 0, na.rm = TRUE)

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE NEGATIVOS (PRE-FILTRO 2)\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo: %d registros válidos\n\n", total_valido))
cat(sprintf("- NO  : %d registros negativos (%.2f%% del total válido)\n", neg_NO, (neg_NO/total_valido)*100))
cat(sprintf("- NO2 : %d registros negativos (%.2f%% del total válido)\n", neg_NO2, (neg_NO2/total_valido)*100))
cat(sprintf("- NOx : %d registros negativos (%.2f%% del total válido)\n", neg_NOx, (neg_NOx/total_valido)*100))

# 3. Crear nueva base para aplicar el segundo filtro de forma segura
datos_filtro2_sinsonda <- datos_filtro1_sinsonda
umbral_ruido <- -3.0
columnas_gases <- c("Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")

# Lista para almacenar los resultados del filtro
reporte_f2 <- list()

# 4. Aplicar las reglas estadísticas de limpieza
for(col in columnas_gases) {
  vec <- datos_filtro2_sinsonda[[col]]
  
  # Identificar las posiciones exactas de ruido térmico y fallas críticas
  idx_ruido  <- which(vec < 0 & vec >= umbral_ruido)
  idx_fallas <- which(vec < umbral_ruido)
  
  # Guardar los conteos en la lista
  reporte_f2[[col]] <- list(ruido = length(idx_ruido), fallas = length(idx_fallas))
  
  # A) Ruido térmico a 0
  vec[idx_ruido] <- 0
  
  # B) Fallas críticas de hardware a NA
  vec[idx_fallas] <- NA
  
  # Sobrescribir la columna en la nueva base
  datos_filtro2_sinsonda[[col]] <- vec
}

# 5. Reporte final post-filtro 2 con porcentajes
cat("\n==============================================\n")
cat("REPORTE SEGUNDO FILTRO: CORRECCIÓN UMBRAL -3.0\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo: %d registros válidos (Post-Filtro 1)\n\n", total_valido))

cat("1. Ruido térmico ajustado a 0 (valores entre 0 y -3.0):\n")
cat(sprintf("- NO  : %d registros corregidos a cero (%.2f%%).\n", reporte_f2$Concentracion_NO$ruido, (reporte_f2$Concentracion_NO$ruido/total_valido)*100))
cat(sprintf("- NO2 : %d registros corregidos a cero (%.2f%%).\n", reporte_f2$Concentracion_NO2$ruido, (reporte_f2$Concentracion_NO2$ruido/total_valido)*100))
cat(sprintf("- NOx : %d registros corregidos a cero (%.2f%%).\n", reporte_f2$Concentracion_NOx$ruido, (reporte_f2$Concentracion_NOx$ruido/total_valido)*100))

cat("\n2. Fallas críticas descartadas a NA (valores < -3.0):\n")
cat(sprintf("- NO  : %d registros eliminados (%.4f%%).\n", reporte_f2$Concentracion_NO$fallas, (reporte_f2$Concentracion_NO$fallas/total_valido)*100))
cat(sprintf("- NO2 : %d registros eliminados (%.4f%%).\n", reporte_f2$Concentracion_NO2$fallas, (reporte_f2$Concentracion_NO2$fallas/total_valido)*100))
cat(sprintf("- NOx : %d registros eliminados (%.4f%%).\n", reporte_f2$Concentracion_NOx$fallas, (reporte_f2$Concentracion_NOx$fallas/total_valido)*100))
cat("==============================================\n")

base_final_sinsonda <- datos_filtro2_sinsonda

#media movil (60 mins) ----
library(zoo)

base_final_sinsonda$Concentracion_NO  <- rollapply(base_final_sinsonda$Concentracion_NO, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
base_final_sinsonda$Concentracion_NO2 <- rollapply(base_final_sinsonda$Concentracion_NO2, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
base_final_sinsonda$Concentracion_NOx <- rollapply(base_final_sinsonda$Concentracion_NOx, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
#normalizacion ----
normalizar_min_max <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  x_norm <- (x - min_x) / (max_x - min_x)
  return(x_norm)
}

base_final_sinsonda_norm <- base_final_sinsonda
base_final_sinsonda_norm$Concentracion_NO  <- normalizar_min_max(base_final_sinsonda$Concentracion_NO)
base_final_sinsonda_norm$Concentracion_NO2 <- normalizar_min_max(base_final_sinsonda$Concentracion_NO2)
base_final_sinsonda_norm$Concentracion_NOx <- normalizar_min_max(base_final_sinsonda$Concentracion_NOx)

cat("\n==============================================\n")
cat("42iQ SIN SONDA - REPORTE NORMALIZACIÓN\n")
cat("==============================================\n")
cat("Normalización Min-Max [0, 1] aplicada exitosamente.\n(Se conserva el 100% de la data válida post-filtros).\n")
cat("==============================================\n")

#medidas de resumen finales ----
generar_resumen_estadistico(base_final_sinsonda_norm, "42iQ SIN SONDA (Normalizado Min-Max)")

############################################################################



# 42iQ con sonda -----------------------####################################

#lectura de las 3 bases ----
# 1. Nombres exactos de los archivos
archivos_csv_sonda <- c("12218618650_UserData42iQ_2025-08-14_12-34-21_Sonda.csv", 
                        "12218618650_UserData42iQ_2025-08-28_23-16-34_Sonda.csv", 
                        "12218618650_UserData42iQ_2025-10-07_13-48-55_Sonda.csv")

# 2. Función de lectura idéntica a la anterior (protegiendo row.names)
leer_csv_42iQ_sonda <- function(archivo) {
  datos <- read.csv(archivo, stringsAsFactors = FALSE, row.names = NULL)
  
  # Extraer Tiempo, NO, NO2, NOx
  datos_limpios <- datos[, c(1, 8, 9, 10)]
  colnames(datos_limpios) <- c("Fecha_y_Hora", "Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")
  
  # Formatear fecha
  datos_limpios$Fecha_y_Hora <- as.POSIXct(datos_limpios$Fecha_y_Hora, format="%d/%m/%Y %H:%M:%S")
  
  return(datos_limpios)
}

# 3. Leer las 3 bases de forma independiente
base1_sonda <- leer_csv_42iQ_sonda(archivos_csv_sonda[1])
base2_sonda <- leer_csv_42iQ_sonda(archivos_csv_sonda[2])
base3_sonda <- leer_csv_42iQ_sonda(archivos_csv_sonda[3])


#analisis de contención de bases ----
rango1_sonda <- range(base1_sonda$Fecha_y_Hora, na.rm = TRUE)
rango2_sonda <- range(base2_sonda$Fecha_y_Hora, na.rm = TRUE)
rango3_sonda <- range(base3_sonda$Fecha_y_Hora, na.rm = TRUE)

cat("\n--- Análisis de Periodos por Archivo (Con Sonda) ---\n")
cat("Base 1 (14 Ago):", format(rango1_sonda[1]), " al ", format(rango1_sonda[2]), "\n")
cat("Base 2 (28 Ago):", format(rango2_sonda[1]), " al ", format(rango2_sonda[2]), "\n")
cat("Base 3 (07 Oct):", format(rango3_sonda[1]), " al ", format(rango3_sonda[2]), "\n")

# Evaluar si la Base 1 está contenida en la Base 2
contenida_1_en_2 <- (rango2_sonda[1] <= rango1_sonda[1]) & (rango2_sonda[2] >= rango1_sonda[2])

cat("\n--- Conclusión de Solapamiento ---\n")
if(contenida_1_en_2) {
  cat("RESULTADO: La Base 1 está contenida 100% dentro de la Base 2.\n")
  cat("ACCIÓN: Se descarta la Base 1. Se unen (rbind) la Base 2 y la Base 3.\n")
  
  # Unimos solo la 2 y la 3
  iQ_sonda_datos_totales <- rbind(base2_sonda, base3_sonda)
  
} else {
  # Por si acaso, si la lógica falla, unimos las tres
  cat("ACCIÓN: Se unen las tres bases.\n")
  iQ_sonda_datos_totales <- do.call(rbind, list(base1_sonda, base2_sonda, base3_sonda))
}

# 4. Ordenar cronológicamente y forzar unicidad (eliminar traslapes exactos)
iQ_sonda_datos_totales <- iQ_sonda_datos_totales[order(iQ_sonda_datos_totales$Fecha_y_Hora), ]
iQ_sonda_datos_totales <- iQ_sonda_datos_totales[!duplicated(iQ_sonda_datos_totales$Fecha_y_Hora), ]



#total de registro unicos y NAs iniciales ----
total_crudos_sonda <- nrow(iQ_sonda_datos_totales)

na_crudos_NO_sonda  <- sum(is.na(iQ_sonda_datos_totales$Concentracion_NO))
na_crudos_NO2_sonda <- sum(is.na(iQ_sonda_datos_totales$Concentracion_NO2))
na_crudos_NOx_sonda <- sum(is.na(iQ_sonda_datos_totales$Concentracion_NOx))

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE CONTINUIDAD (BASE CRUDA - CON SONDA)\n")
cat("==============================================\n")
cat(sprintf("Total de registros crudos: %d\n", total_crudos_sonda))
cat(sprintf("Valores NA iniciales -> NO: %d | NO2: %d | NOx: %d\n", 
            na_crudos_NO_sonda, na_crudos_NO2_sonda, na_crudos_NOx_sonda))
cat("==============================================\n")
#medidas de resuemen crudas ----
generar_resumen_estadistico(iQ_sonda_datos_totales, "42iQ CON SONDA (Datos Crudos)")
#analisis de valores constantes ----
total_crudos_sonda <- nrow(iQ_sonda_datos_totales)
datos_filtro1_sonda <- iQ_sonda_datos_totales

# 2. Función para detectar rachas >= 4 y convertirlas en NA
filtrar_rachas <- function(vector_gases, limite = 4) {
  vector_seguro <- vector_gases
  vector_seguro[is.na(vector_seguro)] <- -999999
  
  rachas <- rle(vector_seguro)
  es_pegado <- rachas$lengths >= limite
  es_pegado[rachas$values == -999999] <- FALSE
  
  indices_pegados <- rep(es_pegado, rachas$lengths)
  vector_gases[indices_pegados] <- NA
  return(vector_gases)
}

# 3. Aplicar filtro a cada contaminante
datos_filtro1_sonda$Concentracion_NO  <- filtrar_rachas(datos_filtro1_sonda$Concentracion_NO, limite = 4)
datos_filtro1_sonda$Concentracion_NO2 <- filtrar_rachas(datos_filtro1_sonda$Concentracion_NO2, limite = 4)
datos_filtro1_sonda$Concentracion_NOx <- filtrar_rachas(datos_filtro1_sonda$Concentracion_NOx, limite = 4)

# 4. Contabilizar datos eliminados y nuevos totales válidos por gas
valido_NO_sonda  <- sum(!is.na(datos_filtro1_sonda$Concentracion_NO))
valido_NO2_sonda <- sum(!is.na(datos_filtro1_sonda$Concentracion_NO2))
valido_NOx_sonda <- sum(!is.na(datos_filtro1_sonda$Concentracion_NOx))

elim_f1_NO_sonda  <- total_crudos_sonda - valido_NO_sonda
elim_f1_NO2_sonda <- total_crudos_sonda - valido_NO2_sonda
elim_f1_NOx_sonda <- total_crudos_sonda - valido_NOx_sonda

# 5. Eliminar físicamente de la tabla las filas donde los 3 gases quedaron en NA
filas_vacias <- is.na(datos_filtro1_sonda$Concentracion_NO) & 
  is.na(datos_filtro1_sonda$Concentracion_NO2) & 
  is.na(datos_filtro1_sonda$Concentracion_NOx)
datos_filtro1_sonda <- datos_filtro1_sonda[!filas_vacias, ]

# 6. Reporte del Primer Filtro con el nuevo total
cat("\n==============================================\n")
cat("REPORTE PRIMER FILTRO: VALORES CONSTANTES (>= 4) CON SONDA\n")
cat("==============================================\n")
cat(sprintf("Total de datos crudos iniciales : %d registros\n\n", total_crudos_sonda))
cat("Datos eliminados y total válido restante (Post-Filtro 1):\n")
cat(sprintf("- NO  : %d eliminados (%.4f%%) -> Quedan %d datos válidos\n", 
            elim_f1_NO_sonda, (elim_f1_NO_sonda / total_crudos_sonda) * 100, valido_NO_sonda))
cat(sprintf("- NO2 : %d eliminados (%.4f%%) -> Quedan %d datos válidos\n", 
            elim_f1_NO2_sonda, (elim_f1_NO2_sonda / total_crudos_sonda) * 100, valido_NO2_sonda))
cat(sprintf("- NOx : %d eliminados (%.4f%%) -> Quedan %d datos válidos\n", 
            elim_f1_NOx_sonda, (elim_f1_NOx_sonda / total_crudos_sonda) * 100, valido_NOx_sonda))
cat("==============================================\n")
#analisis de valores negativos ----
# 1. Conteo de negativos ANTES de aplicar la limpieza (sobre datos_filtro1_sonda ya reducido)
neg_NO_sonda  <- sum(datos_filtro1_sonda$Concentracion_NO < 0, na.rm = TRUE)
neg_NO2_sonda <- sum(datos_filtro1_sonda$Concentracion_NO2 < 0, na.rm = TRUE)
neg_NOx_sonda <- sum(datos_filtro1_sonda$Concentracion_NOx < 0, na.rm = TRUE)

cat("\n==============================================\n")
cat("DIAGNÓSTICO DE NEGATIVOS (PRE-FILTRO 2) CON SONDA\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo (Post-Filtro 1): NO=%d | NO2=%d | NOx=%d\n\n", 
            valido_NO_sonda, valido_NO2_sonda, valido_NOx_sonda))
cat(sprintf("- NO  : %d registros negativos (%.2f%% del total válido)\n", neg_NO_sonda, (neg_NO_sonda/valido_NO_sonda)*100))
cat(sprintf("- NO2 : %d registros negativos (%.2f%% del total válido)\n", neg_NO2_sonda, (neg_NO2_sonda/valido_NO2_sonda)*100))
cat(sprintf("- NOx : %d registros negativos (%.2f%% del total válido)\n", neg_NOx_sonda, (neg_NOx_sonda/valido_NOx_sonda)*100))

# 2. Crear nueva base para aplicar el segundo filtro
datos_filtro2_sonda <- datos_filtro1_sonda
umbral_ruido <- -3.0
columnas_gases <- c("Concentracion_NO", "Concentracion_NO2", "Concentracion_NOx")

reporte_f2_sonda <- list()

# 3. Aplicar las reglas estadísticas de limpieza
for(col in columnas_gases) {
  vec <- datos_filtro2_sonda[[col]]
  
  idx_ruido  <- which(vec < 0 & vec >= umbral_ruido)
  idx_fallas <- which(vec < umbral_ruido)
  
  reporte_f2_sonda[[col]] <- list(ruido = length(idx_ruido), fallas = length(idx_fallas))
  
  vec[idx_ruido] <- 0
  vec[idx_fallas] <- NA
  
  datos_filtro2_sonda[[col]] <- vec
}

# 4. Reporte final post-filtro 2
cat("\n==============================================\n")
cat("REPORTE SEGUNDO FILTRO: CORRECCIÓN UMBRAL -3.0 (CON SONDA)\n")
cat("==============================================\n")
cat(sprintf("Base de cálculo (Post-Filtro 1): %d registros válidos\n\n", nrow(datos_filtro1_sonda)))

cat("1. Ruido térmico ajustado a 0 (valores entre 0 y -3.0):\n")
cat(sprintf("- NO  : %d registros corregidos a cero (%.2f%%).\n", reporte_f2_sonda$Concentracion_NO$ruido, (reporte_f2_sonda$Concentracion_NO$ruido/valido_NO_sonda)*100))
cat(sprintf("- NO2 : %d registros corregidos a cero (%.2f%%).\n", reporte_f2_sonda$Concentracion_NO2$ruido, (reporte_f2_sonda$Concentracion_NO2$ruido/valido_NO2_sonda)*100))
cat(sprintf("- NOx : %d registros corregidos a cero (%.2f%%).\n", reporte_f2_sonda$Concentracion_NOx$ruido, (reporte_f2_sonda$Concentracion_NOx$ruido/valido_NOx_sonda)*100))

cat("\n2. Fallas críticas descartadas a NA (valores < -3.0):\n")
cat(sprintf("- NO  : %d registros eliminados (%.4f%%).\n", reporte_f2_sonda$Concentracion_NO$fallas, (reporte_f2_sonda$Concentracion_NO$fallas/valido_NO_sonda)*100))
cat(sprintf("- NO2 : %d registros eliminados (%.4f%%).\n", reporte_f2_sonda$Concentracion_NO2$fallas, (reporte_f2_sonda$Concentracion_NO2$fallas/valido_NO2_sonda)*100))
cat(sprintf("- NOx : %d registros eliminados (%.4f%%).\n", reporte_f2_sonda$Concentracion_NOx$fallas, (reporte_f2_sonda$Concentracion_NOx$fallas/valido_NOx_sonda)*100))
cat("==============================================\n")
#media movil (60 mins) ----
base_final_sonda$Concentracion_NO  <- rollapply(base_final_sonda$Concentracion_NO, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
base_final_sonda$Concentracion_NO2 <- rollapply(base_final_sonda$Concentracion_NO2, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
base_final_sonda$Concentracion_NOx <- rollapply(base_final_sonda$Concentracion_NOx, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
#normalizacon ----
base_final_sonda_norm <- base_final_sonda
base_final_sonda_norm$Concentracion_NO  <- normalizar_min_max(base_final_sonda$Concentracion_NO)
base_final_sonda_norm$Concentracion_NO2 <- normalizar_min_max(base_final_sonda$Concentracion_NO2)
base_final_sonda_norm$Concentracion_NOx <- normalizar_min_max(base_final_sonda$Concentracion_NOx)

cat("\n==============================================\n")
cat("42iQ CON SONDA - REPORTE NORMALIZACIÓN\n")
cat("==============================================\n")
cat("Normalización Min-Max [0, 1] aplicada exitosamente.\n(Se conserva el 100% de la data válida post-filtros).\n")
cat("==============================================\n")

#medidas de resumen finales ----
generar_resumen_estadistico(base_final_sonda_norm, "42iQ CON SONDA (Normalizado Min-Max)")

############################################################################

base_final_sinsonda <- datos_filtro2_sinsonda
base_final_sonda    <- datos_filtro2_sonda

#reporte de datos 42iQ con y sin sonda ----
base_final_sinsonda <- datos_filtro2_sinsonda
base_final_sonda    <- datos_filtro2_sonda

# 2. Calcular los datos finales útiles (sin NAs) para el equipo SIN SONDA
final_NO_sinsonda  <- sum(!is.na(base_final_sinsonda$Concentracion_NO))
final_NO2_sinsonda <- sum(!is.na(base_final_sinsonda$Concentracion_NO2))
final_NOx_sinsonda <- sum(!is.na(base_final_sinsonda$Concentracion_NOx))

# 3. Calcular los datos finales útiles (sin NAs) para el equipo CON SONDA
final_NO_sonda  <- sum(!is.na(base_final_sonda$Concentracion_NO))
final_NO2_sonda <- sum(!is.na(base_final_sonda$Concentracion_NO2))
final_NOx_sonda <- sum(!is.na(base_final_sonda$Concentracion_NOx))

# 4. Imprimir Reporte Oficial para el documento
cat("\n==============================================\n")
cat("REPORTE OFICIAL: VOLUMEN DE DATOS TOTALMENTE VALIDADOS\n")
cat("==============================================\n")
cat("EQUIPO 42iQ SIN SONDA (Base para gráficos e informes):\n")
cat(sprintf("- NO  : %d registros 100%% válidos\n", final_NO_sinsonda))
cat(sprintf("- NO2 : %d registros 100%% válidos\n", final_NO2_sinsonda))
cat(sprintf("- NOx : %d registros 100%% válidos\n", final_NOx_sinsonda))
cat("----------------------------------------------\n")
cat("EQUIPO 42iQ CON SONDA (Base para gráficos e informes):\n")
cat(sprintf("- NO  : %d registros 100%% válidos\n", final_NO_sonda))
cat(sprintf("- NO2 : %d registros 100%% válidos\n", final_NO2_sonda))
cat(sprintf("- NOx : %d registros 100%% válidos\n", final_NOx_sonda))
cat("==============================================\n")










# 42i sin sonda -------------------------####################################

library(zoo)
#lectura bases ----
archivos_dat_sinsonda <- c("NOx_42i_07oct_PO_Sinsonda.dat", 
                           "NOx_42i_Portillo_14_Agosto_2025_Sinsonda.dat",
                           "NOx_42i_Portillo_28_agosto_2025_Sinsonda.dat")

lista_datos_sinsonda <- lapply(archivos_dat_sinsonda, function(archivo) {
  df <- read.table(archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
  df$Fecha_y_Hora <- as.POSIXct(paste(df$Date, df$Time), format = "%m-%d-%y %H:%M")
  
  # Rescatamos la columna Flags indispensable para el primer filtro
  df_limpio <- df[, c("Fecha_y_Hora", "Flags", "no", "nox")]
  colnames(df_limpio) <- c("Fecha_y_Hora", "Flags", "Concentracion_NO", "Concentracion_NOx")
  return(df_limpio)
})

i_sinsonda_datos_totales <- do.call(rbind, lista_datos_sinsonda)
i_sinsonda_datos_totales <- i_sinsonda_datos_totales[order(i_sinsonda_datos_totales$Fecha_y_Hora), ]
i_sinsonda_datos_totales <- i_sinsonda_datos_totales[!duplicated(i_sinsonda_datos_totales$Fecha_y_Hora), ]

total_crudos_42i_sin <- nrow(i_sinsonda_datos_totales)

#medidas de resumen crudas ----
generar_resumen_estadistico_42i <- function(df, nombre_base) {
  gases <- c("Concentracion_NO", "Concentracion_NOx")
  
  resumen <- data.frame(
    Gas = c("NO", "NOx"),
    Minimo = sapply(df[gases], min, na.rm = TRUE),
    Maximo = sapply(df[gases], max, na.rm = TRUE),
    Promedio = sapply(df[gases], mean, na.rm = TRUE),
    Mediana = sapply(df[gases], median, na.rm = TRUE),
    Desv_Estandar = sapply(df[gases], sd, na.rm = TRUE)
  )
  rownames(resumen) <- NULL
  
  cat("\n========================================================\n")
  cat("ESTADÍSTICAS:", nombre_base, "\n")
  cat("========================================================\n")
  print(format(resumen, digits = 4, nsmall = 4))
  cat("========================================================\n")
}

generar_resumen_estadistico_42i(i_sinsonda_datos_totales, "42i SIN SONDA (Datos Crudos)")


#filtro flags H ----
# Identificamos las filas donde Flags sea distinto a "------" eliminando espacios fantasma
idx_flags_malas <- trimws(i_sinsonda_datos_totales$Flags) == "-------H"
eliminados_flags <- sum(idx_flags_malas, na.rm = TRUE)

# Aplicar NA a las posiciones corruptas
i_sinsonda_datos_totales$Concentracion_NO[idx_flags_malas] <- NA
i_sinsonda_datos_totales$Concentracion_NOx[idx_flags_malas] <- NA

cat("\n==============================================\n")
cat("42i SIN SONDA - REPORTE FILTRO FLAGS\n")
cat("==============================================\n")
cat(sprintf("Datos eliminados por Flag de error (!= '------'): %d\n", eliminados_flags))
cat("==============================================\n")


#filtro de valores constantes ----
datos_filtro1_42i_sin <- i_sinsonda_datos_totales

filtrar_rachas <- function(vector_gases, limite = 4) {
  vector_seguro <- vector_gases
  vector_seguro[is.na(vector_seguro)] <- -999999
  rachas <- rle(vector_seguro)
  es_pegado <- rachas$lengths >= limite
  es_pegado[rachas$values == -999999] <- FALSE
  indices_pegados <- rep(es_pegado, rachas$lengths)
  vector_gases[indices_pegados] <- NA
  return(vector_gases)
}

datos_filtro1_42i_sin$Concentracion_NO  <- filtrar_rachas(datos_filtro1_42i_sin$Concentracion_NO, limite = 4)
datos_filtro1_42i_sin$Concentracion_NOx <- filtrar_rachas(datos_filtro1_42i_sin$Concentracion_NOx, limite = 4)

valido_NO_42i_sin  <- sum(!is.na(datos_filtro1_42i_sin$Concentracion_NO))
valido_NOx_42i_sin <- sum(!is.na(datos_filtro1_42i_sin$Concentracion_NOx))

cat("\n==============================================\n")
cat("42i SIN SONDA - REPORTE FILTRO 1 (CALIBRACIONES)\n")
cat("==============================================\n")
cat(sprintf("- NO eliminados : %d (Quedan %d válidos)\n", total_crudos_42i_sin - valido_NO_42i_sin, valido_NO_42i_sin))
cat(sprintf("- NOx eliminados: %d (Quedan %d válidos)\n", total_crudos_42i_sin - valido_NOx_42i_sin, valido_NOx_42i_sin))
cat("==============================================\n")


#método de tukey (outliers) ----

filtrar_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  # Límite superior para valores atípicos extremos (3 * IQR)
  limite_sup <- q3 + 3 * iqr
  
  idx_outliers <- which(x > limite_sup)
  x[idx_outliers] <- NA
  
  return(list(serie = x, limite = limite_sup, eliminados = length(idx_outliers)))
}

# Aplicar el filtro sobre la base post-valores constantes
resultado_outliers_sin_NO  <- filtrar_outliers_iqr(datos_filtro1_42i_sin$Concentracion_NO)
resultado_outliers_sin_NOx <- filtrar_outliers_iqr(datos_filtro1_42i_sin$Concentracion_NOx)

# Sobrescribir en el dataframe antes de pasar a la media móvil
datos_filtro1_42i_sin$Concentracion_NO  <- resultado_outliers_sin_NO$serie
datos_filtro1_42i_sin$Concentracion_NOx <- resultado_outliers_sin_NOx$serie

cat("\n==============================================\n")
cat("42i SIN SONDA - REPORTE FILTRO OUTLIERS (IQR)\n")
cat("==============================================\n")
cat(sprintf("Límite superior calculado para NO : %.4f ppb\n", resultado_outliers_sin_NO$limite))
cat(sprintf("Outliers extremos de NO eliminados: %d registros\n", resultado_outliers_sin_NO$eliminados))
cat("----------------------------------------------\n")
cat(sprintf("Límite superior calculado para NOx: %.4f ppb\n", resultado_outliers_sin_NOx$limite))
cat(sprintf("Outliers extremos de NOx eliminados: %d registros\n", resultado_outliers_sin_NOx$eliminados))
cat("==============================================\n")

#media movil (60 mins) ----

datos_filtro2_42i_sin <- datos_filtro1_42i_sin

datos_filtro2_42i_sin$Concentracion_NO <- rollapply(datos_filtro2_42i_sin$Concentracion_NO, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
datos_filtro2_42i_sin$Concentracion_NOx <- rollapply(datos_filtro2_42i_sin$Concentracion_NOx, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")

#normalización (75%) ----
normalizar_y_fijar_cero <- function(x) {
  # A. Normalización Min-Max (escala 0 a 1)
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  x_norm <- (x - min_x) / (max_x - min_x)
  
  p25 <- quantile(x_norm, 0.25, na.rm = TRUE)
  x_ajustado <- x_norm - p25
  
  idx_invalidos <- which(x_ajustado < 0)
  x_ajustado[idx_invalidos] <- NA
  
  return(list(serie_final = x_ajustado, descartados = length(idx_invalidos)))
}

resultado_NO  <- normalizar_y_fijar_cero(datos_filtro2_42i_sin$Concentracion_NO)
resultado_NOx <- normalizar_y_fijar_cero(datos_filtro2_42i_sin$Concentracion_NOx)

datos_filtro_final_42i_sin <- datos_filtro2_42i_sin
datos_filtro_final_42i_sin$Concentracion_NO  <- resultado_NO$serie_final
datos_filtro_final_42i_sin$Concentracion_NOx <- resultado_NOx$serie_final

cat("\n==============================================\n")
cat("42i SIN SONDA - REPORTE NORMALIZACIÓN Y AJUSTE '0'\n")
cat("==============================================\n")
cat(sprintf("Datos NO descartados por quedar bajo el nuevo 0 : %d\n", resultado_NO$descartados))
cat(sprintf("Datos NOx descartados por quedar bajo el nuevo 0: %d\n", resultado_NOx$descartados))
cat("==============================================\n")


#medidas de resumen final ----
generar_resumen_estadistico_42i(datos_filtro_final_42i_sin, "42i SIN SONDA (MM 60min + Norm + Ajuste 75%)")

##############################################################################

# 42i con sonda -------------------------####################################

library(zoo)
#lectura bases ----
archivos_dat_sonda <- c("42i 053025 1425_Sonda.dat", 
                        "NOx_42i_Portillo_junio_2025_Sonda.dat", 
                        "NOx_42i_julio_Portillo_Sonda.dat")

lista_datos_sonda <- lapply(archivos_dat_sonda, function(archivo) {
  df <- read.table(archivo, skip = 5, header = TRUE, stringsAsFactors = FALSE)
  df$Fecha_y_Hora <- as.POSIXct(paste(df$Date, df$Time), format = "%m-%d-%y %H:%M")
  
  # Rescatamos la columna Flags indispensable para el filtro
  df_limpio <- df[, c("Fecha_y_Hora", "Flags", "no", "nox")]
  colnames(df_limpio) <- c("Fecha_y_Hora", "Flags", "Concentracion_NO", "Concentracion_NOx")
  return(df_limpio)
})

i_sonda_datos_totales <- do.call(rbind, lista_datos_sonda)
i_sonda_datos_totales <- i_sonda_datos_totales[order(i_sonda_datos_totales$Fecha_y_Hora), ]
i_sonda_datos_totales <- i_sonda_datos_totales[!duplicated(i_sonda_datos_totales$Fecha_y_Hora), ]

total_crudos_42i_sonda <- nrow(i_sonda_datos_totales)


#resumen de medidas crudas ----
generar_resumen_estadistico_42i(i_sonda_datos_totales, "42i CON SONDA (Datos Crudos)")


#filtro flags H ----
# Identificamos las filas donde Flags sea distinto a "------"
idx_flags_malas_sonda <- trimws(i_sonda_datos_totales$Flags) == "------H"
eliminados_flags_sonda <- sum(idx_flags_malas_sonda, na.rm = TRUE)

# Aplicar NA a las posiciones corruptas
i_sonda_datos_totales$Concentracion_NO[idx_flags_malas_sonda] <- NA
i_sonda_datos_totales$Concentracion_NOx[idx_flags_malas_sonda] <- NA

cat("\n==============================================\n")
cat("42i CON SONDA - REPORTE FILTRO FLAGS\n")
cat("==============================================\n")
cat(sprintf("Datos eliminados por Flag de error (!= '------'): %d\n", eliminados_flags_sonda))
cat("==============================================\n")


#filtro de valores constantes ----
datos_filtro1_42i_sonda <- i_sonda_datos_totales

datos_filtro1_42i_sonda$Concentracion_NO  <- filtrar_rachas(datos_filtro1_42i_sonda$Concentracion_NO, limite = 4)
datos_filtro1_42i_sonda$Concentracion_NOx <- filtrar_rachas(datos_filtro1_42i_sonda$Concentracion_NOx, limite = 4)

valido_NO_42i_sonda  <- sum(!is.na(datos_filtro1_42i_sonda$Concentracion_NO))
valido_NOx_42i_sonda <- sum(!is.na(datos_filtro1_42i_sonda$Concentracion_NOx))

cat("\n==============================================\n")
cat("42i CON SONDA - REPORTE FILTRO 1 (CALIBRACIONES)\n")
cat("==============================================\n")
cat(sprintf("- NO eliminados : %d (Quedan %d válidos)\n", total_crudos_42i_sonda - valido_NO_42i_sonda, valido_NO_42i_sonda))
cat(sprintf("- NOx eliminados: %d (Quedan %d válidos)\n", total_crudos_42i_sonda - valido_NOx_42i_sonda, valido_NOx_42i_sonda))
cat("==============================================\n")


#metodo IQR de Tukey (outliers) ----

filtrar_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  limite_sup <- q3 + 3 * iqr
  
  idx_outliers <- which(x > limite_sup)
  x[idx_outliers] <- NA
  
  return(list(serie = x, limite = limite_sup, eliminados = length(idx_outliers)))
}


resultado_outliers_NO  <- filtrar_outliers_iqr(datos_filtro1_42i_sonda$Concentracion_NO)
resultado_outliers_NOx <- filtrar_outliers_iqr(datos_filtro1_42i_sonda$Concentracion_NOx)


datos_filtro1_42i_sonda$Concentracion_NO  <- resultado_outliers_NO$serie
datos_filtro1_42i_sonda$Concentracion_NOx <- resultado_outliers_NOx$serie

cat("\n==============================================\n")
cat("42i CON SONDA - REPORTE FILTRO OUTLIERS (IQR)\n")
cat("==============================================\n")
cat(sprintf("Límite superior calculado para NO : %.2f ppb\n", resultado_outliers_NO$limite))
cat(sprintf("Picos extremos de NO eliminados   : %d registros\n", resultado_outliers_NO$eliminados))
cat("----------------------------------------------\n")
cat(sprintf("Límite superior calculado para NOx: %.2f ppb\n", resultado_outliers_NOx$limite))
cat(sprintf("Picos extremos de NOx eliminados  : %d registros\n", resultado_outliers_NOx$eliminados))
cat("==============================================\n")
#media movil (60 mins) ----
datos_filtro2_42i_sonda <- datos_filtro1_42i_sonda

datos_filtro2_42i_sonda$Concentracion_NO <- rollapply(datos_filtro2_42i_sonda$Concentracion_NO, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")
datos_filtro2_42i_sonda$Concentracion_NOx <- rollapply(datos_filtro2_42i_sonda$Concentracion_NOx, width = 60, FUN = mean, na.rm = TRUE, fill = NA, align = "right")


#normalización (75%) ----
resultado_NO_sonda  <- normalizar_y_fijar_cero(datos_filtro2_42i_sonda$Concentracion_NO)
resultado_NOx_sonda <- normalizar_y_fijar_cero(datos_filtro2_42i_sonda$Concentracion_NOx)

datos_filtro_final_42i_sonda <- datos_filtro2_42i_sonda
datos_filtro_final_42i_sonda$Concentracion_NO  <- resultado_NO_sonda$serie_final
datos_filtro_final_42i_sonda$Concentracion_NOx <- resultado_NOx_sonda$serie_final

cat("\n==============================================\n")
cat("42i CON SONDA - REPORTE NORMALIZACIÓN Y AJUSTE '0'\n")
cat("==============================================\n")
cat(sprintf("Datos NO descartados por quedar bajo el nuevo 0 : %d\n", resultado_NO_sonda$descartados))
cat(sprintf("Datos NOx descartados por quedar bajo el nuevo 0: %d\n", resultado_NOx_sonda$descartados))
cat("==============================================\n")


#medidas de resumen finales ----
generar_resumen_estadistico_42i(datos_filtro_final_42i_sonda, "42i CON SONDA (MM 60min + Norm + Ajuste 75%)")
###############################################################################

#Graficos  --------------######################################################
#series de tiempo (todos juntos en ploty)----
library(plotly)

# Gráfico 1: 42iQ Sin Sonda
g1 <- plot_ly(base_final_sinsonda, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
              type = 'scatter', mode = 'lines', name = '42iQ Sin Sonda', 
              line = list(color = 'blue', width = 1.5)) %>%
  layout(title = "42iQ Sin Sonda", xaxis = list(title = "Fecha"), yaxis = list(title = "NO"))

# Gráfico 2: 42iQ Con Sonda
g2 <- plot_ly(base_final_sonda, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
              type = 'scatter', mode = 'lines', name = '42iQ Con Sonda', 
              line = list(color = 'red', width = 1.5)) %>%
  layout(title = "42iQ Con Sonda", xaxis = list(title = "Fecha"), yaxis = list(title = "NO"))

# Gráfico 3: 42i Sin Sonda 
# (Nota: uso i_sinsonda_datos_totales según la lectura de tu código)
g3 <- plot_ly(i_sinsonda_datos_totales, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
              type = 'scatter', mode = 'lines', name = '42i Sin Sonda', 
              line = list(color = 'green', width = 1.5)) %>%
  layout(title = "42i Sin Sonda", xaxis = list(title = "Fecha"), yaxis = list(title = "NO"))

# Gráfico 4: 42i Con Sonda
g4 <- plot_ly(i_sonda_datos_periodo, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
              type = 'scatter', mode = 'lines', name = '42i Con Sonda', 
              line = list(color = 'purple', width = 1.5)) %>%
  layout(title = "42i Con Sonda", xaxis = list(title = "Fecha"), yaxis = list(title = "NO"))

# --- 2. Gráfico Combinado (Los 4 en uno solo con Range Slider) ---

grafico_combinado <- plot_ly() %>%
  add_lines(data = base_final_sinsonda, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
            name = '42iQ Sin Sonda', line = list(color = 'rgba(0, 0, 255, 0.7)', width = 1.5)) %>%
  add_lines(data = base_final_sonda, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
            name = '42iQ Con Sonda', line = list(color = 'rgba(255, 0, 0, 0.7)', width = 1.5)) %>%
  add_lines(data = i_sinsonda_datos_totales, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
            name = '42i Sin Sonda', line = list(color = 'rgba(0, 128, 0, 0.7)', width = 1.5)) %>%
  add_lines(data = i_sonda_datos_periodo, x = ~Fecha_y_Hora, y = ~Concentracion_NO, 
            name = '42i Con Sonda', line = list(color = 'rgba(128, 0, 128, 0.7)', width = 1.5)) %>%
  layout(title = "Comparativa Global: Equipos NOx con y sin Sonda Termostatizada",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración de NO"))

# Para visualizarlo en RStudio, simplemente llama a la variable:
grafico_combinado




#grafica 42iQ sin sonda NO ----
plot(x = iQ_sinsonda_datos_totales$Fecha_y_Hora, 
     y = iQ_sinsonda_datos_totales$Concentracion_NO, 
     type = "l", col = "blue", lwd = 1.2,
     main = "42iQ Sin Sonda (Datos Crudos) - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración NO [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42iQ sin sonda NO final ----
plot(x = base_final_sinsonda$Fecha_y_Hora, 
     y = base_final_sinsonda$Concentracion_NO, 
     type = "l", col = "blue", lwd = 1.5,
     main = "42iQ Sin Sonda (Filtro ctes + Corrección Umbral -3.0) - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración NO [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42iQ sin sonda NOx ----
plot(x = iQ_sinsonda_datos_totales$Fecha_y_Hora, 
     y = iQ_sinsonda_datos_totales$Concentracion_NOx, 
     type = "l", col = "deepskyblue", lwd = 1.2,
     main = "42iQ Sin Sonda (Datos Crudos) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración NOx [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42iQ sin sonda NOx final ----
plot(x = base_final_sinsonda$Fecha_y_Hora, 
     y = base_final_sinsonda$Concentracion_NOx, 
     type = "l", col = "deepskyblue", lwd = 1.5,
     main = "42iQ Sin Sonda (Filtro ctes + Corrección Umbral -3.0) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración NOx [ppb]")
abline(h = 0, col = "black", lty = 2)


#grafica 42iQ con sonda NO ----
plot(x = iQ_sonda_datos_totales$Fecha_y_Hora, 
     y = iQ_sonda_datos_totales$Concentracion_NO, 
     type = "l", col = "red", lwd = 1.2,
     main = "42iQ Con Sonda (Datos Crudos) - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración NO [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42iQ con sonda NO final ----
plot(x = base_final_sonda$Fecha_y_Hora, 
     y = base_final_sonda$Concentracion_NO, 
     type = "l", col = "red", lwd = 1.5,
     main = "42iQ Con Sonda (Filtro ctes + Corrección Umbral -3.0) - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración NO [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42iQ con sonda NOx ----
plot(x = iQ_sonda_datos_totales$Fecha_y_Hora, 
     y = iQ_sonda_datos_totales$Concentracion_NOx, 
     type = "l", col = "darkred", lwd = 1.2,
     main = "42iQ Con Sonda (Datos Crudos) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración NOx [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42iQ con sonda NOx final ----
plot(x = base_final_sonda$Fecha_y_Hora, 
     y = base_final_sonda$Concentracion_NOx, 
     type = "l", col = "darkred", lwd = 1.5,
     main = "42iQ Con Sonda (Filtro ctes + Corrección Umbral -3.0) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración NOx [ppb]")
abline(h = 0, col = "black", lty = 2)


#grafica 42i sin sonda NO ----
plot(x = i_sinsonda_datos_totales$Fecha_y_Hora, 
     y = i_sinsonda_datos_totales$Concentracion_NO, 
     type = "l", col = "darkgreen", lwd = 1.5,
     main = "42i Sin Sonda (filtro de flag H + filtro de datos ctes) - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración NO [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42i sin sonda NO final ----
plot(x = datos_filtro_final_42i_sin$Fecha_y_Hora, 
     y = datos_filtro_final_42i_sin$Concentracion_NO, 
     type = "l", col = "darkgreen", lwd = 1.5,
     main = "42i Sin Sonda (filtro de flag H+filtro de datos ctes+tukey+media movil+normalización) - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración Relativa de NO")
#grafica 42i sin sonda NOx  ----
plot(x = i_sinsonda_datos_totales$Fecha_y_Hora, 
     y = i_sinsonda_datos_totales$Concentracion_NOx, 
     type = "l", col = "darkorange", lwd = 1.5,
     main = "42i Sin Sonda (filtro de flag H + filtro de datos ctes) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración NOx [ppb]")
abline(h = 0, col = "black", lty = 2)

#grafica 42i sin sonda NOx final ----

plot(x = datos_filtro_final_42i_sin$Fecha_y_Hora, 
     y = datos_filtro_final_42i_sin$Concentracion_NOx, 
     type = "l", col = "darkorange", lwd = 1.5,
     main = "42i Sin Sonda (filtro de flag H+filtro de datos ctes+tukey+media movil+normalización) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración Relativa de NOx")


#grafica 42i con sonda NO ----

plot(x = datos_filtro1_42i_sonda$Fecha_y_Hora, 
     y = datos_filtro1_42i_sonda$Concentracion_NO, 
     type = "l", col = "purple", lwd = 1.5,
     main = "42i Con Sonda (filtro de flag H + ctes) - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración NO [ppb]")
abline(h = 0, col = "black", lty = 2)


#grafica 42i con sonda NOx final (filtro de flag H+filtro de datos ctes+media movil+normalización)
#grafica 42i con sonda NO final ----
plot(x = datos_filtro_final_42i_sonda$Fecha_y_Hora, 
     y = datos_filtro_final_42i_sonda$Concentracion_NO, 
     type = "l", col = "purple", lwd = 1.5,
     main = "42i Con Sonda(filtro de flag H+filtro de datos ctes+tukey+media movil+normalización)  - NO", 
     xlab = "Fecha y Hora", ylab = "Concentración Relativa de NO")
#grafica 42i con sonda NOx ----
plot(x = datos_filtro1_42i_sonda$Fecha_y_Hora, 
     y = datos_filtro1_42i_sonda$Concentracion_NOx, 
     type = "l", col = "brown", lwd = 1.2,
     main = "42i con Sonda (filtro de flag H + filtro de datos ctes) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración NOx [ppb]")
abline(h = 0, col = "black", lty = 2)
#grafica 42i con sonda NOx final ----
plot(x = datos_filtro_final_42i_sonda$Fecha_y_Hora, 
     y = datos_filtro_final_42i_sonda$Concentracion_NOx, 
     type = "l", col = "brown", lwd = 1.5,
     main = "42i con Sonda (filtro de flag H+filtro de datos ctes+tukey+media movil+normalización) - NOx", 
     xlab = "Fecha y Hora", ylab = "Concentración Relativa de NOx")




#Comparaciones efecto sonda


# periodo 1: Mayo a finales de Julio (42i con sonda vs 42iQ sin sonda) ----


#efecto sonda - NO
comp_sonda_periodo1_NO <- plot_ly() %>%
  add_lines(data = datos_filtro_final_42i_sonda, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42i Con Sonda', line = list(color = 'purple', width = 1.2)) %>%
  add_lines(data = base_final_sinsonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42iQ Sin Sonda', line = list(color = 'blue', width = 1.2)) %>%
  layout(title = "Efecto Sonda (Mayo-Julio): NO Relativo",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración NO [0 - 1]"))

comp_sonda_periodo1_NO


#Efecto Sonda - NOx
comp_sonda_periodo1_NOx <- plot_ly() %>%
  add_lines(data = datos_filtro_final_42i_sonda, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42i Con Sonda', line = list(color = 'brown', width = 1.2)) %>%
  add_lines(data = base_final_sinsonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42iQ Sin Sonda', line = list(color = 'deepskyblue', width = 1.2)) %>%
  layout(title = "Efecto Sonda (Mayo-Julio): NOx Relativo",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración NOx [0 - 1]"))

comp_sonda_periodo1_NOx


# PERIODO 2: Finales de Julio a Octubre (42iQ con sonda vs 42i sin sonda) ----

#Efecto Sonda - NO
comp_sonda_periodo2_NO <- plot_ly() %>%
  add_lines(data = base_final_sonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42iQ Con Sonda', line = list(color = 'red', width = 1.2)) %>%
  add_lines(data = datos_filtro_final_42i_sin, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42i Sin Sonda', line = list(color = 'darkgreen', width = 1.2)) %>%
  layout(title = "Efecto Sonda (Julio-Octubre): NO Relativo",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración NO [0 - 1]"))

comp_sonda_periodo2_NO


#Efecto Sonda - NOx
comp_sonda_periodo2_NOx <- plot_ly() %>%
  add_lines(data = base_final_sonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42iQ Con Sonda', line = list(color = 'darkred', width = 1.2)) %>%
  add_lines(data = datos_filtro_final_42i_sin, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42i Sin Sonda', line = list(color = 'darkorange', width = 1.2)) %>%
  layout(title = "Efecto Sonda (Julio-Octubre): NOx Relativo",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "Concentración NOx [0 - 1]"))

comp_sonda_periodo2_NOx




#Comparaciones 42iQ vs 42i

library(plotly)


#comparaciones 42iQ vs 42i


#NO sin Sonda ----
comp_sinsonda_NO <- plot_ly() %>%
  add_lines(data = base_final_sinsonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42iQ Sin Sonda', line = list(color = 'blue', width = 1.2)) %>%
  add_lines(data = datos_filtro_final_42i_sin, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42i Sin Sonda', line = list(color = 'darkgreen', width = 1.2)) %>%
  layout(title = "Comparación NO (Sin Sonda): 42iQ vs 42i",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "NO Relativo [0 - 1]"))

comp_sinsonda_NO


#NOx Sin Sonda ----
comp_sinsonda_NOx <- plot_ly() %>%
  add_lines(data = base_final_sinsonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42iQ Sin Sonda', line = list(color = 'deepskyblue', width = 1.2)) %>%
  add_lines(data = datos_filtro_final_42i_sin, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42i Sin Sonda', line = list(color = 'darkorange', width = 1.2)) %>%
  layout(title = "Comparación NOx (Sin Sonda): 42iQ vs 42i",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "NOx Relativo [0 - 1]"))

comp_sinsonda_NOx


#NO Con Sonda ----
comp_sonda_NO <- plot_ly() %>%
  add_lines(data = base_final_sonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42iQ Con Sonda', line = list(color = 'red', width = 1.2)) %>%
  add_lines(data = datos_filtro_final_42i_sonda, x = ~Fecha_y_Hora, y = ~Concentracion_NO,
            name = '42i Con Sonda', line = list(color = 'purple', width = 1.2)) %>%
  layout(title = "Comparación NO (Con Sonda): 42iQ vs 42i",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "NO Relativo [0 - 1]"))

comp_sonda_NO


#NOx Con Sonda ----
comp_sonda_NOx <- plot_ly() %>%
  add_lines(data = base_final_sonda_norm, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42iQ Con Sonda', line = list(color = 'darkred', width = 1.2)) %>%
  add_lines(data = datos_filtro_final_42i_sonda, x = ~Fecha_y_Hora, y = ~Concentracion_NOx,
            name = '42i Con Sonda', line = list(color = 'green', width = 1.2)) %>%
  layout(title = "Comparación NOx (Con Sonda): 42iQ vs 42i",
         xaxis = list(title = "Fecha y Hora", rangeslider = list(type = "date")),
         yaxis = list(title = "NOx Relativo [0 - 1]"))

comp_sonda_NOx

#
