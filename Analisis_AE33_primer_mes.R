# ============================================================
# ANALISIS EXPLORATORIO - BLACK CARBON
# ============================================================

# ------------------------------------------------------------
# 1. PAQUETES
# ------------------------------------------------------------

# Si no los tienes instalados:
# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)
library(lubridate)


# ------------------------------------------------------------
# 2. IMPORTAR LOS DATOS
# ------------------------------------------------------------

datos <- read.csv(
  "Magee_ONA_smoothed_2026-03-13_a_2026-04-23_Portillo.csv",
  stringsAsFactors = FALSE,
  head=TRUE
)

# Revisar estructura
str(datos)

# Primeras observaciones
head(datos)

# Últimas observaciones
tail(datos)


# ------------------------------------------------------------
# 3. CONVERTIR FECHA
# ------------------------------------------------------------

# Hay fechas que tienen hora y otras que solamente tienen fecha,
# por lo que usamos parse_date_time()

datos$fecha <- parse_date_time(
  datos$date_UTC.4,
  orders = c("ymd HMS", "ymd")
)

# Revisar fechas
head(datos$fecha)

# Eliminar la columna original si no la necesitamos
# (opcional)
# datos$date_UTC.4 <- NULL


# ------------------------------------------------------------
# 4. INFORMACION GENERAL
# ------------------------------------------------------------

# Dimensiones
dim(datos)

# Número de observaciones
nrow(datos)

# Número de variables
ncol(datos)

# Nombres de las variables
names(datos)

# Estructura
str(datos)


# ------------------------------------------------------------
# 5. VALORES FALTANTES
# ------------------------------------------------------------

# Cantidad de NA por variable
colSums(is.na(datos))

# Porcentaje de NA
round(
  colSums(is.na(datos)) / nrow(datos) * 100,
  2
)


# ------------------------------------------------------------
# 6. ESTADISTICA DESCRIPTIVA
# ------------------------------------------------------------

# Resumen general
summary(datos)

# Para las variables numéricas
variables_numericas <- c(
  "BC",
  "RefCh6",
  "Sen1Ch6",
  "ATN",
  "BC_ONA",
  "n_avg"
)

# Estadísticas más detalladas
estadisticas <- datos %>%
  summarise(
    across(
      all_of(variables_numericas),
      list(
        media = ~mean(.x, na.rm = TRUE),
        mediana = ~median(.x, na.rm = TRUE),
        desviacion = ~sd(.x, na.rm = TRUE),
        minimo = ~min(.x, na.rm = TRUE),
        maximo = ~max(.x, na.rm = TRUE),
        Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
        Q3 = ~quantile(.x, 0.75, na.rm = TRUE)
      )
    )
  )

estadisticas


# ------------------------------------------------------------
# 7. HISTOGRAMAS
# ------------------------------------------------------------

# Black Carbon
ggplot(datos, aes(x = BC)) +
  geom_histogram(
    bins = 50,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución de Black Carbon",
    x = "Black Carbon",
    y = "Frecuencia"
  ) +
  theme_minimal()


# BC_ONA
ggplot(datos, aes(x = BC_ONA)) +
  geom_histogram(
    bins = 50,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución de BC_ONA",
    x = "BC_ONA",
    y = "Frecuencia"
  ) +
  theme_minimal()

pacf(datos$BC_ONA, na.action=na.pass)


# ATN
ggplot(datos, aes(x = ATN)) +
  geom_histogram(
    bins = 50,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución de ATN",
    x = "ATN",
    y = "Frecuencia"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 8. BOXPLOTS
# ------------------------------------------------------------

ggplot(datos, aes(y = BC)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Boxplot de Black Carbon",
    y = "Black Carbon"
  ) +
  theme_minimal()


ggplot(datos, aes(y = BC_ONA)) +
  geom_boxplot(na.rm = TRUE) +
  labs(
    title = "Boxplot de BC_ONA",
    y = "BC_ONA"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 9. SERIE TEMPORAL DE BLACK CARBON
# ------------------------------------------------------------

ggplot(datos, aes(x = fecha, y = BC)) +
  geom_line(na.rm = TRUE) +
  labs(
    title = "Black Carbon a través del tiempo",
    x = "Fecha",
    y = "Black Carbon"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 10. SERIE TEMPORAL DE BC_ONA
# ------------------------------------------------------------

ggplot(datos, aes(x = fecha, y = BC_ONA)) +
  geom_line(na.rm = TRUE) +
  labs(
    title = "BC_ONA a través del tiempo",
    x = "Fecha",
    y = "BC_ONA"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 11. COMPARAR BC Y BC_ONA
# ------------------------------------------------------------

ggplot(datos) +
  geom_line(
    aes(x = fecha, y = BC),
    na.rm = TRUE
  ) +
  geom_line(
    aes(x = fecha, y = BC_ONA),
    na.rm = TRUE
  ) +
  labs(
    title = "Comparación entre BC y BC_ONA",
    x = "Fecha",
    y = "Black Carbon"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 12. DISPERSION BC VS BC_ONA
# ------------------------------------------------------------

ggplot(datos, aes(x = BC, y = BC_ONA)) +
  geom_point(alpha = 0.3, na.rm = TRUE) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    na.rm = TRUE
  ) +
  labs(
    title = "Relación entre BC y BC_ONA",
    x = "BC",
    y = "BC_ONA"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 13. MATRIZ DE CORRELACIONES
# ------------------------------------------------------------

correlaciones <- cor(
  datos[variables_numericas],
  use = "complete.obs"
)

round(correlaciones, 3)


# ------------------------------------------------------------
# 14. CORRELACION ESPECIFICAMENTE BC vs BC_ONA
# ------------------------------------------------------------

cor(
  datos$BC,
  datos$BC_ONA,
  use = "complete.obs"
)


# ------------------------------------------------------------
# 15. DETECTAR VALORES EXTREMOS
# ------------------------------------------------------------

# Valores de BC menores que cero
datos %>%
  filter(BC < 0) %>%
  select(fecha, BC)

# Valores extremadamente altos
quantile(
  datos$BC,
  probs = c(0.01, 0.05, 0.5, 0.95, 0.99),
  na.rm = TRUE
)


# ------------------------------------------------------------
# 16. PROMEDIO DIARIO DE BLACK CARBON
# ------------------------------------------------------------

datos_diarios <- datos %>%
  mutate(dia = as.Date(fecha)) %>%
  group_by(dia) %>%
  summarise(
    BC_promedio = mean(BC, na.rm = TRUE),
    BC_mediana = median(BC, na.rm = TRUE),
    BC_sd = sd(BC, na.rm = TRUE),
    BC_min = min(BC, na.rm = TRUE),
    BC_max = max(BC, na.rm = TRUE),
    n = sum(!is.na(BC))
  )

head(datos_diarios)


# ------------------------------------------------------------
# 17. SERIE TEMPORAL DEL PROMEDIO DIARIO
# ------------------------------------------------------------

ggplot(datos_diarios, aes(x = dia, y = BC_promedio)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Concentración promedio diaria de Black Carbon",
    x = "Fecha",
    y = "BC promedio"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 18. PROMEDIO POR HORA DEL DÍA
# ------------------------------------------------------------

datos_hora <- datos %>%
  mutate(hora = hour(fecha)) %>%
  group_by(hora) %>%
  summarise(
    BC_promedio = mean(BC, na.rm = TRUE),
    BC_mediana = median(BC, na.rm = TRUE),
    n = sum(!is.na(BC))
  )

datos_hora


# ------------------------------------------------------------
# 19. CICLO DIARIO
# ------------------------------------------------------------

ggplot(datos_hora, aes(x = hora, y = BC_promedio)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Ciclo diario promedio de Black Carbon",
    x = "Hora del día",
    y = "BC promedio"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 20. PROMEDIO POR DÍA DE LA SEMANA
# ------------------------------------------------------------

datos_dia_semana <- datos %>%
  mutate(
    dia_semana = wday(
      fecha,
      label = TRUE,
      abbr = FALSE
    )
  ) %>%
  group_by(dia_semana) %>%
  summarise(
    BC_promedio = mean(BC, na.rm = TRUE),
    BC_mediana = median(BC, na.rm = TRUE)
  )

datos_dia_semana


ggplot(
  datos_dia_semana,
  aes(x = dia_semana, y = BC_promedio)
) +
  geom_col() +
  labs(
    title = "Black Carbon promedio según día de la semana",
    x = "Día de la semana",
    y = "BC promedio"
  ) +
  theme_minimal()

