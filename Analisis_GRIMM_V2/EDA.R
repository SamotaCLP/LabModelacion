# Análisis exploratorio de datos
# Comparación GRIMM Cetam vs GRIMM MT
# ---------------------------------------------------------------------

# Librerías
pkgs <- c(
  "openxlsx",
  "readxl",
  "tidyverse",
  "here",
  "lubridate",
  "moments"
)

invisible(lapply(pkgs, library, character.only = TRUE))


# ---------------------------------------------------------------------
# 1. LECTURA DE LOS DATOS
# ---------------------------------------------------------------------

archivo_cetam <- here(
  "Datos",
  "GRIMM 11D Cetam 22052025.xlsx"
)

archivo_mt <- here(
  "Datos",
  "GRIMM 11D MT 22052025.xlsx"
)


# ---------------------------------------------------------------------
# 2. EXTRAER MASS VALUES
# ---------------------------------------------------------------------

# Los encabezados reales están en la fila 5 del Excel,
# por lo que se utiliza skip = 4.

cetam_raw <- read_excel(
  archivo_cetam,
  sheet = "Mass values",
  skip = 4
)

mt_raw <- read_excel(
  archivo_mt,
  sheet = "Mass values",
  skip = 4
)


# ---------------------------------------------------------------------
# 3. SELECCIONAR VARIABLES DE INTERÉS
# ---------------------------------------------------------------------

cetam <- cetam_raw %>%
  select(
    date = `date&time`,
    PM10 = `PM10 [ug/m3]`,
    PM2.5 = `PM2,5 [ug/m3]`,
    PM1 = `PM1 [ug/m3]`
  ) %>%
  mutate(
    date = dmy_hms(date)
  )


mt <- mt_raw %>%
  select(
    date = `date&time`,
    PM10 = `PM10 [ug/m3]`,
    PM2.5 = `PM2,5 [ug/m3]`,
    PM1 = `PM1 [ug/m3]`
  ) %>%
  mutate(
    date = dmy_hms(date)
  )


# ---------------------------------------------------------------------
# 4. ELIMINAR REGISTROS FUERA DEL PERÍODO DE INTERÉS
# ---------------------------------------------------------------------

# El archivo MT contiene 3 registros del año 2022.
# Se consideran registros ajenos al período de medición 2025.

cetam <- cetam %>%
  filter(year(date) == 2025)

mt <- mt %>%
  filter(year(date) == 2025)


# ---------------------------------------------------------------------
# 5. SINCRONIZACIÓN TEMPORAL
# ---------------------------------------------------------------------

# Cetam mide aproximadamente cada 6 segundos,
# mientras que MT mide aproximadamente cada minuto.
#
# Para comparar ambas máquinas a la misma resolución temporal,
# se calcula el promedio de las mediciones de Cetam dentro de cada minuto.

cetam_min <- cetam %>%
  mutate(
    date = floor_date(date, unit = "minute")
  ) %>%
  group_by(date) %>%
  summarise(
    cetam_PM1   = mean(PM1, na.rm = TRUE),
    cetam_PM2.5 = mean(PM2.5, na.rm = TRUE),
    cetam_PM10  = mean(PM10, na.rm = TRUE),
    .groups = "drop"
  )


# MT ya tiene aproximadamente una medición por minuto.
# Simplemente redondeamos hacia abajo al minuto correspondiente.

mt_min <- mt %>%
  mutate(
    date = floor_date(date, unit = "minute")
  ) %>%
  group_by(date) %>%
  summarise(
    mt_PM1   = mean(PM1, na.rm = TRUE),
    mt_PM2.5 = mean(PM2.5, na.rm = TRUE),
    mt_PM10  = mean(PM10, na.rm = TRUE),
    .groups = "drop"
  )


# ---------------------------------------------------------------------
# 6. UNIR LAS DOS MÁQUINAS
# ---------------------------------------------------------------------

comparison_df <- cetam_min %>%
  inner_join(
    mt_min,
    by = "date"
  ) %>%
  arrange(date)


# ---------------------------------------------------------------------
# 7. PASAR A FORMATO LARGO
# ---------------------------------------------------------------------

dat <- comparison_df %>%
  pivot_longer(
    cols = -date,
    names_to = c("instrument", "fraction"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = instrument,
    values_from = value
  ) %>%
  rename(
    cetam = CETAM,
    mt = MT
  ) %>%
  mutate(
    date = as.POSIXct(date),
    
    fraction = factor(
      fraction,
      levels = c("PM10", "PM2.5", "PM1")
    ),
    
    # Mediciones negativas se reemplazan por 0
    cetam = pmax(cetam, 0),
    mt = pmax(mt, 0),
    
    # Diferencia absoluta
    D = cetam - mt,
    
    # Promedio de ambas mediciones
    M = (cetam + mt) / 2
  ) %>%
  arrange(fraction, date)


# ---------------------------------------------------------------------
# 8. GUARDAR DATOS EN FORMATO RDS
# ---------------------------------------------------------------------

saveRDS(
  dat,
  here(
    "Resultados",
    "Resultados Grimm-Cetam-MT",
    "dat.rds"
  )
)


# ---------------------------------------------------------------------
# 9. DESCRIPTIVOS POR FRACCIÓN E INSTRUMENTO
# ---------------------------------------------------------------------

desc <- dat %>%
  pivot_longer(
    c(cetam, mt),
    names_to = "instrument",
    values_to = "value"
  ) %>%
  group_by(fraction, instrument) %>%
  summarise(
    n = sum(!is.na(value)),
    media = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    mediana = median(value, na.rm = TRUE),
    IQR = IQR(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    asimetria = moments::skewness(
      value,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


write_csv(
  desc,
  here(
    "Resultados",
    "Resultados Grimm-Cetam-MT",
    "tablas",
    "01descriptivo.csv"
  )
)

print(desc)


# ---------------------------------------------------------------------
# 10. DESCRIPTIVOS DE LAS DIFERENCIAS
# ---------------------------------------------------------------------

desc_dif <- dat %>%
  group_by(fraction) %>%
  summarise(
    media = mean(D, na.rm = TRUE),
    sd = sd(D, na.rm = TRUE),
    mediana = median(D, na.rm = TRUE),
    IQR = IQR(D, na.rm = TRUE),
    min = min(D, na.rm = TRUE),
    max = max(D, na.rm = TRUE),
    asimetria = moments::skewness(
      D,
      na.rm = TRUE
    ),
    curtosis = moments::kurtosis(
      D,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


write_csv(
  desc_dif,
  here(
    "Resultados",
    "Resultados Grimm-Cetam-MT",
    "tablas",
    "02descriptivo_dif.csv"
  )
)

print(desc_dif)


# ---------------------------------------------------------------------
# 11. GRÁFICOS
# ---------------------------------------------------------------------

# Series temporales por fracción
source(
  here(
    "Códigos R",
    "Códigos Grimm-Cetam-MT",
    "EDA Interactivos",
    "Dygraph por fraccion.R"
  )
)


# Series temporales de todas las fracciones
source(
  here(
    "Códigos R",
    "Códigos Grimm-Cetam-MT",
    "EDA Interactivos",
    "Dygraph Fracciones (Todas).R"
  )
)


# Boxplots de las mediciones
source(
  here(
    "Códigos R",
    "Códigos Grimm-Cetam-MT",
    "EDA Interactivos",
    "Boxplot Medidas Interactivos.R"
  )
)


# Histogramas de las diferencias
source(
  here(
    "Códigos R",
    "Códigos Grimm-Cetam-MT",
    "EDA Interactivos",
    "Histogramas Interactivos.R"
  )
)


# Boxplots de las diferencias
source(
  here(
    "Códigos R",
    "Códigos Grimm-Cetam-MT",
    "EDA Interactivos",
    "Boxplots interactivos.R"
  )
)


# Q-Q plots de las diferencias
source(
  here(
    "Códigos R",
    "Códigos Grimm-Cetam-MT",
    "EDA Interactivos",
    "QQ Plots Interactivos.R"
  )
)


# Dispersión Cetam vs MT
source(
  here(
    "Códigos R",
    "Códigos Grimm-Cetam-MT",
    "EDA Interactivos",
    "Scatter Interactivo.R"
  )
)


# ---------------------------------------------------------------------
# 12. PENDIENTE OLS POR FRACCIÓN
# ---------------------------------------------------------------------

pendientes <- dat %>%
  group_by(fraction) %>%
  summarise(
    pendiente_ols = coef(lm(cetam ~ mt))[2],
    intercepto = coef(lm(cetam ~ mt))[1],
    .groups = "drop"
  )

print(pendientes)