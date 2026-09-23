# 1. Cargar paquetes
library(tidyverse)
library(lubridate)
library(patchwork)

# 2. Definir los nombres exactos de los archivos
archivos_csv <- c(
  "NOx-junio-julio/12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv",
  "NOx-junio-julio/12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv"
)

archivos_dat <- c(
  "NOx-junio-julio/NOx_42i_Portillo_junio_2025_Sonda.dat",
  "NOx-junio-julio/NOx_42i_julio_Portillo_Sonda.dat"
)

# 3. Función para leer los CSV (Equipo Sin Sonda)
leer_csv_sinsonda <- function(archivo) {
  
  df <- read_csv(
    archivo,
    show_col_types = FALSE
  )
  
  df_limpio <- df %>%
    select(
      tiempo_raw = 1,
      no_sinsonda = 8,
      nox_sinsonda = 10
    ) %>%
    mutate(
      tiempo = dmy_hms(as.character(tiempo_raw)),
      no_sinsonda = as.numeric(no_sinsonda),
      nox_sinsonda = as.numeric(nox_sinsonda)
    ) %>%
    filter(!is.na(tiempo)) %>%
    mutate(
      tiempo_redondeado = floor_date(tiempo, "10 minutes")
    ) %>%
    group_by(tiempo_redondeado) %>%
    summarise(
      no_sinsonda = mean(no_sinsonda, na.rm = TRUE),
      nox_sinsonda = mean(nox_sinsonda, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_limpio)
}

# 4. Función para leer los DAT (Equipo Sonda) - Corregida
leer_dat_sonda <- function(archivo) {
  
  df <- read_table(
    archivo,
    comment = ";;", # Ignora todas las líneas de encabezado con metadatos (;;)
    col_types = cols(
      Time = col_character(),
      Date = col_character(),
      Flags = col_character(),
      no = col_double(),
      nox = col_double(),
      hino = col_double(),
      hinox = col_double()
    )
  )
  
  df_limpio <- df %>%
    mutate(
      # Unimos Date ("05-30-25") y Time ("14:45") -> mdy_hm procesa bien años de 2 dígitos
      tiempo = mdy_hm(paste(Date, Time)),
      tiempo_redondeado = floor_date(tiempo, "10 minutes"),
      no_sonda = no,
      nox_sonda = nox
    ) %>%
    filter(!is.na(tiempo_redondeado)) %>%
    group_by(tiempo_redondeado) %>%
    summarise(
      no_sonda = mean(no_sonda, na.rm = TRUE),
      nox_sonda = mean(nox_sonda, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_limpio)
}

# 5. Aplicar las funciones a los archivos y apilarlos
todas_sinsonda <- map_dfr(archivos_csv, leer_csv_sinsonda) %>%
  group_by(tiempo_redondeado) %>%
  summarise(no_sinsonda = mean(no_sinsonda, na.rm = TRUE),
            nox_sinsonda = mean(nox_sinsonda, na.rm = TRUE),
            .groups = "drop")

todas_sonda <- map_dfr(archivos_dat, leer_dat_sonda) %>%
  group_by(tiempo_redondeado) %>%
  summarise(no_sonda = mean(no_sonda, na.rm = TRUE),
            nox_sonda = mean(nox_sonda, na.rm = TRUE),
            .groups = "drop")

cat("\n--- SIN SONDA ---\n")
print(range(todas_sinsonda$tiempo_redondeado, na.rm = TRUE))
print(nrow(todas_sinsonda))

cat("\n--- CON SONDA ---\n")
print(range(todas_sonda$tiempo_redondeado, na.rm = TRUE))
print(nrow(todas_sonda))

cat("\n--- FECHAS EN COMÚN ---\n")

fechas_comunes <- intersect(
  todas_sonda$tiempo_redondeado,
  todas_sinsonda$tiempo_redondeado
)

print(length(fechas_comunes))

if (length(fechas_comunes) > 0) {
  print(head(fechas_comunes))
  print(tail(fechas_comunes))
}

# 6. UNIR AMBAS BASES
df_unido <- inner_join(todas_sonda, todas_sinsonda, by = "tiempo_redondeado") %>%
  rename(tiempo = tiempo_redondeado) %>%
  drop_na()

print(paste("--> Puntos emparejados (cada 10 min):", nrow(df_unido)))

cat("\n==============================\n")
cat("DIAGNÓSTICO DE df_unido\n")
cat("==============================\n")

cat("Número de filas:", nrow(df_unido), "\n")

if (nrow(df_unido) > 0) {
  
  print(head(df_unido))
  
  cat("\nRango temporal:\n")
  print(range(df_unido$tiempo, na.rm = TRUE))
  
  cat("\nResumen de NO:\n")
  print(summary(df_unido[, c("no_sonda", "no_sinsonda")]))
  
  cat("\nResumen de NOx:\n")
  print(summary(df_unido[, c("nox_sonda", "nox_sinsonda")]))
  
} else {
  
  cat("\n¡¡¡ df_unido ESTÁ VACÍO !!!\n")
  cat("No existen timestamps coincidentes entre ambos equipos.\n")
}

# 7. CÁLCULOS PARA BLAND-ALTMAN DE AMBOS COMPUESTOS
df_unido <- df_unido %>%
  mutate(
    # Para NO
    dif_no = no_sonda - no_sinsonda,
    prom_no = (no_sonda + no_sinsonda) / 2,
    # Para NOx
    dif_nox = nox_sonda - nox_sinsonda,
    prom_nox = (nox_sonda + nox_sinsonda) / 2
  )


# ---------------------------------------------------------
# 8. CREACIÓN DE GRÁFICOS: NO (Óxido Nítrico)
# ---------------------------------------------------------

series_no <- df_unido %>%
  pivot_longer(cols = c(no_sonda, no_sinsonda), names_to = "equipo", values_to = "medicion") %>%
  ggplot(aes(x = tiempo, y = medicion, color = equipo)) +
  geom_line(alpha = 0.8, linewidth = 0.8) +
  scale_color_manual(values = c("no_sonda" = "#0072B2", "no_sinsonda" = "#D55E00"),
                     labels = c("Con Sonda", "Sin Sonda")) +
  theme_minimal() +
  labs(title = "Series de Tiempo (NO)", x = "Fecha y Hora", y = "Concentración NO (ppb)", color = "Equipo")

cor_no <- ggplot(df_unido, aes(x = no_sinsonda, y = no_sonda)) +
  geom_point(alpha = 0.5, color = "darkgray") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  labs(title = "Correlación (NO)", x = "NO (Sin Sonda)", y = "NO (Con Sonda)")

media_dif_no <- mean(df_unido$dif_no)
sd_dif_no <- sd(df_unido$dif_no)
bland_no <- ggplot(df_unido, aes(x = prom_no, y = dif_no)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_hline(yintercept = media_dif_no, color = "blue", linewidth = 1) + 
  geom_hline(yintercept = media_dif_no + 1.96 * sd_dif_no, color = "red", linetype = "dashed") + 
  geom_hline(yintercept = media_dif_no - 1.96 * sd_dif_no, color = "red", linetype = "dashed") + 
  theme_minimal() +
  labs(title = "Bland-Altman (NO)", x = "Promedio de NO", y = "Diferencia (Sonda - Sin)")

graficos_no <- (series_no) / (cor_no | bland_no) + plot_annotation(title = "ANÁLISIS COMPARATIVO: NO", theme = theme(plot.title = element_text(face = "bold", size = 14)))


# ---------------------------------------------------------
# 9. CREACIÓN DE GRÁFICOS: NOx (Óxidos de Nitrógeno)
# ---------------------------------------------------------

series_nox <- df_unido %>%
  pivot_longer(cols = c(nox_sonda, nox_sinsonda), names_to = "equipo", values_to = "medicion") %>%
  ggplot(aes(x = tiempo, y = medicion, color = equipo)) +
  geom_line(alpha = 0.8, linewidth = 0.8) +
  scale_color_manual(values = c("nox_sonda" = "#009E73", "nox_sinsonda" = "#CC79A7"),
                     labels = c("Con Sonda", "Sin Sonda")) +
  theme_minimal() +
  labs(title = "Series de Tiempo (NOx)", x = "Fecha y Hora", y = "Concentración NOx (ppb)", color = "Equipo")

cor_nox <- ggplot(df_unido, aes(x = nox_sinsonda, y = nox_sonda)) +
  geom_point(alpha = 0.5, color = "darkgray") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  labs(title = "Correlación (NOx)", x = "NOx (Sin Sonda)", y = "NOx (Con Sonda)")

media_dif_nox <- mean(df_unido$dif_nox)
sd_dif_nox <- sd(df_unido$dif_nox)
bland_nox <- ggplot(df_unido, aes(x = prom_nox, y = dif_nox)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_hline(yintercept = media_dif_nox, color = "blue", linewidth = 1) + 
  geom_hline(yintercept = media_dif_nox + 1.96 * sd_dif_nox, color = "red", linetype = "dashed") + 
  geom_hline(yintercept = media_dif_nox - 1.96 * sd_dif_nox, color = "red", linetype = "dashed") + 
  theme_minimal() +
  labs(title = "Bland-Altman (NOx)", x = "Promedio de NOx", y = "Diferencia (Sonda - Sin)")

graficos_nox <- (series_nox) / (cor_nox | bland_nox) + plot_annotation(title = "ANÁLISIS COMPARATIVO: NOx", theme = theme(plot.title = element_text(face = "bold", size = 14)))

# ---------------------------------------------------------
# 10. MOSTRAR Y GUARDAR GRÁFICOS
# ---------------------------------------------------------

# Mostrar en consola/pantalla
print(graficos_no)
print(graficos_nox)

# Definir la ruta del directorio donde se guardarán los gráficos
directorio_salida <- "NOx-junio-julio"

# Guardar el panel de gráficos de NO
ggsave(
  filename = file.path(directorio_salida, "graficos_NO.png"),
  plot = graficos_no,
  width = 10,       # Ancho de la imagen en pulgadas
  height = 8,       # Alto de la imagen en pulgadas
  dpi = 300         # Resolución de alta calidad para informes/publicaciones
)

# Guardar el panel de gráficos de NOx
ggsave(
  filename = file.path(directorio_salida, "graficos_NOx.png"),
  plot = graficos_nox,
  width = 10,
  height = 8,
  dpi = 300
)

cat("\n¡Gráficos guardados exitosamente en la carpeta:", directorio_salida, "!\n")