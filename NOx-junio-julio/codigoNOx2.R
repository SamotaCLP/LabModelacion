# 1. Cargar paquetes
library(tidyverse)
library(lubridate)
library(patchwork)

# 2. Definir los nombres exactos de los archivos
archivos_csv <- c(
  "12218618650_UserData42iQ_2025-06-19_14-49-00_Sinsonda.csv",
  "12218618650_UserData42iQ_2025-07-25_13-48-35_Sinsonda.csv"
)

archivos_dat <- c(
  "NOx_42i_Portillo_junio_2025_Sonda.dat",
  "NOx_42i_julio_Portillo_Sonda.dat"
)

# 3. Función para leer los CSV (Equipo Sin Sonda)
leer_csv_sinsonda <- function(archivo) {
  df <- read_csv(archivo, show_col_types = FALSE)
  
  df_limpio <- df %>%
    # Según la imagen: Col 1 = Tiempo, Col 8 = NO, Col 10 = NOx
    select(tiempo_raw = 1, no_sinsonda = 8, nox_sinsonda = 10) %>%
    mutate(
      tiempo_str = as.character(tiempo_raw),
      tiempo = parse_date_time(tiempo_str, orders = c("dmy_HMS", "mdy_HMS", "ymd_HMS", "mdy_HM")),
      no_sinsonda = as.numeric(no_sinsonda),
      nox_sinsonda = as.numeric(nox_sinsonda)
    ) %>%
    drop_na(tiempo) %>%
    mutate(tiempo_redondeado = floor_date(tiempo, "10 mins")) %>%
    group_by(tiempo_redondeado) %>%
    summarise(
      no_sinsonda = mean(no_sinsonda, na.rm = TRUE),
      nox_sinsonda = mean(nox_sinsonda, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(df_limpio)
}

# 4. Función para leer los DAT (Equipo Sonda)
leer_dat_sonda <- function(archivo) {
  df <- read_table(archivo, skip = 5, show_col_types = FALSE)
  
  df_limpio <- df %>%
    mutate(
      fecha_hora_texto = paste(Date, Time),
      tiempo = parse_date_time(fecha_hora_texto, orders = c("mdy_HM", "dmy_HM")),
      tiempo_redondeado = floor_date(tiempo, "10 mins"),
      no_sonda = as.numeric(no),
      nox_sonda = as.numeric(nox)
    ) %>%
    drop_na(tiempo_redondeado) %>%
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

# 6. UNIR AMBAS BASES
df_unido <- inner_join(todas_sonda, todas_sinsonda, by = "tiempo_redondeado") %>%
  rename(tiempo = tiempo_redondeado) %>%
  drop_na()

print(paste("--> Puntos emparejados (cada 10 min):", nrow(df_unido)))

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
# 10. MOSTRAR GRÁFICOS
# ---------------------------------------------------------
print(graficos_no)
print(graficos_nox)