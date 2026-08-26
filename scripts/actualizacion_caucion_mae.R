# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN DIARIA: CAUCIÓN PROMEDIO (MAE)
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando actualización de caución MAE: ", Sys.time())

# 1. Validar Catálogo
catalogo_completo <- fromJSON("catalogo.json")
catalogo_caucion <- catalogo_completo %>% filter(serie_id == "CAUCION_MAE_TASAPP_NSA_D")

if (nrow(catalogo_caucion) == 0) quit(save = "no")

# 2. Descargar últimos 30 días
fecha_desde <- as.character(Sys.Date() - 30)
fecha_hasta <- as.character(Sys.Date())

query_json <- paste0('{"fechaDesde":"', fecha_desde, '","fechaHasta":"', fecha_hasta, '"}')
url_mae <- paste0("https://api.marketdata.mae.com.ar/api/mercado/titulo/historicocauciones?oTitulo=", URLencode(query_json, reserved = TRUE))

respuesta <- GET(url_mae, user_agent("Mozilla/5.0"))
if (status_code(respuesta) != 200) quit(save = "no")

df_raw <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))
if (length(df_raw) == 0 || nrow(df_raw) == 0) quit(save = "no")

# 3. Parsear JSON y limpiar
df_raw <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))

# Desanidamos la lista de dataframes de la columna 'details' y los unimos
df_detalles <- bind_rows(df_raw$details)

nuevo_df <- df_detalles %>%
  filter(moneda == "$") %>% # Filtramos para quedarnos solo con la tasa en Pesos
  mutate(
    fecha = as.character(as.Date(fecha)), # Limpiamos la hora (de "2026-04-09T00:00:00" a "2026-04-09")
    plazo = as.numeric(plazo),
    valor = as.numeric(tasaPP) # Seleccionamos la Tasa Promedio Ponderada
  ) %>%
  filter(!is.na(fecha) & !is.na(valor)) %>%
  group_by(fecha) %>%
  slice_min(order_by = plazo, n = 1, with_ties = FALSE) %>% # Elegimos el plazo más corto operado ese día
  ungroup() %>%
  select(fecha, valor) %>%
  arrange(fecha)

hoy <- as.character(Sys.Date())

# 4. Lógica ALFRED
path_archivo <- file.path("TC_y_TASAS", "CAUCION_MAE_TASAPP_NSA_D.json")

if (file.exists(path_archivo)) {
  base_actual <- fromJSON(path_archivo)
  obs_viejas <- base_actual$observaciones
  
  obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31") %>% distinct() %>% 
    group_by(fecha) %>% 
    summarise(valor = mean(valor), realtime_start = max(realtime_start), realtime_end = "9999-12-31") %>% 
    ungroup()
  
  obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
  
  actualizadas <- nuevo_df %>%
    left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
    mutate(status = case_when(
      is.na(valor_viejo) ~ "NUEVO",
      round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
      TRUE ~ "SIN_CAMBIOS"
    ))
  
  obs_vigentes_que_cambiaron <- obs_vigentes %>% filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>% mutate(realtime_end = hoy)
  obs_vigentes_sin_cambio <- obs_vigentes %>% filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
  nuevas_inserciones <- actualizadas %>% filter(status %in% c("NUEVO", "REVISADO")) %>% select(fecha, valor = valor_nuevo) %>% mutate(realtime_start = hoy, realtime_end = "9999-12-31")
  
  base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
  base_actual$observaciones <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, obs_vigentes_sin_cambio, nuevas_inserciones) %>% arrange(fecha, realtime_start)
  
  write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
}