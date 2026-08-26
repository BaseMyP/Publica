# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN DIARIA: INTERÉS ABIERTO FUTUROS (MAE)
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando actualización de Interés Abierto de Futuros (MAE): ", Sys.time())

# 1. Validar Catálogo
catalogo_completo <- fromJSON("catalogo.json")
catalogo_futuros <- catalogo_completo %>% filter(metodo_etl == "API_MAE_FUTUROS")

if (nrow(catalogo_futuros) == 0) quit(save = "no")

# 2. Descargar últimos 30 días
fecha_desde <- as.character(Sys.Date() - 30)
fecha_hasta <- as.character(Sys.Date())

query_json <- paste0('{"fechaDesde":"', fecha_desde, '","fechaHasta":"', fecha_hasta, '","contratosSinVolumen":false}')
url_mae <- paste0("https://api.marketdata.mae.com.ar/api/cem/monedas/fut?oData=", URLencode(query_json, reserved = TRUE))

respuesta <- GET(url_mae, user_agent("Mozilla/5.0"))
if (status_code(respuesta) != 200) quit(save = "no")

df_raw <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))
if (length(df_raw) == 0 || nrow(df_raw) == 0) quit(save = "no")

# 3. Limpieza y Agregación
nuevo_df <- df_raw %>%
  filter(str_detect(posicion, "^DLR")) %>%
  mutate(
    fecha = as.character(as.Date(fecha)),
    openInterest = as.numeric(interesAbierto)
  ) %>%
  filter(!is.na(fecha)) %>%
  group_by(fecha) %>%
  summarise(valor = sum(openInterest, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(fecha)

hoy <- as.character(Sys.Date())

# 4. Lógica ALFRED para todas las series de futuros (actualmente solo USD)
for (i in 1:nrow(catalogo_futuros)) {
  serie_id <- catalogo_futuros$serie_id[i]
  tema <- basename(dirname(catalogo_futuros$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (!file.exists(path_archivo)) next
  
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
  message("✓ Actualizada: ", serie_id)
}