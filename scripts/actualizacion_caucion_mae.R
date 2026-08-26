# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN DIARIA: REPOs y CAUCIÓN (MAE)
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando actualización diaria MAE (Cauciones y REPOs): ", Sys.time())

# 1. Validar Catálogo
catalogo_completo <- fromJSON("catalogo.json")
catalogo_mae <- catalogo_completo %>% filter(metodo_etl == "API_MAE")

if (nrow(catalogo_mae) == 0) quit(save = "no")

# 2. Fechas de consulta
fecha_desde <- as.character(Sys.Date() - 30)
fecha_hasta <- as.character(Sys.Date())
query_json <- paste0('{"fechaDesde":"', fecha_desde, '","fechaHasta":"', fecha_hasta, '"}')
query_encoded <- URLencode(query_json, reserved = TRUE)

# Definimos los endpoints
url_caucion <- paste0("https://api.marketdata.mae.com.ar/api/mercado/titulo/historicocauciones?oTitulo=", query_encoded)
url_repo <- paste0("https://api.marketdata.mae.com.ar/api/mercado/titulo/historicorepo?oTitulo=", query_encoded)

# 3. Función auxiliar de descarga y limpieza
descargar_y_limpiar_mae <- function(url) {
  resp <- GET(url, user_agent("Mozilla/5.0"))
  if (status_code(resp) != 200) return(NULL)
  
  df_raw <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  if (length(df_raw) == 0 || nrow(df_raw) == 0) return(NULL)
  
  df_detalles <- bind_rows(df_raw$details) %>% 
    filter(moneda == "$") %>%
    mutate(
      fecha = as.character(as.Date(fecha)),
      plazo = as.numeric(plazo),
      valor = as.numeric(tasaPP)
    )
  return(df_detalles)
}

df_caucion <- descargar_y_limpiar_mae(url_caucion)
df_repo <- descargar_y_limpiar_mae(url_repo)
hoy <- as.character(Sys.Date())

# 4. Lógica ALFRED para todas las series MAE
for (i in 1:nrow(catalogo_mae)) {
  serie_id <- catalogo_mae$serie_id[i]
  tema <- basename(dirname(catalogo_mae$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (!file.exists(path_archivo)) next
  
  # Seleccionamos el dataframe crudo según el ticker
  if (serie_id == "CAUCION_MAE_TASAPP_NSA_D") {
    df_base <- df_caucion
  } else {
    df_base <- df_repo
  }
  
  if (is.null(df_base)) next
  
  # Filtramos y limpiamos el dato específico
  nuevo_df <- df_base %>%
    filter(if (serie_id == "CAUCION_MAE_TASAPP_NSA_D") TRUE else rueda == !!substr(serie_id,1,4)) %>%
    filter(!is.na(fecha) & !is.na(valor)) %>%
    group_by(fecha) %>%
    slice_min(order_by = plazo, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(fecha, valor) %>%
    arrange(fecha)
  
  if(nrow(nuevo_df) == 0) next
  
  # Cruce histórico
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