# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN SEMANAL (MATBA ROFEX)
# ==============================================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando actualización semanal de Dólar Futuro: ", Sys.time())

# 1. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_rofex <- catalogo_completo %>% filter(metodo_etl == "API_ROFEX")
if (nrow(catalogo_rofex) == 0) {
  message("No hay series de Rofex configuradas. Finalizando.")
  quit(save = "no")
}

# 2. Descargar datos de la última semana (para agarrar el cierre del viernes)
endpoint_url <- "https://apicem.matbarofex.com.ar/api/v1/closing-prices"

inicio_mes_anterior <- floor_date(Sys.Date(), "month") - months(1)
fin_mes_anterior <- ceiling_date(inicio_mes_anterior, "month") - days(1)

fecha_desde <- paste0(format(inicio_mes_anterior, "%Y-%m-%d"), "T00:00:00Z")
fecha_hasta <- paste0(format(fin_mes_anterior, "%Y-%m-%d"), "T23:59:59Z")

# Generar contratos vigentes (Mes actual + 18 meses)
meses_vigentes <- seq(floor_date(Sys.Date(), "month"), Sys.Date() + months(18), by = "1 month")
simbolos <- paste0("DLR", format(meses_vigentes, "%m%Y"))

datos_semana <- list()
for (sym in simbolos) {
  req <- request(endpoint_url) %>% req_url_query(symbol = sym, from = fecha_desde, to = fecha_hasta)
  
  # CORRECCIÓN 1: Usar req_error() para que httr2 no detenga el script si hay error 404/400
  resp <- req %>% 
    req_error(is_error = function(resp) FALSE) %>% 
    req_perform()
  
  if (resp_status(resp) == 200) {
    df <- (resp %>% resp_body_string() %>% fromJSON(flatten = TRUE))$data
    
    # CORRECCIÓN 2: Usar is.data.frame() para evitar el error lógico con listas vacías
    if (is.data.frame(df) && nrow(df) > 0) {
      datos_semana[[length(datos_semana) + 1]] <- df
    }
  }
}

if (length(datos_semana) == 0) {
  message("No se detectaron nuevas cotizaciones esta semana.")
  quit(save = "no")
}

# 3. Limpiar y quedarse con la última cotización de la semana para cada contrato
nuevo_df <- bind_rows(datos_semana) %>%
  mutate(
    cotizacion_date = as.Date(dateTime),
    mes_vencimiento = as.numeric(substr(symbol, 4, 5)),
    anio_vencimiento = as.numeric(substr(symbol, 6, 9)),
    fecha_contrato = ceiling_date(make_date(anio_vencimiento, mes_vencimiento, 1), "month") - days(1)
  ) %>%
  group_by(symbol) %>%
  arrange(cotizacion_date) %>%
  slice_tail(n = 1) %>% # Último precio disponible de la semana
  ungroup() %>%
  select(fecha = fecha_contrato, valor = settlement, realtime_start = cotizacion_date) %>%
  mutate(fecha = as.character(fecha), realtime_start = as.character(realtime_start))

hoy <- as.character(Sys.Date())

# 4. Bucle ALFRED (Aplicando sobre el JSON guardado)
for (i in 1:nrow(catalogo_rofex)) {
  serie_id <- catalogo_rofex$serie_id[i]
  tema <- basename(dirname(catalogo_rofex$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    obs_viejas <- base_actual$observaciones
    
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    actualizadas <- nuevo_df %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor = valor_nuevo, realtime_start = realtime_start_nuevo) %>%
      mutate(realtime_end = "9999-12-31")
    
    obs_consolidadas <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, obs_vigentes_sin_cambio, nuevas_inserciones) %>% 
      arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
    message("✓ Actualizada curva de futuros: ", serie_id)
  }
}