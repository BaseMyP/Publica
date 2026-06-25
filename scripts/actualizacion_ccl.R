# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN DIARIA (CCL - ÁMBITO FINANCIERO)
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando actualización diaria del CCL (Ámbito): ", Sys.time())

# 1. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_ccl <- catalogo_completo %>% filter(metodo_etl == "API_AMBITO")

if (nrow(catalogo_ccl) == 0) {
  message("No hay series configuradas para API_AMBITO. Finalizando.")
  quit(save = "no")
}

# 2. Descargar últimos 30 días
fecha_desde <- format(Sys.Date() - 30, "%d-%m-%Y")
fecha_hasta <- format(Sys.Date(), "%d-%m-%Y")
url_ambito <- paste0("https://mercados.ambito.com/dolar/contado-con-liqui/historico-general/", fecha_desde, "/", fecha_hasta)

respuesta_ambito <- GET(
  url_ambito, 
  user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
)

if (status_code(respuesta_ambito) != 200) {
  warning("No se pudo conectar a la API de Ámbito.")
  quit(save = "no")
}

datos_json <- content(respuesta_ambito, as = "text", encoding = "UTF-8")
matriz_ccl <- fromJSON(datos_json)

# Si Ámbito devuelve la matriz vacía o solo los títulos (menos de 2 filas), abortamos
if (is.null(matriz_ccl) || length(matriz_ccl) < 2) {
  message("Sin datos nuevos en Ámbito.")
  quit(save = "no")
}

# 3. Limpieza de datos entrantes
df_ccl_ambito <- as.data.frame(matriz_ccl[-1, ], stringsAsFactors = FALSE)
colnames(df_ccl_ambito) <- matriz_ccl[1, ]

nuevo_df <- df_ccl_ambito %>%
  mutate(
    Fecha = dmy(Fecha),
    Compra = as.numeric(gsub(",", ".", Compra)),
    Venta = as.numeric(gsub(",", ".", Venta)),
    valor = (Compra + Venta) / 2
  ) %>%
  filter(!is.na(Fecha) & !is.na(valor)) %>%
  select(fecha = Fecha, valor) %>%
  mutate(fecha = as.character(fecha)) %>%
  arrange(fecha)

hoy <- as.character(Sys.Date())

# 4. Bucle ALFRED para la serie
for (i in 1:nrow(catalogo_ccl)) {
  serie_id <- catalogo_ccl$serie_id[i]
  tema <- basename(dirname(catalogo_ccl$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    obs_viejas <- base_actual$observaciones
    
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    # Cruzamos datos existentes con los recién descargados
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
      select(fecha, valor = valor_nuevo) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    obs_consolidadas <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, obs_vigentes_sin_cambio, nuevas_inserciones) %>% 
      arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
    message("✓ Actualizada: ", serie_id)
  }
}