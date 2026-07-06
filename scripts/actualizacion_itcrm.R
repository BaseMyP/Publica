# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN: Índices de Tipo de Cambio (ITCRM / ITCNM - BCRA)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando actualización diaria de ITCRM e ITCNM: ", Sys.time())

# 1. Configuración de dependencias (Mismo diccionario que carga inicial)
config_itcr <- list(
  TC_y_TASAS_ITCRM_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales", col = 2),
  TC_y_TASAS_ITCRM_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales prom. mens.", col = 2),
  TC_y_TASAS_ITCRB_USA_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales", col = 6),
  TC_y_TASAS_ITCRB_USA_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales prom. mens.", col = 6),
  TC_y_TASAS_ITCNM_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales", col = 2),
  TC_y_TASAS_ITCNM_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales prom. mens.", col = 2),
  TC_y_TASAS_ITCNB_USA_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales", col = 6),
  TC_y_TASAS_ITCNB_USA_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales prom. mens.", col = 6)
)

# 2. Descargar Excels una sola vez
urls_unicas <- unique(sapply(config_itcr, function(x) x$url))
archivos_locales <- list()

for (u in urls_unicas) {
  tmp <- tempfile(fileext = ".xlsx")
  tryCatch({
    GET(u, write_disk(tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
    archivos_locales[[u]] <- tmp
  }, error = function(e) { warning("No se pudo descargar: ", u) })
}

hoy <- as.character(Sys.Date())

# 3. Bucle ALFRED para cruzar datos
for (serie_id in names(config_itcr)) {
  config <- config_itcr[[serie_id]]
  tema <- "TC_y_TASAS"
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo) && !is.null(archivos_locales[[config$url]])) {
    archivo_leer <- archivos_locales[[config$url]]
    df_raw <- read_excel(archivo_leer, sheet = config$hoja, col_names = FALSE, skip=1)
    
    nuevo_df <- df_raw %>%
      select(fecha_cruda = 1, valor = all_of(config$col)) %>%
      mutate(valor = suppressWarnings(as.numeric(valor))) %>%
      filter(!is.na(valor))
    
    nuevo_df$fecha <- suppressWarnings(as.Date(as.numeric(nuevo_df$fecha_cruda), origin = "1899-12-30"))
    idx_na <- is.na(nuevo_df$fecha)
    if(any(idx_na)) nuevo_df$fecha[idx_na] <- as.Date(nuevo_df$fecha_cruda[idx_na])
    
    nuevo_df <- nuevo_df %>%
      filter(!is.na(fecha)) %>%
      select(fecha, valor) %>%
      mutate(fecha = as.character(fecha), valor = round(valor, 4))
    
    # Separar historia y vigencia
    base_actual <- fromJSON(path_archivo)
    obs_viejas <- base_actual$observaciones
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    # Cruzar datos
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
    
    # Actualizar JSON
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
    message("✓ Actualizada: ", serie_id)
  }
}