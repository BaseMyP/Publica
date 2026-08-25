# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN DIARIA (CAUCIÓN - RAVA BURSÁTIL)
# ==============================================================================

library(httr)
library(rvest)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando actualización diaria de las tasas de caución (Rava): ", Sys.time())

# 1. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_rp <- catalogo_completo %>% filter(serie_id %in% c("CAUCIONARS_1D_NSA_D","CAUCIONARS_7D_NSA_D"))

if (nrow(catalogo_rp) == 0) {
  message("No se encontraron las series de caución en el catálogo. Finalizando.")
  quit(save = "no")
}

hoy <- as.character(Sys.Date())

# 2. Bucle principal: descargar y procesar cada serie individualmente
for (i in 1:nrow(catalogo_rp)) {
  serie_id <- catalogo_rp$serie_id[i]
  tema <- basename(dirname(catalogo_rp$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  # Asignar la URL correspondiente según el ID de la serie
  if (serie_id == "CAUCIONARS_1D_NSA_D") {
    url_rava <- "https://www.rava.com/perfil/CAUCION%201D"
  } else if (serie_id == "CAUCIONARS_7D_NSA_D") {
    url_rava <- "https://www.rava.com/perfil/CAUCION%207D"
  } else {
    next 
  }
  
  message("\nProcesando ", serie_id, " desde: ", url_rava)
  
  # 3. Descargar tabla actual de Rava
  respuesta <- GET(
    url_rava, 
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  )
  
  if (status_code(respuesta) != 200) {
    warning("No se pudo conectar a Rava Bursátil para ", serie_id)
    next
  }
  
  html_doc <- read_html(content(respuesta, as = "text", encoding = "UTF-8"))
  tablas <- html_nodes(html_doc, "table")
  
  if (length(tablas) == 0) {
    message("Sin datos nuevos en Rava para ", serie_id)
    next
  }
  
  # 4. Limpieza de datos entrantes
  df_rava <- html_table(tablas[[1]], fill = TRUE)
  
  nuevo_df <- df_rava %>%
    mutate(
      Fecha = as.Date(Fecha, format = "%d/%m/%Y"),
      valor = as.numeric(gsub(",", ".", gsub("\\.", "", Cierre)))
    ) %>%
    filter(!is.na(Fecha) & !is.na(valor)) %>%
    select(fecha = Fecha, valor) %>%
    mutate(fecha = as.character(fecha)) %>%
    arrange(fecha)
  
  # 5. Lógica ALFRED
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    obs_viejas <- base_actual$observaciones
    
    # Consolidar datos vigentes por si hay duplicados
    obs_viejas_consolidadas <- obs_viejas %>% 
      filter(realtime_end == "9999-12-31") %>% 
      distinct() %>% 
      group_by(fecha) %>% 
      summarise(
        valor = mean(valor), 
        realtime_start = max(realtime_start),
        realtime_end = "9999-12-31"
      ) %>% 
      ungroup()
    
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    # Cruce de información
    actualizadas <- nuevo_df %>%
      left_join(obs_viejas_consolidadas, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    obs_vigentes_que_cambiaron <- obs_viejas_consolidadas %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    obs_vigentes_sin_cambio <- obs_viejas_consolidadas %>%
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
  } else {
    warning("No se encontró el archivo histórico para ", serie_id, ". Recuerda ejecutar la carga inicial primero.")
  }
}