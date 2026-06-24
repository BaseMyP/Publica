# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN SEMANAL (IPMP BCRA)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
source("scripts/funciones_base.R")

message("Iniciando actualización semanal del IPMP: ", Sys.time())

# 1. Leer Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_ipmp <- catalogo_completo %>% filter(metodo_etl == "EXCEL_IPMP")

if (nrow(catalogo_ipmp) == 0) {
  message("No hay series configuradas para EXCEL_IPMP. Finalizando.")
  quit(save = "no")
}

# 2. Descargar el Excel actualizado
url_ipmp <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/IPMPSerie.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
tryCatch({
  GET(url_ipmp, write_disk(temp_file, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Fallo al descargar el archivo del IPMP: ", e$message)
})

# 3. Leer y limpiar
df_limpio <- read_excel(temp_file, sheet = "IPMP mensual desde ene-1997", skip = 2) %>%
  select(1:5) %>% # Tomamos las primeras 5 columnas
  setNames(c("fecha", "IPMP", "IPMPAGRO", "IPMPMETALES", "IPMPPETROLEO")) %>%
  mutate(fecha = as.Date(as.numeric(fecha), origin="1899-12-30")) %>%
  filter(!is.na(fecha))

hoy <- as.character(Sys.Date())

# 4. Bucle de actualización (Lógica ALFRED)
for (i in 1:nrow(catalogo_ipmp)) {
  serie_id <- catalogo_ipmp$serie_id[i]
  tema <- basename(dirname(catalogo_ipmp$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    columna_target <- base_actual$metadatos$id_original
    
    # Nuevo dataframe entrante
    nuevo_df <- df_limpio %>%
      select(fecha, valor = !!sym(columna_target)) %>%
      mutate(fecha = as.character(fecha), valor = as.numeric(valor)) %>%
      filter(!is.na(valor))
    
    # Separar historia y vigencia actual
    obs_viejas <- base_actual$observaciones
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    # Detectar cambios
    actualizadas <- nuevo_df %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    # 1. Vigentes que sufrieron revisión (se cierran hoy)
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    # 2. Vigentes que siguen igual
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    # 3. Nuevas filas a insertar (datos nuevos + datos revisados)
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor = valor_nuevo) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    # Consolidar
    obs_consolidadas <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, obs_vigentes_sin_cambio, nuevas_inserciones) %>% 
      arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
    message("✓ Actualizada: ", serie_id)
  }
}