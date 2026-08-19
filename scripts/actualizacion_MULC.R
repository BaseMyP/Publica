# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN: MULC
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando actualización semanal de MULC.xls: ", Sys.time())

# 1. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_MULC <- catalogo_completo %>% filter(metodo_etl == "EXCEL_MULC")

if (nrow(catalogo_MULC) == 0) {
  message("No hay series configuradas para EXCEL_MULC. Finalizando.")
  quit(save = "no")
}

# 2. Descargar el Excel
url_MULC <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/informes/anexo-estadistico-mercado-cambios-balance-cambiario.xlsx"
archivo_tmp <- tempfile(fileext = ".xlsx")

tryCatch({
  GET(url_MULC, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# IMPORTANTE: Mantener el mismo 'skip' que usaste en la carga inicial
df_raw <- read_excel(archivo_tmp, sheet="Balance Cambiario", skip=10)

hoy <- as.character(Sys.Date())

# 3. Bucle ALFRED para cruzar datos
for (i in 1:nrow(catalogo_MULC)) {
  serie_id <- catalogo_MULC$serie_id[i]
  tema <- basename(dirname(catalogo_MULC$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # Extraemos el índice de la columna desde los metadatos (Ej: "6" para la Col F)
    col_index <- base_actual$metadatos$id_original
    
    # Extraemos la fecha (col 1) y la columna objetivo
    nuevo_df <- df_raw %>%
      select(fecha = "bal000", valor = all_of(col_index)) %>%
      mutate(fecha = as.Date(as.numeric(fecha), origin="1899-12-30")) %>% 
      filter(!is.na(fecha)) %>% 
      filter(year(fecha)>2000) %>%
      arrange(fecha)
    
    # Separar historia y vigencia
    obs_viejas <- base_actual$observaciones
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31") %>% 
      mutate(fecha=as.Date(fecha))
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31") %>% 
      mutate(fecha=as.Date(fecha))
    
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