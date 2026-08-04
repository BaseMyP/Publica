# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN: Balance Mensual BCRA (diar_bas.xls - BCRA)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando actualización semanal de diar_bas.xls: ", Sys.time())

# 1. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_diar_bas <- catalogo_completo %>% filter(metodo_etl == "EXCEL_diar_bas")

if (nrow(catalogo_diar_bas) == 0) {
  message("No hay series configuradas para EXCEL_diar_bas. Finalizando.")
  quit(save = "no")
}

# 2. Descargar el Excel
url_diar_bas <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/diar_bas.xls"
archivo_tmp <- tempfile(fileext = ".xls")

tryCatch({
  GET(url_diar_bas, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# IMPORTANTE: Mantener el mismo 'skip' que usaste en la carga inicial
temp_df <- read_excel(
  archivo_tmp, 
  sheet = "Serie_diaria", 
  skip = 27, 
  col_names = FALSE, 
  n_max = 1
)

# 2. Calculamos cuántas numéricas necesitamos (Total de columnas - 1)
n_cols <- ncol(temp_df)
mis_tipos <- c("date", rep("numeric", n_cols - 1))

# 3. Leemos el archivo real forzando nuestros tipos
df_raw <- read_excel(
  archivo_tmp, 
  sheet = "Serie_diaria", 
  skip = 27, 
  col_names = FALSE,
  col_types = mis_tipos
)
hoy <- as.character(Sys.Date())

# 3. Bucle ALFRED para cruzar datos
for (i in 1:nrow(catalogo_diar_bas)) {
  serie_id <- catalogo_diar_bas$serie_id[i]
  tema <- basename(dirname(catalogo_diar_bas$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # Extraemos el índice de la columna desde los metadatos (Ej: "6" para la Col F)
    col_index <- as.numeric(base_actual$metadatos$id_original)
    
    # Extraemos la fecha (col 1) y la columna objetivo
    nuevo_df <- df_raw %>%
      select(fecha = 1, valor = all_of(col_index)) %>%
      mutate(
        fecha = as.Date(fecha)
      ) %>%
      # Eliminamos la columna auxiliar
      filter(!is.na(fecha) & !is.na(valor)) %>%
      filter(valor>0) %>% 
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