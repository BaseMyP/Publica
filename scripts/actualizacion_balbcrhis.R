# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN: Tasas de Interés Pasivas (pashis.xls - BCRA)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando actualización semanal de balbcrhis.xls: ", Sys.time())

# 1. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_balbcrhis <- catalogo_completo %>% filter(metodo_etl == "EXCEL_BALBCRHIS")

if (nrow(catalogo_pashis) == 0) {
  message("No hay series configuradas para EXCEL_BALBCRHIS. Finalizando.")
  quit(save = "no")
}

# 2. Descargar el Excel
url_balbcrhis <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/balbcrhis.xls"
archivo_tmp <- tempfile(fileext = ".xls")

tryCatch({
  GET(url_balbcrhis, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# IMPORTANTE: Mantener el mismo 'skip' que usaste en la carga inicial
df_raw <- read_excel(archivo_tmp, sheet = "B.C.R.A.", skip = 26, col_names = FALSE)

hoy <- as.character(Sys.Date())

# 3. Bucle ALFRED para cruzar datos
for (i in 1:nrow(catalogo_pashis)) {
  serie_id <- catalogo_pashis$serie_id[i]
  tema <- basename(dirname(catalogo_pashis$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # Extraemos el índice de la columna desde los metadatos (Ej: "6" para la Col F)
    col_index <- as.numeric(base_actual$metadatos$id_original)
    
    # Extraemos la fecha (col 1) y la columna objetivo
    nuevo_df <- df_raw %>%
      select(fecha = 1, valor = all_of(config$col_index)) %>%
      mutate(anio = str_split_fixed(fecha,"\\.",n=2)[,1],
             mes = str_split_fixed(fecha,"\\.",n=2)[,2]) %>% 
      mutate(mes = ifelse(nchar(mes)>=3,
                          round(as.numeric(str_sub(mes,1,3))/10),
                          as.numeric(mes))
      ) %>% 
      filter(!(mes==13 | is.na(mes))) %>% 
      mutate(
        fecha = as.Date(paste0(anio,"-",mes, "-01")),
        valor = as.numeric(valor)
      ) %>%
      # Eliminamos la columna auxiliar
      select(-c(anio,mes)) %>% 
      filter(!is.na(fecha) & !is.na(valor)) %>%
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