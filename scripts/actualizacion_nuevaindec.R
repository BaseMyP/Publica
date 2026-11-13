# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA (NUEVA API INDEC - v1)
# ==============================================================================

library(jsonlite)
library(dplyr)
library(httr2)
library(purrr)
library(stringr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando revisión diaria de Nueva API INDEC: ", Sys.time())

# 1. Leer el catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# 2. Filtrar las series que corresponden a Nueva_API_INDEC
catalogo_nuevaindec <- catalogo_completo %>% 
  filter(metodo_etl == "Nueva_API_INDEC")

if (nrow(catalogo_nuevaindec) == 0) {
  message("No hay series configuradas para Nueva_API_INDEC. Finalizando script.")
  quit(save = "no")
}

# Mapa de parámetros para las 5 series de IPC (se guarda en metadatos$parametros_api si no existe)
mapa_parametros_ipc <- list(
  "IPCGENERAL_INDICE_NSA_M" = list(
    region = "Nacional", nivel = "Nivel General", tipo_indicador = "Indice", filtros_extra = list()
  ),
  "IPCBIENES_INDICE_NSA_M" = list(
    region = "Nacional", nivel = "Categoría", tipo_indicador = "Indice", filtros_extra = list(codigo = "eq.B")
  ),
  "IPCNUCLEO_INDICE_NSA_M" = list(
    region = "Nacional", nivel = "Categoría", tipo_indicador = "Indice", filtros_extra = list(codigo = "eq.N")
  ),
  "IPCREGULADOS_INDICE_NSA_M" = list(
    region = "Nacional", nivel = "Categoría", tipo_indicador = "Indice", filtros_extra = list(codigo = "eq.R")
  ),
  "IPCSERVICIOS_INDICE_NSA_M" = list(
    region = "Nacional", nivel = "Categoría", tipo_indicador = "Indice", filtros_extra = list(codigo = "eq.S")
  )
)

hubo_actualizaciones <- FALSE

# 3. Bucle de actualización
for (i in 1:nrow(catalogo_nuevaindec)) {
  serie_id <- catalogo_nuevaindec$serie_id[i]
  raw_url  <- catalogo_nuevaindec$raw_url[i]
  tema     <- basename(dirname(raw_url))
  
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual     <- fromJSON(path_archivo)
    metadatos_fijos <- base_actual$metadatos
    
    # Si el JSON todavía no tiene parametros_api en sus metadatos, lo toma del mapa
    if (is.null(metadatos_fijos$parametros_api) && serie_id %in% names(mapa_parametros_ipc)) {
      metadatos_fijos$parametros_api <- mapa_parametros_ipc[[serie_id]]
    }
    
    message(sprintf("\n[%s/%s] Consultando Nueva API INDEC para: %s (Carpeta: %s)", 
                    i, nrow(catalogo_nuevaindec), serie_id, tema))
    
    exito <- update_nuevaindec_json_serie(
      serie_id        = serie_id,
      tema            = tema,
      metadatos_fijos = metadatos_fijos
    )
    
    if (isTRUE(exito)) hubo_actualizaciones <- TRUE
  } else {
    warning(sprintf("El archivo local no existe en la ruta: %s. Revisa si la carpeta o el nombre cambiaron.", path_archivo))
  }
}