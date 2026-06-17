# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA (FRED)
# ==============================================================================

library(jsonlite)
library(dplyr)
library(httr)
source("scripts/funciones_base.R")

message("Iniciando revisión diaria de API FRED: ", Sys.time())

# El token se inyectará desde GitHub Actions como variable de entorno
FRED_API_KEY <- Sys.getenv("FRED_API_KEY")

if (FRED_API_KEY == "") {
  stop("ERROR: No se encontró la FRED_API_KEY en las variables de entorno.")
}

# 1. Leer el catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# 2. Filtrar solo las series que corresponden a FRED
catalogo_fred <- catalogo_completo %>% 
  filter(metodo_etl == "API_FRED")

if (nrow(catalogo_fred) == 0) {
  message("No hay series configuradas para API_FRED. Finalizando script.")
  quit(save = "no")
}

# 3. Bucle de actualización
for (i in 1:nrow(catalogo_fred)) {
  serie_id <- catalogo_fred$serie_id[i]
  raw_url <- catalogo_fred$raw_url[i]
  
  # Extraer la carpeta contenedora desde la URL
  tema <- basename(dirname(raw_url))
  
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # Extraer el ID de FRED desde el campo 'id_original'
    id_fred <- base_actual$metadatos$id_original
    metadatos_fijos <- base_actual$metadatos
    
    message(sprintf("\n[%s/%s] Consultando API FRED para: %s (Carpeta: %s)", i, nrow(catalogo_fred), serie_id, tema))
    
    update_fred_json_serie(
      id_fred = id_fred,
      serie_id = serie_id,
      tema = tema,
      metadatos_fijos = metadatos_fijos,
      api_key = FRED_API_KEY
    )
  } else {
    warning(sprintf("El archivo local no existe en la ruta: %s", path_archivo))
  }
}