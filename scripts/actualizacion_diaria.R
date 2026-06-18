# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA
# ==============================================================================

library(jsonlite)
library(dplyr)
library(stringr)
library(httr)
library(lubridate)
source("scripts/funciones_base.R") 

BASE_URL_MONETARIAS <- "https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias/"

message("Iniciando actualización diaria: ", Sys.time())

# 1. Leer el catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# 2. Filtrado estructural: Solo procesar las series asignadas a este método
catalogo_api_bcra <- catalogo_completo %>% 
  filter(metodo_etl == "API_BCRA")

if (nrow(catalogo_api_bcra) == 0) {
  message("No hay series configuradas para API_BCRA. Finalizando script.")
  quit(save = "no")
}

for (i in 1:nrow(catalogo_api_bcra)) {
  serie_id <- catalogo_api_bcra$serie_id[i]
  raw_url <- catalogo_api_bcra$raw_url[i]
  
  # CORRECCIÓN: Extrae dinámicamente el nombre de la carpeta (tema) desde la URL del catálogo
  # Ejemplo: "https://.../EXPECTATIVAS/IPCGBA_MOM_M.json" -> "EXPECTATIVAS"
  tema <- basename(dirname(raw_url))
  
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    id_bcra <- base_actual$metadatos$id_original
    metadatos_fijos <- base_actual$metadatos
    
    message(sprintf("\n[%s/%s] Procesando: %s (Carpeta: %s)", i, nrow(catalogo_api_bcra), serie_id, tema))
    
    update_bcra_json_serie(
      id_variable = id_bcra,
      serie_id = serie_id,
      tema = tema,
      metadatos_fijos = metadatos_fijos
    )
  } else {
    warning(sprintf("El archivo local no existe en la ruta: %s. Revisa si la carpeta o el nombre cambiaron.", path_archivo))
  }
}

# # 3. Subir los cambios a GitHub automáticamente
# message("\nIniciando sincronización con el repositorio remoto...")
# 
# tryCatch({
#   system("git add .")
#   mensaje_commit <- paste0("Update diario automatizado: ", Sys.Date())
#   system(sprintf('git commit -m "%s"', mensaje_commit))
#   system("git push")
#   message("✓ Rutina completada con éxito. Base actualizada en la nube.")
# }, error = function(e) {
#   warning("Hubo un problema al intentar subir los cambios a GitHub: ", e$message)
# })