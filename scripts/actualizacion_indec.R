# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA (INDEC)
# ==============================================================================

library(jsonlite)
library(dplyr)
library(httr)
source("scripts/funciones_base.R")

message("Iniciando revisión diaria de API INDEC: ", Sys.time())

# 1. Leer el catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# 2. Filtrar solo las series que corresponden al INDEC
catalogo_indec <- catalogo_completo %>% 
  filter(metodo_etl == "API_INDEC")

if (nrow(catalogo_indec) == 0) {
  message("No hay series configuradas para API_INDEC. Finalizando script.")
  quit(save = "no")
}

# Variable de control para saber si hubo cambios reales
hubo_actualizaciones <- FALSE

# 3. Bucle de actualización
for (i in 1:nrow(catalogo_indec)) {
  serie_id <- catalogo_indec$serie_id[i]
  raw_url <- catalogo_indec$raw_url[i]
  
  # CORRECCIÓN 1: Extraer el tema dinámicamente desde la URL del catálogo
  tema <- basename(dirname(raw_url))
  
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # CORRECCIÓN 2: Extraer el ID de la API del INDEC desde el campo 'id_original'
    id_indec <- base_actual$metadatos$id_original
    metadatos_fijos <- base_actual$metadatos
    
    message(sprintf("\n[%s/%s] Consultando API INDEC para: %s (Carpeta: %s)", i, nrow(catalogo_indec), serie_id, tema))
    
    # La función update_indec_json_serie se encarga de todo
    exito <- update_indec_json_serie(
      id_indec = id_indec,
      serie_id = serie_id,
      tema = tema,
      metadatos_fijos = metadatos_fijos
    )
    
    if (exito) hubo_actualizaciones <- TRUE
  } else {
    warning(sprintf("El archivo local no existe en la ruta: %s. Revisa si la carpeta o el nombre cambiaron.", path_archivo))
  }
}

# 4. Sincronización con GitHub
# Solo hacemos push si el script logró conectarse y actualizar los JSON
# if (hubo_actualizaciones) {
#   message("\nIniciando sincronización con el repositorio remoto...")
#   tryCatch({
#     system("git add .")
#     
#     # Comprobamos si Git detectó modificaciones reales antes de hacer commit
#     status_git <- system("git status --porcelain", intern = TRUE)
#     
#     if (length(status_git) > 0) {
#       mensaje_commit <- paste0("Update INDEC automatizado: ", Sys.Date())
#       system(sprintf('git commit -m "%s"', mensaje_commit))
#       system("git push")
#       message("✓ Nuevos datos detectados y subidos a GitHub con éxito.")
#     } else {
#       message("✓ API consultada, pero no hay datos nuevos publicados por el INDEC hoy.")
#     }
#   }, error = function(e) {
#     warning("Hubo un problema al intentar subir los cambios a GitHub: ", e$message)
#   })
# }