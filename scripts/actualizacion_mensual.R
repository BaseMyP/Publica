# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN MENSUAL (REM)
# ==============================================================================

library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl) # Para leer el archivo que descargues del BCRA
source("scripts/funciones_base.R")

message("Iniciando revisión de actualización del REM: ", Sys.time())

# 1. Definir la ruta del archivo crudo esperado
ruta_input <- "inputs/rem_nuevo.xlsx"
ruta_historico <- "inputs/historicos/"

# Si la carpeta de históricos no existe, la creamos
if (!dir.exists(ruta_historico)) dir.create(ruta_historico, recursive = TRUE)

# 2. Control de ejecución: ¿Hay un archivo nuevo para procesar?
if (!file.exists(ruta_input)) {
  message("No se detectó un archivo 'rem_nuevo.xlsx' en la carpeta inputs/. Finalizando rutina.")
  quit(save = "no") # El script termina aquí silenciosamente
}

message("¡Nuevo archivo del REM detectado! Iniciando procesamiento...")

# 3. Leer el archivo y ejecutar la función de actualización
rem_entrante <- read_excel(ruta_input)

# Llamamos a la función que diseñamos en la iteración anterior
actualizar_rem_json(rem_nuevo = rem_entrante)

# 4. Sincronización con GitHub
message("\nIniciando sincronización con el repositorio remoto...")
tryCatch({
  system("git add .")
  mensaje_commit <- paste0("Update mensual REM publicado el: ", Sys.Date())
  system(sprintf('git commit -m "%s"', mensaje_commit))
  system("git push")
  message("✓ Rutina del REM completada con éxito. Base actualizada en la nube.")
  
  # 5. Archivar el archivo procesado para no volver a leerlo mañana
  nuevo_nombre <- paste0(ruta_historico, "rem_procesado_", format(Sys.Date(), "%Y%m"), ".xlsx")
  file.rename(from = ruta_input, to = nuevo_nombre)
  message("✓ Archivo crudo movido al historial.")
  
}, error = function(e) {
  warning("Hubo un problema al intentar subir los cambios a GitHub: ", e$message)
})