# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN MENSUAL (REM) UNIFICADO
# ==============================================================================

library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(readxl)
source("scripts/funciones_base.R")

message("Iniciando revisión de actualización del REM: ", Sys.time())

# 1. Definir rutas
ruta_input <- "inputs/rem_nuevo.xlsx"
ruta_historico <- "inputs/historicos/"

if (!dir.exists(ruta_historico)) dir.create(ruta_historico, recursive = TRUE)

# 2. Control de ejecución
if (!file.exists(ruta_input)) {
  message("No se detectó un archivo 'rem_nuevo.xlsx' en la carpeta inputs/. Finalizando rutina.")
  quit(save = "no")
}

message("¡Nuevo archivo del REM detectado! Iniciando procesamiento...")

# 3. Diccionarios de mapeo
dict_variable <- c(
  "Precios minoristas (IPC nivel general-GBA; INDEC)" = "IPCGBA",
  "Precios minoristas (IPC nivel general; INDEC)" = "IPCGENERAL",    
  "Tasa de política monetaria (Lebac)" = "TPMLEBAC",    
  "Tipo de cambio nominal" = "USDA3500",                               
  "Resultado primario del SPNF" = "SUPPRIMARIO", 
  "Resultado Primario del SPNF" = "SUPPRIMARIO", 
  "PIB a precios constantes" = "PIBPCTE",                          
  "Precios minoristas (IPC núcleo-GBA; INDEC)" = "IPCNUCLEOGBA",  
  "Tasa de política monetaria (Pase 7 días)" = "TPMPASE7",         
  "Precios minoristas (IPC núcleo; INDEC)" = "IPCNUCLEO",     
  "Tasa de política monetaria (LELIQ)" = "TPMLELIQ",        
  "Tasa de interés (LELIQ)" = "TASALELIQ",                          
  "Tasa de interés (BADLAR)" = "BADLAR",                  
  "Exportaciones" = "EXPO",                                    
  "Importaciones" = "IMPO",                             
  "Desocupación abierta" = "DESOCUPACION",                             
  "Tasa de interés (TAMAR)" = "TASATAMAR"
)

dict_referencia <- c(
  "var. % mensual" = "MOM",  "var. % i.a." = "YOY", "TNA; %" = "TNA",
  "$/USD" = "LEVEL", "miles de millones $" = "BILARS", "var. % prom. anual" = "YOYAVG",
  "var. % trim. s.e." = "QOQ_SA", "millones de USD" = "MILUSD", "% de la PEA" = "RATE"
)

# 4. Leer el archivo entrante
rem_entrante <- read_excel(ruta_input, sheet = "Base de Datos Completa", skip = 1) %>% 
  mutate(Referencia = str_remove_all(Referencia,"; [a-z]{3}-[0-9]{2}$")) %>% 
  mutate(Referencia = str_remove_all(Referencia,"; Trim\\. [A-Z]{1,2}-[0-9]{2}$")) %>% 
  rename("realtime_start" = "Fecha de pronóstico", "valor"="Mediana") %>% 
  select("realtime_start","Variable","Referencia","Período","valor")

# 5. Lógica de estructuración
message("Paso 1: Estructurando los datos del nuevo reporte...")

rem_limpio_nuevo <- rem_entrante %>%
  mutate(realtime_start = as.Date(realtime_start)) %>%
  mutate(
    var_code = dict_variable[Variable],
    ref_code = dict_referencia[Referencia]
  ) %>%
  mutate(
    fecha_target = case_when(
      str_detect(Período, "^[0-9]{5}$") ~ as.Date(suppressWarnings(as.numeric(Período)), origin = "1899-12-30"),
      str_detect(Período, "^[0-9]{4}$") ~ as.Date(paste0(Período, "-01-01"), format = "%Y-%m-%d"),
      str_detect(Período, "^Trim\\.") ~ suppressWarnings(yq(paste0("20", str_sub(Período, -2), "-", str_extract(Período, "(I|II|III|IV)")))),
      str_detect(Período, "Próx\\. 12 meses") ~ realtime_start %m+% months(12),
      str_detect(Período, "Próx\\. 24 meses") ~ realtime_start %m+% months(24),
      TRUE ~ as.Date(NA)
    ),
    frecuencia = case_when(
      str_detect(Período, "^[0-9]{5}$") ~ "M",
      str_detect(Período, "^[0-9]{4}$") ~ "A",
      str_detect(Período, "^Trim\\.") ~ "Q",
      str_detect(Período, "Próx\\. 12 meses|Próx\\. 24 meses") ~ "M",
      TRUE ~ NA_character_
    )
  ) %>%
  # CORRECCIÓN: Armamos el nombre sin el prefijo "REM_"
  mutate(serie_id = paste(var_code, ref_code, frecuencia, sep = "_")) %>%
  filter(!is.na(fecha_target) & !is.na(serie_id))

series_a_actualizar <- unique(rem_limpio_nuevo$serie_id)
message(sprintf("Paso 2: Actualizando %s archivos JSON...", length(series_a_actualizar)))

# 6. Bucle de actualización por serie
for (id in series_a_actualizar) {
  
  # CORRECCIÓN: Guardamos en la nueva carpeta EXPECTATIVAS
  path_archivo <- file.path("EXPECTATIVAS", paste0(id, ".json"))
  
  datos_nuevos_serie <- rem_limpio_nuevo %>% filter(serie_id == id) %>%
    select(fecha = fecha_target, valor, realtime_start) %>%
    mutate(
      fecha = as.character(fecha), 
      realtime_start = as.character(realtime_start)
    )
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    obs_historicas <- base_actual$observaciones %>% select(fecha, valor, realtime_start)
    
    obs_consolidadas <- bind_rows(obs_historicas, datos_nuevos_serie) %>%
      distinct(fecha, realtime_start, .keep_all = TRUE)
    
    obs_final <- obs_consolidadas %>%
      mutate(f_calc = as.Date(fecha), rs_calc = as.Date(realtime_start)) %>%
      group_by(f_calc) %>%
      arrange(rs_calc) %>%
      mutate(
        realtime_end = lead(rs_calc) - days(1),
        realtime_end = as.character(if_else(is.na(realtime_end), as.Date("9999-12-31"), realtime_end))
      ) %>%
      ungroup() %>%
      arrange(f_calc, rs_calc) %>%
      select(fecha, valor, realtime_start, realtime_end)
    
    base_actual$observaciones <- obs_final
    base_actual$metadatos$ultima_actualizacion <- paste0(as.character(Sys.Date()), "T12:00:00Z")
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  } else {
    warning(sprintf("La serie %s no existe en la base. Ignorando.", id))
  }
}

# 7. Sincronización con GitHub
message("\nIniciando sincronización con el repositorio remoto...")
tryCatch({
  system("git add .")
  mensaje_commit <- paste0("Update mensual REM publicado el: ", Sys.Date())
  system(sprintf('git commit -m "%s"', mensaje_commit))
  system("git push")
  message("✓ Rutina del REM completada con éxito. Base actualizada en la nube.")
  
  # Archivar archivo
  nuevo_nombre <- paste0(ruta_historico, "rem_procesado_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
  file.rename(from = ruta_input, to = nuevo_nombre)
  message("✓ Archivo crudo movido al historial.")
  
}, error = function(e) {
  warning("Hubo un problema al intentar subir los cambios a GitHub: ", e$message)
})