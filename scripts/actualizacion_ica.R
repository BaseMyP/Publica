# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA: INTERCAMBIO COMERCIAL ARGENTINO (ICA)
# Manejo de revisiones e inserción vintage (realtime_start / realtime_end)
# ==============================================================================

library(readr)
library(dplyr)
library(jsonlite)
source("scripts/funciones_base.R")

message("Iniciando revisión de dataset ICA: ", Sys.time())

cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_ica <- catalogo_completo %>% 
  filter(metodo_etl == "CSV_ICA")

if (nrow(catalogo_ica) == 0) {
  message("No hay series configuradas para CSV_ICA. Finalizando script.")
  quit(save = "no")
}

url_ica <- "https://infra.datos.gob.ar/catalog/sspm/dataset/74/distribution/74.3/download/intercambio-comercial-argentino-mensual.csv"

df_nuevo <- tryCatch({
  read_csv(url_ica, show_col_types = FALSE)
}, error = function(e) {
  warning("No se pudo descargar el archivo CSV: ", e$message)
  quit(save = "no")
})

col_fecha <- names(df_nuevo)[grepl("^(indice_tiempo|fecha|time|date)", names(df_nuevo), ignore.case = TRUE)][1]
if (is.na(col_fecha)) {
  stop("Columna temporal no identificada en el CSV remoto.")
}

# Eliminar columnas que son suma de otras dos
df_nuevo <- df_nuevo %>% 
  select(-ica_bienes_capital_partes_piezas,
         -ica_bienes_intermedios_combustibles_lubricantes,
         -ica_importaciones_bs_consumo_vehiculos_automotor_pasajeros)

fecha_hoy <- as.character(Sys.Date())
hubo_actualizaciones <- FALSE

for (i in 1:nrow(catalogo_ica)) {
  serie_id <- catalogo_ica$serie_id[i]
  raw_url  <- catalogo_ica$raw_url[i]
  tema     <- basename(dirname(raw_url))
  
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (!file.exists(path_archivo)) {
    warning(sprintf("El archivo local no existe: %s", path_archivo))
    next
  }
  
  base_actual <- fromJSON(path_archivo)
  col_original <- base_actual$metadatos$id_original
  
  if (!col_original %in% names(df_nuevo)) {
    warning(sprintf("Columna '%s' ausente en el archivo descargado.", col_original))
    next
  }
  
  datos_actuales <- base_actual$datos %>%
    mutate(
      fecha = as.character(fecha),
      realtime_start = as.character(realtime_start),
      realtime_end   = as.character(realtime_end),
      valor = as.numeric(valor)
    )
  
  df_remoto <- df_nuevo %>%
    select(fecha = all_of(col_fecha), valor_remoto = all_of(col_original)) %>%
    filter(!is.na(valor_remoto)) %>%
    mutate(
      fecha = as.character(as.Date(fecha)),
      valor_remoto = as.numeric(valor_remoto)
    )
  
  # Identificar las observaciones que están activas actualmente (realtime_end == 9999-12-31)
  vigentes <- datos_actuales %>% filter(realtime_end == "9999-12-31")
  historico_cerrado <- datos_actuales %>% filter(realtime_end != "9999-12-31")
  
  # Comparar datos vigentes con los datos recién descargados
  comparacion <- full_join(vigentes, df_remoto, by = "fecha")
  
  # 1. Puntos completamente nuevos (meses que no existían)
  nuevos_puntos <- comparacion %>%
    filter(is.na(valor) & !is.na(valor_remoto)) %>%
    transmute(
      fecha          = fecha,
      realtime_start = fecha_hoy,
      realtime_end   = "9999-12-31",
      valor          = valor_remoto
    )
  
  # 2. Revisiones: fechas ya existentes donde el valor remoto difiere del vigente
  fechas_revisadas <- comparacion %>%
    filter(!is.na(valor) & !is.na(valor_remoto) & abs(valor - valor_remoto) > 1e-4) %>%
    pull(fecha)
  
  if (nrow(nuevos_puntos) > 0 || length(fechas_revisadas) > 0) {
    message(sprintf("[%s/%s] Cambios vintage detectados para: %s (Nuevos: %s, Revisiones: %s)", 
                    i, nrow(catalogo_ica), serie_id, nrow(nuevos_puntos), length(fechas_revisadas)))
    
    # Cerrar vigencia de las filas que sufrieron revisión
    vigentes_actualizados <- vigentes %>%
      mutate(
        realtime_end = if_else(fecha %in% fechas_revisadas, fecha_hoy, realtime_end)
      )
    
    # Crear las nuevas versiones vigentes para los datos corregidos
    nuevas_versiones_revisadas <- comparacion %>%
      filter(fecha %in% fechas_revisadas) %>%
      transmute(
        fecha          = fecha,
        realtime_start = fecha_hoy,
        realtime_end   = "9999-12-31",
        valor          = valor_remoto
      )
    
    # Consolidar todo el historial vintage
    datos_finales <- bind_rows(
      historico_cerrado,
      vigentes_actualizados,
      nuevas_versiones_revisadas,
      nuevos_puntos
    ) %>%
      arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- Sys.Date()
    base_actual$datos <- datos_finales
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
    hubo_actualizaciones <- TRUE
  } else {
    message(sprintf("[%s/%s] Sin novedades en: %s", i, nrow(catalogo_ica), serie_id))
  }
}

if (hubo_actualizaciones) {
  message("✓ Actualización completada y registros vintage preservados.")
} else {
  message("✓ Todas las series del ICA se encuentran al día.")
}