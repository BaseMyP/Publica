
library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)

# ------------------------------------------------------------
# 1. CARGA DE LÓGICA CORE Y CONFIGURACIÓN
# ------------------------------------------------------------
# Cargamos tus funciones compartidas del proyecto
source("funciones_base.R")

# Archivo Excel actualizado (el nuevo vintage a comparar)
archivo_excel <- "inputs/historicos/nacional_serie_remuneraciones_mensual.xlsx" 

path_catalogo <- "catalogo.json"
tema_salida   <- "LABORAL"
hoy           <- as.character(Sys.Date())

if (!file.exists(path_catalogo)) {
  stop("ERROR: No se encontró el catálogo central 'catalogo.json'.")
}
catalogo <- fromJSON(path_catalogo)

# Configuración específica de las columnas coincidente con tu carga inicial
columnas_config <- list(
  list(col_idx = 3, id_base = "SALARIOS_PRIV_NOMINAL_NSA_M"), # Columna C (Original)
  list(col_idx = 6, id_base = "SALARIOS_PRIV_NOMINAL_SA_M")   # Columna F (Desestacionalizada)
)

# ------------------------------------------------------------
# 2. PROCESAMIENTO Y COMPARACIÓN DE VINTAGES
# ------------------------------------------------------------
if (!file.exists(archivo_excel)) {
  stop("ERROR: No se encontró el archivo Excel en la ruta: ", archivo_excel)
}

message("Leyendo hoja 'C 1' para actualización de salarios...")
raw <- read_excel(archivo_excel, sheet = "C 1", skip = 5, col_names = FALSE)
raw <- as.data.frame(raw)

# Parseo robusto y homogéneo de las fechas del nuevo archivo
fechas_limpias <- tryCatch({
  as.Date(raw[, 1])
}, error = function(e) {
  as.Date(as.numeric(as.character(raw[, 1])), origin = "1899-12-30")
})

raw$fecha_procesada <- as.character(fechas_limpias)

# Filtrar filas cronológicas válidas eliminando basura del final
raw_filtrado <- raw %>% 
  filter(!is.na(fecha_procesada) & fecha_procesada != "" & str_detect(fecha_procesada, "^\\d{4}-\\d{2}-\\d{2}"))

fechas_series <- raw_filtrado$fecha_procesada

# Iterar sobre las dos series de salarios configuradas
for (config in columnas_config) {
  id_serie_final <- config$id_base
  path_json_existente <- file.path(tema_salida, paste0(id_serie_final, ".json"))
  
  # Si por alguna razón el JSON no existe localmente, advertir y saltear
  if (!file.exists(path_json_existente)) {
    warning("La serie ", id_serie_final, " no existe de forma local en '", tema_salida, "/'. Correr primero la carga inicial.")
    next
  }
  
  # 1. Extraer los datos nuevos del Excel recién publicado
  valores_crudos <- as.numeric(as.character(raw_filtrado[, config$col_idx]))
  
  df_entrante <- data.frame(
    fecha = fechas_series,
    valor = valores_crudos,
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(valor))
  
  if (nrow(df_entrante) == 0) next
  
  # 2. Leer el archivo JSON actual para extraer su historia y metadatos
  json_viejo <- fromJSON(path_json_existente)
  obs_viejas <- as.data.frame(json_viejo$observaciones)
  metadatos  <- json_viejo$metadatos
  
  # 3. CONSOLIDACIÓN DE VINTAGES (Lógica exacta de tu funciones_base.R)
  # Separamos vigentes activos (9999-12-31) e históricos ya cerrados
  obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
  obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
  
  # Ejecutar left_join para clasificar el estado de cada punto temporal
  actualizadas <- df_entrante %>%
    left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
    mutate(
      status = case_when(
        is.na(valor_viejo) ~ "NUEVO",
        round(valor, 4) != round(valor_viejo, 4) ~ "REVISADO",
        TRUE ~ "SIN_CAMBIOS"
      )
    )
  
  # Si todo es "SIN_CAMBIOS", se pasa a la siguiente serie de forma segura
  if (!any(actualizadas$status %in% c("NUEVO", "REVISADO"))) {
    message("  -> Serie sin cambios detectados: ", id_serie_final)
    next
  }
  
  message("  -> Cambios detectados. Actualizando Vintage para: ", id_serie_final)
  
  # Cerrar el vintage de las celdas que sufrieron revisión (realtime_end = hoy)
  obs_vigentes_que_cambiaron <- obs_vigentes %>%
    filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
    mutate(realtime_end = hoy)
  
  # Mantener intactas las vigentes que no se modificaron
  obs_vigentes_sin_cambio <- obs_vigentes %>%
    filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
  
  # Insertar los nuevos registros o los revisados con la marca de tiempo de hoy
  nuevas_inserciones <- actualizadas %>%
    filter(status %in% c("NUEVO", "REVISADO")) %>%
    select(fecha, valor) %>%
    mutate(realtime_start = hoy, realtime_end = "9999-12-31")
  
  # Unificar toda la base cronológica final
  obs_consolidadas <- bind_rows(
    obs_historicas,
    obs_vigentes_que_cambiaron,
    obs_vigentes_sin_cambio,
    nuevas_inserciones
  ) %>% arrange(fecha, realtime_start)
  
  # 4. REESCRITURA DEL ARCHIVO JSON CON HISTORIA CONSOLIDADA
  metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
  metadatos$fecha_inicio <- min(obs_consolidadas$fecha)
  
  salida_json <- list(
    serie_id = id_serie_final,
    metadatos = metadatos,
    observaciones = obs_consolidadas
  )
  
  # Sobreescribimos el JSON físico
  writeLines(toJSON(salida_json, auto_unbox = TRUE, pretty = TRUE), path_json_existente)
  
  # Exportamos el dataframe final al Entorno Global de RStudio para auditoría directa
  assign(id_serie_final, obs_consolidadas, envir = .GlobalEnv)
}

message("\n[PROCESO DE ACTUALIZACIÓN LABORAL FINALIZADO]")
