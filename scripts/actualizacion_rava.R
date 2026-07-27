library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)

# ------------------------------------------------------------
# 1. CARGA DE LÓGICA CORE Y CONFIGURACIÓN
# ------------------------------------------------------------
source("scripts/funciones_base.R")

# Archivo CSV actualizado que irás subiendo a tu carpeta
archivo_csv  <- "inputs/historicos/MERVAL - Cotizaciones historicas.csv" 
path_catalogo <- "catalogo.json"
tema_salida   <- "FINANZAS"
hoy           <- as.character(Sys.Date())

if (!file.exists(path_catalogo)) {
  stop("ERROR: No se encontró el catálogo central 'catalogo.json'.")
}
catalogo <- fromJSON(path_catalogo)

# Configuración de las dos series objetivo
series_config <- list(
  list(col_agregado = "cierre_fin_de_mes", id_base = "MERVAL_FINMES_NSA_M"),
  list(col_agregado = "promedio_mensual",  id_base = "MERVAL_PROMEDIO_NSA_M")
)

# ------------------------------------------------------------
# 2. LECTURA Y ETL MENSUAL DEL CSV ENTRANTE
# ------------------------------------------------------------
if (!file.exists(archivo_csv)) {
  stop("ERROR: No se encontró el archivo CSV en la ruta: ", archivo_csv)
}

message("Procesando datos diarios del MERVAL para actualización...")

df_raw <- read.csv(archivo_csv, stringsAsFactors = FALSE)
colnames(df_raw) <- tolower(trimws(colnames(df_raw)))

if (!all(c("fecha", "cierre") %in% colnames(df_raw))) {
  stop("El CSV debe contener al menos las columnas 'fecha' y 'cierre'.")
}

# Limpieza y agregación mensual a partir del CSV
df_daily <- df_raw %>%
  select(fecha, cierre) %>%
  mutate(
    fecha = parse_date_time(fecha, orders = c("Ymd", "dmY", "Y-m-d", "d/m/Y")),
    cierre_chr = as.character(cierre),
    cierre = if_else(
      grepl(",", cierre_chr),
      gsub(",", ".", gsub("\\.", "", cierre_chr)),
      cierre_chr
    ),
    cierre = as.numeric(cierre)
  ) %>%
  filter(!is.na(fecha) & !is.na(cierre)) %>%
  arrange(fecha) %>%
  mutate(periodo = format(fecha, "%Y-%m"))

# Agregación a nivel mensual (Fin de mes y Promedio)
df_monthly_entrante <- df_daily %>%
  group_by(periodo) %>%
  summarise(
    cierre_fin_de_mes = dplyr::last(cierre),
    promedio_mensual  = mean(cierre, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    fecha = paste0(periodo, "-01") # Formato YYYY-MM-01
  )

# ------------------------------------------------------------
# 3. PROCESAMIENTO Y COMPARACIÓN DE VINTAGES POR SERIE
# ------------------------------------------------------------
for (config in series_config) {
  id_serie_final <- config$id_base
  path_json_existente <- file.path(tema_salida, paste0(id_serie_final, ".json"))
  
  if (!file.exists(path_json_existente)) {
    warning("La serie ", id_serie_final, " no existe en '", tema_salida, "/'. Correr primero la carga inicial.")
    next
  }
  
  # Extraer la columna de interés según la serie (cierre o promedio)
  df_entrante <- df_monthly_entrante %>%
    select(fecha, valor = !!sym(config$col_agregado)) %>%
    filter(!is.na(valor))
  
  if (nrow(df_entrante) == 0) next
  
  # Leer el JSON viejo
  json_viejo <- fromJSON(path_json_existente)
  obs_viejas <- as.data.frame(json_viejo$observaciones)
  metadatos  <- json_viejo$metadatos
  
  # Separar vigentes (activos) e históricos cerrados
  obs_vigentes  <- obs_viejas %>% filter(realtime_end == "9999-12-31")
  obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
  
  # Evaluar diferencias punto a punto
  actualizadas <- df_entrante %>%
    left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
    mutate(
      status = case_when(
        is.na(valor_viejo) ~ "NUEVO",
        round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
        TRUE ~ "SIN_CAMBIOS"
      )
    )
  
  if (!any(actualizadas$status %in% c("NUEVO", "REVISADO"))) {
    message("  -> Serie sin cambios detectados: ", id_serie_final)
    next
  }
  
  message("  -> Cambios/Nuevos datos detectados. Actualizando Vintage para: ", id_serie_final)
  
  # Cerrar vintage para las observaciones revisadas
  obs_vigentes_que_cambiaron <- obs_vigentes %>%
    filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
    mutate(realtime_end = hoy)
  
  # Mantener vigentes no modificadas
  obs_vigentes_sin_cambio <- obs_vigentes %>%
    filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
  
  # Inserciones (Nuevos meses agregados + Revisiones con fecha_start de hoy)
  nuevas_inserciones <- actualizadas %>%
    filter(status %in% c("NUEVO", "REVISADO")) %>%
    select(fecha, valor = valor_nuevo) %>%
    mutate(realtime_start = hoy, realtime_end = "9999-12-31")
  
  # Consolidar historia cronológica completa
  obs_consolidadas <- bind_rows(
    obs_historicas,
    obs_vigentes_que_cambiaron,
    obs_vigentes_sin_cambio,
    nuevas_inserciones
  ) %>% arrange(fecha, realtime_start)
  
  # Actualizar metadatos
  metadatos$ultima_actualizacion <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  metadatos$fecha_inicio <- min(obs_consolidadas$fecha)
  
  salida_json <- list(
    serie_id = id_serie_final,
    metadatos = metadatos,
    observaciones = obs_consolidadas
  )
  
  # Reescritura del archivo JSON físico
  writeLines(toJSON(salida_json, auto_unbox = TRUE, pretty = TRUE), path_json_existente)
  
  assign(id_serie_final, obs_consolidadas, envir = .GlobalEnv)
}

message("\n[PROCESO DE ACTUALIZACIÓN DEL MERVAL FINALIZADO]")
