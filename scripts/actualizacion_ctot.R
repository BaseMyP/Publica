
# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA (IMF CTOT - SDMX 3.0)
# ==============================================================================

library(jsonlite)
library(dplyr)
library(httr)

source("scripts/funciones_base.R")

message("Iniciando revisión diaria de API IMF (CTOT): ", Sys.time())

# ==============================================================================
# 1. PARÁMETROS DE LA CONSULTA
# ==============================================================================

indicador_ctot <- "CEMPI_CTOTXM_GDP.R_FW_IX"
fecha_inicio <- "1980-01"
fecha_fin <- format(Sys.Date(), "%Y-%m")

# ==============================================================================
# 2. FUNCIÓN DE DESCARGA DESDE LA API DEL FMI
# ==============================================================================

fetch_imf_sdmx30_pais <- function(pais_iso3) {
  
  key <- paste0(pais_iso3, ".", indicador_ctot, ".M")
  url_base <- trimws(paste0("https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.RES/CTOT/~/", key))
  
  query_params <- list(
    startPeriod = fecha_inicio,
    endPeriod = fecha_fin
  )
  
  res <- tryCatch(
    {
      GET(
        url_base,
        query = query_params,
        timeout(60),
        add_headers(Accept = "text/csv")
      )
    },
    error = function(e) {
      message("  [ERROR DE RED] ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(res) || status_code(res) != 200) {
    message("  [ERROR HTTP] Código ", if (is.null(res)) "NULL" else status_code(res))
    return(NULL)
  }
  
  raw_csv <- content(res, as = "text", encoding = "UTF-8")
  if (is.null(raw_csv) || nchar(trimws(raw_csv)) == 0) return(NULL)
  
  df <- tryCatch(
    { read.csv(text = raw_csv, stringsAsFactors = FALSE, check.names = FALSE) },
    error = function(e) NULL
  )
  
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  col_pais <- intersect(c("REF_AREA", "COUNTRY"), names(df))[1]
  if (is.na(col_pais) || !"TIME_PERIOD" %in% names(df) || !"OBS_VALUE" %in% names(df)) return(NULL)
  
  df_clean <- df %>%
    mutate(
      pais_iso3 = as.character(.data[[col_pais]]),
      time_str = trimws(as.character(TIME_PERIOD)),
      fecha_str = case_when(
        grepl("^\\d{4}-M\\d{2}$", time_str) ~ paste0(sub("-M", "-", time_str), "-01"),
        grepl("^\\d{4}-\\d{2}$", time_str)  ~ paste0(time_str, "-01"),
        TRUE                               ~ time_str
      ),
      fecha = as.character(as.Date(fecha_str, format = "%Y-%m-%d")),
      valor = round(suppressWarnings(as.numeric(as.character(OBS_VALUE))), 4)
    ) %>%
    select(fecha, valor) %>%
    filter(!is.na(fecha), !is.na(valor)) %>%
    arrange(fecha)
  
  return(df_clean)
}

# ==============================================================================
# 3. FUNCIÓN DE ACTUALIZACIÓN DE JSON (VINTAGES)
# ==============================================================================

update_imf_json_serie <- function(serie_id, tema, metadatos_fijos, df_obs) {
  
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  hoy <- as.character(Sys.Date())
  
  base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
  
  obs_vigentes <- base_actual$observaciones[
    base_actual$observaciones$realtime_end == "9999-12-31",
  ]
  
  obs_historicas <- base_actual$observaciones[
    base_actual$observaciones$realtime_end != "9999-12-31",
  ]
  
  actualizadas <- merge(
    df_obs,
    obs_vigentes,
    by = "fecha",
    all.x = TRUE,
    suffixes = c("_nuevo", "_viejo")
  )
  
  actualizadas$status <- ifelse(
    is.na(actualizadas$valor_viejo),
    "NUEVO",
    ifelse(
      round(actualizadas$valor_nuevo, 4) != round(actualizadas$valor_viejo, 4),
      "REVISADO",
      "SIN_CAMBIOS"
    )
  )
  
  fechas_rev <- actualizadas$fecha[actualizadas$status == "REVISADO"]
                              obs_vigentes_que_cambiaron <- obs_vigentes[obs_vigentes$fecha %in% fechas_rev, ]
                              
                              if (nrow(obs_vigentes_que_cambiaron) > 0) {
                                obs_vigentes_que_cambiaron$realtime_end <- hoy
                              }
                              
                              obs_vigentes_sin_cambio <- obs_vigentes[!(obs_vigentes$fecha %in% fechas_rev), ]
                              nuevas_ins <- actualizadas[actualizadas$status %in% c("NUEVO", "REVISADO"), ]
                              
                              if (nrow(nuevas_ins) > 0) {
                                nuevas_ins <- nuevas_ins[, c("fecha", "valor_nuevo")]
                                colnames(nuevas_ins) <- c("fecha", "valor")
                                nuevas_ins$realtime_start <- hoy
                                nuevas_ins$realtime_end <- "9999-12-31"
                              } else {
                                nuevas_ins <- data.frame(
                                  fecha = character(),
                                  valor = numeric(),
                                  realtime_start = character(),
                                  realtime_end = character(),
                                  stringsAsFactors = FALSE
                                )
                              }
                              
                              obs_consolidadas <- rbind(
                                obs_historicas,
                                obs_vigentes_que_cambiaron,
                                obs_vigentes_sin_cambio,
                                nuevas_ins
                              )
                              
                              # Preservar los metadatos existentes y actualizar únicamente la última revisión
                              metadatos_actualizados <- metadatos_fijos
                              metadatos_actualizados$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
                              
                              lista_final <- list(
                                serie_id = serie_id,
                                metadatos = metadatos_actualizados,
                                observaciones = obs_consolidadas[
                                  order(obs_consolidadas$fecha, obs_consolidadas$realtime_start),
                                ]
                              )
                              
                              write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
                              message("  > Serie actualizada con éxito: ", path_archivo)
                              
                              update_catalogo(
                                serie_id = serie_id,
                                metadatos = metadatos_actualizados,
                                metodo_etl = "API_IMF",
                                tema = tema
                              )
}

# ==============================================================================
# 4. LECTURA DEL CATÁLOGO Y BUCLE DE ACTUALIZACIÓN
# ==============================================================================

cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# Filtrar solo las series cargadas por la API del FMI
catalogo_imf <- catalogo_completo %>% 
  filter(metodo_etl == "API_IMF")

if (nrow(catalogo_imf) == 0) {
  message("No hay series configuradas para API_IMF. Finalizando script.")
  quit(save = "no")
}

for (i in 1:nrow(catalogo_imf)) {
  serie_id <- catalogo_imf$serie_id[i]
  raw_url <- catalogo_imf$raw_url[i]
  
  # Extraer la carpeta contenedora desde la URL
  tema <- basename(dirname(raw_url))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # Extraer el código del país a partir del campo 'id_original' (ej: "ARG.CEMPI_CTOTXM_GDP...")
    codigo_original <- base_actual$metadatos$id_original
    pais_iso3 <- sub("\\..*", "", codigo_original)
    
    metadatos_fijos <- base_actual$metadatos
    
    message(sprintf("\n[%s/%s] Consultando API IMF para: %s (País: %s | Tema: %s)", 
                    i, nrow(catalogo_imf), serie_id, pais_iso3, tema))
    
    df_obs <- fetch_imf_sdmx30_pais(pais_iso3)
    
    if (!is.null(df_obs) && nrow(df_obs) > 0) {
      update_imf_json_serie(
        serie_id = serie_id,
        tema = tema,
        metadatos_fijos = metadatos_fijos,
        df_obs = df_obs
      )
    } else {
      message("  [AVISO] No se obtuvieron nuevas observaciones de la API para ", pais_iso3)
    }
    
  } else {
    warning(sprintf("El archivo local no existe en la ruta: %s", path_archivo))
  }
}

message("\n==================================================================")
message("ACTUALIZACIÓN FINALIZADA")
message("==================================================================")
