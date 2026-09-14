
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

source("scripts/funciones_base.R")


# ============================================================
# 1. DICCIONARIO DE PAÍSES
# ============================================================

paises_dicc <- c(
  "ARG" = "Argentina",
  "BRA" = "Brasil",
  "CHE" = "Suiza",
  "CHL" = "Chile",
  "CAN" = "Canada",
  "CHN" = "China",
  "DEU" = "Alemania",
  "ESP" = "Espana",
  "FRA" = "Francia",
  "GBR" = "Reino Unido",
  "IND" = "India",
  "JPN" = "Japon",
  "ITA" = "Italia",
  "MEX" = "Mexico",
  "NLD" = "Paises Bajos",
  "USA" = "Estados Unidos",
  "URY" = "Uruguay",
  "VNM" = "Vietnam"
)


# ============================================================
# 2. PARÁMETROS DE LA DESCARGA
# ============================================================

indicador_ctot <- "CEMPI_CTOTXM_GDP.R_FW_IX"

fecha_inicio <- "1980-01"

fecha_fin <- format(
  Sys.Date(),
  "%Y-%m"
)


# ============================================================
# 3. DESCARGAR CTOT POR PAÍS DESDE LA API SDMX 3.0
# ============================================================

fetch_imf_sdmx30_pais <- function(pais_iso3) {
  
  key <- paste0(
    pais_iso3,
    ".",
    indicador_ctot,
    ".M"
  )
  
  url_base <- trimws(paste0("https://api.imf.org/external/sdmx/3.0/data/dataflow/IMF.RES/CTOT/~/", key))
  
  query_params <- list(
    startPeriod = fecha_inicio,
    endPeriod = fecha_fin
  )
  
  message("> Solicitando URL: ", url_base)
  
  res <- tryCatch(
    {
      GET(
        url_base,
        query = query_params,
        timeout(60),
        add_headers(
          Accept = "text/csv"
        )
      )
    },
    error = function(e) {
      message("  [ERROR DE RED] ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(res)) {
    return(NULL)
  }
  
  status <- status_code(res)
  
  if (status != 200) {
    message("  [ERROR HTTP] Código ", status)
    return(NULL)
  }
  
  raw_csv <- content(
    res,
    as = "text",
    encoding = "UTF-8"
  )
  
  if (is.null(raw_csv) || nchar(trimws(raw_csv)) == 0) {
    message("  [ERROR] La API devolvió una respuesta vacía.")
    return(NULL)
  }
  
  df <- tryCatch(
    {
      read.csv(
        text = raw_csv,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },
    error = function(e) {
      message("  [ERROR AL LEER CSV] ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(df) || nrow(df) == 0) {
    message("  [AVISO] El CSV recibido no contiene observaciones.")
    return(NULL)
  }
  
  col_pais <- intersect(c("REF_AREA", "COUNTRY"), names(df))[1]
  
  if (is.na(col_pais)) {
    message("  [ERROR] No se encontró la columna del país (REF_AREA o COUNTRY).")
    return(NULL)
  }
  
  if (!"TIME_PERIOD" %in% names(df) || !"OBS_VALUE" %in% names(df)) {
    message("  [ERROR] Faltan columnas TIME_PERIOD u OBS_VALUE.")
    return(NULL)
  }
  
  # ----------------------------------------------------------
  # PARSEO DE FECHA ADAPTADO A SDMX (1980-M01 -> 1980-01-01)
  # ----------------------------------------------------------
  df_clean <- df %>%
    mutate(
      pais_iso3 = as.character(.data[[col_pais]]),
      time_str = trimws(as.character(TIME_PERIOD)),
      
      # Convierte '1980-M01' a '1980-01-01'
      fecha_str = case_when(
        grepl("^\\d{4}-M\\d{2}$", time_str) ~ paste0(sub("-M", "-", time_str), "-01"),
        grepl("^\\d{4}-\\d{2}$", time_str)  ~ paste0(time_str, "-01"),
        TRUE                               ~ time_str
      ),
      
      fecha = as.character(as.Date(fecha_str, format = "%Y-%m-%d")),
      valor = round(suppressWarnings(as.numeric(as.character(OBS_VALUE))), 4)
    ) %>%
    select(pais_iso3, fecha, valor) %>%
    filter(!is.na(fecha), !is.na(valor)) %>%
    arrange(pais_iso3, fecha)
  
  message("  > Observaciones válidas obtenidas: ", nrow(df_clean))
  
  return(df_clean)
}


# ============================================================
# 4. GUARDAR / ACTUALIZAR JSON DE CADA PAÍS
# ============================================================

update_imf_json_serie <- function(
    pais_iso3,
    df_obs
) {
  
  pais_nombre <- paises_dicc[pais_iso3]
  
  if (is.na(pais_nombre)) {
    message("  [AVISO] País no encontrado en diccionario: ", pais_iso3)
    pais_nombre <- pais_iso3
  }
  
  tema_destino <- ifelse(
    pais_iso3 == "ARG",
    "SECTOR_EXTERNO",
    "INTERNACIONAL"
  )
  
  serie_id <- paste0(
    "CTOT_",
    pais_iso3,
    "_INDICE_NSA_M"
  )
  
  path_dir <- file.path(tema_destino)
  
  path_archivo <- file.path(
    tema_destino,
    paste0(serie_id, ".json")
  )
  
  hoy <- as.character(Sys.Date())
  
  codigo_original <- paste0(
    pais_iso3,
    ".CEMPI_CTOTXM_GDP.R_FW_IX.M"
  )
  
  if (!dir.exists(path_dir)) {
    dir.create(
      path_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  
  metadatos <- list(
    titulo = paste0("Commodity Terms of Trade - ", pais_nombre),
    descripcion = "Commodity Net Export Price Index, Individual Commodities Weighted by Ratio of Net Exports to GDP. Fixed Weights, Index (June 2012 = 100)",
    pais = pais_nombre,
    categoria = tema_destino,
    frecuencia_short = "M",
    frecuencia_original = "mensual",
    unidades = "Indice",
    ajuste = "NSA",
    tipo_informacion = "Publica",
    fuente = "API_IMF",
    fuente_original = "IMF Primary Commodity Prices (CTOT)",
    fuente_formato = "API_IMF",
    id_original = codigo_original,
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    fecha_inicio = df_obs$fecha[1],
    url_original = "[https://data.imf.org/en/Data-Explorer?datasetUrn=IMF.RES:CTOT(5.0.1](https://data.imf.org/en/Data-Explorer?datasetUrn=IMF.RES:CTOT(5.0.1))",
    revisable = TRUE,
    notas = list()
  )
  
  if (!file.exists(path_archivo)) {
    
    message("  > Archivo no existe. Creando serie desde cero...")
    
    df_obs$realtime_start <- hoy
    df_obs$realtime_end <- "9999-12-31"
    
    lista_final <- list(
      serie_id = serie_id,
      metadatos = metadatos,
      observaciones = df_obs
    )
    
  } else {
    
    message("  > Archivo existente. Actualizando vintages...")
    
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
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    
    base_actual$observaciones <- obs_consolidadas[
      order(obs_consolidadas$fecha, obs_consolidadas$realtime_start),
    ]
    
    lista_final <- base_actual
  }
  
  write_json(
    lista_final,
    path_archivo,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  message("  > Serie guardada con éxito: ", path_archivo)
  
  update_catalogo(
    serie_id = serie_id,
    metadatos = metadatos,
    metodo_etl = "API_IMF",
    tema = tema_destino
  )
  
  return(TRUE)
}


# ============================================================
# 5. EJECUCIÓN PRINCIPAL
# ============================================================

message("\n==================================================================")
message("DESCARGA CTOT - IMF SDMX 3.0")
message("==================================================================")

for (pais in names(paises_dicc)) {
  
  message("\n----------------------------------------------------------")
  message("Procesando: ", pais)
  message("----------------------------------------------------------")
  
  df_pais <- fetch_imf_sdmx30_pais(pais)
  
  if (is.null(df_pais) || nrow(df_pais) == 0) {
    message("  [AVISO] No se pudieron obtener datos para ", pais)
  } else {
    
    df_sub <- df_pais %>%
      select(fecha, valor)
    
    tryCatch(
      {
        update_imf_json_serie(pais, df_sub)
      },
      error = function(e) {
        message("  [ERROR en ", pais, "]: ", e$message)
      }
    )
  }
}

message("\n==================================================================")
message("DESCARGA FINALIZADA")
message("==================================================================")