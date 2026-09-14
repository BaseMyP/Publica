
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

paises_api <- paste(
  names(paises_dicc),
  collapse = "+"
)

indicador_ctot <- "CEMPI_CTOTXM_GDP.R_FW_IX.M"

fecha_inicio <- "1980-01"

fecha_fin <- format(
  Sys.Date(),
  "%Y-%m"
)


# ============================================================
# 3. DESCARGAR CTOT DESDE LA API SDMX 3.0
# ============================================================

fetch_imf_sdmx30_batch <- function() {
  
  
  # ----------------------------------------------------------
  # Construcción de la key
  #
  # Dimensiones:
  # COUNTRY . INDICATOR . FREQUENCY
  #
  # Ejemplo:
  # ARG.CEMPI_CTOTXM_GDP.R_FW_IX.M
  # ----------------------------------------------------------
  
  key <- paste0(
    "M.",
    paises_api,
    ".",
    indicador_ctot
  )
  
  
  # ----------------------------------------------------------
  # URL
  #
  # ~ = última versión disponible
  # ----------------------------------------------------------
  
  url_base <- paste0(
    "https://api.imf.org/external/sdmx/3.0/",
    "data/dataflow/IMF.RES/CTOT/~/",
    key
  )
  
  
  # ----------------------------------------------------------
  # Parámetros
  # ----------------------------------------------------------
  
  query_params <- list(
    startPeriod = fecha_inicio,
    endPeriod = fecha_fin
  )
  
  
  message("")
  message(
    "=========================================================="
  )
  message(
    "Solicitando CTOT al FMI mediante SDMX 3.0..."
  )
  message(
    "=========================================================="
  )
  
  message(
    "> URL: ",
    url_base
  )
  
  message(
    "> Período: ",
    fecha_inicio,
    " a ",
    fecha_fin
  )
  
  
  # ----------------------------------------------------------
  # Request
  # ----------------------------------------------------------
  
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
      
      message(
        "[ERROR DE RED] ",
        e$message
      )
      
      return(NULL)
    }
  )
  
  
  # ----------------------------------------------------------
  # Verificar respuesta
  # ----------------------------------------------------------
  
  if (is.null(res)) {
    return(NULL)
  }
  
  
  status <- status_code(res)
  
  
  message(
    "> Código HTTP: ",
    status
  )
  
  
  if (status != 200) {
    
    message(
      "[ERROR HTTP] La API devolvió código ",
      status
    )
    
    respuesta <- tryCatch(
      
      content(
        res,
        as = "text",
        encoding = "UTF-8"
      ),
      
      error = function(e) ""
    )
    
    
    if (nchar(respuesta) > 0) {
      
      message(
        "Respuesta del FMI:"
      )
      
      message(
        substr(
          respuesta,
          1,
          2000
        )
      )
    }
    
    
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Leer CSV
  # ----------------------------------------------------------
  
  raw_csv <- content(
    res,
    as = "text",
    encoding = "UTF-8"
  )
  
  
  if (
    is.null(raw_csv) ||
    nchar(raw_csv) == 0
  ) {
    
    message(
      "[ERROR] La API devolvió una respuesta vacía."
    )
    
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Convertir CSV a dataframe
  # ----------------------------------------------------------
  
  df <- tryCatch(
    
    {
      read.csv(
        text = raw_csv,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },
    
    error = function(e) {
      
      message(
        "[ERROR AL LEER CSV] ",
        e$message
      )
      
      return(NULL)
    }
  )
  
  
  if (is.null(df)) {
    return(NULL)
  }
  
  
  message(
    "> Filas descargadas: ",
    nrow(df)
  )
  
  
  message(
    "> Columnas recibidas: ",
    paste(
      names(df),
      collapse = ", "
    )
  )
  
  
  # ----------------------------------------------------------
  # Verificar columnas necesarias
  # ----------------------------------------------------------
  
  # SDMX 3.0 en CSV suele nombrar la zona como REF_AREA o COUNTRY
  col_pais <- intersect(c("REF_AREA", "COUNTRY"), names(df))[1]
  
  if (is.na(col_pais)) {
    message("[ERROR] No se encontró columna de país (REF_AREA o COUNTRY) en la respuesta.")
    return(NULL)
  }
  
  if (!"TIME_PERIOD" %in% names(df) || !"OBS_VALUE" %in% names(df)) {
    message("[ERROR] Faltan columnas TIME_PERIOD u OBS_VALUE en la respuesta.")
    return(NULL)
  }
  
  
  # ----------------------------------------------------------
  # Limpiar (Usando parseo seguro con RegEx y lubridate/as.Date)
  # ----------------------------------------------------------
  
  df_clean <- df %>%
    mutate(
      pais_iso3 = as.character(.data[[col_pais]]),
      
      # Extraemos 'YYYY-MM' o 'YYYY-MM-DD' limpiando cualquier espacio extra
      time_str = trimws(as.character(TIME_PERIOD)),
      
      # Formateamos correctamente agregando -01 únicamente si viene como 'YYYY-MM'
      fecha_str = if_else(
        grepl("^\\d{4}-\\d{2}$", time_str),
        paste0(time_str, "-01"),
        time_str
      ),
      
      fecha = as.character(as.Date(fecha_str, format = "%Y-%m-%d")),
      valor = round(suppressWarnings(as.numeric(as.character(OBS_VALUE))), 4)
    ) %>%
    
    select(pais_iso3, fecha, valor) %>%
    filter(!is.na(fecha), !is.na(valor)) %>%
    arrange(pais_iso3, fecha)
  
  
  message(
    "> Observaciones válidas: ",
    nrow(df_clean)
  )
  
  
  message(
    "> Países encontrados: ",
    paste(
      unique(df_clean$pais_iso3),
      collapse = ", "
    )
  )
  
  
  return(df_clean)
}


# ============================================================
# 4. GUARDAR / ACTUALIZAR JSON DE CADA PAÍS
# ============================================================

update_imf_json_serie <- function(
    pais_iso3,
    df_obs
) {
  
  
  # ----------------------------------------------------------
  # Información del país
  # ----------------------------------------------------------
  
  pais_nombre <- paises_dicc[pais_iso3]
  
  
  if (is.na(pais_nombre)) {
    
    message(
      "  [AVISO] País no encontrado en diccionario: ",
      pais_iso3
    )
    
    pais_nombre <- pais_iso3
  }
  
  
  # ----------------------------------------------------------
  # Tema
  # ----------------------------------------------------------
  
  tema_destino <- ifelse(
    pais_iso3 == "ARG",
    "SECTOR_EXTERNO",
    "INTERNACIONAL"
  )
  
  
  # ----------------------------------------------------------
  # ID de la serie
  # ----------------------------------------------------------
  
  serie_id <- paste0(
    "CTOT_",
    pais_iso3,
    "_INDICE_NSA_M"
  )
  
  
  path_dir <- file.path(
    tema_destino
  )
  
  
  path_archivo <- file.path(
    tema_destino,
    paste0(
      serie_id,
      ".json"
    )
  )
  
  
  hoy <- as.character(
    Sys.Date()
  )
  
  
  codigo_original <- paste0(
    pais_iso3,
    ".CEMPI_CTOTXM_GDP.R_FW_IX.M"
  )
  
  
  # ----------------------------------------------------------
  # Crear directorio
  # ----------------------------------------------------------
  
  if (!dir.exists(path_dir)) {
    
    dir.create(
      path_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  
  
  # ----------------------------------------------------------
  # Metadatos
  # ----------------------------------------------------------
  
  metadatos <- list(
    
    titulo = paste0(
      "Commodity Terms of Trade - ",
      pais_nombre
    ),
    
    descripcion =
      "Commodity Net Export Price Index, Individual Commodities Weighted by Ratio of Net Exports to GDP. Fixed Weights, Index (June 2012 = 100)",
    
    pais = pais_nombre,
    
    categoria = tema_destino,
    
    frecuencia_short = "M",
    
    frecuencia_original = "mensual",
    
    unidades = "Indice",
    
    ajuste = "NSA",
    
    tipo_informacion = "Publica",
    
    fuente = "API_IMF",
    
    fuente_original =
      "IMF Primary Commodity Prices (CTOT)",
    
    fuente_formato = "API_IMF",
    
    id_original = codigo_original,
    
    ultima_actualizacion =
      paste0(
        hoy,
        "T12:00:00Z"
      ),
    
    fecha_inicio = df_obs$fecha[1],
    
    url_original =
      "https://data.imf.org/en/Data-Explorer?datasetUrn=IMF.RES:CTOT(5.0.1)",
    
    revisable = TRUE,
    
    notas = list()
  )
  
  
  # ========================================================
  # 5. SI EL JSON NO EXISTE
  # ========================================================
  
  if (!file.exists(path_archivo)) {
    
    
    message(
      "  > Archivo no existe. Creando serie desde cero..."
    )
    
    
    df_obs$realtime_start <- hoy
    
    df_obs$realtime_end <- "9999-12-31"
    
    
    lista_final <- list(
      
      serie_id = serie_id,
      
      metadatos = metadatos,
      
      observaciones = df_obs
    )
    
    
  } else {
    
    
    # ======================================================
    # 6. LEER JSON EXISTENTE
    # ======================================================
    
    message(
      "  > Archivo existente. Actualizando vintages..."
    )
    
    
    base_actual <- fromJSON(
      path_archivo,
      simplifyVector = TRUE
    )
    
    
    # ======================================================
    # 7. OBSERVACIONES VIGENTES
    # ======================================================
    
    obs_vigentes <- base_actual$observaciones[
      base_actual$observaciones$realtime_end ==
        "9999-12-31",
    ]
    
    
    # ======================================================
    # 8. OBSERVACIONES HISTÓRICAS
    # ======================================================
    
    obs_historicas <- base_actual$observaciones[
      base_actual$observaciones$realtime_end !=
        "9999-12-31",
    ]
    
    
    # ======================================================
    # 9. COMPARAR NUEVO VS. VIGENTE
    # ======================================================
    
    actualizadas <- merge(
      
      df_obs,
      
      obs_vigentes,
      
      by = "fecha",
      
      all.x = TRUE,
      
      suffixes = c(
        "_nuevo",
        "_viejo"
      )
    )
    
    
    # ======================================================
    # 10. CLASIFICAR OBSERVACIONES
    # ======================================================
    
    actualizadas$status <- ifelse(
      
      is.na(
        actualizadas$valor_viejo
      ),
      
      "NUEVO",
      
      ifelse(
        
        round(
          actualizadas$valor_nuevo,
          4
        ) !=
          round(
            actualizadas$valor_viejo,
            4
          ),
        
        "REVISADO",
        
        "SIN_CAMBIOS"
      )
    )
    
    
    # ======================================================
    # 11. FECHAS REVISADAS
    # ======================================================
    
    fechas_rev <- actualizadas$fecha[
      actualizadas$status ==
        "REVISADO"
    ]
    
    
    # ======================================================
    # 12. CERRAR VINTAGE DE DATOS REVISADOS
    # ======================================================
    
    obs_vigentes_que_cambiaron <-
      obs_vigentes[
        obs_vigentes$fecha %in%
          fechas_rev,
      ]
    
    
    if (
      nrow(
        obs_vigentes_que_cambiaron
      ) > 0
    ) {
      
      obs_vigentes_que_cambiaron$realtime_end <-
        hoy
    }
    
    
    # ======================================================
    # 13. CONSERVAR DATOS SIN CAMBIOS
    # ======================================================
    
    obs_vigentes_sin_cambio <-
      obs_vigentes[
        !(
          obs_vigentes$fecha %in%
            fechas_rev
        ),
      ]
    
    
    # ======================================================
    # 14. INCORPORAR NUEVOS + REVISADOS
    # ======================================================
    
    nuevas_ins <- actualizadas[
      actualizadas$status %in%
        c(
          "NUEVO",
          "REVISADO"
        ),
    ]
    
    
    if (
      nrow(nuevas_ins) > 0
    ) {
      
      nuevas_ins <- nuevas_ins[
        ,
        c(
          "fecha",
          "valor_nuevo"
        )
      ]
      
      
      colnames(nuevas_ins) <- c(
        "fecha",
        "valor"
      )
      
      
      nuevas_ins$realtime_start <-
        hoy
      
      
      nuevas_ins$realtime_end <-
        "9999-12-31"
      
      
    } else {
      
      nuevas_ins <- data.frame(
        
        fecha = character(),
        
        valor = numeric(),
        
        realtime_start = character(),
        
        realtime_end = character(),
        
        stringsAsFactors = FALSE
      )
    }
    
    
    # ======================================================
    # 15. CONSOLIDAR
    # ======================================================
    
    obs_consolidadas <- rbind(
      
      obs_historicas,
      
      obs_vigentes_que_cambiaron,
      
      obs_vigentes_sin_cambio,
      
      nuevas_ins
    )
    
    
    # ======================================================
    # 16. ACTUALIZAR METADATOS
    # ======================================================
    
    base_actual$metadatos$ultima_actualizacion <-
      paste0(
        hoy,
        "T12:00:00Z"
      )
    
    
    # ======================================================
    # 17. ORDENAR
    # ======================================================
    
    base_actual$observaciones <-
      obs_consolidadas[
        order(
          obs_consolidadas$fecha,
          obs_consolidadas$realtime_start
        ),
      ]
    
    
    lista_final <- base_actual
  }
  
  
  # ========================================================
  # 18. GUARDAR JSON
  # ========================================================
  
  write_json(
    lista_final,
    path_archivo,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  
  message(
    "  > Serie guardada con éxito: ",
    path_archivo
  )
  
  
  # ========================================================
  # 19. ACTUALIZAR CATÁLOGO
  # ========================================================
  
  update_catalogo(
    
    serie_id = serie_id,
    
    metadatos = metadatos,
    
    metodo_etl = "API_IMF",
    
    tema = tema_destino
  )
  
  
  return(TRUE)
}


# ============================================================
# 20. EJECUCIÓN PRINCIPAL
# ============================================================

message("")
message(
  "=================================================================="
)
message(
  "DESCARGA CTOT - IMF SDMX 3.0"
)
message(
  "=================================================================="
)


# ------------------------------------------------------------
# Descargar
# ------------------------------------------------------------

raw_data <- fetch_imf_sdmx30_batch()


# ------------------------------------------------------------
# Verificar
# ------------------------------------------------------------

if (is.null(raw_data)) {
  
  message("")
  message(
    "NO SE PUDIERON DESCARGAR LOS DATOS."
  )
  
  
} else if (
  nrow(raw_data) == 0
) {
  
  message("")
  message(
    "LA API RESPONDIÓ, PERO NO DEVOLVIÓ OBSERVACIONES."
  )
  
  
} else {
  
  
  # ----------------------------------------------------------
  # Procesar país por país
  # ----------------------------------------------------------
  
  paises_encontrados <-
    unique(
      raw_data$pais_iso3
    )
  
  
  message("")
  message(
    "Países a procesar: ",
    length(paises_encontrados)
  )
  
  
  for (
    pais in paises_encontrados
  ) {
    
    
    message("")
    message(
      "----------------------------------------------------------"
    )
    
    message(
      "Procesando: ",
      pais
    )
    
    message(
      "----------------------------------------------------------"
    )
    
    
    df_pais <- raw_data %>%
      
      filter(
        pais_iso3 == pais
      ) %>%
      
      select(
        fecha,
        valor
      )
    
    
    tryCatch(
      
      {
        
        update_imf_json_serie(
          pais,
          df_pais
        )
        
      },
      
      error = function(e) {
        
        message(
          "  [ERROR en ",
          pais,
          "]: ",
          e$message
        )
      }
    )
  }
  
  
  message("")
  message(
    "=================================================================="
  )
  
  message(
    "DESCARGA FINALIZADA"
  )
  
  message(
    "=================================================================="
  )
}

