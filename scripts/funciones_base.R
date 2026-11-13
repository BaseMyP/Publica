
#' Motor base de descarga paginada para la API v1 del INDEC (PostgREST)
get_indec_api <- function(endpoint,
                          filtros = list(),
                          select = NULL,
                          order = NULL,
                          page_size = 1000,
                          token = Sys.getenv("INDEC_API_TOKEN")) {
  
  # Limpiamos posibles espacios o comillas que hayan quedado en .Renviron
  token <- str_trim(token)
  token <- str_remove_all(token, '^["\']|["\']$')
  
  if (is.null(token) || token == "") {
    stop("No se encontró el token de la API del INDEC. Definí INDEC_API_TOKEN en .Renviron.")
  }
  
  # Si el usuario guardó "Bearer eyJ...", extraemos solo el token para req_auth_bearer_token
  token_limpio <- str_remove(token, regex("^Bearer\\s+", ignore_case = TRUE))
  
  base_url <- paste0("https://api.indec.gob.ar/v1/", endpoint)
  
  req_base <- request(base_url) |>
    req_auth_bearer_token(token_limpio) |>
    req_headers(
      "Accept" = "application/json",
      "Prefer" = "count=exact"
    ) |>
    req_retry(max_tries = 3, backoff = ~ 2)
  
  query_params <- filtros
  if (!is.null(select)) query_params$select <- select
  if (!is.null(order))  query_params$order  <- order
  
  if (length(query_params) > 0) {
    req_base <- req_url_query(req_base, !!!query_params)
  }
  
  offset <- 0
  resultados <- list()
  iter <- 1
  
  repeat {
    rango <- sprintf("%d-%d", offset, offset + page_size - 1)
    
    resp <- req_base |>
      req_headers("Range-Unit" = "items", "Range" = rango) |>
      req_perform()
    
    chunk <- resp_body_json(resp, simplifyVector = TRUE)
    if (length(chunk) == 0 || nrow(as.data.frame(chunk)) == 0) break
    
    resultados[[iter]] <- as_tibble(chunk)
    
    content_range <- resp_header(resp, "content-range")
    if (is.null(content_range)) break
    
    partes <- str_match(content_range, "(\\d+)-(\\d+)/(\\d+|\\*)")
    fin_actual <- as.numeric(partes[1, 3])
    total_rows <- suppressWarnings(as.numeric(partes[1, 4]))
    
    if (!is.na(total_rows) && (fin_actual + 1) >= total_rows) break
    if (nrow(resultados[[iter]]) < page_size) break
    
    offset <- offset + page_size
    iter <- iter + 1
  }
  
  bind_rows(resultados)
}

#' Descarga series limpias de ipc_nacional desde la API del INDEC
get_ipc_indec <- function(region = "Nacional",
                          nivel = NULL,
                          tipo_indicador = "Indice",
                          desde_periodo = NULL,
                          filtros_extra = list(),
                          token = Sys.getenv("INDEC_API_TOKEN")) {
  
  filtros <- filtros_extra
  
  if (!is.null(region)) {
    filtros$region <- if (length(region) > 1) {
      paste0("in.(", paste(region, collapse = ","), ")")
    } else {
      paste0("eq.", region)
    }
  }
  
  if (!is.null(nivel)) {
    filtros$nivel <- if (length(nivel) > 1) {
      paste0("in.(", paste(nivel, collapse = ","), ")")
    } else {
      paste0("eq.", nivel)
    }
  }
  
  if (!is.null(tipo_indicador)) {
    filtros$tipo_de_indicador <- if (length(tipo_indicador) > 1) {
      paste0("in.(", paste(tipo_indicador, collapse = ","), ")")
    } else {
      paste0("eq.", tipo_indicador)
    }
  }
  
  if (!is.null(desde_periodo)) {
    filtros$periodo <- paste0("gte.", desde_periodo)
  }
  
  clave_primaria <- "periodo,region,clasificador,nivel,codigo,indicador,tipo_de_indicador"
  
  df <- get_indec_api(
    endpoint = "ipc_nacional",
    filtros  = filtros,
    order    = clave_primaria,
    token    = token
  )
  
  if (nrow(df) == 0) return(df)
  
  df |>
    mutate(
      fecha = ym(periodo),
      .before = 1
    )
}

#' Actualiza o inicializa una serie desde la Nueva API del INDEC con lógica ALFRED
update_nuevaindec_json_serie <- function(serie_id, tema, metadatos_fijos, params_api = NULL) {
  path_dir <- file.path(tema)
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  hoy <- as.character(Sys.Date())
  
  if (!dir.exists(path_dir)) dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Si no se pasan explícitamente, los toma de metadatos_fijos$parametros_api
  if (is.null(params_api)) {
    params_api <- metadatos_fijos$parametros_api
  }
  
  if (is.null(params_api)) {
    warning("No se definieron 'parametros_api' para la serie: ", serie_id)
    return(FALSE)
  }
  
  message("Descargando datos desde Nueva API INDEC para ", serie_id, "...")
  
  df_crudo <- tryCatch({
    get_ipc_indec(
      region         = params_api$region,
      nivel          = params_api$nivel,
      tipo_indicador = params_api$tipo_indicador,
      filtros_extra  = if (!is.null(params_api$filtros_extra)) as.list(params_api$filtros_extra) else list()
    )
  }, error = function(e) {
    warning("Error al consultar la Nueva API del INDEC para ", serie_id, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(df_crudo) || nrow(df_crudo) == 0) {
    warning("No se obtuvieron registros para ", serie_id)
    return(FALSE)
  }
  
  # Nos quedamos únicamente con $fecha y $valor
  nuevo_df <- df_crudo |>
    select(fecha, valor) |>
    mutate(
      fecha = as.character(fecha),
      valor = as.numeric(valor)
    ) |>
    filter(!is.na(valor)) |>
    arrange(fecha)
  
  if (!file.exists(path_archivo)) {
    observaciones <- nuevo_df |>
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    lista_final <- list(
      serie_id      = serie_id,
      metadatos     = metadatos_fijos,
      observaciones = observaciones
    )
  } else {
    base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
    obs_viejas  <- base_actual$observaciones
    
    obs_vigentes   <- obs_viejas |> filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas |> filter(realtime_end != "9999-12-31")
    
    actualizadas <- nuevo_df |>
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) |>
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    if (!any(actualizadas$status %in% c("NUEVO", "REVISADO"))) {
      message("  -> Sin cambios detectados para ", serie_id)
      return(FALSE)
    }
    
    obs_vigentes_que_cambiaron <- obs_vigentes |>
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) |>
      mutate(realtime_end = hoy)
    
    obs_vigentes_sin_cambio <- obs_vigentes |>
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    nuevas_inserciones <- actualizadas |>
      filter(status %in% c("NUEVO", "REVISADO")) |>
      select(fecha, valor = valor_nuevo) |>
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    obs_consolidadas <- bind_rows(
      obs_historicas,
      obs_vigentes_que_cambiaron,
      obs_vigentes_sin_cambio,
      nuevas_inserciones
    ) |> arrange(fecha, realtime_start)
    
    base_actual$metadatos <- metadatos_fijos
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    lista_final <- base_actual
  }
  
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  message("✓ Serie guardada: ", path_archivo)
  return(TRUE)
}


update_bcra_json_serie <- function(id_variable, serie_id, tema, metadatos_fijos) {
  path_dir <- file.path(tema)
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  hoy <- as.character(Sys.Date())
  
  if (!dir.exists(path_dir)) dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(path_archivo)) {
    message("Inicializando serie ", serie_id, " por primera vez...")
    df_bcra <- fetch_bcra_series(id_variable, from_date_overall = "2002-01-01", to_date_overall = Sys.Date())
  } else {
    message("Actualizando serie ", serie_id, "...")
    fecha_desde <- as.character(Sys.Date() - 180) 
    df_bcra <- fetch_bcra_series(id_variable, from_date_overall = fecha_desde, to_date_overall = Sys.Date())
  }
  
  if (is.null(df_bcra) || nrow(df_bcra) == 0) {
    warning("No se obtuvieron datos de la API para ID: ", id_variable)
    return(FALSE)
  }
  
  nuevo_df <- df_bcra %>%
    select(fecha = Fecha, valor = Valor) %>%
    mutate(fecha = as.character(fecha)) %>%
    arrange(fecha)
  
  if (!file.exists(path_archivo)) {
    observaciones <- nuevo_df %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    lista_final <- list(
      serie_id = serie_id,
      metadatos = metadatos_fijos,
      observaciones = observaciones
    )
  } else {
    base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
    obs_viejas <- base_actual$observaciones
    
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    actualizadas <- nuevo_df %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO", 
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor = valor_nuevo) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    obs_consolidadas <- bind_rows(
      obs_historicas,
      obs_vigentes_que_cambiaron,
      obs_vigentes_sin_cambio,
      nuevas_inserciones
    ) %>% arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    lista_final <- base_actual
  }
  
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  message("✓ Serie guardada: ", path_archivo)
  return(TRUE)
}

#' Actualiza el catálogo maestro agregando la serie (o pisándola si ya existe)
#' Actualiza el catálogo maestro agregando la serie con su Raw URL
update_catalogo <- function(serie_id, metadatos, metodo_etl, tema) {
  cat_path <- "catalogo.json"
  base_url <- "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/"
  
  # CORRECCIÓN: El tema ahora se pasa de forma explícita para armar la URL del nuevo árbol
  url_generada <- paste0(base_url, tema, "/", serie_id, ".json")
  
  nueva_fila <- data.frame(
    serie_id = serie_id,
    titulo = metadatos$titulo,
    frecuencia = metadatos$frecuencia_short,
    tipo_informacion = metadatos$tipo_informacion,
    raw_url = url_generada,
    metodo_etl = metodo_etl,
    stringsAsFactors = FALSE
  )
  
  if (file.exists(cat_path)) {
    cat_actual <- fromJSON(cat_path)
    cat_actual <- cat_actual[cat_actual$serie_id != serie_id, ] 
    cat_final <- dplyr::bind_rows(cat_actual, nueva_fila) 
  } else {
    cat_final <- nueva_fila
  }
  
  write_json(cat_final, cat_path, pretty = TRUE)
  message("✓ Catálogo actualizado.")
}

fetch_bcra_series <- function(id_variable, from_date_overall = NULL, to_date_overall = NULL) {
  # Validar y convertir fechas
  from_date_overall <- as.Date(from_date_overall)
  to_date_overall <- as.Date(to_date_overall)
  
  if (is.na(from_date_overall) || is.na(to_date_overall) || from_date_overall > to_date_overall) {
    message("  > Rango de fechas inválido proporcionado para IdVariable: ", id_variable)
    return(NULL)
  }
  
  message(paste("  > Iniciando descarga paginada para IdVariable:", id_variable, "desde", from_date_overall, "hasta", to_date_overall))
  
  all_chunks_data <- list()
  current.fetch_to_date <- to_date_overall # Empezamos a buscar desde la fecha final global
  
  repeat {
    message(paste("    Buscando chunk: desde", from_date_overall, "hasta", current.fetch_to_date))
    
    # Llamamos a la función de bajo nivel para obtener un chunk de datos.
    # La API del BCRA devolverá un máximo de 1000 observaciones, que serán las más recientes
    # dentro del rango [from_date_overall, current.fetch_to_date].
    chunk_data <- .fetch_single_chunk_bcra_series(id_variable, from_date = from_date_overall, to_date = current.fetch_to_date)
    
    if (is.null(chunk_data) || nrow(chunk_data) == 0) {
      message("    No se encontraron más datos para este rango o la llamada a la API falló. Deteniendo descarga paginada.")
      break # No hay más datos o error, salimos del bucle
    }
    
    # Añadir el chunk actual a la lista
    all_chunks_data[[length(all_chunks_data) + 1]] <- chunk_data
    
    # Encontramos la fecha más antigua en el chunk recién descargado.
    # Esto es CRUCIAL para saber desde dónde buscar el siguiente chunk (hacia atrás en el tiempo).
    earliest_date_in_current_chunk <- min(chunk_data$Fecha, na.rm = TRUE)
    
    message(paste("    Chunk recibido:", nrow(chunk_data), "observaciones. Fecha más antigua en este chunk:", earliest_date_in_current_chunk))
    
    # Condición de parada: Si la fecha más antigua del chunk es igual o anterior
    # a la fecha de inicio global solicitada, ya hemos cubierto todo el rango.
    if (earliest_date_in_current_chunk <= from_date_overall) {
      message(paste("    La fecha más antigua del chunk (", earliest_date_in_current_chunk, ") alcanzó o superó la fecha de inicio global (", from_date_overall, "). Deteniendo descarga paginada.", sep=""))
      break
    }
    
    # Si el chunk contiene menos de 1000 filas y la fecha más antigua del chunk es
    # aún posterior a la fecha de inicio global, esto puede indicar el verdadero
    # inicio de la serie (si es más tarde que from_date_overall) o que se alcanzó el final de los datos.
    # Por si acaso, también podemos detener si el número de filas es pequeño y la fecha no llega a from_date_overall.
    # Sin embargo, la condición de arriba (earliest_date_in_current_chunk <= from_date_overall) es más robusta.
    # Si el número de filas es 1000, es muy probable que haya más datos anteriores.
    
    # Para la próxima iteración, la nueva fecha "hasta" será un día antes
    # de la fecha más antigua del chunk actual.
    current.fetch_to_date <- earliest_date_in_current_chunk - days(1)
  }
  
  # Si se descargaron chunks, los combinamos y limpiamos
  if (length(all_chunks_data) > 0) {
    combined_df <- bind_rows(all_chunks_data) %>%
      distinct(IdVariable, Fecha, .keep_all = TRUE) %>% # Eliminar duplicados si los hubiera por solapamiento
      filter(Fecha >= from_date_overall & Fecha <= to_date_overall) %>% # Asegurar que las fechas estén estrictamente dentro del rango global solicitado
      arrange(IdVariable, Fecha)
    
    message(paste("  > Descarga paginada completa para IdVariable", id_variable, ". Total de registros:", nrow(combined_df)))
    return(combined_df)
  } else {
    message(paste("  > No se pudieron descargar datos para IdVariable", id_variable, "en el rango", from_date_overall, "a", to_date_overall))
    return(NULL)
  }
}

.fetch_single_chunk_bcra_series <- function(id_variable, from_date = NULL, to_date = NULL) {
  
  # Si el ID es texto (EUR, CNH), usamos la API cambiaria. Si es numérico, la monetaria.
  # Podrías agregar más divisas si fuera necesario, como "USD"
  if (as.character(id_variable) %in% c("EUR", "CNH", "USD","AUD","CAD","CHF","GBP","JPY","SEK","XDR","DKK")) { 
    base_url <- BASE_URL_CAMBIARIAS
    is_cambiaria <- TRUE
  } else {
    base_url <- BASE_URL_MONETARIAS
    is_cambiaria <- FALSE
  }
  
  # Construye la URL base para la variable
  url <- paste0(base_url, id_variable)
  
  # Añade los parámetros de fecha
  query_params <- list()
  
  # Nota: La API Cambiaria usa 'fechadesde'/'fechahasta', la Monetaria usa 'Desde'/'Hasta'
  if (!is.null(from_date)) {
    date_str <- format(as.Date(from_date), "%Y-%m-%d")
    if(is_cambiaria) {
      query_params$fechadesde <- date_str
    } else {
      query_params$Desde <- date_str
    }
  }
  
  if (!is.null(to_date)) {
    date_str <- format(as.Date(to_date), "%Y-%m-%d")
    if(is_cambiaria) {
      query_params$fechahasta <- date_str
    } else {
      query_params$Hasta <- date_str
    }
  }
  
  if (length(query_params) > 0) {
    url <- modify_url(url, query = query_params)
  }
  
  # message(paste("    -> API Call URL:", url)) # Para depuración
  
  tryCatch({
    response <- GET(url, config(ssl_verifypeer = 0))
    stop_for_status(response, paste("BCRA API call failed for IdVariable", id_variable))
    
    content_text <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(content_text, flatten = TRUE)
    
    df <- NULL # Inicializa el dataframe resultante
    
    # --- Lógica de extracción y combinación de datos según el tipo de API ---
    if (is_cambiaria) {
      # Para la API Cambiaria, fechas y valores vienen separados en json_data$results
      if (!is.null(json_data$results) && !is.null(json_data$results$fecha) && !is.null(json_data$results$detalle)) {
        fechas_crude <- unlist(json_data$results$fecha) # Extrae las fechas
        
        # Extrae 'tipoCotizacion' de cada elemento de la lista 'detalle'
        valores_crude <- sapply(json_data$results$detalle, function(x) x$tipoCotizacion)
        
        if (length(fechas_crude) > 0 && length(valores_crude) > 0 && length(fechas_crude) == length(valores_crude)) {
          # Crea el dataframe combinando las fechas y valores
          df <- data.frame(
            IdVariable = as.character(id_variable),
            Fecha = as.Date(fechas_crude),
            Valor = as.numeric(valores_crude),
            stringsAsFactors = FALSE
          )
        } else {
          message("Warning: Datos inconsistentes (fechas/valores) para API Cambiaria IdVariable: ", id_variable)
        }
      } else {
        message("Warning: Estructura de respuesta inesperada para API Cambiaria IdVariable: ", id_variable)
      }
    } else { # Esto es para la API Monetarias
      if (length(json_data$results) > 0 && !is.null(json_data$results$detalle) && length(json_data$results$detalle[[1]]) > 0) {
        df_monetaria_raw <- json_data$results$detalle[[1]]
        
        # Normalizar nombres de columnas a minúsculas
        names(df_monetaria_raw) <- tolower(names(df_monetaria_raw))
        
        # Verificar que las columnas 'fecha' y 'valor' existan
        if (!("fecha" %in% names(df_monetaria_raw) && "valor" %in% names(df_monetaria_raw))) {
          message("Warning: Respuesta de API Monetaria para IdVariable: ", id_variable, " no contiene las columnas 'fecha' y 'valor' esperadas.")
          return(NULL)
        }
        
        # Crear el dataframe con las columnas estandarizadas
        df <- data.frame(
          IdVariable = as.character(id_variable),
          Fecha = as.Date(df_monetaria_raw$fecha),
          Valor = as.numeric(df_monetaria_raw$valor),
          stringsAsFactors = FALSE
        )
      } else {
        message("Warning: Estructura de respuesta inesperada para API Monetaria IdVariable: ", id_variable)
      }
    }
    
    # Si no se pudo crear el dataframe, retornar NULL
    if (is.null(df) || nrow(df) == 0) {
      return(NULL) 
    }
    
    # Filtrar NAs en fecha o valor por si acaso
    df <- df %>% filter(!is.na(Fecha), !is.na(Valor))
    
    return(df)
    
  }, error = function(e) {
    message(paste("    Error fetching single chunk for IdVariable", id_variable, ":", e$message))
    return(NULL)
  })
}

#' Descarga una serie desde la API de Datos Argentina (INDEC/MinEco)
fetch_indec_series <- function(id_indec) {
  url_base <- "https://apis.datos.gob.ar/series/api/series"
  
  respuesta <- GET(url_base, query = list(
    ids = id_indec,
    format = "json",
    limit = 5000 
  ))
  
  if (status_code(respuesta) != 200) {
    warning("Fallo al conectar con la API del INDEC para ID: ", id_indec)
    return(NULL)
  }
  
  texto_json <- content(respuesta, as = "text", encoding = "UTF-8")
  datos_crudos <- fromJSON(texto_json)
  
  df_indec <- as.data.frame(datos_crudos$data, stringsAsFactors = FALSE)
  if(nrow(df_indec) == 0) return(NULL)
  
  names(df_indec) <- c("fecha", "valor")
  
  df_indec <- df_indec %>%
    mutate(
      fecha = as.character(fecha),
      valor = as.numeric(valor)
    ) %>%
    filter(!is.na(valor)) %>%
    arrange(fecha)
  
  return(df_indec)
}

#' Extrae y formatea una serie desde un data.frame ya cargado o directamente desde la URL del CSV
fetch_csv_series <- function(col_nombre, df_csv = NULL, url_csv = NULL) {
  if (is.null(df_csv)) {
    if (is.null(url_csv) || url_csv == "") {
      warning("No se proporcionó ni data.frame ni URL para el CSV.")
      return(NULL)
    }
    df_csv <- tryCatch({
      read_csv(url_csv, show_col_types = FALSE)
    }, error = function(e) {
      warning("Fallo al descargar el archivo CSV desde ", url_csv, ": ", e$message)
      return(NULL)
    })
    if (is.null(df_csv)) return(NULL)
  }
  
  # Validaciones de columnas mínimas
  if (!("indice_tiempo" %in% names(df_csv))) {
    warning("El CSV no contiene la columna obligatoria 'indice_tiempo'.")
    return(NULL)
  }
  
  if (!(col_nombre %in% names(df_csv))) {
    warning(sprintf("No se encontró la columna '%s' en el CSV.", col_nombre))
    return(NULL)
  }
  
  df_limpio <- df_csv %>%
    select(fecha = indice_tiempo, valor = all_of(col_nombre)) %>%
    mutate(
      fecha = as.character(fecha),
      valor = as.numeric(valor)
    ) %>%
    filter(!is.na(valor)) %>%
    arrange(fecha)
  
  if (nrow(df_limpio) == 0) {
    warning(sprintf("La columna '%s' no tiene observaciones válidas.", col_nombre))
    return(NULL)
  }
  
  return(df_limpio)
}

# fetch_indec_series <- function(id_indec) {
#   url_base <- "https://apis.datos.gob.ar/series/api/series"
#   
#   respuesta <- GET(url_base, query = list(
#     ids = id_indec,
#     format = "json",
#     limit = 5000 
#   ))
#   
#   if (status_code(respuesta) != 200) {
#     warning("Fallo al conectar con la API del INDEC para ID: ", id_indec)
#     return(NULL)
#   }
#   
#   texto_json <- content(respuesta, as = "text", encoding = "UTF-8")
#   datos_crudos <- fromJSON(texto_json)
#   
#   df_indec <- as.data.frame(datos_crudos$data, stringsAsFactors = FALSE)
#   if(nrow(df_indec) == 0) return(NULL)
#   
#   names(df_indec) <- c("fecha", "valor")
#   
#   df_indec <- df_indec %>%
#     mutate(
#       fecha = as.character(fecha),
#       valor = as.numeric(valor)
#     ) %>%
#     filter(!is.na(valor)) %>%
#     arrange(fecha)
#   
#   return(df_indec)
# }

#' Actualiza o inicializa una serie del INDEC aplicando la lógica ALFRED
update_indec_json_serie <- function(id_indec = NULL, serie_id, tema, metadatos_fijos, df_csv = NULL) {
  path_dir <- file.path(tema)
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  hoy <- as.character(Sys.Date())
  
  if (!dir.exists(path_dir)) dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 1. Determinar fuente de datos: ¿CSV o API?
  url_csv <- metadatos_fijos$url_csv
  col_csv <- metadatos_fijos$col_csv
  
  if (!is.null(url_csv) && !is.null(col_csv) && url_csv != "" && col_csv != "") {
    message("Extrayendo datos desde CSV (Columna: ", col_csv, ") para ", serie_id, "...")
    nuevo_df <- fetch_csv_series(col_nombre = col_csv, df_csv = df_csv, url_csv = url_csv)
  } else {
    message("Descargando datos del INDEC vía API para ", serie_id, "...")
    nuevo_df <- fetch_indec_series(id_indec)
  }
  
  if (is.null(nuevo_df)) return(FALSE)
  
  # 2. Inicialización o actualización ALFRED
  if (!file.exists(path_archivo)) {
    observaciones <- nuevo_df %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    lista_final <- list(serie_id = serie_id, metadatos = metadatos_fijos, observaciones = observaciones)
  } else {
    base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
    obs_viejas <- base_actual$observaciones
    
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    actualizadas <- nuevo_df %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    # Si no hay modificaciones ni datos nuevos, no tocamos el archivo
    if (!any(actualizadas$status %in% c("NUEVO", "REVISADO"))) {
      message("  -> Sin cambios detectados para ", serie_id)
      return(FALSE)
    }
    
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor = valor_nuevo) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    obs_consolidadas <- bind_rows(
      obs_historicas, 
      obs_vigentes_que_cambiaron, 
      obs_vigentes_sin_cambio, 
      nuevas_inserciones
    ) %>% arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    lista_final <- base_actual
  }
  
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  message("✓ Serie guardada: ", path_archivo)
  return(TRUE)
}

# update_indec_json_serie <- function(id_indec, serie_id, tema, metadatos_fijos) {
#   path_dir <- file.path(tema)
#   path_archivo <- file.path(tema, paste0(serie_id, ".json"))
#   hoy <- as.character(Sys.Date())
#   
#   if (!dir.exists(path_dir)) dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
#   
#   message("Descargando datos del INDEC para ", serie_id, "...")
#   nuevo_df <- fetch_indec_series(id_indec)
#   
#   if (is.null(nuevo_df)) return(FALSE)
#   
#   if (!file.exists(path_archivo)) {
#     observaciones <- nuevo_df %>%
#       mutate(realtime_start = hoy, realtime_end = "9999-12-31")
#     
#     lista_final <- list(serie_id = serie_id, metadatos = metadatos_fijos, observaciones = observaciones)
#   } else {
#     base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
#     obs_viejas <- base_actual$observaciones
#     
#     obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
#     obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
#     
#     actualizadas <- nuevo_df %>%
#       left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
#       mutate(
#         status = case_when(
#           is.na(valor_viejo) ~ "NUEVO",
#           round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
#           TRUE ~ "SIN_CAMBIOS"
#         )
#       )
#     
#     obs_vigentes_que_cambiaron <- obs_vigentes %>%
#       filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
#       mutate(realtime_end = hoy)
#     
#     obs_vigentes_sin_cambio <- obs_vigentes %>%
#       filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
#     
#     nuevas_inserciones <- actualizadas %>%
#       filter(status %in% c("NUEVO", "REVISADO")) %>%
#       select(fecha, valor = valor_nuevo) %>%
#       mutate(realtime_start = hoy, realtime_end = "9999-12-31")
#     
#     obs_consolidadas <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, obs_vigentes_sin_cambio, nuevas_inserciones) %>% 
#       arrange(fecha, realtime_start)
#     
#     base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
#     base_actual$observaciones <- obs_consolidadas
#     lista_final <- base_actual
#   }
#   
#   write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
#   message("✓ Serie guardada: ", path_archivo)
#   return(TRUE)
# }

#' Descarga una serie desde la API de FRED
#' Descarga una serie desde la API de FRED
fetch_fred_series <- function(id_fred, api_key) {
  url <- paste0("https://api.stlouisfed.org/fred/series/observations?series_id=", id_fred, "&api_key=", api_key, "&file_type=json")
  
  respuesta <- GET(url)
  
  if (status_code(respuesta) != 200) {
    warning("Fallo al conectar con la API de FRED para ID: ", id_fred)
    return(NULL)
  }
  
  # CORRECCIÓN: Parsear el texto usando jsonlite para evitar el aplanamiento de columnas
  texto_json <- content(respuesta, "text", encoding = "UTF-8")
  datos_crudos <- fromJSON(texto_json, flatten = TRUE)
  
  df_fred <- as.data.frame(datos_crudos$observations, stringsAsFactors = FALSE)
  
  if(nrow(df_fred) == 0) return(NULL)
  
  # La FRED devuelve un punto "." cuando no hay dato. Los filtramos.
  df_fred <- df_fred %>%
    select(fecha = date, valor = value) %>%
    filter(valor != ".") %>% 
    mutate(
      fecha = as.character(fecha),
      valor = as.numeric(valor)
    ) %>%
    arrange(fecha)
  
  return(df_fred)
}

#' Actualiza o inicializa una serie de FRED aplicando la lógica ALFRED
update_fred_json_serie <- function(id_fred, serie_id, tema, metadatos_fijos, api_key) {
  path_dir <- file.path(tema)
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  hoy <- as.character(Sys.Date())
  
  if (!dir.exists(path_dir)) dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  
  message("Descargando datos de FRED para ", serie_id, "...")
  nuevo_df <- fetch_fred_series(id_fred, api_key)
  
  if (is.null(nuevo_df)) return(FALSE)
  
  # --- LÓGICA DE ACTUALIZACIÓN ALFRED ---
  if (!file.exists(path_archivo)) {
    observaciones <- nuevo_df %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    lista_final <- list(serie_id = serie_id, metadatos = metadatos_fijos, observaciones = observaciones)
  } else {
    base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
    obs_viejas <- base_actual$observaciones
    
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    actualizadas <- nuevo_df %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor = valor_nuevo) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    obs_consolidadas <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, obs_vigentes_sin_cambio, nuevas_inserciones) %>% 
      arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    lista_final <- base_actual
  }
  
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  message("✓ Serie guardada: ", path_archivo)
  return(TRUE)
}