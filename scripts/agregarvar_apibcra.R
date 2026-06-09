library(jsonlite)
library(dplyr)
library(httr)
library(lubridate)

# 1. Definimos los metadatos fijos para la nueva variable
meta_reservas <- list(
  titulo = "Reservas Internacionales del BCRA",
  descripcion = "Stock bruto de reservas internacionales del Banco Central de la República Argentina.",
  frecuencia = "D",
  unidades = "Millones de dólares", 
  tipo_informacion = "Pública",
  fuente = "BCRA",
  id_api_bcra = 1
)

# 2. Definimos el nombre oficial de la serie
serie_id_reservas <- "SECEXTERNO_RESERVASBRUTAS_NOMINAL_NSA_D"

#' Actualiza o inicializa una serie JSON aplicando la lógica ALFRED de revisiones
update_bcra_json_serie <- function(id_variable, serie_id, tema, metadatos_fijos) {
  path_dir <- file.path(tema)
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  hoy <- as.character(Sys.Date())
  
  if (!dir.exists(path_dir)) dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Si el archivo no existe, bajamos desde 2002 (fecha de inicio habitual para TCR).
  # Si existe, bajamos los últimos 6 meses para capturar datos nuevos y posibles revisiones recientes.
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
  
  # Estandarizar nombres del df_bcra a nuestra estructura JSON
  nuevo_df <- df_bcra %>%
    select(fecha = Fecha, valor = Valor) %>%
    mutate(fecha = as.character(fecha)) %>%
    arrange(fecha)
  
  # --- LÓGICA DE ACTUALIZACIÓN ALFRED ---
  if (!file.exists(path_archivo)) {
    # CASO A: Creación desde cero
    observaciones <- nuevo_df %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    lista_final <- list(
      serie_id = serie_id,
      metadatos = metadatos_fijos,
      observaciones = observaciones
    )
  } else {
    # CASO B: Actualización (Cruzamos lo vigente con la nueva descarga)
    base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
    obs_viejas <- base_actual$observaciones
    
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    actualizadas <- nuevo_df %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO", # Redondeo de seguridad por los decimales del BCRA
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    # Cerramos el ciclo de vida de los datos que el BCRA revisó/modificó
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    # Mantenemos los que no cambiaron
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    # Preparamos las novedades (Nuevos datos + Nuevas versiones de los revisados)
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor = valor_nuevo) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    # Consolidamos toda la historia
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
update_catalogo <- function(serie_id, metadatos) {
  cat_path <- "catalogo.json"
  base_url <- "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/"
  
  # Deducir la carpeta (Tema) a partir del prefijo del serie_id
  tema <- strsplit(serie_id, "_")[[1]][1]
  url_generada <- paste0(base_url, tema, "/", serie_id, ".json")
  
  nueva_fila <- data.frame(
    serie_id = serie_id,
    titulo = metadatos$titulo,
    frecuencia = metadatos$frecuencia,
    tipo_informacion = metadatos$tipo_informacion,
    raw_url = url_generada
  )
  
  if (file.exists(cat_path)) {
    cat_actual <- fromJSON(cat_path)
    cat_actual <- cat_actual[cat_actual$serie_id != serie_id, ] # Remover duplicado si existe
    
    # Usamos bind_rows en lugar de rbind para evitar errores si las columnas difieren
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

BASE_URL_MONETARIAS <- "https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias/"


# 3. Llamamos a la función maestra (nota que el tema ahora es "SECEXTERNO")
exito_reservas <- update_bcra_json_serie(
  id_variable = 1,
  serie_id = serie_id_reservas,
  tema = "SECEXTERNO",
  metadatos_fijos = meta_reservas
)

# 4. Si la descarga fue exitosa, la sumamos al Catálogo Maestro
if (exito_reservas) {
  update_catalogo(serie_id_reservas, meta_reservas)
  message("¡Serie de Reservas Internacionales agregada con éxito a la base de datos!")
}