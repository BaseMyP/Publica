library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

# Fijar zona horaria de Argentina (útil si corre en GitHub Actions, que usa UTC por defecto)
Sys.setenv(TZ = "America/Argentina/Buenos_Aires")

# -------------------------------------------------------------------------
# 1. Función para descargar el JSON completo del IAMC (una sola vez)
# -------------------------------------------------------------------------
descargar_ibi_raw <- function() {
  url_ibi <- "https://www.iamc.com.ar/d/ibi.json"
  
  resp <- request(url_ibi) |>
    req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36",
      `Referer` = "https://www.iamc.com.ar/"
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  resp |>
    resp_body_string() |>
    fromJSON(simplifyVector = TRUE)
}

# -------------------------------------------------------------------------
# 2. Función para extraer una serie puntual del objeto descargado
# -------------------------------------------------------------------------
extraer_serie_iamc <- function(raw_ibi, id_original) {
  fechas_raw <- raw_ibi$ibi$f
  valores_raw <- raw_ibi$ibi$s[[id_original]]
  
  if (is.null(valores_raw)) {
    stop(paste0("El código '", id_original, "' no existe dentro de raw_ibi$ibi$s"))
  }
  
  fechas <- if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", fechas_raw[1])) {
    as.Date(substr(fechas_raw, 1, 10))
  } else {
    as.Date(fechas_raw, format = "%d/%m/%Y")
  }
  
  tibble(
    fecha = format(fechas, "%Y-%m-%d"),
    valor_nuevo = as.numeric(valores_raw)
  ) |>
    filter(!is.na(fecha), !is.na(valor_nuevo)) |>
    arrange(fecha)
}

# -------------------------------------------------------------------------
# 3. Función de actualización vintage para un archivo JSON local
# -------------------------------------------------------------------------
actualizar_serie_vintage <- function(ruta_local, raw_ibi) {
  if (!file.exists(ruta_local)) {
    warning("No se encontró el archivo local: ", ruta_local)
    return(invisible(FALSE))
  }
  
  json_actual <- fromJSON(ruta_local, simplifyVector = TRUE)
  id_original <- json_actual$metadatos$id_original
  
  # Extraer datos frescos desde el JSON del IAMC
  df_nuevo <- extraer_serie_iamc(raw_ibi, id_original)
  
  # Separar observaciones históricas (cerradas) de las vigentes
  obs_existentes <- as_tibble(json_actual$observaciones) |>
    mutate(
      fecha = as.character(fecha),
      valor = as.numeric(valor),
      realtime_start = as.character(realtime_start),
      realtime_end = as.character(realtime_end)
    )
  
  obs_historicas <- obs_existentes |> filter(realtime_end != "9999-12-31")
  obs_vigentes   <- obs_existentes |> filter(realtime_end == "9999-12-31")
  
  hoy_str  <- format(Sys.Date(), "%Y-%m-%d")
  ayer_str <- format(Sys.Date() - 1, "%Y-%m-%d")
  
  # A) Detectar fechas completamente nuevas
  obs_nuevas <- df_nuevo |>
    anti_join(obs_vigentes, by = "fecha") |>
    transmute(
      fecha = fecha,
      valor = valor_nuevo,
      realtime_start = hoy_str,
      realtime_end = "9999-12-31"
    )
  
  # B) Detectar revisiones sobre fechas ya existentes (tolerancia numérica 1e-8)
  comparacion <- obs_vigentes |>
    inner_join(df_nuevo, by = "fecha") |>
    mutate(cambio = abs(valor - valor_nuevo) > 1e-8)
  
  fechas_revisadas <- comparacion |> filter(cambio) |> pull(fecha)
  
  # Si no hay ni datos nuevos ni revisiones, no tocamos el archivo
  if (nrow(obs_nuevas) == 0 && length(fechas_revisadas) == 0) {
    message("  [Sin cambios] ", basename(ruta_local))
    return(invisible(FALSE))
  }
  
  # Cerrar las observaciones vigentes que fueron revisadas
  obs_vigentes_sin_cambio <- obs_vigentes |>
    filter(!fecha %in% fechas_revisadas)
  
  obs_cerradas_hoy <- obs_vigentes |>
    filter(fecha %in% fechas_revisadas) |>
    # Si se revisó el mismo día en que se cargó, evitamos realtime_end < realtime_start
    filter(realtime_start != hoy_str) |>
    mutate(realtime_end = ayer_str)
  
  obs_revisadas_nuevas <- df_nuevo |>
    filter(fecha %in% fechas_revisadas) |>
    transmute(
      fecha = fecha,
      valor = valor_nuevo,
      realtime_start = hoy_str,
      realtime_end = "9999-12-31"
    )
  
  # Reconstruir tabla completa ordenada por fecha y vintage
  obs_final <- bind_rows(
    obs_historicas,
    obs_cerradas_hoy,
    obs_vigentes_sin_cambio,
    obs_revisadas_nuevas,
    obs_nuevas
  ) |>
    arrange(fecha, realtime_start)
  
  # Actualizar metadatos y guardar
  json_actual$observaciones <- obs_final
  json_actual$metadatos$ultima_actualizacion <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  json_actual$metadatos$fecha_inicio <- min(obs_final$fecha)
  
  write_json(
    json_actual,
    path = ruta_local,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA
  )
  
  message(sprintf(
    "  [Actualizado] %s (+%d nuevas, %d revisadas)",
    basename(ruta_local), nrow(obs_nuevas), length(fechas_revisadas)
  ))
  
  invisible(TRUE)
}

# -------------------------------------------------------------------------
# 4. Ejecución principal sobre catalogo.json
# -------------------------------------------------------------------------
ruta_catalogo <- "catalogo.json"
catalogo <- fromJSON(ruta_catalogo, simplifyVector = FALSE)

series_iamc <- Filter(
  function(x) identical(x$metodo_etl, "IAMC_scrap"),
  catalogo
)

if (length(series_iamc) == 0) {
  message("No se encontraron series con metodo_etl == 'IAMC_scrap' en catalogo.json.")
} else {
  message("Descargando datos desde IAMC (ibi.json)...")
  raw_ibi <- descargar_ibi_raw()
  
  for (serie in series_iamc) {
    # Extrae la ruta relativa del repositorio a partir de raw_url (ej: "FINANZAS/BONOSPESOS_IAMC_INDICE_D.json")
    ruta_local <- sub("^.*/main/", "", serie$raw_url)
    
    tryCatch(
      actualizar_serie_vintage(ruta_local, raw_ibi),
      error = function(e) {
        warning("Error actualizando ", serie$serie_id, ": ", conditionMessage(e))
      }
    )
  }
}