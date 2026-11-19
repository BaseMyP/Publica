library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)

# 1. Función de descarga
get_serie_iamc <- function(codigo_serie = "IAMCGRAP") {
  url_ibi <- "https://www.iamc.com.ar/d/ibi.json"
  
  resp <- request(url_ibi) |>
    req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36",
      `Referer` = "https://www.iamc.com.ar/"
    ) |>
    req_retry(max_tries = 3) |>
    req_perform()
  
  raw_ibi <- resp |>
    resp_body_string() |>
    fromJSON(simplifyVector = TRUE)
  
  fechas_raw <- raw_ibi$ibi$f
  valores_raw <- raw_ibi$ibi$s[[codigo_serie]]
  
  if (is.null(valores_raw)) {
    stop(paste0("El código de serie '", codigo_serie, "' no existe dentro de raw_ibi$ibi$s"))
  }
  
  fechas <- if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", fechas_raw[1])) {
    as.Date(substr(fechas_raw, 1, 10))
  } else {
    as.Date(fechas_raw, format = "%d/%m/%Y")
  }
  
  tibble(
    fecha = fechas,
    valor = as.numeric(valores_raw)
  ) |>
    filter(!is.na(fecha), !is.na(valor)) |>
    arrange(fecha)
}

# 2. Descarga y armado de observaciones en formato vintage
hoy_str <- format(Sys.Date(), "%Y-%m-%d")
ahora_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

df_serie <- get_serie_iamc("IAMCGRAP")

observaciones <- df_serie |>
  transmute(
    fecha = format(fecha, "%Y-%m-%d"),
    valor = valor,
    realtime_start = hoy_str,
    realtime_end = "9999-12-31"
  )

# 3. Armado de metadatos y estructura final del JSON
json_serie <- list(
  metadatos = list(
    titulo = "Índice de Bonos IAMC (IBIAMC)",
    descripcion = "Índice de Bonos IAMC (IBIAMC)",
    pais = "ARG",
    categoria = "FINANZAS",
    frecuencia_short = "D",
    frecuencia_long = "diaria",
    frecuencia_original = "diaria",
    unidades = "Puntos",
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "https://www.iamc.com.ar/",
    fuente_original = "Bolsas y Mercados Argentinos (BYMA)",
    fuente_formato = "json",
    id_original = "IAMCGRAP",
    ultima_actualizacion = ahora_str,
    fecha_inicio = min(observaciones$fecha),
    url_original = "https://www.iamc.com.ar/",
    revisable = FALSE,
    notas = ""
  ),
  observaciones = observaciones
)

# 4. Guardado del JSON en la carpeta FINANZAS
#dir.create("FINANZAS", showWarnings = FALSE, recursive = TRUE)
ruta_serie <- file.path("FINANZAS", "BONOSPESOS_IAMC_INDICE_D.json")

write_json(
  json_serie,
  path = ruta_serie,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA
)

# 5. Actualización de catalogo.json (preservando la estructura heterogénea del resto)
ruta_catalogo <- "catalogo.json"
catalogo <- fromJSON(ruta_catalogo, simplifyVector = FALSE)

nueva_entrada <- list(
  serie_id = "BONOSPESOS_IAMC_INDICE_D",
  titulo = "Índice de Bonos IAMC (IBIAMC)",
  frecuencia = "D",
  tipo_informacion = "Pública",
  raw_url = "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/FINANZAS/BONOSPESOS_IAMC_INDICE_D.json",
  metodo_etl = "IAMC_scrap"
)

ids_existentes <- vapply(catalogo, function(x) x$serie_id, character(1))

if (!"BONOSPESOS_IAMC_INDICE_D" %in% ids_existentes) {
  catalogo[[length(catalogo) + 1]] <- nueva_entrada
  write_json(
    catalogo,
    path = ruta_catalogo,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  message("Serie agregada correctamente a catalogo.json y guardada en ", ruta_serie)
} else {
  message("La serie ya existía en catalogo.json. Se actualizó únicamente ", ruta_serie)
}