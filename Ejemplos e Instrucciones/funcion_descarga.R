library(httr)
library(jsonlite)
library(dplyr)

descargar_serie <- function(serie, 
                            origen = c("publica", "series_propias", "transformaciones", "agregados"), 
                            token = NULL,
                            tiempo_espera = 15) {
  
  origen <- match.arg(origen)
  
  # Mapeo a los endpoints de contenidos de la API de GitHub
  repos_base <- c(
    publica          = "https://api.github.com/repos/BaseMyP/Publica/contents/",
    series_propias   = "https://api.github.com/repos/BaseMyP/Series_Propias/contents/",
    transformaciones = "https://api.github.com/repos/BaseMyP/Transformaciones/contents/",
    agregados        = "https://api.github.com/repos/BaseMyP/Agregados/contents/"
  )
  
  # Asegura el encodeo adecuado por si la ruta de la serie incluye subdirectorios o espacios
  ruta_archivo <- URLencode(paste0(serie, ".json"))
  url_completa <- paste0(repos_base[origen], ruta_archivo, "?ref=main")
  
  # Headers obligatorios para la API de GitHub
  headers_lista <- list(
    Accept       = "application/vnd.github.raw",
    `User-Agent` = "R-script-BaseMyP",
    Connection   = "close"
  )
  
  # Header de autorización si se provee token
  if (!is.null(token) && nzchar(token)) {
    headers_lista$Authorization <- paste("Bearer", token)
  }
  
  # Petición HTTP
  respuesta <- GET(
    url = url_completa, 
    do.call(add_headers, headers_lista),
    timeout(tiempo_espera)
  )
  
  # Validar status HTTP
  if (http_error(respuesta)) {
    stop(sprintf(
      "Error al descargar '%s' desde '%s' [Status %s]: Verifique el nombre de la serie o el token ingresado.",
      serie, origen, status_code(respuesta)
    ), call. = FALSE)
  }
  
  datos_json <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))
  nombre <- sub(".*/", "", serie)
  
  df <- datos_json$observaciones %>%
    filter(realtime_end == "9999-12-31") %>%
    select(fecha, valor) %>%
    mutate(fecha = as.Date(fecha)) %>%
    rename(!!nombre := valor)
  
  return(df)
}

# Ejemplos de uso
# prueba <- descargar_serie(serie="PRESTAMOS/enUSD_SECTORPRIV_NSA_D",
#                           origen="publica")
# 
# prueba2 <- descargar_serie(serie="DEUDA_PUBLICA/VENCIMIENTOS_BOPREAL_CAPITAL_NSA_M",
#                           origen="series_propias",
#                           token=mi_token)

