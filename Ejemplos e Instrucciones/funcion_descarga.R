library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)

descargar_serie <- function(serie, 
                            origen = c("publica", "series_propias", "transformaciones", "agregados"), 
                            token = NULL,
                            tiempo_espera = 15) {
  
  origen <- match.arg(origen)
  
  urls_base <- c(
    publica          = "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/",
    series_propias   = "https://raw.githubusercontent.com/BaseMyP/Series_Propias/refs/heads/main/",
    transformaciones = "https://raw.githubusercontent.com/BaseMyP/Transformaciones/refs/heads/main/",
    agregados        = "https://raw.githubusercontent.com/BaseMyP/Agregados/refs/heads/main/"
  )
  
  url_completa <- paste0(urls_base[origen], serie, ".json")
  
  # Headers base: 'Connection = close' evita reutilizar sockets colgados
  headers_lista <- list(Connection = "close")
  
  if (!is.null(token) && nzchar(token)) {
    headers_lista$Authorization <- paste("token", token)
  }
  
  # timeout() evita que R quede esperando indefinidamente
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

