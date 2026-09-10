library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)

# INSTRUCCIONES PARA ACCEDER A LOS 3 REPOSITORIOS: -----

# PUBLICA: repositorio con variables disponibles públicamente (no requiere token)
# TRANSFORMACIONES: respositorio privado con transformaciones de series públicas (requiere token)
# SERIES_PROPIAS: repositorio privado con series de elaboración propia (requiere token)


# TOKENS -----

# Todos los usuarios vamos a usar los mismos tokens de acceso (son de sólo lectura)
# No subir a la web porque pueden ser bloqueados
# No distribuir fuera de nuestra gerencia
# Si se quiere dar acceso a alguien externo, le podemos gestionar un token diferente.

tk_transformaciones <- ""
tk_series_propias <- ""



# CATÁLOGO DE VARIABLES -----

# Lista todas las variables disponibles en cada repositorio

cat_publico <- jsonlite::fromJSON(
  httr::content(
    httr::GET(
      "https://api.github.com/repos/BaseMyP/Publica/contents/catalogo.json?ref=main",
      httr::add_headers(Accept = "application/vnd.github.raw", `User-Agent` = "R-script")
    ),
    as = "text",
    encoding = "UTF-8"
  ),
  simplifyDataFrame = TRUE
) %>% 
  mutate(Carpeta = basename(dirname(raw_url)),.before = 1)

cat_transformaciones <- fromJSON(content(GET(
  "https://api.github.com/repos/BaseMyP/Transformaciones/contents/catalogo.json?ref=main",
  add_headers(Authorization = paste("Bearer", tk_transformaciones),
              Accept = "application/vnd.github.raw")), as = "text", encoding = "UTF-8")) %>% 
  mutate(Carpeta = basename(dirname(raw_url)),.before = 1)

cat_series_propias <- fromJSON(content(GET(
  "https://api.github.com/repos/BaseMyP/Series_Propias/contents/catalogo.json?ref=main",
  add_headers(Authorization = paste("Bearer", tk_series_propias),
              Accept = "application/vnd.github.raw")), as = "text", encoding = "UTF-8")) %>% 
  mutate(Carpeta = basename(dirname(raw_url)),.before = 1)


# Descarga de series ------

# Se recomienda descargar la siguiente función:
source(
  exprs = parse(
    text = httr::content(
      httr::GET(
        "https://api.github.com/repos/BaseMyP/Publica/contents/Ejemplos%20e%20Instrucciones/funcion_descarga.R?ref=main",
        httr::add_headers(Accept = "application/vnd.github.raw", `User-Agent` = "R-script")
      ),
      as = "text",
      encoding = "UTF-8"
    )
  )
)


# Es necesario identificar en el catálogo la CARPETA y el serie_id
# El argumento "serie" de la función se arma de la siguiente manera: "CARPETA/serie_id"

# El argumento "origen" indica el repositorio de donde se está descartando.
# Valores posibles: "publica", "transformaciones", "series_propias"

# No es necesario completar el argumento token para el repositorio "publica"
# Para los otros dos repositorios, ingresar según corresponda tk_transformaciones o tk_series_propias


# Ejemplos

Reservas <- descargar_serie(serie="SECTOR_EXTERNO/RESERVASBRUTAS_NOMINAL_NSA_D",
                          origen="publica")

PIB_mensualizado_SA <- descargar_serie(serie="ACTIVIDAD/CN_PBI_SA_M",
                           origen="transformaciones",
                           token=tk_transformaciones)

DeudaNetaReal_base92 <- descargar_serie(serie="DEUDA_PUBLICA/DEUDANETA_REAL_DIC1992_NSA_M",
                          origen="series_propias",
                          token=tk_series_propias)