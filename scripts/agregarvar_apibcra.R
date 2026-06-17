library(jsonlite)
library(dplyr)
library(httr)
library(lubridate)

source("scripts/funciones_base.R")
       
# 1. Definimos los metadatos fijos para la nueva variable
meta_reservas <- list(
  titulo = "Reservas internacionales",
  descripcion = "Reservas internacionales Brutas del BCRA",
  pais = "Argentina",
  categoria = "SECTOR_EXTERNO",
  frecuencia_short = "D",
  frecuencia_original = "diaria",
  unidades = "Millones de USD", 
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "BCRA",
  fuente_original = "BCRA",
  fuente_formato = "API_BCRA",
  ultima_actualizacion = Sys.Date(),
  fecha_inicio = as.Date("2003-01-01"),
  url_original = "https://www.bcra.gob.ar/principales-variables-datos/?serie=246",
  revisable = FALSE,
  notas = "id_api_bcra = 1" # Cambiar en la fila 38
)


# 2. Definimos el nombre oficial de la serie
serie_id_reservas <- "RESERVASBRUTAS_NOMINAL_NSA_D"
 
BASE_URL_MONETARIAS <- "https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias/"


# 3. Llamamos a la función maestra (nota que el tema ahora es "SECEXTERNO")
exito_reservas <- update_bcra_json_serie(
  id_variable = 1,
  serie_id = serie_id_reservas,
  tema = meta_reservas$categoria,
  metadatos_fijos = meta_reservas
)

# 4. Si la descarga fue exitosa, la sumamos al Catálogo Maestro
if (exito_reservas) {
  update_catalogo(
    serie_id = serie_id_reservas, 
    metadatos = meta_reservas,
    metodo_etl = "API_BCRA",        # <--- CAMBIO: Agregamos el método
    tema = meta_reservas$categoria         # <--- CAMBIO: Agregamos la nueva carpeta explícita
  )
  message("¡Serie de Reservas Internacionales agregada con éxito a la base de datos!")
}
