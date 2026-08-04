library(jsonlite)
library(dplyr)
library(httr)
library(lubridate)

source("scripts/funciones_base.R")
      
# 1. Definimos los metadatos fijos para la nueva variable
meta_reservas <- list(
  titulo = "Tipo de Cambio Pesos/DEG",
  descripcion = "Tipo de Cambio Pesos/DEG",
  pais = "Argentina",
  categoria = "TC_y_Tasas",
  frecuencia_short = "D",
  frecuencia_original = "diaria",
  unidades = "Pesos/DEG", 
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "BCRA",
  fuente_original = "BCRA",
  #fuente_formato = "API_BCRA",
  fuente_formato = "API_BCRA_Cambiarias",
  id_original = "XDR",
  ultima_actualizacion = Sys.Date(),
  fecha_inicio = as.Date("2018-05-14"),
  url_original = "https://www.bcra.gob.ar/evolucion-moneda/",
  revisable = FALSE,
  notas = NA 
)


# 2. Definimos el nombre oficial de la serie
serie_id_reservas <- "DEG_PESOSporDEG_NSA_D"
 
BASE_URL_MONETARIAS <- "https://api.bcra.gob.ar/estadisticas/v4.0/Monetarias/"
BASE_URL_CAMBIARIAS <- "https://api.bcra.gob.ar/estadisticascambiarias/v1.0/Cotizaciones/"


# 3. Llamamos a la función maestra (nota que el tema ahora es "SECEXTERNO")
exito_reservas <- update_bcra_json_serie(
  id_variable = meta_reservas$id_original,
  serie_id = serie_id_reservas,
  tema = meta_reservas$categoria,
  metadatos_fijos = meta_reservas
)

# 4. Si la descarga fue exitosa, la sumamos al Catálogo Maestro
if (exito_reservas) {
  update_catalogo(
    serie_id = serie_id_reservas, 
    metadatos = meta_reservas,
    #metodo_etl = "API_BCRA",
    metodo_etl = "API_BCRA_Cambiarias",
    tema = meta_reservas$categoria         # <--- CAMBIO: Agregamos la nueva carpeta explícita
  )
  message("¡Serie de Reservas Internacionales agregada con éxito a la base de datos!")
}
