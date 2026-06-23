library(httr)
library(jsonlite)
library(dplyr)

source("scripts/funciones_base.R")

# Reemplaza con tu llave de la FRED para correrlo localmente
FRED_API_KEY <- "" 

# 1. Definimos los metadatos
meta_cpi_us <- list(
  titulo = "Consumer Price Index for All Urban Consumers: All Items in U.S. City Average",
  descripcion = "The Consumer Price Index for All Urban Consumers: All Items (CPIAUCSL) is a price index of a basket of goods and services paid by urban consumers...",
  pais = "USA",
  categoria = "INTERNACIONAL",
  frecuencia_short = "M",
  frecuencia_long = "mensual",
  frecuencia_original = "mensual",
  unidades = "Index 1982-1984=100",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "FRED",
  fuente_original = "Bureau of Labor Statistics",
  fuente_formato = "API",
  id_original = "CPIAUCNS", # <-- Usamos este campo como ID
  ultima_actualizacion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  fecha_inicio = "1947-01-01",
  url_original = "https://fred.stlouisfed.org/series/CPIAUCNS",
  revisable = TRUE,
  notas = NULL
)

serie_id_cpi <- "CPIAUCNS_INDICE_NSA_M"
tema_fijo <- "INTERNACIONAL"

message("Iniciando carga inicial desde FRED...")

exito <- update_fred_json_serie(
  id_fred = meta_cpi_us$id_original,
  serie_id = serie_id_cpi,
  tema = tema_fijo,
  metadatos_fijos = meta_cpi_us,
  api_key = FRED_API_KEY
)

if (exito) {
  # Actualizamos el catálogo con el método ETL correcto
  update_catalogo(
    serie_id = serie_id_cpi, 
    metadatos = meta_cpi_us, 
    metodo_etl = "API_FRED",
    tema = tema_fijo
  )
  message("¡Carga finalizada! Revisa tu carpeta INTERNACIONAL/ y tu catalogo.json.")
}