library(httr)
library(jsonlite)
library(dplyr)
source("scripts/funciones_base.R")

# 1. Definimos los metadatos 
meta_ipc_general_corregido <- list(
  titulo = "Balanza de Pagos. Cuenta corriente. Bienes y servicios. Ingresos por servicios. Trimestral",
  descripcion = "Balanza de Pagos. Cuenta corriente. Bienes y servicios. Ingresos por servicios. Trimestral",
  pais = "Argentina",
  categoria = "SECTOR_EXTERNO",
  frecuencia_short = "Q",
  frecuencia_original = "trimestral",
  unidades = "Millones de dólares", 
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "API_Argentina",
  fuente_original = "INDEC",
  fuente_formato = "API_INDEC",
  id_original = "160.2_CTA_CORIOS_0_T_52",
  ultima_actualizacion = Sys.Date(),
  fecha_inicio = as.Date("2006-01-01"),
  url_original = "https://datosgobar.github.io/series-tiempo-ar-explorer/#/series/?ids=160.2_CTA_CORIOS_0_T_52",
  revisable = FALSE,
  notas = NA
)

serie_id_general <- "BP_EXPORTACIONES_SERVICIOS_NSA_M"

# 3. Consolidamos en una lista para procesar en bucle
# (Se corrigió la llamada a 'meta_ipc_general_corregido')
lista_series <- list(
  list(meta = meta_ipc_general_corregido, id = serie_id_general)
)

message("Iniciando carga inicial masiva desde INDEC...")

# Definimos el tema explícitamente para todo este bloque de series
tema_fijo <- meta_ipc_general_corregido$categoria

# 4. Bucle de extracción, creación y catalogación
exito <- update_indec_json_serie(
  id_indec = meta_ipc_general_corregido$id_original,
  serie_id = serie_id_general,
  tema = tema_fijo,
  metadatos_fijos = meta_ipc_general_corregido
)

if (exito) {
  # Agregamos el parámetro "tema" explícitamente a update_catalogo
  update_catalogo(
    serie_id = serie_id_general, 
    metadatos = meta_ipc_general_corregido, 
    metodo_etl = meta_ipc_general_corregido$fuente_formato,
    tema = tema_fijo
  )
}

message("¡Carga finalizada! Revisa tu carpeta PRECIOS/ y tu catalogo.json.")