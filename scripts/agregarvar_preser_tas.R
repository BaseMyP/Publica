# ==============================================================================
# SCRIPT DE CARGA INICIAL: Tasas De préstamos al sector privado no financiero
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de preser_tas.xls (BCRA)...")

# 1. Descargar archivo
url_preser_tas <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/preser_tas.xls"
archivo_tmp <- tempfile(fileext = ".xls")

tryCatch({
  GET(url_preser_tas, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_preser_tas <- list(
  TASAS_enUSD_SECTORPRIV_ADELANTOS_NSA_M = list( 
    col_index = 67,  
    titulo = "Tasas de Adelantos en cuenta corriente en moneda extranjera. Total",
    descripcion = "Tasas de Adelantos en cuenta corriente en moneda extranjera. Total",
    unidades = "TNA %"           
  ),
  TASAS_enUSD_SECTORPRIV_ADELANTOS_OTROS_NSA_M = list( 
    col_index = 71,  
    titulo = "Tasas de Otros adelantos en moneda extranjera. Total",
    descripcion = "Tasas de Otros adelantos en moneda extranjera. Total",
    unidades = "TNA %"           
  ),
  TASAS_enUSD_SECTORPRIV_DOCUMENTOS_NSA_M = list( 
    col_index = 75,  
    titulo = "Tasas de Documentos a sola firma en moneda extranjera. Total",
    descripcion = "Tasas de Documentos a sola firma en moneda extranjera. Total",
    unidades = "TNA %"           
  ),
  TASAS_enUSD_SECTORPRIV_HIPOTECARIOS_NSA_M = list( 
    col_index = 93,  
    titulo = "Tasas de Préstamos Hipotecarios en moneda extranjera. Total",
    descripcion = "Tasas de Préstamos Hipotecarios en moneda extranjera. Total",
    unidades = "TNA %"           
  ),
  TASAS_enUSD_SECTORPRIV_PRENDARIOS_NSA_M = list( 
    col_index = 101,  
    titulo = "Tasas de Préstamos Prendarios en moneda extranjera. Total",
    descripcion = "Tasas de Préstamos Prendarios en moneda extranjera. Total",
    unidades = "TNA %"           
  ),
  TASAS_enUSD_SECTORPRIV_PERSONALESyTARJETA_NSA_M = list( 
    col_index = 107,  
    titulo = "Tasas de Préstamos Personales y sistema de tarjetas de crédito en moneda extranjera. Total",
    descripcion = "Tasas de Préstamos Personales y sistema de tarjetas de crédito en moneda extranjera. Total",
    unidades = "TNA %"           
  ),
  TASAS_enUSD_SECTORPRIV_OTROS_NSA_M = list( 
    col_index = 123,  
    titulo = "Tasas de Otros préstamos en moneda extranjera. Total",
    descripcion = "Tasas de Otros préstamos en moneda extranjera. Total",
    unidades = "TNA %"           
  )
)

# 3. Leer y limpiar el Excel
# IMPORTANTE: Ajusta 'skip = 8' a la cantidad real de filas de encabezado que tenga la hoja
df_raw <- read_excel(archivo_tmp, sheet = "Tasas_sector_privado", skip = 26, col_names = FALSE)

hoy <- as.character(Sys.Date())
tema_fijo <- "PRESTAMOS"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_preser_tas)) {
  
  config <- series_preser_tas[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    filter(!is.na(...2) & !is.na(...3)) %>% 
    select(fecha = 3, valor = all_of(config$col_index)) %>%
    mutate(anio = str_sub(fecha,1,4),
           mes = str_sub(fecha,5,6)) %>% 
    mutate(mes = as.numeric(mes)) %>% 
    mutate(
      fecha = as.Date(paste0(anio,"-",mes, "-01")),
      valor = as.numeric(valor)
    ) %>%
    # Eliminamos la columna auxiliar
    select(-c(anio,mes)) %>% 
    filter(!is.na(fecha) & !is.na(valor)) %>%
    arrange(fecha) %>%
    mutate(
      fecha = as.character(fecha),
      realtime_start = hoy,
      realtime_end = "9999-12-31"
    )
  
  # Metadatos
  meta_actual <- list(
    titulo = config$titulo,
    descripcion = config$descripcion,
    pais = "Argentina",
    categoria = tema_fijo,
    frecuencia_short = "M",
    frecuencia_original = "mensual",
    unidades = config$unidades,
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "BCRA",
    fuente_original = "BCRA",
    fuente_formato = "Excel",
    id_original = as.character(config$col_index), # Guardamos el índice de la columna para la actualización
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    url_original = url_preser_tas,
    revisable = TRUE,
    notas = "Hoja: Tasas_sector_privado"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_PRESER_TAS", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}