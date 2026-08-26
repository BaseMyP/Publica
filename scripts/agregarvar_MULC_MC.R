# ==============================================================================
# SCRIPT DE CARGA INICIAL: Archivo MULC - HOJA MERCADO DE CAMBIOS
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica del archivo del MULC (BCRA)...")

# 1. Descargar archivo
url_MULC <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/informes/anexo-estadistico-mercado-cambios-balance-cambiario.xlsx"
archivo_tmp <- tempfile(fileext = ".xlsx")

tryCatch({
  GET(url_MULC, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_MULC <- list(
  MULC_MercCamb_PRESTFIN_TOTAL_NSA_M = list( 
    col_index = "mlc076",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Total",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Total",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_INGRESOS_NSA_M = list( 
    col_index = "mlc077",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Ingresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_OOII_INGRESOS_NSA_M = list( 
    col_index = "mlc078",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. OOII. Ingresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. OOII. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_OTROS_INGRESOS_NSA_M = list( 
    col_index = "mlc079",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Otros préstamos. Ingresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Otros préstamos. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_LINEAS_INGRESOS_NSA_M = list( 
    col_index = "mlc080",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Líneas de crédito. Ingresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Líneas de crédito. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_FinLocal_INGRESOS_NSA_M = list( 
    col_index = "mlc081",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Financiaciones locales. Ingresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Financiaciones locales. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_EGRESOS_NSA_M = list( 
    col_index = "mlc082",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Egresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_OOII_EGRESOS_NSA_M = list( 
    col_index = "mlc083",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. OOII. Egresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. OOII. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_OTROS_EGRESOS_NSA_M = list( 
    col_index = "mlc084",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Otros préstamos. Egresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Otros préstamos. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_LINEAS_EGRESOS_NSA_M = list( 
    col_index = "mlc085",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Líneas de crédito. Egresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Líneas de crédito. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_MercCamb_PRESTFIN_FinLocal_EGRESOS_NSA_M = list( 
    col_index = "mlc086",               
    titulo = "Préstamos financieros, títulos de deuda y líneas de crédito. Financiaciones locales. Egresos",
    descripcion = "Préstamos financieros, títulos de deuda y líneas de crédito. Financiaciones locales. Egresos",
    unidades = "Millones de USD"           
  )
)

# 3. Leer y limpiar el Excel
# IMPORTANTE: Ajusta 'skip = 8' a la cantidad real de filas de encabezado que tenga la hoja
df_raw <- read_excel(archivo_tmp, sheet="Mercado de Cambios", skip=10)

hoy <- as.character(Sys.Date())
tema_fijo <- "SECTOR_EXTERNO"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_MULC)) {
  
  config <- series_MULC[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    select(fecha = "mlc000", valor = config$col_index) %>%
    mutate(fecha = as.Date(as.numeric(fecha), origin="1899-12-30")) %>% 
    filter(!is.na(fecha)) %>% 
    filter(year(fecha)>2000) %>%
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
    url_original = url_MULC,
    revisable = TRUE,
    notas = "Hoja: Mercado de Cambios"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_MULC", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}