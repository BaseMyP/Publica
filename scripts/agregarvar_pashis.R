# ==============================================================================
# SCRIPT DE CARGA INICIAL: Tasas de Interés Pasivas (pashis.xls - BCRA)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de pashis.xls (BCRA)...")

# 1. Descargar archivo
url_pashis <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/pashis.xls"
archivo_tmp <- tempfile(fileext = ".xls")

tryCatch({
  GET(url_pashis, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_pashis <- list(
  TASA_PF3059_PRIV_NSA_M = list( # <-- Cambia este ID por el nombre definitivo que quieras darle
    col_index = 6,               # Columna F es la 6ta columna
    titulo = "Tasa de Interés Plazos Fijos a 30-59 días en bancos privados",
    descripcion = "Tasa de interés de depósitos del sector privado. Extraída de la columna F de la hoja Serie_mensual_bcos.priv del archivo pashis.xls.",
    unidades = "% TNA"           # Ajustar unidad si corresponde
  )
)

# 3. Leer y limpiar el Excel
# IMPORTANTE: Ajusta 'skip = 8' a la cantidad real de filas de encabezado que tenga la hoja
df_raw <- read_excel(archivo_tmp, sheet = "Serie_mensual_bcos.priv", skip = 29, col_names = FALSE)

hoy <- as.character(Sys.Date())
tema_fijo <- "TC_y_TASAS"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_pashis)) {
  
  config <- series_pashis[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    select(fecha = 1, valor = all_of(config$col_index)) %>%
    filter(!str_detect(fecha,"\\.13$")) %>% 
    mutate(
      fecha = as.Date(paste0(str_sub(fecha,1,4),"-",str_sub(fecha,6,7),"-01")), # read_excel suele detectar las fechas numéricas de Excel automáticamente
      valor = as.numeric(valor)
    ) %>%
    filter(!is.na(fecha) & !is.na(valor)) %>%
    filter(valor>0) %>% 
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
    url_original = url_pashis,
    revisable = TRUE,
    notas = "Hoja: Serie_mensual_bcos.priv"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_PASHIS", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}