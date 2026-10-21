# ==============================================================================
# SCRIPT DE CARGA INICIAL: Préstamos y depósitos al / del sector privado no financiero
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de perser_priv.xls (BCRA)...")

# 1. Descargar archivo
url_perserpriv <- "https://www.bcra.gob.ar/Pdfs/PublicacionesEstadisticas/perser_priv.xls"
archivo_tmp <- tempfile(fileext = ".xls")

tryCatch({
  GET(url_perserpriv, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_perserpriv <- list(
  DEPUSD_PerFisicas_enARS_NSA_M = list( # <-- Cambia este ID por el nombre definitivo que quieras darle
    col_index = 31,               # Columna F es la 6ta columna
    titulo = "Total de depósitos en moneda extranjera de Personas Físicas, valuado en pesos",
    descripcion = "Total de depósitos en moneda extranjera de Personas Físicas, valuado en pesos",
    unidades = "Millones de Pesos"           # Ajustar unidad si corresponde
  )
)

# 3. Leer y limpiar el Excel
# IMPORTANTE: Ajusta 'skip = 8' a la cantidad real de filas de encabezado que tenga la hoja
df_raw <- read_excel(archivo_tmp, sheet = "Dep-tot", skip = 25)

hoy <- as.character(Sys.Date())
tema_fijo <- "DEPOSITOS"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_perserpriv)) {
  
  config <- series_perserpriv[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    select(fecha = 3, valor = all_of(config$col_index)) %>%
    mutate(anio = str_sub(fecha,1,4),
           mes = str_sub(fecha,5,6))  %>% 
    filter(!(mes=="13" | is.na(mes))) %>% 
    mutate(
      fecha = as.Date(paste0(anio,"-",mes, "-01")),
      valor = as.numeric(valor)/1000
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
    url_original = url_perserpriv,
    revisable = TRUE,
    notas = "Hoja: Dep-tot"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_perserpriv", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}