# ==============================================================================
# SCRIPT DE CARGA INICIAL: Balance Diario BCRA
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de diar_bas.xls (BCRA)...")

# 1. Descargar archivo
url_diar_bas <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/diar_bas.xls"
archivo_tmp <- tempfile(fileext = ".xls")

tryCatch({
  GET(url_diar_bas, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_diar_bas <- list(
  DepGobPesosBCRA_NSA_M = list( # <-- Cambia este ID por el nombre definitivo que quieras darle
    col_index = 32,               # Columna F es la 6ta columna
    titulo = "Depósitos del Gobierno en Pesos depositados en el BCRA",
    descripcion = "Depósitos del Gobierno en Pesos depositados en el BCRA. Cuentas 2020 y 2021",
    unidades = "Millones de Pesos"           # Ajustar unidad si corresponde
  ),
  DepGobUsdBCRA_NSA_M = list( # <-- Cambia este ID por el nombre definitivo que quieras darle
    col_index = 33,               # Columna F es la 6ta columna
    titulo = "Depósitos del Gobierno en USD depositados en el BCRA, valuados en pesos",
    descripcion = "Depósitos del Gobierno en USD depositados en el BCRA, valuados en pesos",
    unidades = "Millones de Pesos"           # Ajustar unidad si corresponde
  )
)

# 3. Leer y limpiar el Excel
# 1. Leemos solo 1 fila para ver cuántas columnas hay en total
temp_df <- read_excel(
  archivo_tmp, 
  sheet = "Serie_diaria", 
  skip = 27, 
  col_names = FALSE, 
  n_max = 1
)

# 2. Calculamos cuántas numéricas necesitamos (Total de columnas - 1)
n_cols <- ncol(temp_df)
mis_tipos <- c("date", rep("numeric", n_cols - 1))

# 3. Leemos el archivo real forzando nuestros tipos
df_raw <- read_excel(
  archivo_tmp, 
  sheet = "Serie_diaria", 
  skip = 27, 
  col_names = FALSE,
  col_types = mis_tipos
)
hoy <- as.character(Sys.Date())
tema_fijo <- "BALANCE_BCRA"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_diar_bas)) {
  
  config <- series_diar_bas[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    select(fecha = 1, valor = all_of(config$col_index)) %>%
    mutate(
      fecha = as.Date(fecha)
    ) %>%
    # Eliminamos la columna auxiliar
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
    frecuencia_short = "D",
    frecuencia_original = "diaria",
    unidades = config$unidades,
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "BCRA",
    fuente_original = "BCRA",
    fuente_formato = "Excel",
    id_original = as.character(config$col_index), # Guardamos el índice de la columna para la actualización
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    url_original = url_diar_bas,
    revisable = TRUE,
    notas = "Hoja: Serie_diaria"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_diar_bas", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}