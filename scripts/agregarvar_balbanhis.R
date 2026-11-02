# ==============================================================================
# SCRIPT DE CARGA INICIAL: BALANCES CONSOLIDADOS DEL SISTEMA FINANCIERO
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de balbanhis.xls (BCRA)...")

# 1. Descargar archivo
url_balbanhis <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/balbanhis.xls"
archivo_tmp <- tempfile(fileext = ".xls")

tryCatch({
  GET(url_balbanhis, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_balbanhis <- list(
  EFECTIVO_enBANCOS_USD_enARS_NSA_M = list( # <-- Cambia este ID por el nombre definitivo que quieras darle
    col_index = 9,               # Columna F es la 6ta columna
    titulo = "Efectivo en Moneda Extranjera en el Activo del Sistema Financiero Consolidado",
    descripcion = "Efectivo en Moneda Extranjera en el Activo del Sistema Financiero Consolidado",
    unidades = "Millones de Pesos"           # Ajustar unidad si corresponde
  )
)

# 3. Leer y limpiar el Excel
# IMPORTANTE: Ajusta 'skip = 8' a la cantidad real de filas de encabezado que tenga la hoja
df_raw <- read_excel(archivo_tmp, sheet = "Bancos", skip = 25, col_names = TRUE)

hoy <- as.character(Sys.Date())
tema_fijo <- "SISTEMA_FINANCIERO"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_balbanhis)) {
  
  config <- series_balbanhis[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    select(fecha = 1, valor = all_of(config$col_index)) %>%
    mutate(anio = str_split_fixed(fecha,"\\.",n=2)[,1],
           mes = str_split_fixed(fecha,"\\.",n=2)[,2]) %>% 
    mutate(mes = ifelse(nchar(mes)>=3,
                        round(as.numeric(str_sub(mes,1,3))/10),
                        as.numeric(mes))
           ) %>% 
    filter(!(mes==13 | is.na(mes))) %>% 
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
    url_original = url_balbanhis,
    revisable = TRUE,
    notas = "Hoja: Bancos"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_balbanhis", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}