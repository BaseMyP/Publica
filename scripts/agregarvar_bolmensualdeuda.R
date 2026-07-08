# ==============================================================================
# SCRIPT DE CARGA INICIAL: Boletín Mensual de Deuda (Finanzas)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
library(rvest)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica del boletín mensual de deuda")

# 1. Definir la URL base
url <- "https://www.argentina.gob.ar/economia/finanzas/datos-mensuales"

# 2. Leer la página web
pagina <- read_html(url)

# 3. Extraer todos los enlaces (etiquetas <a>) y filtrar el que termine en .xlsx
url_excel <- pagina %>%
  html_nodes("a") %>%
  html_attr("href")
# Filtrar aquellos que contienen la estructura de archivos del sitio
url_excel <- url_excel[which(grepl("\\.xlsx$",url_excel))][1]
url_excel <- str_extract(url_excel,"http.*")

archivo_tmp <- tempfile(fileext = ".xlsx")

tryCatch({
  GET(url_excel, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_deudaOOII <- list(
  DEUDA_MULTyBIL_USD_NSA_M = list(
    fil_index = c("ORGANISMOS INTERNACIONALES","ORGANISMOS OFICIALES"),
    titulo = "Deuda Pública en USD con OOII, Club de París y Bilaterales",
    descripcion = "Deuda Pública en USD con OOII, Club de París y Bilaterales. Boletín Mensual de Deuda de la Secretaría de Finanzas",
    unidades = "Millones de USD"
  )
)

# 3. Leer y limpiar el Excel
# IMPORTANTE: Ajusta 'skip = 8' a la cantidad real de filas de encabezado que tenga la hoja
df_raw <- read_excel(archivo_tmp, sheet = "A.1", skip = 8)

hoy <- as.character(Sys.Date())
tema_fijo <- "DEUDA_PUBLICA"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_deudaOOII)) {
  
  config <- series_deudaOOII[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    filter(`...1` %in% config$fil_index) %>%
    t() %>% 
    as.data.frame()
  names(df_serie) <- df_serie[1,]
  df_serie <- df_serie[-1,]
  df_serie$fecha <- as.Date(as.numeric(row.names(df_serie)), origin="1899-12-30")
  index <- which(is.na(df_serie$fecha) & str_detect(rownames(df_serie),"[a-z]{3}-[0-9]{2}"))
  for(i in index) {
    df_serie$fecha[i] <- floor_date(df_serie$fecha[i-1] + days(35),"months")
  }
  df_serie <- df_serie[!is.na(df_serie$fecha),]
  index <- which(names(df_serie)!="fecha")
  for(i in index) {
    df_serie[,i] <- as.numeric(df_serie[,i])
  }
  df_serie <- df_serie %>% 
    mutate(valor= `ORGANISMOS INTERNACIONALES` + `ORGANISMOS OFICIALES`) %>% 
    select(fecha,valor) %>%
    arrange(fecha) %>%
    mutate(
      fecha = as.character(fecha),
      realtime_start = hoy,
      realtime_end = "9999-12-31"
    )
  row.names(df_serie) <- NULL
  
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
    fuente = "Secretaría de Finanzas",
    fuente_original = "Secretaría de Finanzas",
    fuente_formato = "Excel",
    id_original = as.character(config$fil_index), # Guardamos el índice de la columna para la actualización
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    url_original = url_excel,
    revisable = TRUE,
    notas = "Hoja: A.1"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_BOLMENFINANZAS", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}