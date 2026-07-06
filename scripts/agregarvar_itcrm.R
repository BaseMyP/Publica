# ==============================================================================
# SCRIPT DE CARGA INICIAL: Índices de Tipo de Cambio (ITCRM / ITCNM - BCRA)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de ITCRM e ITCNM (BCRA)...")

# 1. Configuración de las series a extraer
config_itcr <- list(
  TC_y_TASAS_ITCRM_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales", col = 2, freq_short = "D", freq_orig = "diaria", titulo = "Índice de Tipo de Cambio Real Multilateral (ITCRM)"),
  TC_y_TASAS_ITCRM_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales prom. mens.", col = 2, freq_short = "M", freq_orig = "mensual", titulo = "Índice de Tipo de Cambio Real Multilateral (ITCRM) - Promedio Mensual"),
  TC_y_TASAS_ITCRB_USA_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales", col = 6, freq_short = "D", freq_orig = "diaria", titulo = "Índice de Tipo de Cambio Real Bilateral (ITCRB) con Estados Unidos"),
  TC_y_TASAS_ITCRB_USA_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx", hoja = "ITCRM y bilaterales prom. mens.", col = 6, freq_short = "M", freq_orig = "mensual", titulo = "Índice de Tipo de Cambio Real Bilateral (ITCRB) con Estados Unidos - Promedio Mensual"),
  
  TC_y_TASAS_ITCNM_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales", col = 2, freq_short = "D", freq_orig = "diaria", titulo = "Índice de Tipo de Cambio Nominal Multilateral (ITCNM)"),
  TC_y_TASAS_ITCNM_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales prom. mens.", col = 2, freq_short = "M", freq_orig = "mensual", titulo = "Índice de Tipo de Cambio Nominal Multilateral (ITCNM) - Promedio Mensual"),
  TC_y_TASAS_ITCNB_USA_NSA_D = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales", col = 6, freq_short = "D", freq_orig = "diaria", titulo = "Índice de Tipo de Cambio Nominal Bilateral (ITCNB) con Estados Unidos"),
  TC_y_TASAS_ITCNB_USA_NSA_M = list(url = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCNMSerie.xlsx", hoja = "ITCNM y bilaterales prom. mens.", col = 6, freq_short = "M", freq_orig = "mensual", titulo = "Índice de Tipo de Cambio Nominal Bilateral (ITCNB) con Estados Unidos - Promedio Mensual")
)

# 2. Descargar los Excels (solo una vez por URL para ahorrar tiempo)
urls_unicas <- unique(sapply(config_itcr, function(x) x$url))
archivos_locales <- list()

for (u in urls_unicas) {
  tmp <- tempfile(fileext = ".xlsx")
  tryCatch({
    GET(u, write_disk(tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
    archivos_locales[[u]] <- tmp
  }, error = function(e) stop("Error al descargar: ", u))
}

hoy <- as.character(Sys.Date())
tema_fijo <- "TC_y_TASAS"
if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 3. Bucle de procesamiento y guardado
for (serie_id in names(config_itcr)) {
  
  config <- config_itcr[[serie_id]]
  archivo_leer <- archivos_locales[[config$url]]
  
  # Leer crudo (sin forzar encabezados para no perder fechas)
  df_raw <- read_excel(archivo_leer, sheet = config$hoja, col_names = FALSE, skip=1)
  
  # Aislamiento de columnas y forzado numérico (elimina texto de títulos)
  nuevo_df <- df_raw %>%
    select(fecha_cruda = 1, valor = all_of(config$col)) %>%
    mutate(valor = suppressWarnings(as.numeric(valor))) %>%
    filter(!is.na(valor))
  
  # Parseo Inteligente de Fechas
  nuevo_df$fecha <- suppressWarnings(as.Date(as.numeric(nuevo_df$fecha_cruda), origin = "1899-12-30"))
  idx_na <- is.na(nuevo_df$fecha)
  if(any(idx_na)) nuevo_df$fecha[idx_na] <- as.Date(nuevo_df$fecha_cruda[idx_na])
  
  df_serie <- nuevo_df %>%
    filter(!is.na(fecha)) %>%
    arrange(fecha) %>%
    transmute(
      fecha = as.character(fecha),
      valor = round(valor, 4),
      realtime_start = hoy,
      realtime_end = "9999-12-31"
    )
  
  # Metadatos
  meta_actual <- list(
    titulo = config$titulo,
    descripcion = config$titulo,
    pais = "Argentina",
    categoria = tema_fijo,
    frecuencia_short = config$freq_short,
    frecuencia_original = config$freq_orig,
    unidades = "Índice base 17/12/15=100",
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "BCRA",
    fuente_original = "BCRA",
    fuente_formato = "Excel",
    id_original = as.character(config$col),
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    fecha_inicio = as.character(min(df_serie$fecha)),
    url_original = config$url,
    revisable = TRUE,
    notas = paste0("Extraído de la hoja: '", config$hoja, "'")
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_ITCR", tema = tema_fijo)
  message("✓ Serie generada exitosamente: ", serie_id)
}