# ============================================================
# inicial_indec_indices_expo.R
# Descarga inicial - Índices de exportaciones INDEC
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(httr)

source("scripts/funciones_base.R")

# ------------------------------------------------------------
# Configuración
# ------------------------------------------------------------
tema <- "SECTOR_EXTERNO"
hoy <- as.character(Sys.Date())

# ------------------------------------------------------------
# Lectura
# ------------------------------------------------------------

URL_INDICES_EXPO <-
  "https://www.indec.gob.ar/ftp/cuadros/economia/serie_mensual_indices_expo.xls"

archivo_tmp <- tempfile(fileext = ".xls")

tryCatch(
  {
    download.file(
      url = URL_INDICES_EXPO,
      destfile = archivo_tmp,
      mode = "wb",
      quiet = TRUE
    )
  },
  error = function(e)
  {
    stop(
      paste(
        "No se pudo descargar el archivo:",
        url_excel
      )
    )
  }
)

raw <- read_excel(
  archivo_tmp,
  col_names = FALSE
)

# ------------------------------------------------------------
# Construcción de fechas
# ------------------------------------------------------------

datos <- raw[6:nrow(raw), ]

colnames(datos) <- c(
  "anio",
  "mes",
  "NG_VALOR",
  "NG_PRECIO",
  "NG_CANTIDAD",
  "v1",
  "PP_VALOR",
  "PP_PRECIO",
  "PP_CANTIDAD",
  "v2",
  "MOA_VALOR",
  "MOA_PRECIO",
  "MOA_CANTIDAD",
  "v3",
  "MOI_VALOR",
  "MOI_PRECIO",
  "MOI_CANTIDAD",
  "v4",
  "CE_VALOR",
  "CE_PRECIO",
  "CE_CANTIDAD"
)

datos <- datos %>%
  select(-starts_with("v"))

# completar año

library(stringr)

datos$anio <- as.character(datos$anio)

# elimina asteriscos y cualquier otro texto
datos$anio <- str_extract(datos$anio, "\\d{4}")

# completa hacia abajo
datos$anio <- zoo::na.locf(datos$anio, na.rm = FALSE)

datos <- datos %>%
  filter(!is.na(mes))

meses <- c(
  Enero = 1,
  Febrero = 2,
  Marzo = 3,
  Abril = 4,
  Mayo = 5,
  Junio = 6,
  Julio = 7,
  Agosto = 8,
  Septiembre = 9,
  Octubre = 10,
  Noviembre = 11,
  Diciembre = 12
)

datos <- datos %>%
  mutate(
    anio = as.integer(anio),
    mes_num = meses[trimws(mes)],
    fecha = as.Date(
      sprintf("%04d-%02d-01", anio, mes_num)
    )
  ) %>%
  filter(!is.na(fecha))

# ------------------------------------------------------------
# Mapeo de series
# ------------------------------------------------------------

series <- list(
  
  EXPO_NG_VALOR_INDICE_NSA_M =
    list(columna = "NG_VALOR",
         titulo = "Exportaciones - Índice de valor - Nivel general"),
  
  EXPO_NG_PRECIO_INDICE_NSA_M =
    list(columna = "NG_PRECIO",
         titulo = "Exportaciones - Índice de precios - Nivel general"),
  
  EXPO_NG_CANTIDAD_INDICE_NSA_M =
    list(columna = "NG_CANTIDAD",
         titulo = "Exportaciones - Índice de cantidad - Nivel general"),
  
  EXPO_PP_VALOR_INDICE_NSA_M =
    list(columna = "PP_VALOR",
         titulo = "Exportaciones - Índice de valor - Productos primarios"),
  
  EXPO_PP_PRECIO_INDICE_NSA_M =
    list(columna = "PP_PRECIO",
         titulo = "Exportaciones - Índice de precios - Productos primarios"),
  
  EXPO_PP_CANTIDAD_INDICE_NSA_M =
    list(columna = "PP_CANTIDAD",
         titulo = "Exportaciones - Índice de cantidad - Productos primarios"),
  
  EXPO_MOA_VALOR_INDICE_NSA_M =
    list(columna = "MOA_VALOR",
         titulo = "Exportaciones - Índice de valor - MOA"),
  
  EXPO_MOA_PRECIO_INDICE_NSA_M =
    list(columna = "MOA_PRECIO",
         titulo = "Exportaciones - Índice de precios - MOA"),
  
  EXPO_MOA_CANTIDAD_INDICE_NSA_M =
    list(columna = "MOA_CANTIDAD",
         titulo = "Exportaciones - Índice de cantidad - MOA"),
  
  EXPO_MOI_VALOR_INDICE_NSA_M =
    list(columna = "MOI_VALOR",
         titulo = "Exportaciones - Índice de valor - MOI"),
  
  EXPO_MOI_PRECIO_INDICE_NSA_M =
    list(columna = "MOI_PRECIO",
         titulo = "Exportaciones - Índice de precios - MOI"),
  
  EXPO_MOI_CANTIDAD_INDICE_NSA_M =
    list(columna = "MOI_CANTIDAD",
         titulo = "Exportaciones - Índice de cantidad - MOI"),
  
  EXPO_CE_VALOR_INDICE_NSA_M =
    list(columna = "CE_VALOR",
         titulo = "Exportaciones - Índice de valor - Combustibles y energía"),
  
  EXPO_CE_PRECIO_INDICE_NSA_M =
    list(columna = "CE_PRECIO",
         titulo = "Exportaciones - Índice de precios - Combustibles y energía"),
  
  EXPO_CE_CANTIDAD_INDICE_NSA_M =
    list(columna = "CE_CANTIDAD",
         titulo = "Exportaciones - Índice de cantidad - Combustibles y energía")
)

# ------------------------------------------------------------
# Generación de JSON
# ------------------------------------------------------------

dir.create(tema, showWarnings = FALSE)

for (serie_id in names(series)) {
  
  info <- series[[serie_id]]
  
  observaciones <- datos %>%
    transmute(
      fecha = as.character(fecha),
      valor = as.numeric(.data[[info$columna]]),
      realtime_start = hoy,
      realtime_end = "9999-12-31"
    ) %>%
    filter(!is.na(valor))
  
  metadatos <- list(
    titulo = info$titulo,
    descripcion = "Indices de exportaciones incluidas en la publicación Precios y cantidades del comercio exterior de INDEC",
    pais = "Argentina",
    categoria = "SECTOR_EXTERNO",
    frecuencia_original = "Mensual",
    frecuencia_short = "M",
    unidades = "Índice base 2004=100",
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "INDEC",
    fuente_original = "INDEC",
    fuente_formato = "Excel",
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    fecha_inicio = "2004-01-01",
    url_original = "https://www.indec.gob.ar/ftp/cuadros/economia/serie_mensual_indices_expo.xls",
    revisable = TRUE,
    notas = ""
  )
  
  salida <- list(
    serie_id = serie_id,
    metadatos = metadatos,
    observaciones = observaciones
  )
  
  write_json(
    salida,
    file.path(tema, paste0(serie_id, ".json")),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  update_catalogo(
    serie_id = serie_id,
    metadatos = metadatos,
    tema = tema,
    #titulo = serie_id,
    #frecuencia = "M",
    #tipo_informacion = "Pública",
    #raw_url = "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/SECTOR_EXTERNO/serie_id.json",
    metodo_etl = "EXCEL_WEB_INDEC"
  )
  
  cat("Generada:", serie_id, "\n")
}
