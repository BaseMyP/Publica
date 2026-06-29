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

URL_INDICES_IMPO <-
  "https://www.indec.gob.ar/ftp/cuadros/economia/serie_mensual_indices_impo_ue.xls"

archivo_tmp <- tempfile(fileext = ".xls")

tryCatch(
  {
    download.file(
      url = URL_INDICES_IMPO,
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
  
  "BK_VALOR",
  "BK_PRECIO",
  "BK_CANTIDAD",
  
  "v2",
  
  "BI_VALOR",
  "BI_PRECIO",
  "BI_CANTIDAD",
  
  "v3",
  
  "CL_VALOR",
  "CL_PRECIO",
  "CL_CANTIDAD",
  
  "v4",
  
  "PA_VALOR",
  "PA_PRECIO",
  "PA_CANTIDAD",
  
  "v5",
  
  "BC_VALOR",
  "BC_PRECIO",
  "BC_CANTIDAD",
  
  "v6",
  
  "VA_VALOR",
  "VA_PRECIO",
  "VA_CANTIDAD"
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
  
  IMPO_NG_VALOR_INDICE_NSA_M =
    list(columna = "NG_VALOR",
         titulo = "Importaciones - Índice de valor - Nivel general"),
  
  IMPO_NG_PRECIO_INDICE_NSA_M =
    list(columna = "NG_PRECIO",
         titulo = "Importaciones - Índice de precios - Nivel general"),
  
  IMPO_NG_CANTIDAD_INDICE_NSA_M =
    list(columna = "NG_CANTIDAD",
         titulo = "Importaciones - Índice de cantidad - Nivel general"),
  
  IMPO_BK_VALOR_INDICE_NSA_M =
    list(columna = "BK_VALOR",
         titulo = "Importaciones - Índice de valor - Bienes de capital"),
  
  IMPO_BK_PRECIO_INDICE_NSA_M =
    list(columna = "BK_PRECIO",
         titulo = "Importaciones - Índice de precios - Bienes de capital"),
  
  IMPO_BK_CANTIDAD_INDICE_NSA_M =
    list(columna = "BK_CANTIDAD",
         titulo = "Importaciones - Índice de cantidad - Bienes de capital"),
  
  IMPO_BI_VALOR_INDICE_NSA_M =
    list(columna = "BI_VALOR",
         titulo = "Importaciones - Índice de valor - Bienes intermedios"),
  
  IMPO_BI_PRECIO_INDICE_NSA_M =
    list(columna = "BI_PRECIO",
         titulo = "Importaciones - Índice de precios - Bienes intermedios"),
  
  IMPO_BI_CANTIDAD_INDICE_NSA_M =
    list(columna = "BI_CANTIDAD",
         titulo = "Importaciones - Índice de cantidad - Bienes intermedios"),
  
  IMPO_CL_VALOR_INDICE_NSA_M =
    list(columna = "CL_VALOR",
         titulo = "Importaciones - Índice de valor - Combustibles y lubricantes"),
  
  IMPO_CL_PRECIO_INDICE_NSA_M =
    list(columna = "CL_PRECIO",
         titulo = "Importaciones - Índice de precios - Combustibles y lubricantes"),
  
  IMPO_CL_CANTIDAD_INDICE_NSA_M =
    list(columna = "CL_CANTIDAD",
         titulo = "Importaciones - Índice de cantidad - Combustibles y lubricantes"),
  
  IMPO_PA_VALOR_INDICE_NSA_M =
    list(columna = "PA_VALOR",
         titulo = "Importaciones - Índice de valor - Piezas y accesorios para bienes de capital"),
  
  IMPO_PA_PRECIO_INDICE_NSA_M =
    list(columna = "PA_PRECIO",
         titulo = "Importaciones - Índice de precios - Piezas y accesorios para bienes de capital"),
  
  IMPO_PA_CANTIDAD_INDICE_NSA_M =
    list(columna = "PA_CANTIDAD",
         titulo = "Importaciones - Índice de cantidad - Piezas y accesorios para bienes de capital"),
  
  IMPO_BC_VALOR_INDICE_NSA_M =
    list(columna = "BC_VALOR",
         titulo = "Importaciones - Índice de valor - Bienes de consumo"),
  
  IMPO_BC_PRECIO_INDICE_NSA_M =
    list(columna = "BC_PRECIO",
         titulo = "Importaciones - Índice de precios - Bienes de consumo"),
  
  IMPO_BC_CANTIDAD_INDICE_NSA_M =
    list(columna = "BC_CANTIDAD",
         titulo = "Importaciones - Índice de cantidad - Bienes de consumo"),
  
  IMPO_VA_VALOR_INDICE_NSA_M =
    list(columna = "VA_VALOR",
         titulo = "Importaciones - Índice de valor - Vehículos automotores de pasajeros"),
  
  IMPO_VA_PRECIO_INDICE_NSA_M =
    list(columna = "VA_PRECIO",
         titulo = "Importaciones - Índice de precios - Vehículos automotores de pasajeros"),
  
  IMPO_VA_CANTIDAD_INDICE_NSA_M =
    list(columna = "VA_CANTIDAD",
         titulo = "Importaciones - Índice de cantidad - Vehículos automotores de pasajeros")
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
    descripcion = "Indices de importaciones incluidas en la publicación Precios y cantidades del comercio exterior de INDEC",
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
    url_original = "https://www.indec.gob.ar/ftp/cuadros/economia/serie_mensual_indices_impo_ue.xls",
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
    metodo_etl = "EXCEL_WEB_INDEC"
  )
  
  cat("Generada:", serie_id, "\n")
}
