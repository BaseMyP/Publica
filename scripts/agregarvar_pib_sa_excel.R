
library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)
library(zoo)

source("scripts/funciones_base.R")

# ------------------------------------------------------------
# Configuración
# ------------------------------------------------------------

archivo_excel <- "inputs/historicos/sh_oferta_demanda_desest_06_26.xls"

tema <- "ACTIVIDAD"

hoy <- as.character(Sys.Date())

# ------------------------------------------------------------
# Lectura
# ------------------------------------------------------------

raw <- read_excel(
  archivo_excel,
  sheet = "desestacionalizado n",
  col_names = FALSE
)

datos <- raw[7:nrow(raw), ]

colnames(datos) <- c(
  "anio",
  "trimestre",
  "PIB",
  "IMPORTACIONES",
  "CONSUMO_PRIVADO",
  "CONSUMO_PUBLICO",
  "FBCF",
  "EXPORTACIONES"
)

# ------------------------------------------------------------
# Completar año
# ------------------------------------------------------------

datos$anio <- as.character(datos$anio)

datos$anio <- str_extract(
  datos$anio,
  "\\d{4}"
)

datos$anio <- zoo::na.locf(
  datos$anio,
  na.rm = FALSE
)

datos <- datos %>%
  filter(!is.na(trimestre))

# ------------------------------------------------------------
# Fecha trimestral
# ------------------------------------------------------------

trimestres <- c(
  "I"   = "01",
  "II"  = "04",
  "III" = "07",
  "IV"  = "10"
)

datos <- datos %>%
  mutate(
    anio = as.integer(anio),
    mes = trimestres[trimws(trimestre)],
    fecha = as.Date(
      paste0(
        anio,
        "-",
        mes,
        "-01"
      )
    )
  ) %>%
  filter(!is.na(fecha))

# ------------------------------------------------------------
# Definición de series
# ------------------------------------------------------------

series <- list(
  
  CN_PBI_SA_T =
    list(
      columna = "PIB",
      titulo = "PIB desestacionalizado en millones de pesos, a precios de 2004"
    ),
  
  CN_CONSUMO_PRIVADO_SA_T =
    list(
      columna = "CONSUMO_PRIVADO",
      titulo = "Consumo privado desestacionalizado en millones de pesos, a precios de 2004"
    ),
  
  CN_CONSUMO_PUBLICO_SA_T =
    list(
      columna = "CONSUMO_PUBLICO",
      titulo = "Consumo público desestacionalizado en millones de pesos, a precios de 2004"
    ),
  
  CN_FBCF_SA_T =
    list(
      columna = "FBCF",
      titulo = "Formación Bruta de Capital Fijo desestacionalizada en millones de pesos, a precios de 2004"
    ),
  
  CN_EXPORTACIONES_SA_T =
    list(
      columna = "EXPORTACIONES",
      titulo = "Exportaciones desestacionalizadas en millones de pesos, a precios de 2004"
    ),
  
  CN_IMPORTACIONES_SA_T =
    list(
      columna = "IMPORTACIONES",
      titulo = "Importaciones desestacionalizadas en millones de pesos, a precios de 2004"
    )
  
)

# ------------------------------------------------------------
# Generación JSON
# ------------------------------------------------------------

dir.create(
  tema,
  showWarnings = FALSE
)

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
    descripcion = info$titulo,
    pais = "Argentina",
    categoria = "ACTIVIDAD",
    frecuencia_short = "Q",
    frecuencia_original = "trimestral",
    unidades = "Millones de pesos a precios de 2004",
    ajuste = "SA",
    tipo_informacion = "Pública",
    fuente = "INDEC",
    fuente_original = "INDEC",
    fuente_formato = "Excel",
    ultima_actualizacion = paste0(
      hoy,
      "T12:00:00Z"
    ),
    fecha_inicio = min(observaciones$fecha),
    url_original = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_desest_06_26.xls",
    revisable = TRUE,
    notas = "Como INDEC cambia el nombre del excel web en cada actualización, se actualiza la variable a partir de un excel en la carpeta /inputs"
  )
  
  salida <- list(
    serie_id = serie_id,
    metadatos = metadatos,
    observaciones = observaciones
  )
  
  write_json(
    salida,
    file.path(
      tema,
      paste0(
        serie_id,
        ".json"
      )
    ),
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  update_catalogo(
    serie_id = serie_id,
    metadatos = metadatos,
    metodo_etl = "EXCEL_CARPETA_INPUTS",
    tema = tema
  )
  
  cat(
    "Generada:",
    serie_id,
    "\n"
  )
}
