library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando descarga del Excel del IPMP...")

# 1. Descargar el archivo temporalmente
url_ipmp <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/IPMPSerie.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
GET(url_ipmp, write_disk(temp_file, overwrite = TRUE), config(ssl_verifypeer = 0))

# 2. Leer la hoja (Ajusta 'skip' si el BCRA cambia el formato del encabezado)
df_raw <- read_excel(temp_file, sheet = "IPMP mensual desde ene-1997")

# Limpiar nombres de columnas y formatear fechas
df_limpio <- df_raw %>%
  select(1:5) %>% # Tomamos las primeras 5 columnas
  setNames(c("fecha", "IPMP", "IPMPAGRO", "IPMPMETALES", "IPMPPETROLEO")) %>%
  mutate(fecha = as.Date(as.numeric(fecha), origin="1899-12-30")) %>%
  filter(!is.na(fecha))

# 3. Definir Metadatos para las 4 series
metadatos_base <- list(
  pais = "Argentina",
  categoria = "SECTOR_EXTERNO",
  frecuencia_short = "M",
  frecuencia_original = "mensual",
  unidades = "Índice dic-01=100",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "BCRA",
  fuente_original = "BCRA",
  fuente_formato = "Excel",
  url_original = url_ipmp,
  revisable = TRUE
)

series_config <- list(
  IPMP_INDICE_NSA_M = list(
    titulo = "IPMP - Nivel General",
    descripcion = "Índice de Precios de las Materias Primas. Nivel General.",
    columna = "IPMP"
  ),
  IPMPAGRO_INDICE_NSA_M = list(
    titulo = "IPMP - Productos Agropecuarios",
    descripcion = "Índice de Precios de las Materias Primas. Productos Agropecuarios.",
    columna = "IPMPAGRO"
  ),
  IPMPMETALES_INDICE_NSA_M = list(
    titulo = "IPMP - Metales",
    descripcion = "Índice de Precios de las Materias Primas. Metales.",
    columna = "IPMPMETALES"
  ),
  IPMPPETROLEO_INDICE_NSA_M = list(
    titulo = "IPMP - Petróleo Crudo",
    descripcion = "Índice de Precios de las Materias Primas. Petróleo Crudo.",
    columna = "IPMPPETROLEO"
  )
)

hoy <- as.character(Sys.Date())
tema_fijo <- metadatos_base$categoria

# 4. Bucle de creación
for (id_serie in names(series_config)) {
  config <- series_config[[id_serie]]
  
  # Preparar metadatos específicos
  meta_actual <- metadatos_base
  meta_actual$titulo <- config$titulo
  meta_actual$descripcion <- config$descripcion
  meta_actual$id_original <- config$columna
  meta_actual$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
  
  # Extraer la serie específica
  df_serie <- df_limpio %>%
    select(fecha, valor = !!sym(config$columna)) %>%
    mutate(
      fecha = as.character(fecha),
      valor = as.numeric(valor),
      realtime_start = hoy,
      realtime_end = "9999-12-31"
    ) %>%
    filter(!is.na(valor)) %>%
    arrange(fecha)
  
  # Guardar JSON
  lista_final <- list(serie_id = id_serie, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(id_serie, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar catálogo
  update_catalogo(serie_id = id_serie, metadatos = meta_actual, metodo_etl = "EXCEL_IPMP", tema = tema_fijo)
}

message("¡Carga del IPMP finalizada exitosamente!")