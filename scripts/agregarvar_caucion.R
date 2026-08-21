# ==============================================================================
# SCRIPT DE CARGA INICIAL: RIESGO PAÍS (RAVA BURSÁTIL)
# ==============================================================================

library(httr)
library(rvest)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando descarga del Riesgo País desde Rava...")

# 1. Descarga de datos
url_rava <- "https://www.rava.com/perfil/CAUCION%207D"
respuesta <- GET(
  url_rava, 
  user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
)

if (status_code(respuesta) != 200) stop("Error al acceder a Rava Bursátil.")

# 2. Extracción de la tabla HTML
html_doc <- read_html(content(respuesta, as = "text", encoding = "UTF-8"))
tablas <- html_nodes(html_doc, "table")

if (length(tablas) == 0) stop("No se encontró la tabla de cotizaciones en la página de Rava.")

df_rava <- html_table(tablas[[1]], fill = TRUE)

# 3. Limpieza de datos (usamos la columna Cierre)
df_limpio <- df_rava %>%
  mutate(
    # Rava suele usar formato YYYY-MM-DD. ymd() lo procesa correctamente.
    Fecha = as.Date(Fecha, format=("%d/%m/%Y")),
    # Quitamos puntos de miles y cambiamos coma decimal por punto
    valor = as.numeric(gsub(",", ".", gsub("\\.", "", Cierre))) 
  ) %>%
  filter(!is.na(Fecha) & !is.na(valor)) %>%
  select(fecha = Fecha, valor) %>%
  arrange(fecha)

# 4. Estructuración ALFRED
hoy <- as.character(Sys.Date())

obs_final <- df_limpio %>%
  mutate(
    fecha = as.character(fecha),
    realtime_start = hoy,
    realtime_end = "9999-12-31"
  )

# 5. Metadatos
tema_fijo <- "TC_y_TASAS"
serie_id <- "CAUCIONARS_7D_NSA_D"

meta_rp <- list(
  titulo = "Tasa de Caución en pesos a 7 días",
  descripcion = "Tasa de Caución en pesos a 7 días",
  pais = "Argentina",
  categoria = tema_fijo,
  frecuencia_short = "D",
  frecuencia_original = "diaria",
  unidades = "%",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "Rava Bursátil",
  fuente_original = "BYMA",
  fuente_formato = "Scraping HTML",
  id_original = "CAUCION 7D",
  ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
  revisable = TRUE,
  notas = NULL
)

# 6. Guardado y Catálogo
if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

lista_final <- list(serie_id = serie_id, metadatos = meta_rp, observaciones = obs_final)
path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))

write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)

update_catalogo(serie_id = serie_id, metadatos = meta_rp, metodo_etl = "SCRAPING_RAVA", tema = tema_fijo)

message("¡Carga del Riesgo País finalizada con éxito!")