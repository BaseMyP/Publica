# ==============================================================================
# SCRIPT DE CARGA INICIAL: CAUCIÓN PROMEDIO (MAE)
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de cauciones desde MAE...")

# 1. Definir rango (ajusta el año si MAE permite más historia)
fecha_desde <- "2026-03-01" 
fecha_hasta <- as.character(Sys.Date())

# 2. Construir la URL codificada
query_json <- paste0('{"fechaDesde":"', fecha_desde, '","fechaHasta":"', fecha_hasta, '"}')
query_encoded <- URLencode(query_json, reserved = TRUE)
url_mae <- paste0("https://api.marketdata.mae.com.ar/api/mercado/titulo/historicocauciones?oTitulo=", query_encoded)

respuesta <- GET(url_mae, user_agent("Mozilla/5.0"))

if (status_code(respuesta) != 200) stop("Error al conectar con la API del MAE")

# 3. Parsear JSON y limpiar
df_raw <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))

# Desanidamos la lista de dataframes de la columna 'details' y los unimos
df_detalles <- bind_rows(df_raw$details)

df_limpio <- df_detalles %>%
  filter(moneda == "$") %>% # Filtramos para quedarnos solo con la tasa en Pesos
  mutate(
    fecha = as.character(as.Date(fecha)), # Limpiamos la hora (de "2026-04-09T00:00:00" a "2026-04-09")
    plazo = as.numeric(plazo),
    valor = as.numeric(tasaPP) # Seleccionamos la Tasa Promedio Ponderada
  ) %>%
  filter(!is.na(fecha) & !is.na(valor)) %>%
  group_by(fecha) %>%
  slice_min(order_by = plazo, n = 1, with_ties = FALSE) %>% # Elegimos el plazo más corto operado ese día
  ungroup() %>%
  select(fecha, valor) %>%
  arrange(fecha)

# 4. Estructuración ALFRED
hoy <- as.character(Sys.Date())

obs_final <- df_limpio %>%
  mutate(
    realtime_start = hoy,
    realtime_end = "9999-12-31"
  )

# 5. Metadatos y Guardado
tema_fijo <- "TC_y_TASAS"
serie_id <- "CAUCION_MAE_TASAPP_NSA_D"

meta_caucion <- list(
  titulo = "Tasa de Caución Bursátil (Promedio Ponderado MAE)",
  descripcion = "Tasa de interés promedio ponderada por volumen para cauciones. Se selecciona diariamente el plazo más corto disponible (típicamente 1 día hábil).",
  pais = "Argentina",
  categoria = tema_fijo,
  frecuencia_short = "D",
  unidades = "Porcentaje (TNA)",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "Mercado Abierto Electrónico (MAE)",
  fuente_original = "MAE",
  fuente_formato = "API_MAE",
  id_original = "CAUCIONES",
  ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
  revisable = TRUE,
  notas = NULL
)

lista_final <- list(serie_id = serie_id, metadatos = meta_caucion, observaciones = obs_final)
path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))

write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
update_catalogo(serie_id = serie_id, metadatos = meta_caucion, metodo_etl = "API_MAE", tema = tema_fijo)

message("¡Carga inicial finalizada con éxito!")