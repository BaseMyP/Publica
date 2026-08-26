# ==============================================================================
# SCRIPT DE CARGA INICIAL: INTERÉS ABIERTO FUTUROS USD (MAE)
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica del Interés Abierto desde MAE...")

# 1. Definir rango (ajusta el año si MAE permite más o menos historia)
fecha_desde <- "2025-09-01" 
fecha_hasta <- as.character(Sys.Date())

# 2. Construir la URL codificada
query_json <- paste0('{"fechaDesde":"', fecha_desde, '","fechaHasta":"', fecha_hasta, '","contratosSinVolumen":false}')
query_encoded <- URLencode(query_json, reserved = TRUE)
url_mae <- paste0("https://api.marketdata.mae.com.ar/api/cem/monedas/fut?oData=", query_encoded)

respuesta <- GET(url_mae, user_agent("Mozilla/5.0"))

if (status_code(respuesta) != 200) stop("Error al conectar con la API del MAE")

# 3. Parsear JSON y limpiar
df_raw <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))

# ATENCIÓN: Si en el JSON la columna no se llama 'openInterest', cambialo en el sum() (ej: interesAbierto o ia)
df_limpio <- df_raw %>%
  filter(str_detect(posicion, "^DLR")) %>% # Nos quedamos solo con los que empiezan con "DLR"
  mutate(
    fecha = as.character(as.Date(fecha)),
    # Convertimos a numérico por seguridad, asumiendo que la columna se llama openInterest
    openInterest = as.numeric(interesAbierto)
  ) %>%
  filter(!is.na(fecha)) %>%
  group_by(fecha) %>%
  summarise(valor = sum(openInterest, na.rm = TRUE)) %>% # Sumamos el I.A. de todas las posiciones
  ungroup() %>%
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
serie_id <- "FUTUROS_USD_IA_NSA_D"

meta_ia <- list(
  titulo = "Interés Abierto Total de Futuros de Dólar",
  descripcion = "Suma total del interés abierto (contratos vigentes) de todas las posiciones de futuros de dólar (DLR) operadas en el mercado.",
  pais = "Argentina",
  categoria = tema_fijo,
  frecuencia_short = "D",
  unidades = "Contratos",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "Mercado Abierto Electrónico (MAE)",
  fuente_original = "MAE",
  fuente_formato = "API_MAE_FUTUROS",
  id_original = "FUTUROS_USD",
  ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
  revisable = TRUE,
  notas = NULL
)

lista_final <- list(serie_id = serie_id, metadatos = meta_ia, observaciones = obs_final)
path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
update_catalogo(serie_id = serie_id, metadatos = meta_ia, metodo_etl = "API_MAE_FUTUROS", tema = tema_fijo)

message("¡Carga inicial finalizada con éxito!")