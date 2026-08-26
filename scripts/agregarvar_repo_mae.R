# ==============================================================================
# SCRIPT DE CARGA INICIAL: TASAS REPO (MAE)
# ==============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de REPOs desde MAE...")

# 1. Configuración de Series
series_a_procesar <- list(
  list(ticker_mae = "REPO", serie_id = "REPO_MAE_TASAPP_NSA_D", titulo = "Tasa REPO (Promedio Ponderado MAE)"),
  list(ticker_mae = "REPX", serie_id = "REPX_MAE_TASAPP_NSA_D", titulo = "Tasa REPX (Promedio Ponderado MAE)"),
  list(ticker_mae = "SIMU", serie_id = "SIMU_MAE_TASAPP_NSA_D", titulo = "Tasa Simultáneas SIMU (Promedio Ponderado MAE)"),
  list(ticker_mae = "REPI", serie_id = "REPI_MAE_TASAPP_NSA_D", titulo = "Tasa REPI (Promedio Ponderado MAE)")
)

# 2. Descarga de datos
fecha_desde <- "2025-09-01" 
fecha_hasta <- as.character(Sys.Date())
query_json <- paste0('{"fechaDesde":"', fecha_desde, '","fechaHasta":"', fecha_hasta, '"}')
query_encoded <- URLencode(query_json, reserved = TRUE)

#url_mae <- paste0("https://api.marketdata.mae.com.ar/api/mercado/repo/titulosfecha?oTitulo=", query_encoded)
url_mae <- paste0("https://api.marketdata.mae.com.ar/api/mercado/titulo/historicorepo?oTitulo=", query_encoded)

respuesta <- GET(url_mae, user_agent("Mozilla/5.0"))

if (status_code(respuesta) != 200) stop("Error al conectar con la API del MAE")

# 3. Parsear JSON y Desanidar
df_raw <- fromJSON(content(respuesta, as = "text", encoding = "UTF-8"))
if (length(df_raw) == 0 || nrow(df_raw) == 0) stop("La API no devolvió datos.")

df_detalles <- bind_rows(df_raw$details) %>% 
  filter(moneda == "$") %>%
  mutate(
    fecha = as.character(as.Date(fecha)),
    plazo = as.numeric(plazo),
    valor = as.numeric(tasaPP)
  )

hoy <- as.character(Sys.Date())
tema_fijo <- "TC_y_TASAS"
if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Iteración y Estructuración ALFRED
for (item in series_a_procesar) {
  message("Procesando ", item$ticker_mae, "...")
  
  df_limpio <- df_detalles %>%
    filter(rueda == item$ticker_mae) %>%
    filter(!is.na(fecha) & !is.na(valor)) %>%
    group_by(fecha) %>%
    slice_min(order_by = plazo, n = 1, with_ties = FALSE) %>% # Plazo más corto del día
    ungroup() %>%
    select(fecha, valor) %>%
    arrange(fecha)
  
  if (nrow(df_limpio) == 0) {
    message("Sin datos para ", item$ticker_mae)
    next
  }
  
  obs_final <- df_limpio %>% mutate(realtime_start = hoy, realtime_end = "9999-12-31")
  
  meta_repo <- list(
    titulo = item$titulo,
    descripcion = paste("Tasa de interés promedio ponderada para operaciones de", item$ticker_mae, "en pesos. Se selecciona diariamente el plazo más corto disponible."),
    pais = "Argentina",
    categoria = tema_fijo,
    frecuencia_short = "D",
    unidades = "Porcentaje (TNA)",
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "Mercado Abierto Electrónico (MAE)",
    fuente_original = "MAE",
    fuente_formato = "API_MAE",
    id_original = item$ticker_mae,
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    revisable = TRUE,
    notas = NULL
  )
  
  lista_final <- list(serie_id = item$serie_id, metadatos = meta_repo, observaciones = obs_final)
  path_archivo <- file.path(tema_fijo, paste0(item$serie_id, ".json"))
  
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  update_catalogo(serie_id = item$serie_id, metadatos = meta_repo, metodo_etl = "API_MAE", tema = tema_fijo)
  message("✓ Creada: ", item$serie_id)
}
message("¡Carga inicial finalizada!")