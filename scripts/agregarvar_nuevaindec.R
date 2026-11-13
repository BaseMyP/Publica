# ==============================================================================
# SCRIPT PARA AGREGAR NUEVAS SERIES DE LA NUEVA API INDEC AL CATÁLOGO Y REPO
# ==============================================================================

library(jsonlite)
library(dplyr)
library(httr2)
library(purrr)
library(stringr)
library(lubridate)
source("scripts/funciones_base.R")

agregar_serie_nuevaindec <- function(serie_id,
                                     tema,
                                     nombre,
                                     descripcion,
                                     frecuencia = "Mensual",
                                     unidades = "Índice Base Dic-2016 = 100",
                                     region = "Nacional",
                                     nivel = "Nivel General",
                                     tipo_indicador = "Indice",
                                     filtros_extra = list(),
                                     usuario_repo = "tu_usuario/tu_repo") {
  
  parametros_api <- list(
    region         = region,
    nivel          = nivel,
    tipo_indicador = tipo_indicador,
    filtros_extra  = filtros_extra
  )
  
  metadatos_fijos <- list(
    nombre               = nombre,
    descripcion          = descripcion,
    fuente               = "INDEC",
    frecuencia           = frecuencia,
    unidades             = unidades,
    metodo_etl           = "Nueva_API_INDEC",
    parametros_api       = parametros_api,
    ultima_actualizacion = paste0(Sys.Date(), "T12:00:00Z")
  )
  
  # 1. Crear el archivo JSON de la serie con toda su historia
  exito <- update_nuevaindec_json_serie(
    serie_id        = serie_id,
    tema            = tema,
    metadatos_fijos = metadatos_fijos,
    params_api      = parametros_api
  )
  
  if (!exito) stop("No se pudo inicializar el archivo JSON para ", serie_id)
  
  # 2. Actualizar catalogo.json
  cat_path <- "catalogo.json"
  catalogo <- fromJSON(cat_path)
  
  raw_url <- sprintf("https://raw.githubusercontent.com/%s/main/%s/%s.json", 
                     usuario_repo, tema, serie_id)
  
  nueva_fila <- data.frame(
    serie_id   = serie_id,
    nombre     = nombre,
    tema       = tema,
    frecuencia = frecuencia,
    unidades   = unidades,
    fuente     = "INDEC",
    metodo_etl = "Nueva_API_INDEC",
    raw_url    = raw_url,
    stringsAsFactors = FALSE
  )
  
  # Evitar duplicados si ya existía en el catálogo
  catalogo_actualizado <- catalogo %>%
    filter(serie_id != !!serie_id) %>%
    bind_rows(nueva_fila)
  
  write_json(catalogo_actualizado, cat_path, pretty = TRUE, auto_unbox = TRUE)
  message("✓ Catálogo actualizado con la serie: ", serie_id)
}

# --- EJEMPLO DE USO (comentado) ---
# agregar_serie_nuevaindec(
#   serie_id       = "IPCESTACIONAL_INDICE_NSA_M",
#   tema           = "precios",
#   nombre         = "IPC Estacional - Índice Nacional",
#   descripcion    = "Índice de Precios al Consumidor. Categoría Estacional. Total Nacional.",
#   region         = "Nacional",
#   nivel          = "Categoría",
#   tipo_indicador = "Indice",
#   filtros_extra  = list(codigo = "eq.E")
# )