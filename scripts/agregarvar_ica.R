# ==============================================================================
# SCRIPT DE CARGA INICIAL: INTERCAMBIO COMERCIAL ARGENTINO (ICA)
# ==============================================================================

library(readr)
library(dplyr)
library(jsonlite)
source("scripts/funciones_base.R")

url_ica <- "https://infra.datos.gob.ar/catalog/sspm/dataset/74/distribution/74.3/download/intercambio-comercial-argentino-mensual.csv"
tema_fijo <- "SECTOR_EXTERNO"

message("Descargando dataset ICA desde datos.gob.ar...")
df_ica <- tryCatch({
  read_csv(url_ica, show_col_types = FALSE)
}, error = function(e) {
  stop("Error al descargar el archivo CSV: ", e$message)
})

# Identificar la columna temporal
col_fecha <- names(df_ica)[grepl("^(indice_tiempo|fecha|time|date)", names(df_ica), ignore.case = TRUE)][1]
if (is.na(col_fecha)) {
  stop("No se encontró una columna de fecha válida en el archivo.")
}

# Eliminar columnas que son suma de otras dos
df_ica <- df_ica %>% 
  select(-ica_bienes_capital_partes_piezas,
         -ica_bienes_intermedios_combustibles_lubricantes,
         -ica_importaciones_bs_consumo_vehiculos_automotor_pasajeros)

# Renombrar
names_old <- names(df_ica)

names(df_ica) <- c("indice_tiempo",
                   "EXPO_TOTAL",
                   "EXPO_PP",
                   "EXPO_MOA",
                   "EXPO_MOI",
                   "EXPO_CyE",
                   "IMPO_TOTAL",
                   "IMPO_BIENES_CAPITAL",
                   "IMPO_BIENES_INTERMEDIOS",
                   "IMPO_CyL",
                   "IMPO_BIENES_PIEZASyACCESORIOS",
                   "IMPO_BIENES_CONSUMO",
                   "IMPO_VEHICULOS",
                   "IMPO_RESTO",
                   "SALDO_COMERCIAL")


columnas_series <- setdiff(names(df_ica), col_fecha)

# Diccionario de nombres descriptivos (opcional para títulos claros)
obtener_titulo <- function(id_col) {
  titulos <- list(
    "EXPO_TOTAL"             = "ICA. Exportaciones Totales (FOB). Millones de USD",
    "EXPO_PP"             = "ICA. Exportaciones Totales (FOB). Productos Primarios. Millones de USD",
    "EXPO_MOA"           = "ICA. Exportaciones Totales (FOB). MOA. Millones de USD",
    "EXPO_MOI"     = "ICA. Exportaciones Totales (FOB). MOI. Millones de USD",
    "EXPO_CyE"     = "ICA. Exportaciones Totales (FOB). CyE. Millones de USD",
    "IMPO_TOTAL"             = "ICA. Importaciones Totales (CIF). Millones de USD",
    "IMPO_BIENES_CAPITAL"             = "ICA. Importaciones Totales (CIF). Bienes de Capital. Millones de USD",
    "IMPO_BIENES_INTERMEDIOS"             = "ICA. Importaciones Totales (CIF). Bienes intermedios. Millones de USD",
    "IMPO_CyL"             = "ICA. Importaciones Totales (CIF). Combustibles y Lubricantes. Millones de USD",
    "IMPO_BIENES_PIEZASyACCESORIOS"             = "ICA. Importaciones Totales (CIF). Piezas y accesorios. Millones de USD",
    "IMPO_BIENES_CONSUMO"             = "ICA. Importaciones Totales (CIF). Bienes de Consumo. Millones de USD",
    "IMPO_VEHICULOS"             = "ICA. Importaciones Totales (CIF). Vehículos. Millones de USD",
    "IMPO_RESTO"             = "ICA. Importaciones Totales (CIF). Resto. Millones de USD",
    "SALDO_COMERCIAL"             = "ICA. Saldo Comercial. Millones de USD"
  )
  if (id_col %in% names(titulos)) titulos[[id_col]] else gsub("_", " ", id_col)
}

message(sprintf("Procesando %s series encontradas en el dataset...", length(columnas_series)))

for (col in columnas_series) {
  serie_id <- paste0("ICA_", toupper(col), "_NSA_M")
  
  df_serie <- df_ica %>%
    select(fecha = all_of(col_fecha), valor = all_of(col)) %>%
    filter(!is.na(valor)) %>%
    mutate(fecha = as.character(as.Date(fecha))) %>%
    arrange(fecha)
  
  if (nrow(df_serie) == 0) next
  
  metadatos <- list(
    titulo               = obtener_titulo(col),
    descripcion          = obtener_titulo(col),
    pais                 = "Argentina",
    categoria            = tema_fijo,
    frecuencia_short     = "M",
    frecuencia_original  = "mensual",
    unidades             = "Millones de USD",
    ajuste               = "NSA",
    tipo_informacion     = "Pública",
    fuente               = "SSPM / INDEC",
    fuente_original      = "INDEC",
    fuente_formato       = "CSV_ICA",
    id_original          = names_old[which(names(df_ica)==col)],
    ultima_actualizacion = Sys.Date(),
    fecha_inicio         = min(as.Date(df_serie$fecha)),
    url_original         = url_ica,
    revisable            = TRUE,
    notas                = "Serie extraída de la distribución mensual SSPM"
  )
  
  # Estructura con soporte de formato vintage
  # Cada registro conserva el valor observado y las marcas de auditoría histórica
  datos_vintage <- df_serie %>%
    mutate(
      realtime_start = Sys.Date(),
      realtime_end   = "9999-12-31"
    ) %>%
    select(fecha, realtime_start, realtime_end, valor)
  
  objeto_json <- list(
    metadatos = metadatos,
    observaciones     = datos_vintage
  )
  
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(objeto_json, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar catálogo
  update_catalogo(
    serie_id   = serie_id,
    metadatos  = metadatos,
    metodo_etl = "CSV_ICA",
    tema       = tema_fijo
  )
  
  message(sprintf("✓ Serie creada y catalogada: %s", serie_id))
}

message("¡Carga inicial finalizada con éxito!")