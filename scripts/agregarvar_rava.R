
library(dplyr)
library(jsonlite)
library(lubridate)

source("scripts/funciones_base.R")

# -----------------------------------------------------------------------------
# 1. Función ETL para procesar el CSV local
# -----------------------------------------------------------------------------
get_merval_monthly_from_csv <- function(path_csv) {
  if (!file.exists(path_csv)) {
    stop(paste0("El archivo no existe en la ruta especificada: '", path_csv, "'"))
  }
  
  message("Procesando datos diarios del MERVAL desde el CSV...")
  
  df_raw <- read.csv(path_csv, stringsAsFactors = FALSE)
  colnames(df_raw) <- tolower(trimws(colnames(df_raw)))
  
  if (!all(c("fecha", "cierre") %in% colnames(df_raw))) {
    stop("El CSV debe contener al menos las columnas 'fecha' y 'cierre'.")
  }
  
  df_daily <- df_raw %>%
    select(fecha, cierre) %>%
    mutate(
      fecha = parse_date_time(fecha, orders = c("Ymd", "dmY", "Y-m-d", "d/m/Y")),
      cierre_chr = as.character(cierre),
      cierre = if_else(
        grepl(",", cierre_chr),
        gsub(",", ".", gsub("\\.", "", cierre_chr)),
        cierre_chr
      ),
      cierre = as.numeric(cierre)
    ) %>%
    filter(!is.na(fecha) & !is.na(cierre)) %>%
    arrange(fecha) %>%
    mutate(periodo = format(fecha, "%Y-%m"))
  
  df_monthly <- df_daily %>%
    group_by(periodo) %>%
    summarise(
      cierre_fin_de_mes = dplyr::last(cierre),
      promedio_mensual  = mean(cierre, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      fecha = as.Date(paste0(periodo, "-01"))
    )
  
  return(df_monthly)
}

# -----------------------------------------------------------------------------
# 2. Configuración de Parámetros y Metadatos
# -----------------------------------------------------------------------------

FECHA_HOY <- format(Sys.Date(), "%Y-%m-%d")

# Ruta al archivo CSV input
PATH_CSV_INPUT <- "inputs/historicos/MERVAL - Cotizaciones historicas.csv"

# Identificadores para el repositorio y categoría
tema_fijo <- "FINANZAS"
serie_id_merval_eom <- "MERVAL_FINMES_NSA_M"
serie_id_merval_avg <- "MERVAL_PROMEDIO_NSA_M"

# Metadatos: Cierre Fin de Mes
meta_merval_eom <- list(
  titulo = "Índice S&P MERVAL - Cierre Fin de Mes",
  descripcion = "Índice S&P MERVAL cotización al cierre del último día hábil del mes.",
  pais = "ARG",
  categoria = "FINANZAS",
  frecuencia_short = "M",
  frecuencia_long = "mensual",
  frecuencia_original = "diaria",
  unidades = "Puntos",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "CSV publicado por RAVA",
  fuente_original = "Bolsas y Mercados Argentinos (BYMA)",
  fuente_formato = "CSV",
  id_original = "MERVAL",
  ultima_actualizacion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  fecha_inicio = "2025-07-01",
  url_original = "https://www.rava.com/perfil/MERVAL",
  revisable = FALSE,
  notas = "Calculado como el último valor registrado en el mes a partir de la serie diaria."
)

# Metadatos: Promedio Mensual
meta_merval_avg <- list(
  titulo = "Índice S&P MERVAL - Promedio Mensual",
  descripcion = "Índice S&P MERVAL promedio de las cotizaciones de cierre diarias del mes.",
  pais = "ARG",
  categoria = "FINANZAS",
  frecuencia_short = "M",
  frecuencia_long = "mensual",
  frecuencia_original = "diaria",
  unidades = "Puntos",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "CSV publicado por RAVA",
  fuente_original = "Bolsas y Mercados Argentinos (BYMA)",
  fuente_formato = "CSV",
  id_original = "MERVAL",
  ultima_actualizacion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  fecha_inicio = "2025-07-01",
  url_original = "https://www.rava.com/perfil/MERVAL",
  revisable = FALSE,
  notas = "Promedio simple mensual de los valores diarios de cierre."
)

# -----------------------------------------------------------------------------
# 3. Carga Inicial con Vintages
# -----------------------------------------------------------------------------

message("Iniciando carga inicial del S&P MERVAL desde CSV con componentes de vintages...")

tryCatch({
  datos_merval <- get_merval_monthly_from_csv(PATH_CSV_INPUT)
  
  dir.create(tema_fijo, showWarnings = FALSE)
  
  # A. Serie: Cierre Fin de Mes
  merval_eom_obs <- datos_merval %>%
    transmute(
      fecha = format(fecha, "%Y-%m-%d"),
      valor = cierre_fin_de_mes,
      realtime_start = FECHA_HOY,
      realtime_end = "9999-12-31"
    )
  
  write_json(
    list(metadatos = meta_merval_eom, observaciones = merval_eom_obs),
    path = file.path(tema_fijo, paste0(serie_id_merval_eom, ".json")),
    pretty = TRUE, auto_unbox = TRUE
  )
  
  update_catalogo(
    serie_id = serie_id_merval_eom,
    metadatos = meta_merval_eom,
    metodo_etl = "CSV_CARPETA_INPUTS",
    tema = tema_fijo
  )
  
  # B. Serie: Promedio Mensual
  merval_avg_obs <- datos_merval %>%
    transmute(
      fecha = format(fecha, "%Y-%m-%d"),
      valor = promedio_mensual,
      realtime_start = FECHA_HOY,
      realtime_end = "9999-12-31"
    )
  
  write_json(
    list(metadatos = meta_merval_avg, observaciones = merval_avg_obs),
    path = file.path(tema_fijo, paste0(serie_id_merval_avg, ".json")),
    pretty = TRUE, auto_unbox = TRUE
  )
  
  update_catalogo(
    serie_id = serie_id_merval_avg,
    metadatos = meta_merval_avg,
    metodo_etl = "CSV_CARPETA_INPUTS",
    tema = tema_fijo
  )
  
  message("¡Carga finalizada con éxito! Las series JSON incluyen la clave 'observaciones' y las marcas de realtime.")
  
}, error = function(e) {
  msg <- conditionMessage(e)
  if (length(msg) == 0 || msg == "") msg <- paste(e, collapse = " ")
  message("Error durante el procesamiento: ", msg)
})


