
library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)

# ------------------------------------------------------------
# 1. CONFIGURACIÓN DE RUTAS Y ARCHIVO EXCEL
# ------------------------------------------------------------
# Ajustá el nombre si tu archivo Excel real se llama de otra manera en tu directorio
archivo_excel <- "inputs/historicos/nacional_serie_remuneraciones_mensual.xlsx" 

path_catalogo <- "catalogo.json"
tema_salida   <- "LABORAL"   # Nueva carpeta destino para estas series
hoy           <- as.character(Sys.Date())

# Crear la carpeta física si no existe
if (!dir.exists(tema_salida)) {
  dir.create(tema_salida, recursive = TRUE, showWarnings = FALSE)
}

# Cargar catálogo o inicializarlo en el Entorno Global
if (file.exists(path_catalogo)) {
  catalogo <- fromJSON(path_catalogo)
} else {
  catalogo <- data.frame(
    serie_id = character(), titulo = character(), frecuencia = character(),
    tipo_informacion = character(), raw_url = character(), metodo_etl = character(),
    stringsAsFactors = FALSE
  )
}

# Configuración específica de las dos columnas de la hoja "C 1"
# Columna 3 es la C (Original) y Columna 6 es la F (Desestacionalizada)
columnas_config <- list(
  list(
    col_idx = 3, 
    id_base = "SALARIOS_PRIV_NOMINAL_NSA_M", 
    ajuste = "NSA",
    titulo_meta = "Remuneración promedio por todo concepto de los trabajadores registrados del sector privado en pesos - Serie Original",
    nota = "Serie original con estacionalidad, expresada en pesos a valores corrientes."
  ),
  list(
    col_idx = 6, 
    id_base = "SALARIOS_PRIV_NOMINAL_SA_M", 
    ajuste = "SA",
    titulo_meta = "Remuneración promedio normal y permanente de los trabajadores registrados del sector privado en pesos - Serie Desestacionalizada",
    nota = "Serie desestacionalizada libre de efectos estacionales y calendarios, expresada en pesos a valores corrientes."
  )
)






# ------------------------------------------------------------
# 2. PROCESAMIENTO VERTICAL DE LA HOJA "C 1"
# ------------------------------------------------------------
if (!file.exists(archivo_excel)) {
  stop("ERROR CRÍTICO: No se encontró el archivo Excel en la ruta: ", archivo_excel)
}

message("Leyendo hoja 'C 1' desde Excel...")
# Leemos desde la fila 6 para evitar títulos decorativos
raw <- read_excel(archivo_excel, sheet = "C 1", skip = 5, col_names = FALSE)
raw <- as.data.frame(raw)

# --- SOLUCIÓN CRÍTICA: Convertir períodos a fechas reales de forma robusta ---
# Si readxl leyó la columna como POSIXct (Fecha), janitor/Excel serial, o texto, esto lo estandariza:
fechas_limpias <- tryCatch({
  # Intentamos la conversión directa si readxl ya lo leyó como objeto temporal o texto ISO
  as.Date(raw[, 1])
}, error = function(e) {
  # Si falla (por ejemplo, si son números de serie de Excel guardados como texto), los recuperamos
  as.Date(as.numeric(as.character(raw[, 1])), origin = "1899-12-30")
})

# Agregamos temporalmente la fecha limpia al dataframe para filtrar con seguridad
raw$fecha_procesada <- as.character(fechas_limpias)

# Filtramos filas donde la fecha procesada sea válida (Año-Mes-Día)
# Esto descarta de forma automática las notas aclaratorias y celdas vacías del final del Excel
raw_filtrado <- raw %>% 
  filter(!is.na(fecha_procesada) & fecha_procesada != "" & str_detect(fecha_procesada, "^\\d{4}-\\d{2}-\\d{2}"))

# Extraemos el vector definitivo de fechas homogéneas
fechas_series <- raw_filtrado$fecha_procesada

# ------------------------------------------------------------
# Procesar cada una de las dos series configuradas
# ------------------------------------------------------------
for (config in columnas_config) {
  id_serie_final <- config$id_base
  
  # Extraemos los valores correspondientes a las filas que superaron el filtro de fechas
  valores_crudos <- as.numeric(as.character(raw_filtrado[, config$col_idx]))
  
  message("  -> Generando datos y JSON para: ", id_serie_final)
  message("     (Cantidad de fechas: ", length(fechas_series), " | Cantidad de valores: ", length(valores_crudos), ")")
  
  # Crear el dataframe de observaciones bajo el estándar Real-Time de forma segura
  df_observaciones <- data.frame(
    fecha          = fechas_series,
    valor          = valores_crudos,
    realtime_start = hoy,
    realtime_end   = "9999-12-31",
    stringsAsFactors = FALSE
  ) %>% filter(!is.na(valor))
  
  if (nrow(df_observaciones) == 0) next
  
  # Registrar la serie en tu panel global de RStudio para verificación
  assign(id_serie_final, df_observaciones, envir = .GlobalEnv)
  
  # Estructurar metadatos homogéneos (Tus especificaciones)
  metadatos <- list(
    titulo              = config$titulo_meta,
    descripcion         = "Remuneración nominal de los trabajadores asalariados registrados del sector privado a partir de las declaraciones juradas del SIPA.",
    pais                = "Argentina",
    categoria           = "LABORAL",
    frecuencia_short    = "M",
    frecuencia_original = "mensual",
    unidades            = "Pesos corrientes",
    ajuste              = config$ajuste,
    tipo_informacion    = "Pública",
    fuente              = "OEDE - Secretaría de Trabajo",
    fuente_original     = "SIPA",
    fuente_formato      = "Excel",
    ultima_actualizacion= paste0(hoy, "T12:00:00Z"),
    fecha_inicio        = min(df_observaciones$fecha),
    url_original        = "https://www.argentina.gob.ar/trabajo/estadisticas/oede-estadisticas-nacionales#1",
    revisable           = TRUE,
    notas               = config$nota
  )
  
  salida_json <- list(serie_id = id_serie_final, metadatos = metadatos, observaciones = df_observaciones)
  
  # Guardar archivo JSON físico en la nueva carpeta INGRESOS/
  path_salida_json <- file.path(tema_salida, paste0(id_serie_final, ".json"))
  writeLines(toJSON(salida_json, auto_unbox = TRUE, pretty = TRUE), path_salida_json)
  
  # Inyectar/actualizar fila en el dataframe central del catálogo
  nueva_entrada_cat <- data.frame(
    serie_id         = id_serie_final,
    titulo           = metadatos$titulo,
    frecuencia       = "M",
    tipo_informacion = "Pública",
    raw_url          = paste0("https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/", tema_salida, "/", id_serie_final, ".json"),
    metodo_etl       = "EXCEL_CARPETA_INPUTS",
    stringsAsFactors = FALSE
  )
  
  catalogo <- catalogo %>% filter(serie_id != id_serie_final)
  catalogo <- bind_rows(catalogo, nueva_entrada_cat)
}

# ------------------------------------------------------------
# 3. GUARDAR EL CATÁLOGO FÍSICO DEFINITIVO
# ------------------------------------------------------------
writeLines(toJSON(catalogo, auto_unbox = TRUE, pretty = TRUE), path_catalogo)

message("\n[PROCESO COMPLETADO EXITOSAMENTE]")
