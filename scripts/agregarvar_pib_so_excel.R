
library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)
library(zoo)

# ------------------------------------------------------------
# 1. CONFIGURACIÓN DE RUTAS Y ARCHIVO EXCEL
# ------------------------------------------------------------
archivo_excel <- "inputs/historicos/sh_oferta_demanda.xls" 

path_catalogo <- "catalogo.json"
tema_salida   <- "ACTIVIDAD"
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

# Configuración de los cuadros y las hojas correspondientes del Excel
cuadros_config <- list(
  list(hoja = "cuadro 1", sufijo = "_CONSTANTES_Q", unidades = "Millones de pesos a precios de 2004", nota = "Precios constantes"),
  list(hoja = "cuadro 8", sufijo = "_CORRIENTES_Q", unidades = "Millones de pesos corrientes", nota = "Precios corrientes"),
  list(hoja = "cuadro 9", sufijo = "_PRECIOS_IMPLICITOS_Q", unidades = "Índice de precios implícitos (Base 2004 = 100)", nota = "Precios implícitos")
)

# ------------------------------------------------------------
# 2. PROCESAMIENTO SECUENCIAL DESDE LAS HOJAS DEL EXCEL
# ------------------------------------------------------------

if (!file.exists(archivo_excel)) {
  stop("ERROR CRÍTICO: No se encontró el archivo Excel original en la ruta: ", archivo_excel)
}

for (config in cuadros_config) {
  message("Procesando de forma visible desde Excel: ", config$hoja)
  
  raw <- read_excel(archivo_excel, sheet = config$hoja, col_names = FALSE)
  raw <- as.data.frame(raw)
  
  fila_anios <- as.character(raw[4, ])
  fila_trimestres <- as.character(raw[5, ])
  anios_limpios <- str_extract(fila_anios, "\\d{4}")
  anios_completos <- zoo::na.locf(anios_limpios, na.rm = FALSE)
  
  columnas_trimestrales <- which(!is.na(fila_trimestres) & fila_trimestres != "" & 
                                   fila_trimestres != "NA" & !is.na(anios_completos) & 
                                   str_detect(fila_trimestres, "(1º|2º|3º|4º)"))
  
  fechas_series <- sapply(columnas_trimestrales, function(idx) {
    anio <- anios_completos[idx]
    trim <- fila_trimestres[idx]
    mes <- case_when(
      str_detect(trim, "1º") ~ "01",
      str_detect(trim, "2º") ~ "04",
      str_detect(trim, "3º") ~ "07",
      str_detect(trim, "4º") ~ "10"
    )
    return(paste0(anio, "-", mes, "-01"))
  })
  
  for (r_idx in 6:nrow(raw)) {
    nombre_original <- trimws(as.character(raw[r_idx, 2]))
    if (is.na(nombre_original) || nombre_original == "" || nombre_original == "NA") next
    
    nombre_limpio <- case_when(
      str_detect(nombre_original, "Producto Interno Bruto") ~ "PIB",
      str_detect(nombre_original, "Importaciones") ~ "IMPORTACIONES",
      str_detect(nombre_original, "Oferta Global") ~ "OFERTA_GLOBAL",
      str_detect(nombre_original, "Demanda Global") ~ "DEMANDA_GLOBAL",
      str_detect(nombre_original, "Consumo privado") ~ "CONSUMO_PRIVADO",
      str_detect(nombre_original, "Consumo público") ~ "CONSUMO_PUBLICO",
      str_detect(nombre_original, "Exportaciones") ~ "EXPORTACIONES",
      str_detect(nombre_original, "Formación bruta de capital fijo") ~ "FBCF",
      str_detect(nombre_original, "Variación de existencias") ~ "VARIACION_EXISTENCIAS",
      str_detect(nombre_original, "Objetos valiosos") ~ "OBJETOS_VALIOSOS",
      str_detect(nombre_original, "Discrepancia estadística") ~ "DISCREPANCIA_ESTADISTICA",
      TRUE ~ NA_character_
    )
    
    # MODIFICACIÓN: Si no se reconoce la variable, u pertenece a los grupos excluidos, se saltea de inmediato
    if (is.na(nombre_limpio) || nombre_limpio %in% c("OBJETOS_VALIOSOS", "DISCREPANCIA_ESTADISTICA")) next
    
    id_serie_final <- paste0("CN_", nombre_limpio, config$sufijo)
    valores_crudos <- as.numeric(as.character(raw[r_idx, columnas_trimestrales]))
    
    message("  -> Generando datos y JSON para: ", id_serie_final)
    
    df_observaciones <- data.frame(
      fecha = fechas_series,
      valor = valores_crudos,
      realtime_start = hoy,
      realtime_end = "9999-12-31",
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(valor))
    
    if (nrow(df_observaciones) == 0) next
    
    assign(id_serie_final, df_observaciones, envir = .GlobalEnv)
    
    metadatos <- list(
      titulo = paste0(nombre_original, " - ", config$unidades),
      descripcion = paste0("Serie de Cuentas Nacionales INDEC: ", nombre_original),
      pais = "Argentina", categoria = "ACTIVIDAD", frecuencia_short = "Q", frecuencia_original = "trimestral",
      unidades = config$unidades, ajuste = "NSA", tipo_informacion = "Pública",
      fuente = "INDEC", fuente_original = "INDEC", fuente_formato = "Excel",
      ultima_actualizacion = paste0(hoy, "T12:00:00Z"), fecha_inicio = min(df_observaciones$fecha),
      url_original = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_06_26.xls",
      revisable = TRUE, notas = config$nota
    )
    
    salida_json <- list(serie_id = id_serie_final, metadatos = metadatos, observaciones = df_observaciones)
    
    path_salida_json <- file.path(tema_salida, paste0(id_serie_final, ".json"))
    writeLines(toJSON(salida_json, auto_unbox = TRUE, pretty = TRUE), path_salida_json)
    
    nueva_entrada_cat <- data.frame(
      serie_id = id_serie_final,
      titulo = metadatos$titulo,
      frecuencia = "Q",
      tipo_informacion = "Pública",
      raw_url = paste0("https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/", tema_salida, "/", id_serie_final, ".json"),
      metodo_etl = "EXCEL_CARPETA_INPUTS",
      stringsAsFactors = FALSE
    )
    
    catalogo <- catalogo %>% filter(serie_id != id_serie_final)
    catalogo <- bind_rows(catalogo, nueva_entrada_cat)
  }
}

# ------------------------------------------------------------
# 3. GUARDAR EL CATÁLOGO FÍSICO DEFINITIVO
# ------------------------------------------------------------
writeLines(toJSON(catalogo, auto_unbox = TRUE, pretty = TRUE), path_catalogo)

message("\n[PROCESO COMPLETADO EXITOSAMENTE]")