
library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)
library(zoo)

# ------------------------------------------------------------
# 1. CARGA DE LOGICA CORE Y CONFIGURACIÓN
# ------------------------------------------------------------
# Cargamos tus funciones compartidas del proyecto
source("funciones_base.R")

# Ruta al nuevo archivo Excel publicado (el nuevo vintage a comparar)
archivo_excel <- "inputs/historicos/sh_oferta_demanda.xls" 

path_catalogo <- "catalogo.json"
tema_salida   <- "ACTIVIDAD"
hoy           <- as.character(Sys.Date())

if (!file.exists(path_catalogo)) {
  stop("ERROR: No se encontró el catálogo central 'catalogo.json'.")
}
catalogo <- fromJSON(path_catalogo)

# Configuración de los cuadros coincidente con la carga inicial
cuadros_config <- list(
  list(hoja = "cuadro 1", sufijo = "_CONSTANTES_Q"),
  list(hoja = "cuadro 8", sufijo = "_CORRIENTES_Q"),
  list(hoja = "cuadro 9", sufijo = "_PRECIOS_IMPLICITOS_Q")
)

# ------------------------------------------------------------
# 2. PROCESAMIENTO Y COMPARACIÓN DE VINTAGES
# ------------------------------------------------------------
if (!file.exists(archivo_excel)) {
  stop("ERROR: No se encontró el archivo Excel en la ruta: ", archivo_excel)
}

for (config in cuadros_config) {
  message("Leyendo hoja para actualización: ", config$hoja)
  
  raw <- read_excel(archivo_excel, sheet = config$hoja, col_names = FALSE)
  raw <- as.data.frame(raw)
  
  # Reconstrucción de la grilla horizontal de fechas
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
    
    # Identificación por patrones de las variables deseadas
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
      TRUE ~ NA_character_
    )
    
    # Excluimos explícitamente Objetos Valiosos, Discrepancia o no mapeados
    if (is.na(nombre_limpio)) next
    
    id_serie_final <- paste0("CN_", nombre_limpio, config$sufijo)
    path_json_existente <- file.path(tema_salida, paste0(id_serie_final, ".json"))
    
    # Si por alguna razón la serie no existe en formato JSON, se saltea para que lo maneje el script inicial
    if (!file.exists(path_json_existente)) {
      warning("La serie ", id_serie_final, " no existe de forma local. Correr primero la carga inicial.")
      next
    }
    
    # 1. Extraer los datos nuevos recién publicados (New Vintage)
    valores_crudos <- as.numeric(as.character(raw[r_idx, columnas_trimestrales]))
    
    df_entrante <- data.frame(
      fecha = fechas_series,
      valor = valores_crudos,
      stringsAsFactors = FALSE
    ) %>% filter(!is.na(valor))
    
    if (nrow(df_entrante) == 0) next
    
    # 2. Leer el archivo JSON actual para extraer su historia y metadatos
    json_viejo <- fromJSON(path_json_existente)
    obs_viejas <- as.data.frame(json_viejo$observaciones)
    metadatos  <- json_viejo$metadatos
    
    # 3. CONSOLIDACIÓN DE VINTAGES (Utilizando la lógica exacta de funciones_base.R)
    # Filtramos vigentes (end == 9999-12-31) e históricas ya cerradas
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    # Realizamos el left_join para trackear el status de cada celda de tiempo
    actualizadas <- df_entrante %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    # Si no hay datos nuevos ni datos revisados, la serie no cambió en este vintage
    if (!any(actualizadas$status %in% c("NUEVO", "REVISADO"))) {
      message("  -> Serie sin cambios detectados: ", id_serie_final)
      next
    }
    
    message("  -> Actualizando Vintage para: ", id_serie_final)
    
    # Cerrar el vintage de las observaciones que sufrieron revisión histórica (end = hoy)
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    # Mantener intactas las vigentes que no se modificaron
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    # Insertar los nuevos puntos o los revisados con la marca temporal de hoy
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    # Unificar todo el set cronológico ordenado por fecha y luego por start_date
    obs_consolidadas <- bind_rows(
      obs_historicas,
      obs_vigentes_que_cambiaron,
      obs_vigentes_sin_cambio,
      nuevas_inserciones
    ) %>% arrange(fecha, realtime_start)
    
    # 4. ACTUALIZACIÓN DEL ARCHIVO
    # Actualizamos la marca de tiempo de los metadatos
    metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    metadatos$fecha_inicio <- min(obs_consolidadas$fecha)
    
    salida_json <- list(
      serie_id = id_serie_final,
      metadatos = metadatos,
      observaciones = obs_consolidadas
    )
    
    # Sobreescribimos el JSON con la historia consolidada
    writeLines(toJSON(salida_json, auto_unbox = TRUE, pretty = TRUE), path_json_existente)
    
    # Exportamos el dataframe vigente al Entorno Global para análisis rápido en RStudio
    assign(id_serie_final, obs_consolidadas, envir = .GlobalEnv)
  }
}

message("\n[PROCESO DE ACTUALIZACIÓN FINALIZADO]")
