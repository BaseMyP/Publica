# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN: Índices de Precios y Cantidades (Expo e Impo - INDEC)
# ==============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)
library(stringr)
library(zoo)
source("scripts/funciones_base.R")

message("Iniciando actualización de Exportaciones e Importaciones (INDEC): ", Sys.time())

# 1. Leer Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# Filtramos las series que corresponden a este método ETL
catalogo_indec <- catalogo_completo %>% filter(metodo_etl == "EXCEL_WEB_INDEC")

if (nrow(catalogo_indec) == 0) {
  message("No hay series configuradas para EXCEL_WEB_INDEC. Finalizando.")
  quit(save = "no")
}

# 2. Función auxiliar para descargar y limpiar ambos Excels (DRY)
procesar_excel_indec <- function(url, nombres_columnas) {
  archivo_tmp <- tempfile(fileext = ".xls")
  tryCatch({
    download.file(url, destfile = archivo_tmp, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    stop("Fallo al descargar el archivo: ", url)
  })
  
  raw <- read_excel(archivo_tmp, col_names = FALSE)
  datos <- raw[6:nrow(raw), ]
  colnames(datos) <- nombres_columnas
  datos <- datos %>% select(-starts_with("v"))
  
  datos$anio <- as.character(datos$anio)
  datos$anio <- str_extract(datos$anio, "\\d{4}")
  datos$anio <- na.locf(datos$anio, na.rm = FALSE)
  
  meses <- c(Enero=1, Febrero=2, Marzo=3, Abril=4, Mayo=5, Junio=6, 
             Julio=7, Agosto=8, Septiembre=9, Octubre=10, Noviembre=11, Diciembre=12)
  
  datos %>%
    filter(!is.na(mes)) %>%
    mutate(
      anio = as.integer(anio),
      mes_num = meses[trimws(mes)],
      fecha = as.Date(sprintf("%04d-%02d-01", anio, mes_num))
    ) %>%
    filter(!is.na(fecha))
}

# 3. Descargar y procesar los dos DataFrames
cols_expo <- c("anio", "mes", "NG_VALOR", "NG_PRECIO", "NG_CANTIDAD", "v1", "PP_VALOR", "PP_PRECIO", "PP_CANTIDAD", "v2", "MOA_VALOR", "MOA_PRECIO", "MOA_CANTIDAD", "v3", "MOI_VALOR", "MOI_PRECIO", "MOI_CANTIDAD", "v4", "CE_VALOR", "CE_PRECIO", "CE_CANTIDAD")
cols_impo <- c("anio", "mes", "NG_VALOR", "NG_PRECIO", "NG_CANTIDAD", "v1", "BK_VALOR", "BK_PRECIO", "BK_CANTIDAD", "v2", "BI_VALOR", "BI_PRECIO", "BI_CANTIDAD", "v3", "CL_VALOR", "CL_PRECIO", "CL_CANTIDAD", "v4", "PA_VALOR", "PA_PRECIO", "PA_CANTIDAD", "v5", "BC_VALOR", "BC_PRECIO", "BC_CANTIDAD", "v6", "VA_VALOR", "VA_PRECIO", "VA_CANTIDAD")

url_expo <- "https://www.indec.gob.ar/ftp/cuadros/economia/serie_mensual_indices_expo.xls"
url_impo <- "https://www.indec.gob.ar/ftp/cuadros/economia/serie_mensual_indices_impo_ue.xls"

df_expo <- procesar_excel_indec(url_expo, cols_expo)
df_impo <- procesar_excel_indec(url_impo, cols_impo)

hoy <- as.character(Sys.Date())

# 4. Bucle ALFRED para actualizar los JSON
for (i in 1:nrow(catalogo_indec)) {
  serie_id <- catalogo_indec$serie_id[i]
  tema <- basename(dirname(catalogo_indec$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # Deducir dinámicamente el origen y la columna a buscar desde el serie_id
    # Ej: EXPO_NG_VALOR_... -> tipo="EXPO", columna="NG_VALOR"
    partes <- strsplit(serie_id, "_")[[1]]
    tipo_flujo <- partes[1] 
    columna_target <- paste(partes[2], partes[3], sep="_")
    
    # Elegir el dataframe correspondiente
    df_fuente <- if (tipo_flujo == "EXPO") df_expo else if (tipo_flujo == "IMPO") df_impo else NULL
    if (is.null(df_fuente)) next
    
    # Extraer la columna de datos nuevos
    nuevo_df <- df_fuente %>%
      select(fecha, valor = !!sym(columna_target)) %>%
      mutate(fecha = as.character(fecha), valor = as.numeric(valor)) %>%
      filter(!is.na(valor))
    
    # Lógica Vintage / ALFRED
    obs_viejas <- base_actual$observaciones
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31")
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31")
    
    actualizadas <- nuevo_df %>%
      left_join(obs_vigentes, by = "fecha", suffix = c("_nuevo", "_viejo")) %>%
      mutate(
        status = case_when(
          is.na(valor_viejo) ~ "NUEVO",
          round(valor_nuevo, 4) != round(valor_viejo, 4) ~ "REVISADO",
          TRUE ~ "SIN_CAMBIOS"
        )
      )
    
    obs_vigentes_que_cambiaron <- obs_vigentes %>%
      filter(fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"]) %>%
      mutate(realtime_end = hoy)
    
    obs_vigentes_sin_cambio <- obs_vigentes %>%
      filter(!fecha %in% actualizadas$fecha[actualizadas$status == "REVISADO"])
    
    nuevas_inserciones <- actualizadas %>%
      filter(status %in% c("NUEVO", "REVISADO")) %>%
      select(fecha, valor = valor_nuevo) %>%
      mutate(realtime_start = hoy, realtime_end = "9999-12-31")
    
    obs_consolidadas <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, 
                                  obs_vigentes_sin_cambio, nuevas_inserciones) %>% 
      arrange(fecha, realtime_start)
    
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
    message("✓ Actualizada: ", serie_id)
  }
}