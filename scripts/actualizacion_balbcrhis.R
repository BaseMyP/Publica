# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN: Boletín Mensual de Deuda (Finanzas)
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando actualización semanal de boletin mensual: ", Sys.time())

# 1. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

catalogo_bolmenfinanzas <- catalogo_completo %>% filter(metodo_etl == "EXCEL_BOLMENFINANZAS")

if (nrow(catalogo_bolmenfinanzas) == 0) {
  message("No hay series configuradas para EXCEL_BALBCRHIS. Finalizando.")
  quit(save = "no")
}

# 2. Descargar el Excel
url <- "https://www.argentina.gob.ar/economia/finanzas/datos-mensuales"

# 2. Leer la página web
pagina <- read_html(url)

# 3. Extraer todos los enlaces (etiquetas <a>) y filtrar el que termine en .xlsx
url_excel <- pagina %>%
  html_nodes("a") %>%
  html_attr("href")
# Filtrar aquellos que contienen la estructura de archivos del sitio
url_excel <- url_excel[which(grepl("\\.xlsx$",url_excel))][1]
url_excel <- str_extract(url_excel,"http.*")
archivo_tmp <- tempfile(fileext = ".xlsx")

tryCatch({
  GET(url_excel, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# IMPORTANTE: Mantener el mismo 'skip' que usaste en la carga inicial
df_raw <- read_excel(archivo_tmp, sheet = "A.1", skip = 8)

hoy <- as.character(Sys.Date())

# 3. Bucle ALFRED para cruzar datos
for (i in 1:nrow(catalogo_bolmenfinanzas)) {
  serie_id <- catalogo_bolmenfinanzas$serie_id[i]
  tema <- basename(dirname(catalogo_bolmenfinanzas$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    # Extraemos el índice de la columna desde los metadatos (Ej: "6" para la Col F)
    col_index <- as.character(base_actual$metadatos$id_original)
    
    # Extraemos la fecha (col 1) y la columna objetivo
    df_serie <- df_raw %>%
      filter(`...1` %in% col_index) %>%
      t() %>% 
      as.data.frame()
    names(df_serie) <- df_serie[1,]
    df_serie <- df_serie[-1,]
    df_serie$fecha <- as.Date(as.numeric(row.names(df_serie)), origin="1899-12-30")
    index <- which(is.na(df_serie$fecha) & str_detect(rownames(df_serie),"[a-z]{3}-[0-9]{2}"))
    for(i in index) {
      df_serie$fecha[i] <- floor_date(df_serie$fecha[i-1] + days(35),"months")
    }
    df_serie <- df_serie[!is.na(df_serie$fecha),]
    index <- which(names(df_serie)!="fecha")
    for(i in index) {
      df_serie[,i] <- as.numeric(df_serie[,i])
    }
    df_serie <- df_serie %>% 
      mutate(valor= `ORGANISMOS INTERNACIONALES` + `ORGANISMOS OFICIALES`) %>% 
      select(fecha,valor) %>%
      arrange(fecha) 
    row.names(df_serie) <- NULL
    nuevo_df <- df_serie
    
    # Separar historia y vigencia
    obs_viejas <- base_actual$observaciones
    obs_vigentes <- obs_viejas %>% filter(realtime_end == "9999-12-31") %>% 
      mutate(fecha=as.Date(fecha))
    obs_historicas <- obs_viejas %>% filter(realtime_end != "9999-12-31") %>% 
      mutate(fecha=as.Date(fecha))
    
    # Cruzar datos
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
    
    obs_consolidadas <- bind_rows(obs_historicas, obs_vigentes_que_cambiaron, obs_vigentes_sin_cambio, nuevas_inserciones) %>% 
      arrange(fecha, realtime_start)
    
    # Actualizar JSON
    base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
    base_actual$observaciones <- obs_consolidadas
    
    write_json(base_actual, path_archivo, pretty = TRUE, auto_unbox = TRUE)
    message("✓ Actualizada: ", serie_id)
  }
}