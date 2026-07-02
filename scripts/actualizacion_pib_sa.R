# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN: PIB y Componentes Desestacionalizados (INDEC)
# ==============================================================================

library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)
library(zoo)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando actualización de PIB Desestacionalizado: ", Sys.time())

# 1. Verificar si hay archivo nuevo en la carpeta inputs/
# Se espera que cuando haya datos nuevos subas el archivo con este nombre genérico
ruta_input <- "inputs/sh_oferta_demanda_desest.xls"

if (!file.exists(ruta_input)) {
  message("No se detectó un archivo nuevo (", ruta_input, "). Finalizando rutina.")
  quit(save = "no")
}

# 2. Validar Catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# Filtramos por el método ETL asignado
catalogo_pib <- catalogo_completo %>% filter(metodo_etl == "EXCEL_CARPETA_INPUTS")

if (nrow(catalogo_pib) == 0) {
  message("No hay series configuradas para EXCEL_CARPETA_INPUTS. Finalizando.")
  quit(save = "no")
}

# 3. Leer y procesar el nuevo Excel (misma lógica de tu script de carga)
raw <- read_excel(ruta_input, sheet = "desestacionalizado n", col_names = FALSE)
datos <- raw[7:nrow(raw), ]

colnames(datos) <- c("anio", "trimestre", "PIB", "IMPORTACIONES", 
                     "CONSUMO_PRIVADO", "CONSUMO_PUBLICO", "FBCF", "EXPORTACIONES")

datos$anio <- str_extract(as.character(datos$anio), "\\d{4}")
datos$anio <- na.locf(datos$anio, na.rm = FALSE)

trimestres <- c("I" = "01", "II" = "04", "III" = "07", "IV" = "10")

datos_procesados <- datos %>%
  filter(!is.na(trimestre)) %>%
  mutate(
    anio = as.integer(anio),
    mes = trimestres[trimws(trimestre)],
    fecha = as.Date(paste0(anio, "-", mes, "-01"))
  ) %>%
  filter(!is.na(fecha))

# Mapeo para relacionar los IDs de la base con las columnas del DataFrame
mapeo_columnas <- c(
  "CN_PBI_SA_T" = "PIB",
  "CN_CONSUMO_PRIVADO_SA_T" = "CONSUMO_PRIVADO",
  "CN_CONSUMO_PUBLICO_SA_T" = "CONSUMO_PUBLICO",
  "CN_FBCF_SA_T" = "FBCF",
  "CN_EXPORTACIONES_SA_T" = "EXPORTACIONES",
  "CN_IMPORTACIONES_SA_T" = "IMPORTACIONES"
)

hoy <- as.character(Sys.Date())

# 4. Bucle ALFRED para cruzar datos
for (i in 1:nrow(catalogo_pib)) {
  serie_id <- catalogo_pib$serie_id[i]
  tema <- basename(dirname(catalogo_pib$raw_url[i]))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo) && serie_id %in% names(mapeo_columnas)) {
    columna_target <- mapeo_columnas[[serie_id]]
    
    # Aislar la columna correspondiente
    nuevo_df <- datos_procesados %>%
      select(fecha, valor = !!sym(columna_target)) %>%
      mutate(fecha = as.character(fecha), valor = as.numeric(valor)) %>%
      filter(!is.na(valor))
    
    # Lógica de Revisiones
    base_actual <- fromJSON(path_archivo)
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

# 5. Mover el archivo para no volver a procesarlo mañana
if (!dir.exists("inputs/historicos")) dir.create("inputs/historicos", recursive = TRUE)
nuevo_nombre <- paste0("inputs/historicos/pib_actualizado_", format(Sys.Date(), "%Y%m%d"), ".xls")
file.rename(ruta_input, nuevo_nombre)

message("✓ Proceso finalizado exitosamente. Archivo resguardado en: ", nuevo_nombre)