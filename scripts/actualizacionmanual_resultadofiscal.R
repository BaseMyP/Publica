source("scripts/funciones_base.R")

message("Iniciando revisión diaria de API INDEC / CSV: ", Sys.time())

# 1. Leer el catálogo
cat_path <- "catalogo.json"
catalogo_completo <- fromJSON(cat_path)

# 2. Filtrar solo las series que corresponden al INDEC
catalogo_indec <- catalogo_completo %>% 
  filter(metodo_etl == "API_INDEC")

if (nrow(catalogo_indec) == 0) {
  message("No hay series configuradas para API_INDEC. Finalizando script.")
  quit(save = "no")
}

hubo_actualizaciones <- FALSE
cache_csv <- list()

series_imig <- catalogo_indec %>% filter(stringr::str_detect(titulo,"^IMIG")) %>% pull(serie_id)

df_fuente <- tryCatch({
  read_csv("imig-mensual.csv", show_col_types = FALSE)
})
df_fuente2 <- tryCatch({
  read_csv2("imig-mensual-actualizacion.csv", show_col_types = FALSE)
})
df_fuente2 <- df_fuente2[-2,]
df_fuente2[,which(df_fuente2[1,]=="0.0")] <- 0
df_fuente2$indice_tiempo <- as.Date("2026-10-01")
df_fuente <- rbind(df_fuente,df_fuente2)


for (i in 1:length(series_imig)) {
  serie_id <- series_imig[i]
  raw_url <- catalogo_indec$raw_url[which(catalogo_indec$serie_id==serie_id)]
  
  tema <- basename(dirname(raw_url))
  path_archivo <- file.path(tema, paste0(serie_id, ".json"))
  
  if (file.exists(path_archivo)) {
    base_actual <- fromJSON(path_archivo)
    
    id_indec <- base_actual$metadatos$id_original
    metadatos_fijos <- base_actual$metadatos
    
    message(sprintf("\n[%s/%s] Procesando: %s (Carpeta: %s)", i, nrow(catalogo_indec), serie_id, tema))
    
    # Manejo de caché si la serie tiene configurado un CSV
    
    
    # La función update_indec_json_serie se encarga de todo (API o CSV)
    exito <- update_indec_json_serie(
      id_indec        = id_indec,
      serie_id        = serie_id,
      tema            = tema,
      metadatos_fijos = metadatos_fijos,
      df_csv          = df_fuente
    )
    
    if (isTRUE(exito)) hubo_actualizaciones <- TRUE
  } else {
    warning(sprintf("El archivo local no existe en la ruta: %s. Revisa si la carpeta o el nombre cambiaron.", path_archivo))
  }
}



