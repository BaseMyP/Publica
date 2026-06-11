library(jsonlite)
library(dplyr)

base_url <- "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/"

# Descargar el catálogo de variables
cat_path <- "catalogo.json"
catalogo <- fromJSON(paste0(base_url,cat_path))

# Descarga serie del tipo de cambio
serie <- "MACRO/MACRO_USDA3500_NOMINAL_NSA_D.json"
tcA3500 <- fromJSON(paste0(base_url,serie))
tcA3500_metadatos <- tcA3500$metadatos
tcA3500_serie <-tcA3500$observaciones

# Expectativa de inflación anual para 2026 (REM) - serie histórica
serie <- "REM/REM_IPCGENERAL_YOY_A.json"
rem_ipc_2026 <- fromJSON(paste0(base_url,serie))
rem_ipc_2026_metadatos <- rem_ipc_2026$metadatos
rem_ipc_2026_serie <- rem_ipc_2026$observaciones %>% filter(fecha=="2026-01-01")

# Expectativa de inflación anual para 2026 (REM) - último dato
rem_ipc_2026_ult <- rem_ipc_2026$observaciones %>% filter(fecha=="2026-01-01",realtime_end=="9999-12-31")

# Expectativa de inflación anual para 2026 (REM) vigente a principios de año
rem_ipc_2026_ene26 <- rem_ipc_2026$observaciones %>% filter(fecha=="2026-01-01",realtime_end>="2026-01-01") %>% 
  slice_min(realtime_end)

