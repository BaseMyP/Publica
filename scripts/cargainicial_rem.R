library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(jsonlite)

rem <- read_excel("historico-relevamiento-expectativas-mercado.xlsx",
                  sheet = "Base de Datos Completa",skip = 1) %>% 
  mutate(Referencia = str_remove_all(Referencia,"; [a-z]{3}-[0-9]{2}$")) %>% 
  mutate(Referencia = str_remove_all(Referencia,"; Trim\\. [A-Z]{1,2}-[0-9]{2}$")) %>% 
  rename("realtime_start" = "Fecha de pronóstico",
         "valor"="Mediana") %>% 
  select("realtime_start","Variable","Referencia","Período","valor")

# 1. Armamos los diccionarios
dict_variable <- c(
  "Precios minoristas (IPC nivel general-GBA; INDEC)" = "IPCGBA",
  "Precios minoristas (IPC nivel general; INDEC)" = "IPCGENERAL",    
  "Tasa de política monetaria (Lebac)" = "TPMLEBAC",    
  "Tipo de cambio nominal" = "USDA3500",                               
  "Resultado primario del SPNF" = "SUPPRIMARIO", 
  "Resultado Primario del SPNF" = "SUPPRIMARIO", # Contempla la mayúscula
  "PIB a precios constantes" = "PIBPCTE",                          
  "Precios minoristas (IPC núcleo-GBA; INDEC)" = "IPCNUCLEOGBA",  
  "Tasa de política monetaria (Pase 7 días)" = "TPMPASE7",         
  "Precios minoristas (IPC núcleo; INDEC)" = "IPCNUCLEO",     
  "Tasa de política monetaria (LELIQ)" = "TPMLELIQ",        
  "Tasa de interés (LELIQ)" = "TASALELIQ",                          
  "Tasa de interés (BADLAR)" = "BADLAR",                  
  "Exportaciones" = "EXPO",                                    
  "Importaciones" = "IMPO",                             
  "Desocupación abierta" = "DESOCUPACION",                             
  "Tasa de interés (TAMAR)" = "TASATAMAR"
)

dict_referencia <- c(
  "var. % mensual" = "MOM",
  "var. % i.a." = "YOY",
  "TNA; %" = "TNA",
  "$/USD" = "LEVEL",
  "miles de millones $" = "BILARS",
  "var. % prom. anual" = "YOYAVG",
  "var. % trim. s.e." = "QOQ_SA",
  "millones de USD" = "MILUSD",
  "% de la PEA" = "RATE"
)

# 2. Procesamiento de la base
rem_limpio <- rem %>%
  mutate(realtime_start = as.Date(realtime_start)) %>%
  mutate(
    var_code = dict_variable[Variable],
    ref_code = dict_referencia[Referencia]
  ) %>%
  mutate(
    fecha_target = case_when(
      str_detect(Período, "^[0-9]{5}$") ~ as.Date(suppressWarnings(as.numeric(Período)), origin = "1899-12-30"),
      str_detect(Período, "^[0-9]{4}$") ~ as.Date(paste0(Período, "-01-01"), format = "%Y-%m-%d"),
      str_detect(Período, "^Trim\\.") ~ suppressWarnings(yq(paste0("20", str_sub(Período, -2), "-", str_extract(Período, "(I|II|III|IV)")))),
      
      # NUEVOS CASOS RELATIVOS: sumamos meses exactos a la fecha de publicación
      str_detect(Período, "Próx\\. 12 meses") ~ realtime_start %m+% months(12),
      str_detect(Período, "Próx\\. 24 meses") ~ realtime_start %m+% months(24),
      
      TRUE ~ as.Date(NA)
    ),
    
    frecuencia = case_when(
      str_detect(Período, "^[0-9]{5}$") ~ "M",
      str_detect(Período, "^[0-9]{4}$") ~ "A",
      str_detect(Período, "^Trim\\.") ~ "Q",
      # Los pronósticos a meses cerrados entran en la serie mensual:
      str_detect(Período, "Próx\\. 12 meses|Próx\\. 24 meses") ~ "M",
      
      TRUE ~ NA_character_
    )
  ) %>%
  # Construimos el nombre final de la serie
  #mutate(serie_id = paste("EXPECTATIVAS", var_code, ref_code, frecuencia, sep = "_")) %>%
  mutate(serie_id = paste(var_code, ref_code, frecuencia, sep = "_")) %>%
  
  # Filtramos posibles NAs residuales antes de armar el Vintage
  filter(!is.na(fecha_target) & !is.na(serie_id)) %>%
  
  # Lógica de ALFRED:
  group_by(serie_id, fecha_target) %>%
  arrange(realtime_start) %>%
  mutate(
    realtime_end = lead(realtime_start) - days(1),
    realtime_end = if_else(is.na(realtime_end), as.Date("9999-12-31"), realtime_end)
  ) %>%
  ungroup()

# Verificamos si logramos limpiar toda la base o si quedó algún "Período" raro sin mapear
cat("Filas con fecha_target NA:", sum(is.na(rem_limpio$fecha_target)), "\n")
head(rem_limpio)


# 1. Crear el directorio contenedor
#dir.create("REM", showWarnings = FALSE)

# 2. Identificar todas las series únicas generadas
series_unicas <- unique(rem_limpio$serie_id)

# Lista para almacenar temporalmente las nuevas filas del catálogo
nuevos_registros_catalogo <- list()
base_url <- "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/"

message(sprintf("Iniciando generación de %s series del REM...", length(series_unicas)))

# 3. Bucle de fraccionamiento
for (id in series_unicas) {
  
  # A. Aislar los datos de la serie específica
  datos_serie <- rem_limpio %>% filter(serie_id == id)
  
  # B. Extraer valores originales para armar metadatos descriptivos
  var_original <- datos_serie$Variable[1]
  ref_original <- datos_serie$Referencia[1]
  freq_letra <- datos_serie$frecuencia[1]
  
  # C. Construir el bloque de metadatos fijos
  metadatos <- list(
    titulo = paste("REM:", var_original, "-", ref_original),
    descripcion = paste("Pronósticos agregados del Relevamiento de Expectativas de Mercado (REM). Variable original:", var_original),
    pais = "Argentina",
    categoria = "INTERNACIONAL",
    frecuencia_short = freq_letra,
    frecuencia_original = ifelse(freq_letra=="M","mensual",ifelse(freq_letra=="A","anual",stop())),
    unidades = ref_original,
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "BCRA",
    fuente_original = "BCRA",
    fuente_formato = "EXCEL",
    # No incluimos id_api_bcra porque el REM se actualiza distinto a las series diarias
    ultima_actualizacion = datos_serie %>% slice_max(realtime_start) %>% pull(realtime_start),
    fecha_inicio = (datos_serie %>% slice_min(realtime_start) %>% pull(realtime_start))[1],
    url_original = "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/informes/historico-relevamiento-expectativas-mercado.xlsx",
    revisable = TRUE,
    notas = NA
  )
  
  # D. Estructurar el bloque de observaciones (formato ALFRED)
  observaciones <- datos_serie %>%
    select(
      fecha = fecha_target,
      valor = valor,
      realtime_start,
      realtime_end
    ) %>%
    mutate(
      fecha = as.character(fecha),
      realtime_start = as.character(realtime_start),
      realtime_end = as.character(realtime_end)
    ) %>%
    # Orden estricto: cronológico por fecha a pronosticar, y luego por fecha de publicación
    arrange(fecha, realtime_start) 
  
  # E. Empaquetar y exportar la serie
  lista_final <- list(
    serie_id = id,
    metadatos = metadatos,
    observaciones = observaciones
  )
  
  path_archivo <- file.path("EXPECTATIVAS", paste0(id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # F. Preparar la fila para el catálogo maestro
  nuevos_registros_catalogo[[id]] <- data.frame(
    serie_id = id,
    titulo = metadatos$titulo,
    frecuencia = metadatos$frecuencia_short,
    tipo_informacion = metadatos$tipo_informacion,
    raw_url = paste0(base_url, "EXPECTATIVAS/", id, ".json"), # <--- CAMBIO: Nueva carpeta
    metodo_etl = "EXCEL_REM",                                 # <--- CAMBIO: Agregamos el método ETL
    stringsAsFactors = FALSE
  )
}

# 4. Actualización Masiva del Catálogo Maestro
cat_path <- "catalogo.json"
df_nuevos_cat <- bind_rows(nuevos_registros_catalogo)

if (file.exists(cat_path)) {
  cat_actual <- fromJSON(cat_path)
  
  # Limpiamos las series del REM previas (si existían) para evitar duplicados
  cat_actual <- cat_actual %>% filter(!serie_id %in% df_nuevos_cat$serie_id)
  
  # Unimos la historia con las series recién procesadas
  cat_final <- bind_rows(cat_actual, df_nuevos_cat)
} else {
  cat_final <- df_nuevos_cat
}

write_json(cat_final, cat_path, pretty = TRUE)

message(sprintf("¡Éxito! Se exportaron %s archivos JSON en la carpeta EXPECTATIVAS/ y el Catálogo Maestro fue actualizado.", length(series_unicas)))