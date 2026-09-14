
library(httr)
library(jsonlite)
library(dplyr)
library(readxl)
library(purrr)

source("scripts/funciones_base.R")

# ============================================================
# 1. MAPEO DE COLUMNAS A NOMBRES DE SERIE
# ============================================================

paises_map <- list(
  "Brasil"         = list(id_suffix = "BRASIL",         nombre = "Brasil"),
  "Canadá"         = list(id_suffix = "CANADA",         nombre = "Canadá"),
  "Chile"          = list(id_suffix = "CHILE",          nombre = "Chile"),
  "Estados Unidos" = list(id_suffix = "ESTADOS_UNIDOS", nombre = "Estados Unidos"),
  "México"         = list(id_suffix = "MEXICO",         nombre = "México"),
  "Uruguay"        = list(id_suffix = "URUGUAY",        nombre = "Uruguay"),
  "China"          = list(id_suffix = "CHINA",          nombre = "China"),
  "India"          = list(id_suffix = "INDIA",          nombre = "India"),
  "Japón"          = list(id_suffix = "JAPON",          nombre = "Japón"),
  "Reino Unido"    = list(id_suffix = "REINO_UNIDO",    nombre = "Reino Unido"),
  "Suiza"          = list(id_suffix = "SUIZA",          nombre = "Suiza"),
  "Zona Euro"      = list(id_suffix = "ZONA_EURO",      nombre = "Zona Euro"),
  "Vietnam"        = list(id_suffix = "VIETNAM",        nombre = "Vietnam")
)

# ============================================================
# 2. PARÁMETROS Y DESCARGA DEL EXCEL DESDE EL BCRA
# ============================================================

url_excel <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/ITCRMSerie.xlsx"

fetch_ponderadores_itcrm <- function() {
  
  temp_file <- tempfile(fileext = ".xlsx")
  
  message("> Solicitando Excel al BCRA: ", url_excel)
  
  res <- tryCatch(
    {
      GET(
        url_excel,
        write_disk(temp_file, overwrite = TRUE),
        timeout(60),
        user_agent("Mozilla/5.0")
      )
    },
    error = function(e) {
      message("  [ERROR DE RED] ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(res) || status_code(res) != 200) {
    message("  [ERROR HTTP] No se pudo descargar el archivo del BCRA.")
    return(NULL)
  }
  
  df_raw <- tryCatch(
    {
      read_excel(temp_file, sheet = "Ponderadores", skip = 1)
    },
    error = function(e) {
      message("  [ERROR AL LEER EXCEL] ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(df_raw) || nrow(df_raw) == 0) {
    message("  [ERROR] La hoja 'Ponderadores' está vacía o no existe.")
    return(NULL)
  }
  
  col_fecha <- names(df_raw)[1]
  
  df_clean <- df_raw %>%
    filter(!is.na(.data[[col_fecha]])) %>%
    mutate(
      fecha = as.character(as.Date(.data[[col_fecha]]))
    )
  
  return(df_clean)
}

# ============================================================
# 3. GUARDAR / ACTUALIZAR JSON DE CADA PAÍS
# ============================================================

update_ponderador_json_serie <- function(
    col_nombre,
    id_suffix,
    pais_nombre,
    df_datos
) {
  
  tema_destino <- "SECTOR_EXTERNO"
  
  serie_id <- paste0("PONDERADOR_ITCRM_", id_suffix, "_M")
  
  path_dir <- file.path(tema_destino)
  path_archivo <- file.path(tema_destino, paste0(serie_id, ".json"))
  
  hoy <- as.character(Sys.Date())
  codigo_original <- paste0("ITCRM_PONDERADOR_", id_suffix, "_M")
  
  if (!dir.exists(path_dir)) {
    dir.create(path_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Extraer datos exactos sin operaciones matemáticas
  df_obs <- df_datos %>%
    transmute(
      fecha = fecha,
      valor = as.numeric(.data[[col_nombre]])
    ) %>%
    filter(!is.na(fecha), !is.na(valor)) %>%
    arrange(fecha)
  
  if (nrow(df_obs) == 0) {
    message("  [AVISO] No hay observaciones válidas para ", pais_nombre)
    return(FALSE)
  }
  
  metadatos <- list(
    titulo = paste0("Ponderador ITCRM - ", pais_nombre),
    descripcion = paste0("Participación del país en el comercio internacional de Argentina con sus principales socios (excluyendo productos primarios, combustibles y energía), en el promedio móvil de los últimos 12 meses del mes anterior"),
    pais = pais_nombre,
    categoria = tema_destino,
    frecuencia_short = "M",
    frecuencia_original = "mensual",
    unidades = "Porcentaje",
    ajuste = "NSA",
    tipo_informacion = "Publica",
    fuente = "BCRA",
    fuente_original = "Índice de Tipo de Cambio Real Multilateral (ITCRM) - BCRA",
    fuente_formato = "EXCEL_BCRA",
    id_original = codigo_original,
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    fecha_inicio = df_obs$fecha[1],
    url_original = url_excel,
    revisable = TRUE,
    notas = list()
  )
  
  if (!file.exists(path_archivo)) {
    
    message("  > Archivo no existe. Creando serie desde cero...")
    
    df_obs$realtime_start <- hoy
    df_obs$realtime_end <- "9999-12-31"
    
    lista_final <- list(
      serie_id = serie_id,
      metadatos = metadatos,
      observaciones = df_obs
    )
    
  } else {
    
    message("  > Archivo existente. Actualizando vintages...")
    
    base_actual <- fromJSON(path_archivo, simplifyVector = TRUE)
    
    obs_vigentes <- base_actual$observaciones[
      base_actual$observaciones$realtime_end == "9999-12-31",
    ]
    
    obs_historicas <- base_actual$observaciones[
      base_actual$observaciones$realtime_end != "9999-12-31",
    ]
    
    actualizadas <- merge(
      df_obs,
      obs_vigentes,
      by = "fecha",
      all.x = TRUE,
      suffixes = c("_nuevo", "_viejo")
    )
    
    actualizadas$status <- ifelse(
      is.na(actualizadas$valor_viejo),
      "NUEVO",
      ifelse(
        actualizadas$valor_nuevo != actualizadas$valor_viejo,
        "REVISADO",
        "SIN_CAMBIOS"
      )
    )
    
    fechas_rev <- actualizadas$fecha[actualizadas$status == "REVISADO"]
                                obs_vigentes_que_cambiaron <- obs_vigentes[obs_vigentes$fecha %in% fechas_rev, ]
                                
                                if (nrow(obs_vigentes_que_cambiaron) > 0) {
                                  obs_vigentes_que_cambiaron$realtime_end <- hoy
                                }
                                
                                obs_vigentes_sin_cambio <- obs_vigentes[!(obs_vigentes$fecha %in% fechas_rev), ]
                                
                                nuevas_ins <- actualizadas[actualizadas$status %in% c("NUEVO", "REVISADO"), ]
                                
                                if (nrow(nuevas_ins) > 0) {
                                  nuevas_ins <- nuevas_ins[, c("fecha", "valor_nuevo")]
                                  colnames(nuevas_ins) <- c("fecha", "valor")
                                  nuevas_ins$realtime_start <- hoy
                                  nuevas_ins$realtime_end <- "9999-12-31"
                                } else {
                                  nuevas_ins <- data.frame(
                                    fecha = character(),
                                    valor = numeric(),
                                    realtime_start = character(),
                                    realtime_end = character(),
                                    stringsAsFactors = FALSE
                                  )
                                }
                                
                                obs_consolidadas <- rbind(
                                  obs_historicas,
                                  obs_vigentes_que_cambiaron,
                                  obs_vigentes_sin_cambio,
                                  nuevas_ins
                                )
                                
                                base_actual$metadatos$ultima_actualizacion <- paste0(hoy, "T12:00:00Z")
                                
                                base_actual$observaciones <- obs_consolidadas[
                                  order(obs_consolidadas$fecha, obs_consolidadas$realtime_start),
                                ]
                                
                                lista_final <- base_actual
  }
  
  write_json(
    lista_final,
    path_archivo,
    pretty = TRUE,
    auto_unbox = TRUE
  )
  
  message("  > Serie guardada con éxito: ", path_archivo)
  
  update_catalogo(
    serie_id = serie_id,
    metadatos = metadatos,
    metodo_etl = "EXCEL_BCRA",
    tema = tema_destino
  )
  
  return(TRUE)
}

# ============================================================
# 4. EJECUCIÓN PRINCIPAL
# ============================================================

message("\n==================================================================")
message("CARGA INICIAL PONDERADORES ITCRM - BCRA")
message("==================================================================")

df_ponderadores <- fetch_ponderadores_itcrm()

if (is.null(df_ponderadores) || nrow(df_ponderadores) == 0) {
  message("\nNO SE PUDIERON DESCARGAR O PROCESAR LOS DATOS DEL BCRA.")
} else {
  
  message("Observaciones leídas: ", nrow(df_ponderadores))
  
  for (col_excel in names(paises_map)) {
    
    if (!col_excel %in% names(df_ponderadores)) {
      message("\n[AVISO] La columna '", col_excel, "' no existe en la hoja Ponderadores.")
      next
    }
    
    info_pais <- paises_map[[col_excel]]
    
    message("\n----------------------------------------------------------")
    message("Procesando: ", info_pais$nombre)
    message("----------------------------------------------------------")
    
    tryCatch(
      {
        update_ponderador_json_serie(
          col_nombre = col_excel,
          id_suffix = info_pais$id_suffix,
          pais_nombre = info_pais$nombre,
          df_datos = df_ponderadores
        )
      },
      error = function(e) {
        message("  [ERROR en ", info_pais$nombre, "]: ", e$message)
      }
    )
  }
  
  message("\n==================================================================")
  message("CARGA INICIAL FINALIZADA")
  message("==================================================================")
}