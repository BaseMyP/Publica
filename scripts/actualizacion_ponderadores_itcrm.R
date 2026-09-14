# ==============================================================================
# SCRIPT DE ACTUALIZACIÓN AUTOMÁTICA (PONDERADORES ITCRM - BCRA)
# ==============================================================================

library(jsonlite)
library(dplyr)
library(httr)
library(readxl)

source("scripts/funciones_base.R")

message("Iniciando revisión diaria de Ponderadores ITCRM (BCRA): ", Sys.time())


# ==============================================================================
# 1. PARÁMETROS Y DESCARGA DEL EXCEL DESDE EL BCRA
# ==============================================================================

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
      read_excel(
        temp_file,
        sheet = "Ponderadores",
        skip = 1
      )
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


# ==============================================================================
# 2. FUNCIÓN DE ACTUALIZACIÓN DE JSON (VINTAGES)
# ==============================================================================

# ==============================================================================
# FUNCIÓN DE ACTUALIZACIÓN DE JSON - PONDERADORES ITCRM
# ==============================================================================

update_ponderador_json_serie <- function(
    serie_id,
    tema,
    metadatos_fijos,
    df_obs
) {
  
  path_archivo <- file.path(
    tema,
    paste0(serie_id, ".json")
  )
  
  hoy <- as.character(Sys.Date())
  
  message("  > Archivo: ", path_archivo)
  
  
  # ============================================================================
  # 1. LEER JSON
  # ============================================================================
  
  base_actual <- fromJSON(
    path_archivo,
    simplifyVector = TRUE
  )
  
  obs <- base_actual$observaciones
  
  
  # ============================================================================
  # 2. NORMALIZAR ESTRUCTURA
  # ============================================================================
  
  obs <- obs %>%
    mutate(
      
      # Fecha de observación
      fecha = substr(
        as.character(fecha),
        1,
        10
      ),
      
      # Valor
      valor = as.numeric(valor),
      
      # Fechas de vintage
      realtime_start = substr(
        as.character(realtime_start),
        1,
        10
      ),
      
      realtime_end = substr(
        as.character(realtime_end),
        1,
        10
      )
    )
  
  
  # ============================================================================
  # 3. NORMALIZAR df NUEVO
  # ============================================================================
  
  df_obs <- df_obs %>%
    mutate(
      fecha = substr(
        as.character(fecha),
        1,
        10
      ),
      
      valor = as.numeric(valor)
    ) %>%
    filter(
      !is.na(fecha),
      !is.na(valor)
    ) %>%
    arrange(fecha)
  
  
  # ============================================================================
  # 4. IDENTIFICAR VIGENTES
  #
  # IMPORTANTE:
  # No asumimos que realtime_end viene exactamente con el formato original.
  # ============================================================================
  
  obs_vigentes <- obs %>%
    filter(
      realtime_end == "9999-12-31"
    )
  
  
  obs_historicas <- obs %>%
    filter(
      realtime_end != "9999-12-31"
    )
  
  
  # ============================================================================
  # 5. CONTROL DE DUPLICADOS VIGENTES
  # ============================================================================
  
  duplicados <- obs_vigentes %>%
    count(fecha) %>%
    filter(n > 1)
  
  
  if (nrow(duplicados) > 0) {
    
    message("")
    message("  [ADVERTENCIA] Hay múltiples registros vigentes para:")
    message(
      "  ",
      paste(duplicados$fecha, collapse = ", ")
    )
    
    message(
      "  Se conservará únicamente el registro vigente ",
      "más reciente por fecha."
    )
    
    
    # Para cada fecha, conservar el registro con realtime_start
    # más reciente.
    
    obs_vigentes <- obs_vigentes %>%
      arrange(
        fecha,
        desc(realtime_start)
      ) %>%
      distinct(
        fecha,
        .keep_all = TRUE
      )
  }
  
  
  # ============================================================================
  # 6. COMPARACIÓN CON LOS DATOS DEL BCRA
  # ============================================================================
  
  comparacion <- df_obs %>%
    left_join(
      obs_vigentes %>%
        select(
          fecha,
          valor_viejo = valor
        ),
      by = "fecha"
    ) %>%
    mutate(
      
      # --------------------------------------------------------------
      # No existe en el JSON -> observación nueva
      # --------------------------------------------------------------
      
      status = case_when(
        
        is.na(valor_viejo) ~ "NUEVO",
        
        
        # ------------------------------------------------------------
        # Diferencia insignificante
        # ------------------------------------------------------------
        
        abs(
          round(valor, 10) -
            round(valor_viejo, 10)
        ) <= 0.00000001 ~ "SIN_CAMBIOS",
        
        
        # ------------------------------------------------------------
        # Diferencia real
        # ------------------------------------------------------------
        
        TRUE ~ "REVISADO"
      )
    )
  
  
  # ============================================================================
  # 7. MOSTRAR TODAS LAS REVISIONES DETECTADAS
  # ============================================================================
  
  revisiones <- comparacion %>%
    filter(status == "REVISADO") %>%
    mutate(
      diferencia = valor - valor_viejo
    )
  
  
  if (nrow(revisiones) > 0) {
    
    message("")
    message("==============================================================")
    message("REVISIONES HISTÓRICAS DETECTADAS")
    message("==============================================================")
    
    for (j in seq_len(nrow(revisiones))) {
      
      message(
        sprintf(
          paste0(
            "Fecha: %s | ",
            "Viejo: %.15f | ",
            "Nuevo: %.15f | ",
            "Diferencia: %.15f"
          ),
          revisiones$fecha[j],
          revisiones$valor_viejo[j],
          revisiones$valor[j],
          revisiones$diferencia[j]
        )
      )
    }
    
    message("==============================================================")
  }
  
  
  # ============================================================================
  # 8. IDENTIFICAR NUEVAS Y REVISADAS
  # ============================================================================
  
  fechas_rev <- comparacion %>%
    filter(
      status == "REVISADO"
    ) %>%
    pull(fecha)
  
  
  fechas_nuevas <- comparacion %>%
    filter(
      status == "NUEVO"
    ) %>%
    pull(fecha)
  
  
  # ============================================================================
  # 9. SI NO CAMBIÓ NADA -> NO TOCAR EL ARCHIVO
  # ============================================================================
  
  if (
    length(fechas_rev) == 0 &&
    length(fechas_nuevas) == 0
  ) {
    
    message(
      "  > NO HAY REVISIONES NI OBSERVACIONES NUEVAS."
    )
    
    message(
      "  > No se modifica el JSON."
    )
    
    return(FALSE)
  }
  
  
  # ============================================================================
  # 10. CERRAR ÚNICAMENTE LOS REGISTROS REALMENTE REVISADOS
  # ============================================================================
  
  obs_vigentes_que_cambiaron <- obs_vigentes %>%
    filter(
      fecha %in% fechas_rev
    ) %>%
    mutate(
      realtime_end = hoy
    )
  
  
  # ============================================================================
  # 11. MANTENER VIGENTES SIN CAMBIO
  # ============================================================================
  
  obs_vigentes_sin_cambio <- obs_vigentes %>%
    filter(
      !fecha %in% fechas_rev
    )
  
  
  # ============================================================================
  # 12. NUEVOS REGISTROS / REVISIONES
  # ============================================================================
  
  nuevas_ins <- comparacion %>%
    filter(
      status %in% c(
        "NUEVO",
        "REVISADO"
      )
    ) %>%
    transmute(
      fecha = fecha,
      valor = valor,
      realtime_start = hoy,
      realtime_end = "9999-12-31"
    )
  
  
  # ============================================================================
  # 13. CONSOLIDAR
  # ============================================================================
  
  obs_consolidadas <- bind_rows(
    obs_historicas,
    obs_vigentes_que_cambiaron,
    obs_vigentes_sin_cambio,
    nuevas_ins
  )
  
  
  # ============================================================================
  # 14. PROTECCIÓN FINAL
  #
  # NUNCA debe quedar más de un registro vigente para una fecha.
  # ============================================================================
  
  control_vigentes <- obs_consolidadas %>%
    filter(
      realtime_end == "9999-12-31"
    ) %>%
    count(fecha) %>%
    filter(n > 1)
  
  
  if (nrow(control_vigentes) > 0) {
    
    stop(
      paste0(
        "ERROR CRÍTICO: quedaron múltiples observaciones vigentes ",
        "para las fechas: ",
        paste(
          control_vigentes$fecha,
          collapse = ", "
        )
      )
    )
  }
  
  
  # ============================================================================
  # 15. ACTUALIZAR METADATOS
  # ============================================================================
  
  metadatos_actualizados <- metadatos_fijos
  
  metadatos_actualizados$ultima_actualizacion <-
    paste0(
      hoy,
      "T12:00:00Z"
    )
  
  
  # ============================================================================
  # 16. CONSTRUIR JSON
  # ============================================================================
  
  lista_final <- list(
    
    serie_id = serie_id,
    
    metadatos = metadatos_actualizados,
    
    observaciones = obs_consolidadas %>%
      arrange(
        fecha,
        realtime_start
      )
  )
  
  
  # ============================================================================
  # 17. ESCRIBIR
  # ============================================================================
  
  write_json(
    lista_final,
    path_archivo,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = 15
  )
  
  
  # ============================================================================
  # 18. MENSAJE FINAL
  # ============================================================================
  
  if (length(fechas_rev) > 0) {
    
    message(
      "  > REVISIÓN HISTÓRICA REAL:"
    )
    
    message(
      "    ",
      paste(
        fechas_rev,
        collapse = ", "
      )
    )
  }
  
  
  if (length(fechas_nuevas) > 0) {
    
    message(
      "  > NUEVAS OBSERVACIONES:"
    )
    
    message(
      "    ",
      paste(
        fechas_nuevas,
        collapse = ", "
      )
    )
  }
  
  
  return(TRUE)
}


# ==============================================================================
# FIN
# ==============================================================================

message(
  "\n=================================================================="
)

message(
  "ACTUALIZACIÓN FINALIZADA"
)

message(
  "=================================================================="
)