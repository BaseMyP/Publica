library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica de Dólar Futuro (Matba Rofex)...")

# 1. Definir parámetros base
endpoint_url <- "https://apicem.matbarofex.com.ar/api/v1/closing-prices"
tema_fijo <- "TC_y_TASAS"
serie_id <- "FUTURO_USD_NSA_M"

# Generar lista de contratos: desde Ene-2025 hasta 24 meses hacia adelante
meses_contratos <- seq(as.Date("2025-01-01"), Sys.Date() + months(24), by = "1 month")
simbolos <- paste0("DLR", format(meses_contratos, "%m%Y"))

# Generar lista de meses a consultar (desde Dic-2024 hasta el mes previo al actual)
mes_anterior <- floor_date(Sys.Date(), "month") - months(1)
meses_consulta <- seq(as.Date("2024-12-01"), mes_anterior, by = "1 month")

datos_historicos <- list()

# 2. Descarga iterativa para esquivar límites de paginación de la API
# 2. Descarga iterativa para esquivar límites de paginación de la API
for (sym in simbolos) {
  for (i in seq_along(meses_consulta)) {        # <--- CAMBIO: Iteramos sobre el índice
    inicio_mes <- meses_consulta[i]             # <--- CAMBIO: Extraemos la fecha conservando su formato Date
    fin_mes <- ceiling_date(inicio_mes, "month") - days(1)
    
    req <- request(endpoint_url) %>%
      req_url_query(
        symbol = sym,
        from = paste0(format(inicio_mes, "%Y-%m-%d"), "T00:00:00Z"),
        to = paste0(format(fin_mes, "%Y-%m-%d"), "T23:59:59Z")
      )
    
    # Check_status = FALSE evita que R frene el script si el contrato no existía en ese mes
    resp <- req %>% 
      req_error(is_error = function(resp) FALSE) %>% 
      req_perform()
    
    if (resp_status(resp) == 200) {
      resp_json <- resp %>% resp_body_string() %>% fromJSON(flatten = TRUE)
      df <- resp_json$data
      if (is.data.frame(df) && nrow(df) > 0) {
        datos_historicos[[length(datos_historicos) + 1]] <- df
      }
    }
  }
}

df_completo <- bind_rows(datos_historicos)

# 3. Limpieza y Estructuración ALFRED (Último día de cada mes)
df_procesado <- df_completo %>%
  mutate(
    cotizacion_date = as.Date(dateTime),
    mes_cierre = floor_date(cotizacion_date, "month"),
    # Extraemos el mes/año del símbolo para saber cuándo vence el contrato
    mes_vencimiento = as.numeric(substr(symbol, 4, 5)),
    anio_vencimiento = as.numeric(substr(symbol, 6, 9)),
    fecha_contrato = ceiling_date(make_date(anio_vencimiento, mes_vencimiento, 1), "month") - days(1)
  ) %>%
  group_by(symbol, mes_cierre) %>%
  arrange(cotizacion_date) %>%
  slice_tail(n = 1) %>% # Nos quedamos exclusivamente con la observación del último día del mes
  ungroup() %>%
  select(
    fecha = fecha_contrato,
    valor = settlement,
    realtime_start = cotizacion_date
  ) %>%
  mutate(
    fecha = as.character(fecha),
    realtime_start = as.character(realtime_start)
  ) %>%
  arrange(fecha, realtime_start)

# 4. Cálculo de realtime_end
obs_consolidadas <- df_procesado %>%
  mutate(f_calc = as.Date(fecha), rs_calc = as.Date(realtime_start)) %>%
  group_by(f_calc) %>%
  arrange(rs_calc) %>%
  mutate(
    realtime_end = lead(rs_calc) - days(1),
    realtime_end = as.character(if_else(is.na(realtime_end), as.Date("9999-12-31"), realtime_end))
  ) %>%
  ungroup() %>%
  arrange(f_calc, rs_calc) %>%
  select(fecha, valor, realtime_start, realtime_end)

# 5. Metadatos y Guardado
meta_futuros <- list(
  titulo = "Curva de Dólar Futuro (Matba Rofex)",
  descripcion = "Precios de cierre de los contratos de dólar futuro. Cada observación representa el tipo de cambio esperado al vencimiento de cada contrato. En el campo fecha se indica el vencimiento del contrato.",
  pais = "Argentina",
  categoria = tema_fijo,
  frecuencia_short = "M",
  frecuencia_long = "mensual",
  unidades = "$/USD",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "Matba Rofex",
  fuente_original = "Matba Rofex",
  fuente_formato = "API",
  id_original = "DLR",
  ultima_actualizacion = paste0(as.character(Sys.Date()), "T12:00:00Z"),
  revisable = TRUE,
  notas = NULL
)

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

lista_final <- list(serie_id = serie_id, metadatos = meta_futuros, observaciones = obs_consolidadas)
path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))

write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)

# 6. Catálogo
update_catalogo(serie_id = serie_id, metadatos = meta_futuros, metodo_etl = "API_ROFEX", tema = tema_fijo)

message("¡Carga histórica de Futuros finalizada con éxito!")