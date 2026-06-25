library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica del CCL desde Ámbito Financiero...")

# 1. Descarga de datos
fecha_desde <- "01-01-2002"
fecha_hasta <- format(Sys.Date(), "%d-%m-%Y")
url_ambito <- paste0("https://mercados.ambito.com/dolar/contado-con-liqui/historico-general/", fecha_desde, "/", fecha_hasta)

respuesta_ambito <- GET(
  url_ambito, 
  user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
)

if (status_code(respuesta_ambito) != 200) stop("Error al acceder a los datos de Ámbito Financiero")

datos_json <- content(respuesta_ambito, as = "text", encoding = "UTF-8")
matriz_ccl <- fromJSON(datos_json)

# 2. Limpieza (Usando tu lógica)
df_ccl_ambito <- as.data.frame(matriz_ccl[-1, ], stringsAsFactors = FALSE)
colnames(df_ccl_ambito) <- matriz_ccl[1, ]

df_limpio <- df_ccl_ambito %>%
  mutate(
    Fecha = dmy(Fecha),
    Compra = as.numeric(gsub(",", ".", Compra)), 
    Venta = as.numeric(gsub(",", ".", Venta)),
    valor = (Compra + Venta) / 2 # Promedio de puntas
  ) %>%
  filter(!is.na(Fecha) & !is.na(valor)) %>%
  select(fecha = Fecha, valor) %>%
  arrange(fecha)

# 3. Estructuración ALFRED
hoy <- as.character(Sys.Date())

obs_final <- df_limpio %>%
  mutate(
    fecha = as.character(fecha),
    realtime_start = hoy,
    realtime_end = "9999-12-31"
  )

# 4. Metadatos
tema_fijo <- "TC_y_TASAS"
serie_id <- "CCL_NOMINAL_NSA_D"

meta_ccl <- list(
  titulo = "Tipo de Cambio Contado con Liquidación (Promedio)",
  descripcion = "Tipo de cambio implícito en operaciones de compraventa de títulos valores (Contado con Liquidación). Promedio entre las puntas compradora y vendedora.",
  pais = "Argentina",
  categoria = tema_fijo,
  frecuencia_short = "D",
  frecuencia_original = "diaria",
  unidades = "$/USD",
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "Ámbito Financiero",
  fuente_original = "Ámbito Financiero",
  fuente_formato = "Scrapping Ambito Financiero",
  id_original = "CCL",
  ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
  revisable = TRUE,
  notas = NULL
)

# 5. Guardado y Catálogo
if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

lista_final <- list(serie_id = serie_id, metadatos = meta_ccl, observaciones = obs_final)
path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))

write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)

update_catalogo(serie_id = serie_id, metadatos = meta_ccl, metodo_etl = "API_AMBITO", tema = tema_fijo)

message("¡Carga inicial del CCL finalizada con éxito!")