source("scripts/funciones_base.R")

# 1. Definimos los metadatos 
meta_ipc_general_corregido <- list(
  titulo = "IPC Servicios",
  descripcion = "IPC Servicios. Indice Dic-16=100",
  pais = "Argentina",
  categoria = "PRECIOS",
  frecuencia_short = "M",
  frecuencia_original = "mensual",
  unidades = "Índice", 
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "API_Argentina",
  fuente_original = "INDEC",
  fuente_formato = "API_INDEC",
  id_original = "147.3_ISERVICNAL_DICI_T_22",
  ultima_actualizacion = Sys.Date(),
  fecha_inicio = as.Date("2016-12-01"),
  url_original = "https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-5-31",
  revisable = FALSE,
  notas = NA
)

serie_id_general <- "IPCSERVICIOS_INDICE_NSA_M"

# 3. Consolidamos en una lista para procesar en bucle
# (Se corrigió la llamada a 'meta_ipc_general_corregido')
lista_series <- list(
  list(meta = meta_ipc_general_corregido, id = serie_id_general)
)

message("Iniciando carga inicial masiva desde INDEC...")

# Definimos el tema explícitamente para todo este bloque de series
tema_fijo <- meta_ipc_general_corregido$categoria

# 4. Bucle de extracción, creación y catalogación
exito <- update_indec_json_serie(
  id_indec = meta_ipc_general_corregido$id_original,
  serie_id = serie_id_general,
  tema = tema_fijo,
  metadatos_fijos = meta_ipc_general_corregido
)

if (exito) {
  # Agregamos el parámetro "tema" explícitamente a update_catalogo
  update_catalogo(
    serie_id = serie_id_general, 
    metadatos = meta_ipc_general_corregido, 
    metodo_etl = meta_ipc_general_corregido$fuente_formato,
    tema = tema_fijo
  )
}

message("¡Carga finalizada! Revisa tu carpeta PRECIOS/ y tu catalogo.json.")