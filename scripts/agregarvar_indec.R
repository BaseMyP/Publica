source("scripts/funciones_base.R")

# 1. Definimos los metadatos para el Nivel General
meta_ipc_general_corregido <- list(
  titulo = "IPC Nacional - Nivel General",
  descripcion = "Índice de Precios al Consumidor, cobertura nacional. Nivel General. Base Diciembre 2016=100.",
  pais = "Argentina",
  categoria = "PRECIOS",
  frecuencia_short = "M",
  frecuencia_original = "mensual",
  unidades = "Índice", 
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "API_Argentina",
  fuente_original = "INDEC",
  fuente_formato = "API_Argentina",
  ultima_actualizacion = Sys.Date(),
  fecha_inicio = as.Date("2003-01-01"),
  url_original = "https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-5-31",
  revisable = FALSE,
  notas = "148.3_INIVELNAL_DICI_M_26" 
)

serie_id_general <- "IPCGENERAL_INDICE_NSA_M"

# 2. Definimos los metadatos para el IPC Núcleo
meta_ipc_nucleo <- list(
  titulo = "IPC Nacional - Núcleo",
  descripcion = "Índice de Precios al Consumidor, cobertura nacional. IPC Núcleo. Base Diciembre 2016=100.",
  pais = "Argentina",
  categoria = "PRECIOS",
  frecuencia_short = "M",
  frecuencia_original = "mensual",
  unidades = "Índice", 
  ajuste = "NSA",
  tipo_informacion = "Pública",
  fuente = "API_Argentina",
  fuente_original = "INDEC",
  fuente_formato = "API_Argentina",
  ultima_actualizacion = Sys.Date(),
  fecha_inicio = as.Date("2003-01-01"),
  url_original = "https://www.indec.gob.ar/indec/web/Nivel4-Tema-3-5-31",
  revisable = FALSE,
  notas = "148.3_INUCLEONAL_DICI_M_19" 
)

serie_id_nucleo <- "IPCNUCLEO_INDICE_NSA_M"

# 3. Consolidamos en una lista para procesar en bucle
# (Se corrigió la llamada a 'meta_ipc_general_corregido')
lista_series <- list(
  list(meta = meta_ipc_general_corregido, id = serie_id_general),
  list(meta = meta_ipc_nucleo, id = serie_id_nucleo)
)

message("Iniciando carga inicial masiva desde INDEC...")

# Definimos el tema explícitamente para todo este bloque de series
tema_fijo <- "PRECIOS"

# 4. Bucle de extracción, creación y catalogación
for (item in lista_series) {
  
  exito <- update_indec_json_serie(
    id_indec = item$meta$notas,
    serie_id = item$id,
    tema = tema_fijo,
    metadatos_fijos = item$meta
  )
  
  if (exito) {
    # Agregamos el parámetro "tema" explícitamente a update_catalogo
    update_catalogo(
      serie_id = item$id, 
      metadatos = item$meta, 
      metodo_etl = "API_INDEC",
      tema = tema_fijo
    )
  }
}

message("¡Carga finalizada! Revisa tu carpeta PRECIOS/ y tu catalogo.json.")