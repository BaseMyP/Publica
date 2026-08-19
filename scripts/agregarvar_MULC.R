# ==============================================================================
# SCRIPT DE CARGA INICIAL: Archivo MULC
# ==============================================================================

library(httr)
library(readxl)
library(dplyr)
library(jsonlite)
library(lubridate)
library(stringr)
source("scripts/funciones_base.R")

message("Iniciando descarga histórica del archivo del MULC (BCRA)...")

# 1. Descargar archivo
url_MULC <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/informes/anexo-estadistico-mercado-cambios-balance-cambiario.xlsx"
archivo_tmp <- tempfile(fileext = ".xlsx")

tryCatch({
  GET(url_MULC, write_disk(archivo_tmp, overwrite = TRUE), config(ssl_verifypeer = 0))
}, error = function(e) {
  stop("Error al descargar el archivo: ", e$message)
})

# 2. Configurar las series a extraer
# Aquí puedes agregar más variables en el futuro simplemente copiando el bloque
series_MULC <- list(
  MULC_CC_TOTAL_NSA_M = list( 
    col_index = "bal001",               
    titulo = "Cuenta Corriente Cambiaria. Total",
    descripcion = "Cuenta Corriente Cambiaria. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_BIENES_TOTAL_NSA_M = list( 
    col_index = "bal002",               
    titulo = "Cuenta Corriente Cambiaria. Bienes. Total",
    descripcion = "Cuenta Corriente Cambiaria. Bienes. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_BIENES_EXPO_NSA_M = list( 
    col_index = "bal003",               
    titulo = "Cuenta Corriente Cambiaria. Bienes. Cobros de Exportaciones",
    descripcion = "Cuenta Corriente Cambiaria. Bienes. Cobros de Exportaciones",
    unidades = "Millones de USD"           
  ),
  MULC_CC_BIENES_IMPO_NSA_M = list( 
    col_index = "bal004",               
    titulo = "Cuenta Corriente Cambiaria. Bienes. Pagos de Importaciones",
    descripcion = "Cuenta Corriente Cambiaria. Bienes. Pagos de Importaciones",
    unidades = "Millones de USD"           
  ),
  MULC_CC_SERVICIOS_TOTAL_NSA_M = list( 
    col_index = "bal005",               
    titulo = "Cuenta Corriente Cambiaria. Servicios. Total",
    descripcion = "Cuenta Corriente Cambiaria. Servicios. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_SERVICIOS_INGRESOS_NSA_M = list( 
    col_index = "bal006",               
    titulo = "Cuenta Corriente Cambiaria. Servicios. Ingresos",
    descripcion = "Cuenta Corriente Cambiaria. Servicios. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CC_SERVICIOS_EGRESOS_NSA_M = list( 
    col_index = "bal007",               
    titulo = "Cuenta Corriente Cambiaria. Servicios. Engresos",
    descripcion = "Cuenta Corriente Cambiaria. Servicios. Engresos",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_TOTAL_NSA_M = list( 
    col_index = "bal008",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Total",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_INTERESES_TOTAL_NSA_M = list( 
    col_index = "bal009",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Total",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_INTERESES_INGRESOS_NSA_M = list( 
    col_index = "bal010",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Ingresos",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_INTERESES_EGRESOS_TOTAL_NSA_M = list( 
    col_index = "bal011",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. Total",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_INTERESES_EGRESOS_FMI_NSA_M = list( 
    col_index = "bal012",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. FMI",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. FMI",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_INTERESES_EGRESOS_OOII_NSA_M = list( 
    col_index = "bal013",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. OOII",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. OOII",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_INTERESES_EGRESOS_OTROS_NSA_M = list( 
    col_index = "bal014",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. Otros",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. Otros",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_INTERESES_EGRESOS_OTROS_GOB_NSA_M = list( 
    col_index = "bal015",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. Otros Gobierno Nacional",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Intereses. Egresos. Otros Gobierno Nacional",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_UTILDIV_TOTAL_NSA_M = list( 
    col_index = "bal016",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Utilidades y Dividendos. Total",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Utilidades y Dividendos. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_UTILDIV_INGRESOS_NSA_M = list( 
    col_index = "bal017",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Utilidades y Dividendos. Ingresos",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Utilidades y Dividendos. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGPRIM_UTILDIV_EGRESOS_NSA_M = list( 
    col_index = "bal018",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Primario. Utilidades y Dividendos. Egresos",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Primario. Utilidades y Dividendos. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGSEC_TOTAL_NSA_M = list( 
    col_index = "bal019",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Secundario. Total",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Secundario. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGSEC_INGRESOS_NSA_M = list( 
    col_index = "bal020",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Secundario. Ingresos",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Secundario. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CC_INGSEC_EGRESOS_NSA_M = list( 
    col_index = "bal021",               
    titulo = "Cuenta Corriente Cambiaria. Ingreso Secundario. Egresos",
    descripcion = "Cuenta Corriente Cambiaria. Ingreso Secundario. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CK_NSA_M = list( 
    col_index = "bal023",               
    titulo = "Cuenta de Capital Cambiaria. Total",
    descripcion = "Cuenta de Capital Cambiaria. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_TOTAL_NSA_M = list( 
    col_index = "bal024",               
    titulo = "Cuenta Financiera. Total",
    descripcion = "Cuenta Financiera. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_IED_TOTAL_NSA_M = list( 
    col_index = "bal025",               
    titulo = "Cuenta Financiera. Inversión directa de no residentes. Total",
    descripcion = "Cuenta Financiera. Inversión directa de no residentes. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_IED_INGRESOS_NSA_M = list( 
    col_index = "bal026",               
    titulo = "Cuenta Financiera. Inversión directa de no residentes. Ingresos",
    descripcion = "Cuenta Financiera. Inversión directa de no residentes. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_IED_EGRESOS_NSA_M = list( 
    col_index = "bal027",               
    titulo = "Cuenta Financiera. Inversión directa de no residentes. Egresos",
    descripcion = "Cuenta Financiera. Inversión directa de no residentes. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_PORT_TOTAL_NSA_M = list( 
    col_index = "bal028",               
    titulo = "Cuenta Financiera. Inversión de portafolio de no residentes. Total",
    descripcion = "Cuenta Financiera. Inversión de portafolio de no residentes. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_PORT_INGRESOS_NSA_M = list( 
    col_index = "bal028",               
    titulo = "Cuenta Financiera. Inversión de portafolio de no residentes. Ingresos",
    descripcion = "Cuenta Financiera. Inversión de portafolio de no residentes. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_PORT_EGRESOS_NSA_M = list( 
    col_index = "bal030",               
    titulo = "Cuenta Financiera. Inversión de portafolio de no residentes. Egresos",
    descripcion = "Cuenta Financiera. Inversión de portafolio de no residentes. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_PRESTFIN_TOTAL_NSA_M = list( 
    col_index = "bal031",               
    titulo = "Cuenta Financiera. Préstamos financieros, títulos de deuda y líneas de crédito. Total",
    descripcion = "Cuenta Financiera. Préstamos financieros, títulos de deuda y líneas de crédito. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_PRESTFIN_INGRESOS_NSA_M = list( 
    col_index = "bal032",               
    titulo = "Cuenta Financiera. Préstamos financieros, títulos de deuda y líneas de crédito. Ingresos",
    descripcion = "Cuenta Financiera. Préstamos financieros, títulos de deuda y líneas de crédito. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_PRESTFIN_EGRESOS_NSA_M = list( 
    col_index = "bal033",               
    titulo = "Cuenta Financiera. Préstamos financieros, títulos de deuda y líneas de crédito. Egresos",
    descripcion = "Cuenta Financiera. Préstamos financieros, títulos de deuda y líneas de crédito. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OPFMI_TOTAL_NSA_M = list( 
    col_index = "bal034",               
    titulo = "Cuenta Financiera. Operaciones con el FMI. Total",
    descripcion = "Cuenta Financiera. Operaciones con el FMI. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OPFMI_INGRESOS_NSA_M = list( 
    col_index = "bal035",               
    titulo = "Cuenta Financiera. Operaciones con el FMI. Ingresos",
    descripcion = "Cuenta Financiera. Operaciones con el FMI. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OPFMI_EGRESOS_NSA_M = list( 
    col_index = "bal036",               
    titulo = "Cuenta Financiera. Operaciones con el FMI. Egresos",
    descripcion = "Cuenta Financiera. Operaciones con el FMI. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OOII_TOTAL_NSA_M = list( 
    col_index = "bal037",               
    titulo = "Cuenta Financiera. Préstamos de otros Org. Int. y otros bilaterales. Total",
    descripcion = "Cuenta Financiera. Préstamos de otros Org. Int. y otros bilaterales. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OOII_INGRESOS_NSA_M = list( 
    col_index = "bal038",               
    titulo = "Cuenta Financiera. Préstamos de otros Org. Int. y otros bilaterales. Ingresos",
    descripcion = "Cuenta Financiera. Préstamos de otros Org. Int. y otros bilaterales. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OOII_EGRESOS_NSA_M = list( 
    col_index = "bal039",               
    titulo = "Cuenta Financiera. Préstamos de otros Org. Int. y otros bilaterales. Egresos",
    descripcion = "Cuenta Financiera. Préstamos de otros Org. Int. y otros bilaterales. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_FAE_TOTAL_NSA_M = list( 
    col_index = "bal040",               
    titulo = "Cuenta Financiera. Compra-venta de billetes y divisas sin fines específicos del sector privado no financiero. Total",
    descripcion = "Cuenta Financiera. Compra-venta de billetes y divisas sin fines específicos del sector privado no financiero. Total",
    unidades = "Millones de USD"           
  ),
  MULC_CF_FAE_INGRESOS_NSA_M = list( 
    col_index = "bal041",               
    titulo = "Cuenta Financiera. Compra-venta de billetes y divisas sin fines específicos del sector privado no financiero. Ingresos",
    descripcion = "Cuenta Financiera. Compra-venta de billetes y divisas sin fines específicos del sector privado no financiero. Ingresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_FAE_EGRESOS_NSA_M = list( 
    col_index = "bal042",               
    titulo = "Cuenta Financiera. Compra-venta de billetes y divisas sin fines específicos del sector privado no financiero. Egresos",
    descripcion = "Cuenta Financiera. Compra-venta de billetes y divisas sin fines específicos del sector privado no financiero. Egresos",
    unidades = "Millones de USD"           
  ),
  MULC_CF_CANJEEXT_NSA_M = list( 
    col_index = "bal043",               
    titulo = "Cuenta Financiera. Operaciones de canje por transferencias con el exterior",
    descripcion = "Cuenta Financiera. Operaciones de canje por transferencias con el exterior",
    unidades = "Millones de USD"           
  ),
  MULC_CF_PGCBANCOS_NSA_M = list( 
    col_index = "bal044",               
    titulo = "Cuenta Financiera. Formación de activos externos del sector financiero (PGC)",
    descripcion = "Cuenta Financiera. Formación de activos externos del sector financiero (PGC)",
    unidades = "Millones de USD"           
  ),
  MULC_CF_FAESP_NSA_M = list( 
    col_index = "bal045",               
    titulo = "Cuenta Financiera. Formación de activos externos del sector público",
    descripcion = "Cuenta Financiera. Formación de activos externos del sector público",
    unidades = "Millones de USD"           
  ),
  MULC_CF_TITULOS_NSA_M = list( 
    col_index = "bal046",               
    titulo = "Cuenta Financiera. Compra-venta de títulos valores",
    descripcion = "Cuenta Financiera. Compra-venta de títulos valores",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OTRASSPN_NSA_M = list( 
    col_index = "bal047",               
    titulo = "Cuenta Financiera. Otras operaciones del sector público nacional (neto)",
    descripcion = "Cuenta Financiera. Otras operaciones del sector público nacional (neto)",
    unidades = "Millones de USD"           
  ),
  MULC_CF_OTROS_NSA_M = list( 
    col_index = "bal048",               
    titulo = "Cuenta Financiera. Otros movimientos netos",
    descripcion = "Cuenta Financiera. Otros movimientos netos",
    unidades = "Millones de USD"           
  ),
  MULC_NOINFORMADO_NSA_M = list( 
    col_index = "bal049",               
    titulo = "Concepto no informado por el cliente (neto)",
    descripcion = "Concepto no informado por el cliente (neto)",
    unidades = "Millones de USD"           
  ),
  MULC_VarRESERVAS_TRANSACCIONES_NSA_M = list( 
    col_index = "bal050",               
    titulo = "Variación de Reservas Internacionales por transacciones",
    descripcion = "Variación de Reservas Internacionales por transacciones",
    unidades = "Millones de USD"           
  ),
  MULC_VarRESERVAS_CONTABLE_NSA_M = list( 
    col_index = "bal051",               
    titulo = "Variación contable de Reservas Internacionales del BCRA",
    descripcion = "Variación contable de Reservas Internacionales del BCRA",
    unidades = "Millones de USD"           
  ),
  MULC_AJUSTE_NSA_M = list( 
    col_index = "bal052",               
    titulo = "Ajuste por tipo de pase y valuación",
    descripcion = "Ajuste por tipo de pase y valuación",
    unidades = "Millones de USD"           
  ),
  MULC_MEMORANDUM_NSA_M = list( 
    col_index = "bal053",               
    titulo = "Item de Memorandum: Pago del saldo en moneda extranjera por uso de tarjetas en el exterior",
    descripcion = "Item de Memorandum: Pago del saldo en moneda extranjera por uso de tarjetas en el exterior",
    unidades = "Millones de USD"           
  )
)

# 3. Leer y limpiar el Excel
# IMPORTANTE: Ajusta 'skip = 8' a la cantidad real de filas de encabezado que tenga la hoja
df_raw <- read_excel(archivo_tmp, sheet="Balance Cambiario", skip=10)

hoy <- as.character(Sys.Date())
tema_fijo <- "SECTOR_EXTERNO"

if (!dir.exists(tema_fijo)) dir.create(tema_fijo, recursive = TRUE)

# 4. Bucle de procesamiento y guardado
for (serie_id in names(series_MULC)) {
  
  config <- series_MULC[[serie_id]]
  
  # Extraemos solo la fecha (columna 1) y la columna deseada
  df_serie <- df_raw %>%
    select(fecha = "bal000", valor = config$col_index) %>%
    mutate(fecha = as.Date(as.numeric(fecha), origin="1899-12-30")) %>% 
    filter(!is.na(fecha)) %>% 
    filter(year(fecha)>2000) %>%
    arrange(fecha) %>%
    mutate(
      fecha = as.character(fecha),
      realtime_start = hoy,
      realtime_end = "9999-12-31"
    )
  
  # Metadatos
  meta_actual <- list(
    titulo = config$titulo,
    descripcion = config$descripcion,
    pais = "Argentina",
    categoria = tema_fijo,
    frecuencia_short = "M",
    frecuencia_original = "mensual",
    unidades = config$unidades,
    ajuste = "NSA",
    tipo_informacion = "Pública",
    fuente = "BCRA",
    fuente_original = "BCRA",
    fuente_formato = "Excel",
    id_original = as.character(config$col_index), # Guardamos el índice de la columna para la actualización
    ultima_actualizacion = paste0(hoy, "T12:00:00Z"),
    url_original = url_MULC,
    revisable = TRUE,
    notas = "Hoja: Balance Cambiario"
  )
  
  # Guardado
  lista_final <- list(serie_id = serie_id, metadatos = meta_actual, observaciones = df_serie)
  path_archivo <- file.path(tema_fijo, paste0(serie_id, ".json"))
  write_json(lista_final, path_archivo, pretty = TRUE, auto_unbox = TRUE)
  
  # Actualizar Catálogo (Registramos con un método ETL específico)
  update_catalogo(serie_id = serie_id, metadatos = meta_actual, metodo_etl = "EXCEL_MULC", tema = tema_fijo)
  
  message("✓ Serie generada exitosamente: ", serie_id)
}