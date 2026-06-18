# 📊 Base de Datos Económicos

![Actualización](https://img.shields.io/badge/Actualización-Diaria-brightgreen)
![Datos](https://img.shields.io/badge/Datos-Series_Económicas-blue)
![Automatización](https://img.shields.io/badge/ETL-GitHub_Actions-orange)

Este repositorio centraliza, estandariza y preserva el historial de revisión de las principales series económicas, combinando extracciones automáticas de APIs y procesamientos semi-automáticos de reportes.

---

## 📑 Tabla de Contenidos
1. [Estructura de Directorios](#-1-estructura-de-directorios)
2. [Nomenclatura de las Series](#-2-nomenclatura-de-las-series-identificadores)
3. [Estructura de Metadatos](#-3-estructura-de-metadatos)
4. [Control de Revisiones (ALFRED)](#-4-control-de-revisiones-metodología-alfred)
5. [Proceso de Actualización (ETL)](#-5-proceso-de-actualización-etl)
6. [Guía para Administradores](#-6-guía-para-administradores-cómo-agregar-una-nueva-variable)
7. [Ejemplos de Uso e Instrucciones](#-7-ejemplos-de-uso-e-instrucciones)

---

## 📂 1. Estructura de Directorios

La base se organiza temáticamente en carpetas. Cada variable se almacena como un archivo `.json` independiente dentro de su categoría correspondiente. Las carpetas principales son:

* 📈 **ACTIVIDAD**
* 🔮 **EXPECTATIVAS**
* 🌎 **INTERNACIONAL**
* 👷 **LABORAL**
* 💵 **MONETARIO**
* 🛒 **PRECIOS**
* 🚢 **SECTOR_EXTERNO**
* 💱 **TC_y_TASAS**

> **El Catálogo Maestro:** En la raíz del repositorio se encuentra el archivo `catalogo.json`. Este archivo funciona como el índice general de la base. Contiene el nombre de la serie, su metadata básica, la URL cruda (`raw_url`) que indica en qué carpeta se encuentra, y el método de actualización (`metodo_etl`) asignado.

---

## 🏷️ 2. Nomenclatura de las Series (Identificadores)

Para mantener la base limpia y legible, los identificadores (`serie_id`) siguen el siguiente formato: "[VARIABLE]_[FORMATO]_[FRECUENCIA]". 

En la parte de ´FORMATO´ suele indicarse la unidad (INDICE, MILUSD, etc.), el tipo de variación (YOY, MOM, etc.), y si tiene o no ajuste estacional (SA, NSA).

Para ´FRECUENCIA´ se sigue la nomenclatura A, Q, M, D para series Anuales, Trimestrales, Mensuales o Diarias, respectivamente.

---

## 📄 3. Estructura de Metadatos

Cada archivo JSON consta de tres bloques principales: el `serie_id`, los `metadatos` y las `observaciones`. El bloque de metadatos provee todo el contexto necesario para interpretar la variable correctamente. 

Los campos estándar incluyen:

| Campo | Descripción |
| :--- | :--- |
| **`titulo`** | Nombre descriptivo de la serie. |
| **`descripcion`** | Explicación metodológica detallada y alcance del indicador. |
| **`pais`** | País o región al que corresponde el dato. |
| **`categoria`** | Tema o carpeta donde se aloja. |
| **`frecuencia_short`** | Frecuencia en formato corto (ej: `M` para mensual, `A` para anual). |
| **`unidades`** | Unidad de medida (ej: Índice, Millones de USD, % TNA). |
| **`ajuste`** | Tipo de ajuste estacional (ej: `NSA`, `SA`). |
| **`fuente_original`** | Organismo que produce el dato primario (INDEC, BLS, etc.). |
| **`fuente`** | Plataforma de donde se extrae (FRED, API_Argentina, BCRA). |
| **`id_original`** / **`notas`** | Código identificador original de la fuente primaria. |
| **`revisable`** | Valor booleano (`true`/`false`) que indica si la serie se revisa hacia atrás en cada actualización. |

---

## 🕰️ 4. Control de Revisiones (Metodología ALFRED)

Una de las características más potentes de esta base es el tratamiento de datos provisorios y definitivos. En lugar de simplemente sobrescribir un dato cuando la fuente oficial lo corrige, utilizamos una lógica de "Vintage Data" inspirada en ALFRED (ArchivaL Federal Reserve Economic Data).

Cada observación en el bloque de datos incluye cuatro columnas: `fecha`, `valor`, `realtime_start` y `realtime_end`.

* **`realtime_start`:** Indica la fecha exacta en la que ese valor específico fue publicado o conocido por primera vez.
* **`realtime_end`:** Indica hasta qué fecha ese valor fue considerado oficial. 
* 🟢 **Dato Vigente:** Si un dato es el actual y no ha sido modificado, su `realtime_end` es `"9999-12-31"`.
* 🔴 **Dato Revisado:** Si un dato fue corregido, el valor original cierra su ciclo de vida (su `realtime_end` pasa a ser la fecha de la revisión) y se inserta una nueva fila con el mismo período (`fecha`), el nuevo `valor`, y un nuevo `realtime_start`.

*Esto permite a los investigadores reconstruir exactamente qué información estaba disponible en cualquier fecha del pasado.*

---

## ⚙️ 5. Proceso de Actualización (ETL)

El mantenimiento de la base está altamente automatizado mediante **GitHub Actions**. El flujo de actualización consolidado se ejecuta de **lunes a viernes a las 20:07 hora de Argentina (23:07 UTC)**.

El proceso corre cuatro rutinas principales:

1. 🏦 **API BCRA:** Consulta la API del Banco Central. Filtra el catálogo por el método `API_BCRA` y descarga los nuevos registros diarios.
2. 🇦🇷 **API INDEC:** Consulta la API de Datos Argentina. Filtra el catálogo por el método `API_INDEC` y actualiza las series de precios, actividad, etc.
3. 🌐 **API FRED:** Descarga datos internacionales desde la base de la Reserva Federal de St. Louis. Requiere inyección de un token de seguridad (`FRED_API_KEY`) almacenado en los *Secrets* de GitHub.
4. 📊 **REM:** Rutina semi-automatizada. El script revisa la carpeta local `inputs/`. Si un administrador sube un archivo llamado `rem_nuevo.xlsx`, el script lo procesa, actualiza las series, lo mueve a la carpeta de históricos y sube los cambios. Si no hay archivo nuevo, el script termina sin realizar acciones.

---

## 🛠️ 6. Guía para Administradores: Cómo agregar una nueva variable

Para incorporar una nueva serie a la base, el administrador debe seguir estos pasos:

1. **Definir los Metadatos:** Crear una lista en R con la estructura estándar (título, descripción, fuente, frecuencia, etc.).
2. **Identificar el Tema:** Seleccionar una de las 8 carpetas principales.
3. **Ejecutar el Script de Carga Inicial:** Utilizar el script correspondiente a la fuente (ej: `agregarvar_fred.R` o `agregarvar_apibcra.R`). Este script debe invocar a la función `update_catalogo()`.
4. **Verificar Parámetros Clave:** Asegurarse de que al actualizar el catálogo se pasen explícitamente los parámetros `metodo_etl` (ej: `API_FRED`) y `tema` (ej: `INTERNACIONAL`). 
5. **Commit y Push:** Subir los cambios a GitHub. La rutina de actualización diaria (`actualizacion.yml`) tomará automáticamente la nueva serie al leer el `catalogo.json` actualizado.

## 💡 7. Ejemplos de Uso e Instrucciones

Para facilitar el consumo y análisis de las series económicas, hemos preparado una carpeta dedicada con tutoriales y recursos prácticos. 

En la carpeta [📁 Ejemplos e Instrucciones](./Ejemplos%20e%20Instrucciones) encontrarás guías paso a paso y scripts de muestra para conectar tu entorno de trabajo directamente a esta base de datos utilizando:

* 🖥️ **R**
* 🐍 **Python**
* 📊 **Excel**

¡Te invitamos a explorarla para comenzar a integrar los datos en tus propios proyectos y tableros!