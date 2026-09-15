# PAQUETE DE CONSULTA A LA BASE DE DATOS DE MODELOS Y PRONÃ“STICOS EN GITHUB #

import requests
import pandas as pd

def _construir_url_y_headers(repositorio: str, endpoint: str, token: str = None):
    """Construye la URL apuntando a la raíz del repositorio público/privado de BaseMyP."""
    # Apunta directo a la raíz sin la carpeta 'data/'
    base_url = f"https://raw.githubusercontent.com/BaseMyP/{repositorio}/main/{endpoint}"
    
    headers = {}
    if token:
        headers["Authorization"] = f"Bearer {token}"
        
    return base_url, headers


def BaseMyP_obtener_catalogo(repositorio: str = "publica", token: str = None) -> pd.DataFrame:
    """Obtiene el catálogo de series disponibles en un repositorio determinado."""
    url, headers = _construir_url_y_headers(repositorio, "catalogo.json", token)
    response = requests.get(url, headers=headers)
    
    if response.status_code != 200:
        raise ConnectionError(f"Error al obtener el catálogo ({response.status_code}): {response.text}")
    
    return pd.DataFrame(response.json())


def BaseMyP_descargar_serie(serie: str, origen: str = "publica", token: str = None) -> pd.DataFrame:
    """Descarga los datos temporales de una serie específica."""
    endpoint = f"{serie}.json"
    url, headers = _construir_url_y_headers(origen, endpoint, token)
    response = requests.get(url, headers=headers)
    
    if response.status_code != 200:
        raise ConnectionError(f"Error al descargar la serie '{serie}' ({response.status_code})")
    
    df = pd.DataFrame(response.json())
    if "fecha" in df.columns:
        df["fecha"] = pd.to_datetime(df["fecha"])
    return df


def BaseMyP_metadatos(serie: str, origen: str = "publica", token: str = None) -> dict:
    """Recupera la información técnica y metadatos de una serie."""
    endpoint = f"{serie}_metadatos.json"
    url, headers = _construir_url_y_headers(origen, endpoint, token)
    response = requests.get(url, headers=headers)
    
    if response.status_code != 200:
        raise ConnectionError(f"Error al obtener metadatos de '{serie}' ({response.status_code})")
    
    return response.json()