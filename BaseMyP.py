# PAQUETE DE CONSULTA A LA BASE DE DATOS DE MODELOS Y PRONÃ“STICOS EN GITHUB #

import requests
import pandas as pd

def _descargar_json_github(repositorio: str, endpoint: str, token: str = None):
    """
    Busca el recurso probando distintas ramas (main/master) y rutas (data/ o raíz).
    """
    ramas = ["main", "master"]
    rutas = [f"data/{endpoint}", endpoint]
    
    headers = {}
    if token:
        headers["Authorization"] = f"Bearer {token}"
        
    errores = []

    for rama in ramas:
        for ruta in rutas:
            url = f"https://raw.githubusercontent.com/BaseMyP/{repositorio}/{rama}/{ruta}"
            response = requests.get(url, headers=headers)
            
            if response.status_code == 200:
                return response.json()
            else:
                errores.append(f"URL probada: {url} (Status: {response.status_code})")

    # Si ninguna combinación funcionó, lanza una excepción detallada
    detalle_errores = "\n".join(errores)
    raise FileNotFoundError(
        f"No se pudo encontrar '{endpoint}' en el repositorio '{repositorio}'.\n"
        f"Intentos fallidos:\n{detalle_errores}"
    )


def BaseMyP_obtener_catalogo(repositorio: str = "publica", token: str = None) -> pd.DataFrame:
    """Obtiene el catálogo de series disponibles en un repositorio determinado."""
    data = _descargar_json_github(repositorio, "catalogo.json", token)
    return pd.DataFrame(data)


def BaseMyP_descargar_serie(serie: str, origen: str = "publica", token: str = None) -> pd.DataFrame:
    """Descarga los datos temporales de una serie específica."""
    endpoint = f"{serie}.json"
    data = _descargar_json_github(origen, endpoint, token)
    
    df = pd.DataFrame(data)
    if "fecha" in df.columns:
        df["fecha"] = pd.to_datetime(df["fecha"])
    return df


def BaseMyP_metadatos(serie: str, origen: str = "publica", token: str = None) -> dict:
    """Recupera la información técnica y metadatos de una serie."""
    endpoint = f"{serie}_metadatos.json"
    return _descargar_json_github(origen, endpoint, token)