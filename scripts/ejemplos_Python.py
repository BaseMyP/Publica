import pandas as pd
import requests

base_url = "https://raw.githubusercontent.com/BaseMyP/Publica/refs/heads/main/"

# Descargar el catálogo de variables
cat_path = "catalogo.json"
catalogo = pd.read_json(base_url + cat_path)

# Descarga serie del tipo de cambio
serie = "MACRO/MACRO_USDA3500_NOMINAL_NSA_D.json"
tcA3500 = requests.get(base_url + serie).json()
tcA3500_metadatos = tcA3500.get("metadatos")
tcA3500_serie = pd.DataFrame(tcA3500.get("observaciones"))

# Expectativa de inflación anual para 2026 (REM) - serie histórica
serie = "REM/REM_IPCGENERAL_YOY_A.json"
rem_ipc_2026 = requests.get(base_url + serie).json()
rem_ipc_2026_metadatos = rem_ipc_2026.get("metadatos")
rem_ipc_obs = pd.DataFrame(rem_ipc_2026.get("observaciones"))
rem_ipc_2026_serie = rem_ipc_obs[rem_ipc_obs["fecha"] == "2026-01-01"]

# Expectativa de inflación anual para 2026 (REM) - último dato
rem_ipc_2026_ult = rem_ipc_obs[
  (rem_ipc_obs["fecha"] == "2026-01-01")
  & (rem_ipc_obs["realtime_end"] == "9999-12-31")
]

# Expectativa de inflación anual para 2026 (REM) vigente a principios de año
filtro_ene = rem_ipc_obs[
  (rem_ipc_obs["fecha"] == "2026-01-01")
  & (rem_ipc_obs["realtime_end"] >= "2026-01-01")
]

rem_ipc_2026_ene26 = filtro_ene[
  filtro_ene["realtime_end"] == filtro_ene["realtime_end"].min()
]