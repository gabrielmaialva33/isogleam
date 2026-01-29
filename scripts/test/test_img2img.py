#!/usr/bin/env python3
"""
Teste img2img - Usar satélite como base
"""
import requests
from pathlib import Path

# Baixar tile satélite de Capão Bonito (centro)
# Coordenadas: -24.0056, -48.3470
LAT, LON = -24.0056, -48.3470
ZOOM = 17

# Tile coords
import math
def deg2num(lat_deg, lon_deg, zoom):
    lat_rad = math.radians(lat_deg)
    n = 2.0 ** zoom
    xtile = int((lon_deg + 180.0) / 360.0 * n)
    ytile = int((1.0 - math.asinh(math.tan(lat_rad)) / math.pi) / 2.0 * n)
    return (xtile, ytile)

x, y = deg2num(LAT, LON, ZOOM)
print(f"Tile coords: {x}, {y} (zoom {ZOOM})")

# Baixar do OpenStreetMap
url = f"https://tile.openstreetmap.org/{ZOOM}/{x}/{y}.png"
print(f"Downloading: {url}")

headers = {'User-Agent': 'IsoGleam/1.0'}
r = requests.get(url, headers=headers)

if r.status_code == 200:
    out = Path(__file__).parent.parent.parent / "data" / "inputs" / "reference" / "satellite_center.png"
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_bytes(r.content)
    print(f"✓ Saved: {out} ({len(r.content)} bytes)")
else:
    print(f"✗ Error: {r.status_code}")
