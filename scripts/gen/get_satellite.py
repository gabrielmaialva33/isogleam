#!/usr/bin/env python3
"""
Baixar imagem satélite real de Capão Bonito
"""
from pathlib import Path
import math

# Centro de Capão Bonito
LAT, LON = -24.0056, -48.3470
ZOOM = 18  # Mais detalhe

def deg2num(lat_deg, lon_deg, zoom):
    lat_rad = math.radians(lat_deg)
    n = 2.0 ** zoom
    xtile = int((lon_deg + 180.0) / 360.0 * n)
    ytile = int((1.0 - math.asinh(math.tan(lat_rad)) / math.pi) / 2.0 * n)
    return (xtile, ytile)

x, y = deg2num(LAT, LON, ZOOM)

# ESRI World Imagery (satélite gratuito)
url = f"https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{ZOOM}/{y}/{x}"
print(f"ESRI Satellite: {url}")

import requests
r = requests.get(url)
if r.status_code == 200:
    out = Path("data/reference/satellite_esri.jpg")
    out.parent.mkdir(exist_ok=True)
    out.write_bytes(r.content)
    print(f"✓ Saved: {out} ({len(r.content)} bytes)")
    
    # Converter pra PNG 512x512
    from PIL import Image
    img = Image.open(out)
    img = img.resize((512, 512), Image.LANCZOS)
    png_out = Path("data/reference/satellite_512.png")
    img.save(png_out, "PNG")
    print(f"✓ Resized: {png_out}")
else:
    print(f"✗ Error: {r.status_code}")
