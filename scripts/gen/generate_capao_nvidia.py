#!/usr/bin/env python3
"""
IsoGleam - Gerador de Capão Bonito com NVIDIA API
5 keys = 200 RPM = custo $0 = VRAM livre
"""
import os
import json
import base64
import requests
from pathlib import Path
from PIL import Image
from io import BytesIO
import time
from itertools import cycle

# Pool de API keys NVIDIA
NVIDIA_KEYS = [k for k in [
    os.getenv("NVIDIA_API_KEY_1"),
    os.getenv("NVIDIA_API_KEY_2"),
    os.getenv("NVIDIA_API_KEY_3"),
    os.getenv("NVIDIA_API_KEY_4"),
    os.getenv("NVIDIA_API_KEY_5"),
] if k]
key_pool = cycle(NVIDIA_KEYS)

ENDPOINT = "https://ai.api.nvidia.com/v1/genai/stabilityai/stable-diffusion-xl"
OUTPUT_DIR = Path("data/capao_nvidia")
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# Prompt base roubado + adaptado pra Capão Bonito
def make_prompt(tile_desc: str) -> str:
    return f"""isometric pixel art city tile of Brazilian small town, SimCity 2000 style, 16-bit graphics, {tile_desc}.
Style: RollerCoaster Tycoon 2 aesthetic, limited color palette, visible pixel grid, dithering patterns.
CRITICAL: TRUE ISOMETRIC projection, 26.565-degree vertical angle, 30-degree ground plane.
CRITICAL: Pixel-perfect edges, NO anti-aliasing, NO smooth gradients, clean retro pixels.
Features: Red tile roofs typical of Brazilian interior towns, white/cream walls, palm trees, tropical vegetation.
Background: Light gray (#E0E0E0) for easy compositing."""

NEGATIVE = "blurry, realistic, photo, 3d render, antialiased, smooth gradients, modern, high resolution, dark, night"

# Tiles de Capão Bonito centro
TILES = [
    {"id": "praca_central", "desc": "central town square with bandstand gazebo, park benches, fountain, palm trees, colonial church in background"},
    {"id": "igreja_matriz", "desc": "large white colonial church with bell tower, red tile roof, stone steps, plaza in front"},
    {"id": "comercio_01", "desc": "row of small shops, bakery, pharmacy, colorful facades, awnings, pedestrians"},
    {"id": "residencial_01", "desc": "residential block with small houses, red tile roofs, small yards, trees"},
    {"id": "residencial_02", "desc": "residential houses with gardens, fences, parked cars, street lamps"},
    {"id": "escola", "desc": "public school building, two floors, Brazilian flag, playground, students"},
    {"id": "hospital", "desc": "small town hospital, white building, red cross, ambulance, emergency entrance"},
    {"id": "prefeitura", "desc": "town hall building, Brazilian flag, colonial architecture, plaza"},
    {"id": "mercado", "desc": "local market building, open stalls, fruits, vegetables, busy people"},
    {"id": "posto_gasolina", "desc": "gas station with canopy, fuel pumps, convenience store, cars"},
    {"id": "parque", "desc": "town park with walking paths, benches, playground, trees, families"},
    {"id": "rodoviaria", "desc": "bus station, buses, ticket booth, passengers, luggage"},
]

def generate_tile(tile: dict) -> bool:
    key = next(key_pool)
    prompt = make_prompt(tile["desc"])
    
    payload = {
        "text_prompts": [
            {"text": prompt, "weight": 1},
            {"text": NEGATIVE, "weight": -1}
        ],
        "cfg_scale": 8,
        "height": 1024,
        "width": 1024,
        "steps": 35,
        "seed": hash(tile["id"]) % 2**32
    }
    
    headers = {
        "Authorization": f"Bearer {key}",
        "Content-Type": "application/json",
        "Accept": "application/json"
    }
    
    try:
        r = requests.post(ENDPOINT, headers=headers, json=payload, timeout=90)
        if r.status_code == 200:
            data = r.json()
            if "artifacts" in data:
                img_b64 = data["artifacts"][0]["base64"]
                img_bytes = base64.b64decode(img_b64)
                
                # Salvar 1024px
                out_full = OUTPUT_DIR / f"{tile['id']}_1024.png"
                out_full.write_bytes(img_bytes)
                
                # Resize pra 512px (tile final)
                img = Image.open(BytesIO(img_bytes))
                img_512 = img.resize((512, 512), Image.LANCZOS)
                out_512 = OUTPUT_DIR / f"{tile['id']}.png"
                img_512.save(out_512, "PNG")
                
                print(f"  ✓ {tile['id']}")
                return True
        else:
            print(f"  ✗ {tile['id']}: HTTP {r.status_code}")
            return False
    except Exception as e:
        print(f"  ✗ {tile['id']}: {e}")
        return False

def main():
    print("=" * 60)
    print("IsoGleam - Capão Bonito Generator (NVIDIA API)")
    print(f"Keys: {len(NVIDIA_KEYS)} | RPM: {len(NVIDIA_KEYS)*40} | Tiles: {len(TILES)}")
    print("=" * 60)
    
    success = 0
    for i, tile in enumerate(TILES):
        print(f"[{i+1}/{len(TILES)}] {tile['id']}...", end=" ", flush=True)
        if generate_tile(tile):
            success += 1
        time.sleep(2)  # Rate limit
    
    print("\n" + "=" * 60)
    print(f"✓ Gerados: {success}/{len(TILES)}")
    print(f"✓ Output: {OUTPUT_DIR}")

if __name__ == "__main__":
    main()
