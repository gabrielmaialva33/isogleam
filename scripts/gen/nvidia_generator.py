#!/usr/bin/env python3
"""
IsoGleam Generator - NVIDIA NIM API (5 keys = 200 RPM)
Roubado do IsoCity-fal, adaptado pra local
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

# Pool de API keys NVIDIA (40 RPM cada)
NVIDIA_KEYS = [
    os.getenv("NVIDIA_API_KEY_1"),
    os.getenv("NVIDIA_API_KEY_2"),
    os.getenv("NVIDIA_API_KEY_3"),
    os.getenv("NVIDIA_API_KEY_4"),
    os.getenv("NVIDIA_API_KEY_5"),
]
NVIDIA_KEYS = [k for k in NVIDIA_KEYS if k]  # Remove None
key_pool = cycle(NVIDIA_KEYS)

def get_next_key():
    return next(key_pool)

# Endpoints NVIDIA
NVIDIA_BASE = "https://integrate.api.nvidia.com/v1"
SDXL_ENDPOINT = f"{NVIDIA_BASE}/images/generations"

# Prompts roubados
def building_prompt(description: str) -> str:
    return f"""Highly detailed realistic isometric building render: {description}.
Style: Professional isometric city-builder game asset, 45-degree isometric perspective, photorealistic materials and textures, intricate architectural details like windows, doors, roof tiles, bricks, and trim work. Similar quality to Cities Skylines or SimCity 4 building assets.
CRITICAL PROJECTION: Use TRUE ISOMETRIC DIMETRIC PROJECTION with EXACTLY 26.565-degree vertical angle. The ground plane MUST be at precisely 30-degree angles from horizontal (left and right edges). All horizontal lines must be parallel and at 30-degree angles. This is non-negotiable for game asset compatibility.
CRITICAL COMPOSITION: The building must be PERFECTLY CENTERED both horizontally and vertically in the image. The building's center axis must align exactly with the image center. Equal empty space on left and right sides. The building base should be centered at the bottom third of the image.
CRITICAL FRAMING: The ENTIRE building must be fully visible from base to rooftop with NO cropping. Minimize padding - leave only THIN margins (5-10% of image size) on all sides. The building should fill most of the frame while still being completely visible.
CRITICAL BASE REQUIREMENT: The building MUST include a ground-level base/platform/plot that is FLAT and perfectly aligned with the isometric ground plane. The base should occupy 25-35% of the image height and include pavement, plaza tiles, sidewalk, grass patches, small landscaping, or decorative ground details.
Technical: Sharp clean render, studio lighting, pure white or light gray solid background, single isolated building with its ground plot, no additional shadows cast outside the asset.
Quality: Ultra high detail, realistic proportions, natural color palette, visible textures (glass, metal, stone, brick, pavement, grass), professional 3D render quality."""

def pixelart_prompt(description: str) -> str:
    return f"""Isometric pixel art building: {description}.
Style: SimCity 2000, RollerCoaster Tycoon 2, 16-bit graphics, limited color palette (64 colors max), visible dithering patterns, retro game aesthetic.
CRITICAL PROJECTION: TRUE ISOMETRIC with 26.565-degree vertical angle, 30-degree ground plane angles.
CRITICAL: Pixel-perfect edges, NO anti-aliasing, NO smooth gradients, clean pixel art.
CRITICAL BASE: Flat isometric ground tile, grass or pavement, aligned to pixel grid.
Background: Pure white (#FFFFFF) for easy removal.
Quality: Clean pixel art, consistent pixel size, retro SimCity 2000 aesthetic."""

def generate_with_nvidia(prompt: str, output_path: Path, style: str = "realistic") -> bool:
    """Gera imagem usando NVIDIA SDXL API"""
    key = get_next_key()
    
    # Escolher prompt baseado no estilo
    if style == "pixel":
        full_prompt = pixelart_prompt(prompt)
    else:
        full_prompt = building_prompt(prompt)
    
    headers = {
        "Authorization": f"Bearer {key}",
        "Content-Type": "application/json",
    }
    
    payload = {
        "model": "stabilityai/stable-diffusion-xl-base-1.0",
        "prompt": full_prompt,
        "negative_prompt": "blurry, low quality, distorted, cropped, cut off, partial building, multiple buildings",
        "height": 1024,
        "width": 1024,
        "steps": 30,
        "cfg_scale": 7.5,
    }
    
    try:
        print(f"  Gerando com NVIDIA SDXL...")
        r = requests.post(SDXL_ENDPOINT, headers=headers, json=payload, timeout=60)
        
        if r.status_code == 200:
            data = r.json()
            if "artifacts" in data and len(data["artifacts"]) > 0:
                img_b64 = data["artifacts"][0]["base64"]
                img_bytes = base64.b64decode(img_b64)
                img = Image.open(BytesIO(img_bytes))
                img.save(output_path, "PNG")
                print(f"  ✓ Salvo: {output_path}")
                return True
        else:
            print(f"  ✗ Erro NVIDIA: {r.status_code} - {r.text[:200]}")
            return False
    except Exception as e:
        print(f"  ✗ Exception: {e}")
        return False

def remove_background(input_path: Path, output_path: Path) -> bool:
    """Remove fundo usando rembg local"""
    try:
        from rembg import remove
        img = Image.open(input_path)
        output = remove(img)
        output.save(output_path, "PNG")
        print(f"  ✓ Background removido: {output_path}")
        return True
    except ImportError:
        print("  ⚠ rembg não instalado, pulando remoção de fundo")
        import shutil
        shutil.copy(input_path, output_path)
        return True
    except Exception as e:
        print(f"  ✗ Erro rembg: {e}")
        return False

def main():
    print("=" * 60)
    print("IsoGleam Generator - NVIDIA NIM API")
    print(f"Keys disponíveis: {len(NVIDIA_KEYS)}")
    print(f"RPM total: {len(NVIDIA_KEYS) * 40}")
    print("=" * 60)
    
    # Teste: gerar building de Capão Bonito
    test_buildings = [
        "Brazilian small town church with bell tower, colonial style, white walls, red tile roof",
        "Small Brazilian residential house, simple design, red brick walls, terracotta roof tiles",
        "Local Brazilian bakery shop, colorful facade, awning, display window with bread",
        "Brazilian town square with central fountain, benches, palm trees",
        "Small Brazilian pharmacy, green cross sign, glass storefront",
    ]
    
    output_dir = Path(__file__).parent.parent.parent / "data" / "outputs" / "buildings"
    output_dir.mkdir(parents=True, exist_ok=True)
    
    for i, desc in enumerate(test_buildings):
        print(f"\n[{i+1}/{len(test_buildings)}] {desc[:50]}...")
        
        raw_path = output_dir / f"building_{i+1:02d}_raw.png"
        final_path = output_dir / f"building_{i+1:02d}.png"
        
        if generate_with_nvidia(desc, raw_path, style="realistic"):
            remove_background(raw_path, final_path)
        
        time.sleep(1.5)  # Rate limiting
    
    print("\n" + "=" * 60)
    print(f"✓ Geração completa! Output: {output_dir}")
    
if __name__ == "__main__":
    main()
