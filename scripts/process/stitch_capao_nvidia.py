#!/usr/bin/env python3
"""
Stitch dos tiles NVIDIA em mapa de Capão Bonito
"""
import numpy as np
from PIL import Image
from pathlib import Path

INPUT_DIR = Path("data/capao_nvidia")
OUTPUT = Path("data/capao_nvidia/mapa_capao_bonito.png")

# Layout do centro de Capão Bonito (3x4 grid)
LAYOUT = [
    ["parque",        "praca_central", "igreja_matriz", "prefeitura"],
    ["residencial_01", "comercio_01",   "mercado",      "hospital"],
    ["residencial_02", "escola",        "posto_gasolina", "rodoviaria"],
]

TILE_SIZE = 512
BLEND = 32  # pixels de overlap

def load_tile(name: str) -> np.ndarray | None:
    path = INPUT_DIR / f"{name}.png"
    if path.exists():
        return np.array(Image.open(path).convert("RGB"))
    return None

def create_map():
    rows = len(LAYOUT)
    cols = len(LAYOUT[0])
    
    # Tamanho com blend
    effective = TILE_SIZE - BLEND
    width = cols * effective + BLEND
    height = rows * effective + BLEND
    
    canvas = np.zeros((height, width, 3), dtype=np.float32)
    weights = np.zeros((height, width, 1), dtype=np.float32)
    
    # Máscara de peso (fade nas bordas)
    tile_weight = np.ones((TILE_SIZE, TILE_SIZE, 1), dtype=np.float32)
    for i in range(BLEND):
        alpha = i / BLEND
        tile_weight[i, :, 0] *= alpha
        tile_weight[-(i+1), :, 0] *= alpha
        tile_weight[:, i, 0] *= alpha
        tile_weight[:, -(i+1), 0] *= alpha
    
    for row_idx, row in enumerate(LAYOUT):
        for col_idx, tile_name in enumerate(row):
            tile = load_tile(tile_name)
            if tile is None:
                print(f"  ⚠ Tile não encontrado: {tile_name}")
                continue
            
            x = col_idx * effective
            y = row_idx * effective
            
            # Blend weighted
            region = canvas[y:y+TILE_SIZE, x:x+TILE_SIZE]
            w_region = weights[y:y+TILE_SIZE, x:x+TILE_SIZE]
            
            new_weight = w_region + tile_weight
            canvas[y:y+TILE_SIZE, x:x+TILE_SIZE] = (
                region * w_region + tile.astype(np.float32) * tile_weight
            ) / (new_weight + 1e-6)
            weights[y:y+TILE_SIZE, x:x+TILE_SIZE] = new_weight
            
            print(f"  ✓ {tile_name} @ ({col_idx}, {row_idx})")
    
    return canvas.astype(np.uint8)

def main():
    print("=" * 60)
    print("Mapa de Capão Bonito - Stitch")
    print("=" * 60)
    
    mapa = create_map()
    
    img = Image.fromarray(mapa)
    img.save(OUTPUT, "PNG")
    
    print(f"\n✓ Mapa salvo: {OUTPUT}")
    print(f"✓ Tamanho: {mapa.shape[1]}x{mapa.shape[0]} px")

if __name__ == "__main__":
    main()
