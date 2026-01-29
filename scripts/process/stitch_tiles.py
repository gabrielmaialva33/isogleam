#!/usr/bin/env python3
"""
IsoGleam Tile Stitching
Junta tiles adjacentes com blending suave nas bordas
"""

import json
import numpy as np
from PIL import Image
from pathlib import Path
from dataclasses import dataclass

# Config
DATA_DIR = Path(__file__).parent.parent.parent / "data"
TILES_DIR = DATA_DIR / "outputs" / "tiles"
STITCHED_DIR = DATA_DIR / "outputs" / "stitched"
STITCHED_DIR.mkdir(exist_ok=True)

TILE_SIZE = 512
BLEND_WIDTH = 32  # Pixels de overlap para blending

@dataclass
class TilePair:
    tile_a: str
    tile_b: str
    direction: str  # "horizontal" or "vertical"

def load_tile(name: str) -> np.ndarray | None:
    """Carrega tile como numpy array"""
    path = TILES_DIR / f"{name}.png"
    if not path.exists():
        return None
    img = Image.open(path).convert("RGB")
    return np.array(img)

def create_blend_mask(size: int, width: int, direction: str) -> np.ndarray:
    """Cria máscara de gradiente para blending"""
    if direction == "horizontal":
        # Gradiente horizontal (esquerda para direita)
        gradient = np.linspace(0, 1, width)
        mask = np.tile(gradient, (size, 1))
    else:
        # Gradiente vertical (baixo para cima)
        gradient = np.linspace(0, 1, width)
        mask = np.tile(gradient.reshape(-1, 1), (1, size))
    return mask

def blend_tiles_horizontal(left: np.ndarray, right: np.ndarray) -> np.ndarray:
    """Junta dois tiles horizontalmente com blending"""
    h, w, c = left.shape

    # Criar imagem combinada (largura = 2*w - blend_width)
    combined_w = 2 * w - BLEND_WIDTH
    combined = np.zeros((h, combined_w, c), dtype=np.uint8)

    # Copiar tile esquerdo
    combined[:, :w-BLEND_WIDTH, :] = left[:, :w-BLEND_WIDTH, :]

    # Copiar tile direito
    combined[:, w:, :] = right[:, BLEND_WIDTH:, :]

    # Blending na região de overlap
    mask = create_blend_mask(h, BLEND_WIDTH, "horizontal")
    mask = np.stack([mask] * c, axis=-1)

    overlap_left = left[:, w-BLEND_WIDTH:, :].astype(float)
    overlap_right = right[:, :BLEND_WIDTH, :].astype(float)

    blended = overlap_left * (1 - mask) + overlap_right * mask
    combined[:, w-BLEND_WIDTH:w, :] = blended.astype(np.uint8)

    return combined

def blend_tiles_vertical(bottom: np.ndarray, top: np.ndarray) -> np.ndarray:
    """Junta dois tiles verticalmente com blending"""
    h, w, c = bottom.shape

    # Criar imagem combinada (altura = 2*h - blend_width)
    combined_h = 2 * h - BLEND_WIDTH
    combined = np.zeros((combined_h, w, c), dtype=np.uint8)

    # Copiar tile de baixo
    combined[:h-BLEND_WIDTH, :, :] = bottom[:h-BLEND_WIDTH, :, :]

    # Copiar tile de cima
    combined[h:, :, :] = top[BLEND_WIDTH:, :, :]

    # Blending na região de overlap
    mask = create_blend_mask(w, BLEND_WIDTH, "vertical")
    mask = np.stack([mask] * c, axis=-1)

    overlap_bottom = bottom[h-BLEND_WIDTH:, :, :].astype(float)
    overlap_top = top[:BLEND_WIDTH, :, :].astype(float)

    blended = overlap_bottom * (1 - mask) + overlap_top * mask
    combined[h-BLEND_WIDTH:h, :, :] = blended.astype(np.uint8)

    return combined

def stitch_pair(pair: TilePair) -> Path | None:
    """Faz stitch de um par de tiles"""
    tile_a = load_tile(pair.tile_a)
    tile_b = load_tile(pair.tile_b)

    if tile_a is None or tile_b is None:
        return None

    if pair.direction == "horizontal":
        combined = blend_tiles_horizontal(tile_a, tile_b)
    else:
        combined = blend_tiles_vertical(tile_a, tile_b)

    # Salvar
    output_name = f"{pair.tile_a}_{pair.tile_b}.png"
    output_path = STITCHED_DIR / output_name

    Image.fromarray(combined).save(output_path)
    return output_path

def find_adjacent_pairs() -> list[TilePair]:
    """Encontra todos os pares de tiles adjacentes"""
    generated = [f.stem for f in TILES_DIR.glob("tile_*.png")]
    generated_set = set(generated)

    pairs = []
    for name in generated:
        parts = name.split("_")
        if len(parts) != 3:
            continue
        x, y = int(parts[1]), int(parts[2])

        # Vizinho direito
        right = f"tile_{x+1:02d}_{y:02d}"
        if right in generated_set:
            pairs.append(TilePair(name, right, "horizontal"))

        # Vizinho acima
        up = f"tile_{x:02d}_{y+1:02d}"
        if up in generated_set:
            pairs.append(TilePair(name, up, "vertical"))

    return pairs

def create_grid_stitch(tiles: list[tuple[int, int, str]]) -> np.ndarray | None:
    """Cria um grid stitched de múltiplos tiles"""
    if not tiles:
        return None

    # Encontrar bounds
    xs = [t[0] for t in tiles]
    ys = [t[1] for t in tiles]
    min_x, max_x = min(xs), max(xs)
    min_y, max_y = min(ys), max(ys)

    grid_w = max_x - min_x + 1
    grid_h = max_y - min_y + 1

    # Tamanho com blending
    tile_effective = TILE_SIZE - BLEND_WIDTH
    canvas_w = grid_w * tile_effective + BLEND_WIDTH
    canvas_h = grid_h * tile_effective + BLEND_WIDTH

    canvas = np.zeros((canvas_h, canvas_w, 3), dtype=np.uint8)
    weight = np.zeros((canvas_h, canvas_w, 1), dtype=np.float32)

    # Criar máscara de peso para cada tile
    tile_weight = np.ones((TILE_SIZE, TILE_SIZE, 1), dtype=np.float32)
    # Fade nas bordas
    for i in range(BLEND_WIDTH):
        alpha = i / BLEND_WIDTH
        tile_weight[i, :, 0] *= alpha  # Top
        tile_weight[-(i+1), :, 0] *= alpha  # Bottom
        tile_weight[:, i, 0] *= alpha  # Left
        tile_weight[:, -(i+1), 0] *= alpha  # Right

    # Colocar cada tile
    for x, y, name in tiles:
        tile = load_tile(name)
        if tile is None:
            continue

        # Posição no canvas
        px = (x - min_x) * tile_effective
        py = (max_y - y) * tile_effective  # Inverter Y

        # Adicionar com peso
        canvas_region = canvas[py:py+TILE_SIZE, px:px+TILE_SIZE].astype(np.float32)
        weight_region = weight[py:py+TILE_SIZE, px:px+TILE_SIZE]

        canvas[py:py+TILE_SIZE, px:px+TILE_SIZE] = (
            canvas_region * weight_region + tile.astype(np.float32) * tile_weight
        ) / (weight_region + tile_weight + 1e-6)

        weight[py:py+TILE_SIZE, px:px+TILE_SIZE] += tile_weight

    return canvas.astype(np.uint8)

def main():
    print("=" * 60)
    print("IsoGleam Tile Stitching")
    print("=" * 60)

    # Encontrar pares
    pairs = find_adjacent_pairs()
    print(f"\nPares adjacentes: {len(pairs)}")

    # Stitch pares individuais
    print("\n--- Stitching pares ---")
    stitched = 0
    for pair in pairs[:10]:  # Primeiros 10
        print(f"  {pair.tile_a} + {pair.tile_b}...", end=" ")
        result = stitch_pair(pair)
        if result:
            print("✓")
            stitched += 1
        else:
            print("✗")

    # Criar grid do centro urbano
    print("\n--- Criando grid central ---")

    # Carregar todos os tiles com coordenadas
    tiles = []
    for f in TILES_DIR.glob("tile_*.png"):
        parts = f.stem.split("_")
        if len(parts) == 3:
            x, y = int(parts[1]), int(parts[2])
            tiles.append((x, y, f.stem))

    # Encontrar cluster central (tiles próximos)
    if tiles:
        # Centro aproximado
        center_x = sum(t[0] for t in tiles) // len(tiles)
        center_y = sum(t[1] for t in tiles) // len(tiles)

        # Filtrar tiles próximos do centro (raio 3)
        central_tiles = [
            t for t in tiles
            if abs(t[0] - center_x) <= 3 and abs(t[1] - center_y) <= 3
        ]

        print(f"  Tiles no cluster central: {len(central_tiles)}")

        if len(central_tiles) >= 4:
            grid = create_grid_stitch(central_tiles)
            if grid is not None:
                output_path = STITCHED_DIR / "central_grid.png"
                Image.fromarray(grid).save(output_path)
                print(f"  ✓ Grid salvo: {output_path}")
                print(f"  ✓ Tamanho: {grid.shape[1]}x{grid.shape[0]} px")

    # Resumo
    print("\n" + "=" * 60)
    print("RESUMO")
    print("=" * 60)
    print(f"Pares stitched: {stitched}")
    print(f"Output: {STITCHED_DIR}")

    # Listar outputs
    outputs = list(STITCHED_DIR.glob("*.png"))
    print(f"Arquivos gerados: {len(outputs)}")

    print("\n✓ Stitching completo!")

if __name__ == "__main__":
    main()
