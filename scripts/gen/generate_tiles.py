#!/usr/bin/env python3
"""
IsoGleam Tile Generator
Gera tiles isométricos usando Qwen-Image-Edit via Gradio API
"""

import json
import time
import shutil
from pathlib import Path
from datetime import datetime
from PIL import Image
from gradio_client import Client

# Config
QWEN_URL = "http://localhost:7860"
DATA_DIR = Path(__file__).parent.parent.parent / "data"
OUTPUT_DIR = DATA_DIR / "outputs" / "tiles"
OUTPUT_DIR.mkdir(exist_ok=True)

# Generation params
RESOLUTION = "Standard (512x512)"
LORA_WEIGHT = 0.8

def generate_tile(client: Client, prompt: str, tile_id: str) -> Path | None:
    """Gera um tile usando Qwen via Gradio API"""
    try:
        result = client.predict(
            prompt,
            RESOLUTION,
            LORA_WEIGHT,
            api_name="/generate_pixel_art"
        )

        if result:
            output_path = OUTPUT_DIR / f"{tile_id}.png"

            # Carregar e converter para PNG (resultado pode ser webp)
            img = Image.open(result)
            img.save(output_path, "PNG")

            return output_path

    except Exception as e:
        print(f"Erro: {e}")

    return None

def main():
    print("=" * 60)
    print("IsoGleam Tile Generator")
    print("=" * 60)

    # Carregar prompts
    prompts_file = DATA_DIR / "grid" / "prompts.json"
    with open(prompts_file) as f:
        prompts = json.load(f)

    print(f"\nCarregados {len(prompts)} prompts")
    print(f"Output: {OUTPUT_DIR}")

    # Conectar ao Qwen
    print(f"\nConectando ao Qwen em {QWEN_URL}...")
    try:
        client = Client(QWEN_URL)
        print("  ✓ Conectado")
    except Exception as e:
        print(f"  ✗ Erro: {e}")
        return

    # Gerar tiles
    print("\n" + "=" * 60)
    print("GERANDO TILES")
    print("=" * 60)

    generated = 0
    skipped = 0
    failed = 0
    start_time = time.time()

    for i, p in enumerate(prompts):
        tile_id = p["id"]
        output_path = OUTPUT_DIR / f"{tile_id}.png"

        # Skip se já existe
        if output_path.exists():
            print(f"[{i+1}/{len(prompts)}] {tile_id}: já existe")
            skipped += 1
            continue

        print(f"[{i+1}/{len(prompts)}] {tile_id}...", end=" ", flush=True)

        # Gerar
        result = generate_tile(client, p["prompt"], tile_id)

        if result:
            print(f"✓")
            generated += 1
        else:
            print("✗")
            failed += 1

        # Rate limit mínimo
        time.sleep(0.2)

    # Resumo
    elapsed = time.time() - start_time
    print("\n" + "=" * 60)
    print("RESUMO")
    print("=" * 60)
    print(f"Gerados: {generated}")
    print(f"Pulados: {skipped}")
    print(f"Falhas: {failed}")
    if generated > 0:
        print(f"Tempo: {elapsed:.1f}s ({elapsed/generated:.1f}s/tile)")
    print(f"Output: {OUTPUT_DIR}")

    # Salvar log
    log = {
        "timestamp": datetime.now().isoformat(),
        "generated": generated,
        "skipped": skipped,
        "failed": failed,
        "elapsed_seconds": elapsed,
        "output_dir": str(OUTPUT_DIR)
    }
    with open(OUTPUT_DIR / "generation_log.json", "w") as f:
        json.dump(log, f, indent=2)

    print("\n✓ Geração completa!")

if __name__ == "__main__":
    main()
