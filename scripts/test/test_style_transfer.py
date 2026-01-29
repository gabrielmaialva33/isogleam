#!/usr/bin/env python3
"""
Style transfer: Satélite → Pixel art isométrico
Usando Qwen-Image-Edit com imagem de input
"""
from gradio_client import Client, handle_file
from pathlib import Path
from PIL import Image

PROJECT_ROOT = Path(__file__).parent.parent.parent
INPUT = PROJECT_ROOT / "data" / "inputs" / "reference" / "satellite_512.png"
OUTPUT = PROJECT_ROOT / "data" / "outputs" / "tiles" / "test_style_transfer.png"
OUTPUT.parent.mkdir(parents=True, exist_ok=True)

# Conectar ao Qwen
client = Client("http://localhost:7860")

# Prompt de transformação
PROMPT = """Transform this satellite image into isometric pixel art style.
SimCity 2000 aesthetic, 16-bit graphics, limited color palette, dithering.
Convert top-down view to isometric 45-degree angle.
Keep the same building layout and street pattern."""

print(f"Input: {INPUT}")
print(f"Prompt: {PROMPT[:50]}...")

# Testar diferentes endpoints
try:
    # Tentar image editing endpoint
    result = client.predict(
        handle_file(str(INPUT)),
        PROMPT,
        api_name="/edit_image"
    )
    print(f"Result: {result}")
    
    if result:
        img = Image.open(result)
        img.save(OUTPUT, "PNG")
        print(f"✓ Saved: {OUTPUT}")
except Exception as e:
    print(f"edit_image failed: {e}")
    
    # Tentar com variation
    try:
        result = client.predict(
            handle_file(str(INPUT)),
            PROMPT,
            0.8,  # strength
            api_name="/create_variation"
        )
        print(f"Variation result: {result}")
        if result:
            img = Image.open(result)
            img.save(OUTPUT, "PNG")
            print(f"✓ Saved: {OUTPUT}")
    except Exception as e2:
        print(f"create_variation failed: {e2}")
        
        # Listar endpoints disponíveis
        print("\nAvailable endpoints:")
        for fn in client.view_api():
            print(f"  - {fn}")
