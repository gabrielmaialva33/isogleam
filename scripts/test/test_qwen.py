#!/usr/bin/env python3
"""Test Qwen Image Edit via Gradio API"""
import requests
import base64
import json
import sys
from pathlib import Path

QWEN_URL = "http://localhost:7860/api/predict"

def image_to_base64(path: str) -> str:
    """Convert image file to base64"""
    with open(path, "rb") as f:
        return base64.b64encode(f.read()).decode("utf-8")

def test_qwen_health():
    """Check if Qwen is running"""
    try:
        r = requests.get("http://localhost:7860/", timeout=5)
        return r.status_code == 200
    except:
        return False

def generate_pixel_art(input_image: str, output_path: str):
    """Generate pixel art from 3D render"""

    # IsoGleam prompt - our secret sauce
    prompt = """Transform into isometric pixel art, SimCity 2000 style:
    - 16-bit graphics aesthetic
    - Ordered dithering patterns (Bayer matrix)
    - Limited color palette (max 64 colors)
    - Clean pixel edges, no anti-aliasing
    - Bird's eye isometric view
    - Nostalgic retro game feel"""

    # Encode image
    img_b64 = image_to_base64(input_image)

    # Gradio API format
    payload = {
        "data": [
            f"data:image/png;base64,{img_b64}",  # Image
            prompt,                               # Prompt
            "Square (1024x1024)"                  # Resolution
        ]
    }

    print(f"Sending request to Qwen...")
    print(f"Input: {input_image}")
    print(f"Prompt: {prompt[:50]}...")

    try:
        response = requests.post(QWEN_URL, json=payload, timeout=300)

        if response.status_code == 200:
            result = response.json()
            print(f"Response: {json.dumps(result, indent=2)[:200]}...")

            # Extract output image from response
            if "data" in result and len(result["data"]) > 0:
                output_data = result["data"][0]

                # Save if it's base64
                if isinstance(output_data, str) and output_data.startswith("data:image"):
                    img_data = output_data.split(",")[1]
                    with open(output_path, "wb") as f:
                        f.write(base64.b64decode(img_data))
                    print(f"Saved output to: {output_path}")
                    return True

            print("Unexpected response format")
            return False
        else:
            print(f"Error: {response.status_code}")
            print(response.text[:500])
            return False

    except requests.exceptions.Timeout:
        print("Request timed out (5 min limit)")
        return False
    except Exception as e:
        print(f"Error: {e}")
        return False

if __name__ == "__main__":
    print("=== IsoGleam Qwen Test ===\n")

    # Check health
    if not test_qwen_health():
        print("ERROR: Qwen not running on localhost:7860")
        print("Start with: cd ~/qwen-image-edit-docker && docker compose up -d")
        sys.exit(1)

    print("Qwen is running!\n")

    # Test generation
    assets_dir = Path(__file__).parent.parent.parent / "test" / "assets"
    input_path = assets_dir / "test_3d_render.png"
    output_path = assets_dir / "test_pixel_art.png"

    if not input_path.exists():
        print(f"ERROR: Input image not found: {input_path}")
        sys.exit(1)

    success = generate_pixel_art(str(input_path), str(output_path))

    if success:
        print("\n✅ Generation successful!")
        print(f"Compare: {input_path} → {output_path}")
    else:
        print("\n❌ Generation failed")
        sys.exit(1)
