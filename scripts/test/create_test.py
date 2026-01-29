#!/usr/bin/env python3
"""Create test image for Qwen Image Edit"""
from PIL import Image, ImageDraw

from pathlib import Path

# Create a simple isometric building placeholder
img = Image.new('RGB', (512, 512), (135, 206, 235))  # Sky blue background
draw = ImageDraw.Draw(img)

# Ground plane (green)
draw.polygon([(0, 350), (256, 450), (512, 350), (512, 512), (0, 512)], fill=(34, 139, 34))

# Simple building (gray cube)
# Front face
draw.polygon([(156, 200), (256, 250), (256, 400), (156, 350)], fill=(128, 128, 128))
# Right face
draw.polygon([(256, 250), (356, 200), (356, 350), (256, 400)], fill=(96, 96, 96))
# Top face
draw.polygon([(156, 200), (256, 150), (356, 200), (256, 250)], fill=(160, 160, 160))

# Road
draw.polygon([(0, 380), (200, 440), (200, 480), (0, 420)], fill=(64, 64, 64))

output_path = Path(__file__).parent.parent.parent / "test" / "assets" / "test_3d_render.png"
img.save(output_path)
print(f"Created {output_path} (512x512)")
