#!/usr/bin/env python3
"""
img2img: Satélite → Pixel art isométrico
Usando Stable Diffusion XL img2img
"""
import torch
from diffusers import AutoPipelineForImage2Image
from PIL import Image
from pathlib import Path

INPUT = Path("data/reference/satellite_512.png")
OUTPUT = Path("data/tiles/test_img2img.png")

print("Loading SDXL img2img pipeline...")
print(f"VRAM disponível: {torch.cuda.get_device_properties(0).total_memory / 1e9:.1f} GB")

# Carregar modelo (vai baixar ~6GB na primeira vez)
pipe = AutoPipelineForImage2Image.from_pretrained(
    "stabilityai/sdxl-turbo",
    torch_dtype=torch.float16,
    variant="fp16",
)
pipe.to("cuda")
pipe.enable_model_cpu_offload()

print("✓ Pipeline loaded")

# Carregar imagem de input
init_image = Image.open(INPUT).convert("RGB").resize((512, 512))

# Prompt de estilo
prompt = """isometric pixel art city tile, SimCity 2000 style, 16-bit graphics, 
dithering, limited color palette, bird's eye view 45 degrees, 
residential houses with red tile roofs, paved roads"""

negative_prompt = "blurry, smooth gradients, realistic, photo, 3d render, antialiased"

print(f"Generating with strength=0.7...")

# Gerar
image = pipe(
    prompt=prompt,
    negative_prompt=negative_prompt,
    image=init_image,
    strength=0.7,  # 0.7 = mantém 30% da estrutura original
    guidance_scale=7.5,
    num_inference_steps=20,
).images[0]

image.save(OUTPUT)
print(f"✓ Saved: {OUTPUT}")
