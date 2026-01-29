import os
import base64
import io
import logging
from typing import List, Optional
from fastapi import FastAPI, HTTPException, Body
from pydantic import BaseModel
import torch
from PIL import Image
import numpy as np
import uvicorn

# Setup logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("isogleam-brain")

app = FastAPI(title="IsoGleam Local AI Brain", version="1.0.0")

# --- Global Models (Lazy Loaded) ---
MODELS = {
    "pipeline": None,
    "clip_model": None,
    "clip_processor": None,
    "controlnet": None
}

DEVICE = "cuda" if torch.cuda.is_available() else "cpu"
logger.info(f"Running on device: {DEVICE}")

# --- Pydantic Models ---

class GenerateRequest(BaseModel):
    prompt: str
    # Research-backed negative prompt for isometric game assets
    negative_prompt: str = (
        "perspective, vanishing point, diagonal lines, cropped, "
        "symmetry, intricate details, harsh lighting, shadows, "
        "tiling, patterns, borders, frame, "
        "text, watermark, signature, logo, "
        "low quality, blurry, distorted, deformed, disfigured, "
        "photorealistic, photograph, realistic"
    )
    width: int = 512
    height: int = 512
    steps: int = 25  # Sweet spot for quality/speed on 4090
    guidance_scale: float = 7.5  # Good for structured assets
    control_image_b64: Optional[str] = None
    seed: int = -1

class GenerateResponse(BaseModel):
    image_b64: str
    seed: int

class ClipEmbedRequest(BaseModel):
    image_b64: str

class ClipEmbedResponse(BaseModel):
    embedding: List[float]

class ClassifyRequest(BaseModel):
    image_b64: str
    labels: List[str]

class ClassificationResponse(BaseModel):
    labels: List[str]
    scores: List[float]

# --- Helper Functions ---

def decode_image(b64_str: str) -> Image.Image:
    try:
        image_data = base64.b64decode(b64_str)
        return Image.open(io.BytesIO(image_data)).convert("RGB")
    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Invalid image data: {e}")

def encode_image(image: Image.Image) -> str:
    buffered = io.BytesIO()
    image.save(buffered, format="PNG")
    return base64.b64encode(buffered.getvalue()).decode("utf-8")

# --- Model Loading ---

def get_pipeline():
    if MODELS["pipeline"] is None:
        logger.info("Loading Stable Diffusion Pipeline...")
        from diffusers import StableDiffusionControlNetPipeline, ControlNetModel, UniPCMultistepScheduler

        # Use a high-quality isometric suitable model
        # For simplicity and speed/quality balance on a 4090, using a solid 1.5 base or dreamshaper typically works very well for game assets
        model_id = "runwayml/stable-diffusion-v1-5"
        controlnet_id = "lllyasviel/control_v11p_sd15_canny"

        controlnet = ControlNetModel.from_pretrained(controlnet_id, torch_dtype=torch.float16)
        pipe = StableDiffusionControlNetPipeline.from_pretrained(
            model_id, controlnet=controlnet, torch_dtype=torch.float16, safety_checker=None
        )
        pipe.scheduler = UniPCMultistepScheduler.from_config(pipe.scheduler.config)

        # Enable optimizations for 4090
        pipe.enable_xformers_memory_efficient_attention()
        pipe.enable_model_cpu_offload() # Saves VRAM, though 4090 has plenty (24GB). modify if needed.
        # If pure speed is needed and 24GB is dedicated:
        # pipe.to(DEVICE)

        MODELS["pipeline"] = pipe
        logger.info("Stable Diffusion Loaded.")
    return MODELS["pipeline"]

def get_clip():
    if MODELS["clip_model"] is None:
        logger.info("Loading CLIP Model...")
        from transformers import CLIPProcessor, CLIPModel

        model_id = "openai/clip-vit-large-patch14"
        MODELS["clip_model"] = CLIPModel.from_pretrained(model_id).to(DEVICE)
        MODELS["clip_processor"] = CLIPProcessor.from_pretrained(model_id)
        logger.info("CLIP Loaded.")
    return MODELS["clip_model"], MODELS["clip_processor"]

# --- Endpoints ---

@app.get("/health")
def health():
    return {"status": "ok", "device": DEVICE, "vram": "24GB (Assumed 4090)"}

@app.post("/generate", response_model=GenerateResponse)
def generate(req: GenerateRequest):
    pipe = get_pipeline()

    generator = torch.Generator(device="cpu").manual_seed(req.seed) if req.seed != -1 else None

    # Process ControlNet image if present
    control_image = None
    if req.control_image_b64:
        # Assuming input is the raw render, we might need to preprocess it for Canny
        # or assume the client sends a preprocessed canny map.
        # For this implementation, let's assume the client sends the raw render
        # and we apply Canny here for consistency.
        import cv2
        raw_img = decode_image(req.control_image_b64)
        img_np = np.array(raw_img)
        # Simple Canny for architectural lines
        img_canny = cv2.Canny(img_np, 100, 200)
        img_canny = np.stack([img_canny] * 3, axis=2)
        control_image = Image.fromarray(img_canny)

    # Research-backed prompt engineering for isometric game assets
    # Based on best practices from Stable Diffusion community
    isometric_keywords = (
        "isometric view, orthographic projection, "
        "view from above, angular, centered, "
        "game asset, clean design"
    )

    style_keywords = (
        "vector art, illustration, 3d render style, "
        "soft shading, ambient occlusion, "
        "simple design, gradients, "
        "high quality, sharp focus"
    )

    # Build final prompt with proper structure
    full_prompt = f"{req.prompt}, {isometric_keywords}, {style_keywords}"

    args = {
        "prompt": full_prompt,
        "negative_prompt": req.negative_prompt,
        "width": req.width,
        "height": req.height,
        "num_inference_steps": req.steps,
        "guidance_scale": req.guidance_scale,
        "generator": generator,
    }

    if control_image:
        args["image"] = control_image
        # Research-backed ControlNet settings for isometric assets
        # conditioning_scale of 1.0-1.2 provides strong structural guidance
        # while still allowing the model creative freedom for textures/details
        args["controlnet_conditioning_scale"] = 1.0

    # Run inference
    # Note: If control_image is None, this pipeline might fail if it's strictly a ControlNetPipeline.
    # In a full prod version, we'd check and use a txt2img pipe if no control image.
    # For IsoGleam, we almost ALWAYS have a render guide.
    if control_image is None:
        # Fallback or error? Let's just create a blank black image for now or error.
        # Better: Create a dummy or handle it.
        raise HTTPException(status_code=400, detail="Control Image is required for Isogleam Pipeline")

    result = pipe(**args).images[0]

    return GenerateResponse(
        image_b64=encode_image(result),
        seed=req.seed
    )

@app.post("/clip/embed", response_model=ClipEmbedResponse)
def clip_embed(req: ClipEmbedRequest):
    model, processor = get_clip()
    image = decode_image(req.image_b64)

    inputs = processor(images=image, return_tensors="pt").to(DEVICE)
    with torch.no_grad():
        image_features = model.get_image_features(**inputs)

    # Normalize
    image_features = image_features / image_features.norm(p=2, dim=-1, keepdim=True)
    embedding = image_features.cpu().numpy().tolist()[0]

    return ClipEmbedResponse(embedding=embedding)

@app.post("/clip/classify", response_model=ClassificationResponse)
def clip_classify(req: ClassifyRequest):
    model, processor = get_clip()
    image = decode_image(req.image_b64)

    inputs = processor(text=req.labels, images=image, return_tensors="pt", padding=True).to(DEVICE)

    with torch.no_grad():
        outputs = model(**inputs)
        logits_per_image = outputs.logits_per_image  # this is the image-text similarity score
        probs = logits_per_image.softmax(dim=1)  # we can take the softmax to get the label probabilities

    scores = probs.cpu().numpy().tolist()[0]

    return ClassificationResponse(labels=req.labels, scores=scores)

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
