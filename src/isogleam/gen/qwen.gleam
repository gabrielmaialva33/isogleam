/// IsoGleam Gen - Qwen Image-Edit Integration
/// Interface to Qwen-Image-Edit-2511 for pixel art generation
/// Improvement over Isometric NYC: using newer 2511 model vs old version

import gleam/string
import gleam/list
import gleam/option.{type Option, None, Some}
import isogleam/ffi/http

/// Qwen model configuration
pub type QwenConfig {
  QwenConfig(
    /// API endpoint (local Docker or remote)
    endpoint: String,
    /// Model identifier
    model: String,
    /// Generation parameters
    params: GenerationParams,
  )
}

/// Generation parameters
pub type GenerationParams {
  GenerationParams(
    /// Number of inference steps
    steps: Int,
    /// Guidance scale
    guidance: Float,
    /// Random seed (None = random)
    seed: Option(Int),
    /// Output size
    width: Int,
    height: Int,
  )
}

/// Generation request
pub type GenerationRequest {
  GenerationRequest(
    /// Input image path (3D render)
    input_image: String,
    /// Edit instruction
    instruction: String,
    /// Mask image path (optional, for infill)
    mask: Option(String),
    /// Context images (neighbors for consistency)
    context: List(String),
  )
}

/// Generation result
pub type GenerationResult {
  GenerationResult(
    /// Output image path
    output_path: String,
    /// Generation metadata
    metadata: GenerationMetadata,
  )
}

/// Generation metadata
pub type GenerationMetadata {
  GenerationMetadata(
    /// Time taken in ms
    duration_ms: Int,
    /// Seed used
    seed: Int,
    /// Model version
    model_version: String,
  )
}

/// Default config for local Docker Qwen
pub fn default_config() -> QwenConfig {
  QwenConfig(
    endpoint: "http://localhost:7860/api/predict",
    model: "Qwen/Qwen-Image-Edit-2511",
    params: GenerationParams(
      steps: 28,
      guidance: 7.5,
      seed: None,
      width: 512,
      height: 512,
    ),
  )
}

/// Config for GGUF quantized version (fits in 4090)
pub fn gguf_config() -> QwenConfig {
  QwenConfig(
    endpoint: "http://localhost:8080/completion",
    model: "unsloth/Qwen-Image-Edit-2511-GGUF",
    params: GenerationParams(
      steps: 20,  // Faster for quantized
      guidance: 7.0,
      seed: None,
      width: 512,
      height: 512,
    ),
  )
}

/// Build the pixel art prompt template
/// This is our secret sauce - tuned for SimCity 2000 / RCT2 style
pub fn build_prompt(
  tile_type: TileType,
  style_hints: List(String),
) -> String {
  let base_prompt = case tile_type {
    ResidentialTile -> "isometric pixel art residential building"
    CommercialTile -> "isometric pixel art commercial building with shopfront"
    IndustrialTile -> "isometric pixel art industrial building with smokestacks"
    RoadTile -> "isometric pixel art asphalt road"
    WaterTile -> "isometric pixel art water with waves pattern"
    ParkTile -> "isometric pixel art park with trees"
    EmptyTile -> "isometric pixel art grass terrain"
  }

  let style_suffix = string.concat([
    ", SimCity 2000 style, 16-bit graphics, ordered dithering, ",
    "limited color palette, bird's eye view, clean pixel edges, ",
    "no anti-aliasing, ",
    string.join(style_hints, ", "),
  ])

  base_prompt <> style_suffix
}

/// Negative prompt (things to avoid)
pub fn negative_prompt() -> String {
  string.concat([
    "blurry, smooth gradients, 3d render, realistic, antialiased, ",
    "modern, high resolution, photorealistic, soft shadows, ",
    "ambient occlusion, global illumination, raytracing",
  ])
}

/// Tile type enumeration
pub type TileType {
  ResidentialTile
  CommercialTile
  IndustrialTile
  RoadTile
  WaterTile
  ParkTile
  EmptyTile
}

/// Generate a tile using Qwen Image-Edit
pub fn generate(
  request: GenerationRequest,
  config: QwenConfig,
) -> Result(GenerationResult, String) {
  // Build JSON payload for Gradio API
  let payload = build_gradio_payload(request, config)

  // Make HTTP request
  case http.post_json(config.endpoint, payload) {
    Error(_) -> Error("Failed to connect to Qwen endpoint")
    Ok(response) -> {
      case http.is_success(response) {
        False -> Error("Qwen returned error: " <> response.body)
        True -> {
          // Parse response (simplified - real impl would parse JSON)
          Ok(GenerationResult(
            output_path: "/tmp/generated_tile.png",
            metadata: GenerationMetadata(
              duration_ms: 0,
              seed: 42,
              model_version: config.model,
            ),
          ))
        }
      }
    }
  }
}

/// Build Gradio API payload
fn build_gradio_payload(request: GenerationRequest, config: QwenConfig) -> String {
  let prompt = request.instruction
  let neg = negative_prompt()
  let steps = config.params.steps
  let guidance = config.params.guidance

  // Gradio expects specific JSON format
  string.concat([
    "{\"data\":[\"", request.input_image, "\",\"",
    prompt, "\",\"", neg, "\",",
    int_to_string(steps), ",",
    float_to_string(guidance), ",",
    int_to_string(config.params.width), ",",
    int_to_string(config.params.height),
    "]}"
  ])
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String

@external(erlang, "erlang", "float_to_binary")
fn float_to_string(f: Float) -> String

/// Generate with infill mask (for seamless borders)
/// This is the key innovation from Isometric NYC
pub fn generate_with_infill(
  request: GenerationRequest,
  neighbors: NeighborContext,
  _config: QwenConfig,
) -> Result(GenerationResult, String) {
  // Build context from neighbors
  let _context_prompt = build_neighbor_context(neighbors)

  case request.mask {
    None -> Error("Infill generation requires a mask")
    Some(_mask_path) -> {
      // TODO: Implement masked generation
      Error("Infill generation not yet implemented")
    }
  }
}

/// Neighbor context for consistent generation
pub type NeighborContext {
  NeighborContext(
    north: Option(String),
    south: Option(String),
    east: Option(String),
    west: Option(String),
  )
}

fn build_neighbor_context(ctx: NeighborContext) -> String {
  let parts = [
    option_to_part("north", ctx.north),
    option_to_part("south", ctx.south),
    option_to_part("east", ctx.east),
    option_to_part("west", ctx.west),
  ]
  |> list.filter(fn(s) { s != "" })

  case list.length(parts) {
    0 -> ""
    _ -> "Match borders with neighbors: " <> string.join(parts, ", ")
  }
}

fn option_to_part(name: String, opt: Option(String)) -> String {
  case opt {
    None -> ""
    Some(_path) -> name <> " tile present"
  }
}

/// Batch generation for multiple tiles
pub fn generate_batch(
  requests: List(GenerationRequest),
  config: QwenConfig,
) -> List(Result(GenerationResult, String)) {
  list.map(requests, fn(req) { generate(req, config) })
}

/// Health check for Qwen endpoint
pub fn health_check(config: QwenConfig) -> Bool {
  http.health_check(config.endpoint)
}

/// Estimate VRAM usage
pub fn estimate_vram_mb(config: QwenConfig) -> Int {
  case string.contains(config.model, "GGUF") {
    True -> case string.contains(config.model, "Q4") {
      True -> 12_000   // Q4 quantized
      False -> 18_000  // Q8 quantized
    }
    False -> 40_000    // Full BF16 (won't fit in 4090)
  }
}
