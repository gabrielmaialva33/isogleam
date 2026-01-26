/// IsoGleam FFI - NVIDIA NIM Client
/// Interface to local NVIDIA NIMs (CLIP, Trellis, etc)


/// NIM endpoint configuration
pub type NimConfig {
  NimConfig(
    host: String,
    port: Int,
    model: String,
    timeout_ms: Int,
  )
}

/// CLIP embedding result
pub type ClipEmbedding {
  ClipEmbedding(
    vector: List(Float),
    dimensions: Int,
  )
}

/// Image classification result
pub type Classification {
  Classification(
    label: String,
    confidence: Float,
  )
}

/// Default CLIP config (local NIM)
pub fn clip_config() -> NimConfig {
  NimConfig(
    host: "localhost",
    port: 8000,
    model: "nvidia/nv-clip-vit-h-14-laion2b",
    timeout_ms: 30_000,
  )
}

/// Default Trellis config (3D generation)
pub fn trellis_config() -> NimConfig {
  NimConfig(
    host: "localhost",
    port: 8001,
    model: "microsoft/TRELLIS-image-large",
    timeout_ms: 120_000,
  )
}

/// Get CLIP embedding for image
/// Returns 1024-dimensional vector
pub fn clip_embed(
  _image_path: String,
  _config: NimConfig,
) -> Result(ClipEmbedding, String) {
  // TODO: Implement actual HTTP call to NIM
  // For now, return placeholder
  Error("NIM CLIP not implemented - use /orq --vision")
}

/// Classify image using CLIP
pub fn clip_classify(
  _image_path: String,
  _labels: List(String),
  _config: NimConfig,
) -> Result(List(Classification), String) {
  // TODO: Implement zero-shot classification
  Error("NIM CLIP classify not implemented")
}

/// Generate 3D from image using Trellis
pub fn trellis_generate(
  _image_path: String,
  _config: NimConfig,
) -> Result(String, String) {
  // TODO: Implement Trellis 3D generation
  // Returns path to generated GLB file
  Error("NIM Trellis not implemented")
}

/// Check if NIM is available
pub fn health_check(_config: NimConfig) -> Bool {
  // TODO: Implement health check
  False
}

/// Build NIM URL
pub fn build_url(config: NimConfig, endpoint: String) -> String {
  "http://"
  <> config.host
  <> ":"
  <> int_to_string(config.port)
  <> endpoint
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String
