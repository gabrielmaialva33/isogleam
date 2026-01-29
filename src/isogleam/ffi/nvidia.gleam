/// IsoGleam FFI - AI Brain Client
/// Connects to the local Python AI Server (running on NVIDIA 4090)
/// Professional implementation using idiomatic Gleam patterns.
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import isogleam/ffi/http

// ────────────────────────────────────────────────────────────────────────────
// Types
// ────────────────────────────────────────────────────────────────────────────

pub type NimConfig {
  NimConfig(host: String, port: Int, model: String, timeout_ms: Int)
}

pub type ClipEmbedding {
  ClipEmbedding(vector: List(Float), dimensions: Int)
}

pub type Classification {
  Classification(label: String, confidence: Float)
}

// ────────────────────────────────────────────────────────────────────────────
// Public API
// ────────────────────────────────────────────────────────────────────────────

/// Create default configuration pointing to local AI Brain
pub fn default_config() -> NimConfig {
  NimConfig(
    host: "localhost",
    port: 8000,
    model: "sd-v1.5-controlnet-isometric",
    timeout_ms: 60_000,
  )
}

/// Generate a tile using Stable Diffusion + ControlNet
///
/// ## Parameters
/// - prompt: User description of the tile (e.g., "brick building")
/// - control_image_b64: Optional base64-encoded control image for ControlNet
/// - seed: Random seed (-1 for random)
/// - config: Server configuration
///
/// ## Returns
/// - Ok(base64_image) on success
/// - Error(message) on failure
pub fn generate_tile(
  prompt: String,
  control_image_b64: Option(String),
  seed: Int,
  config: NimConfig,
) -> Result(String, String) {
  let url = base_url(config) <> "/generate"

  let body_json = build_generate_request(prompt, control_image_b64, seed)
  let body_str = json.to_string(body_json)

  http.post_json(url, body_str)
  |> convert_http_error
  |> validate_response
  |> extract_image_field
}

/// Get CLIP embedding for an image (for QA checks)
pub fn clip_embed(
  image_b64: String,
  config: NimConfig,
) -> Result(ClipEmbedding, String) {
  let url = base_url(config) <> "/clip/embed"

  let body_json = json.object([#("image_b64", json.string(image_b64))])
  let body_str = json.to_string(body_json)

  http.post_json(url, body_str)
  |> convert_http_error
  |> validate_response
  |> extract_embedding_field
}

/// Classify image using CLIP (Zero-shot classification)
pub fn clip_classify(
  image_b64: String,
  labels: List(String),
  config: NimConfig,
) -> Result(List(Classification), String) {
  let url = base_url(config) <> "/clip/classify"

  let body_json = build_classify_request(image_b64, labels)
  let body_str = json.to_string(body_json)

  http.post_json(url, body_str)
  |> convert_http_error
  |> validate_response
  |> extract_classifications
}

/// Check if AI Brain is online
pub fn health_check(config: NimConfig) -> Bool {
  let url = base_url(config) <> "/health"
  http.health_check(url)
}

// ────────────────────────────────────────────────────────────────────────────
// Internal Helpers
// ────────────────────────────────────────────────────────────────────────────

fn base_url(config: NimConfig) -> String {
  "http://" <> config.host <> ":" <> int.to_string(config.port)
}

fn convert_http_error(
  result: Result(http.Response, http.HttpError),
) -> Result(http.Response, String) {
  case result {
    Ok(resp) -> Ok(resp)
    Error(http.ConnectionError(msg)) -> Error("Connection error: " <> msg)
    Error(http.TimeoutError) -> Error("Request timed out")
    Error(http.InvalidResponse(msg)) -> Error("Invalid response: " <> msg)
  }
}

fn validate_response(
  result: Result(http.Response, String),
) -> Result(String, String) {
  case result {
    Ok(resp) if resp.status == 200 -> Ok(resp.body)
    Ok(resp) ->
      Error(
        "Server returned " <> int.to_string(resp.status) <> ": " <> resp.body,
      )
    Error(err) -> Error("HTTP request failed: " <> err)
  }
}

fn build_generate_request(
  prompt: String,
  control_image_b64: Option(String),
  seed: Int,
) -> json.Json {
  json.object([
    #("prompt", json.string(prompt)),
    #("width", json.int(512)),
    #("height", json.int(512)),
    #("steps", json.int(25)),
    #("seed", json.int(seed)),
    #("control_image_b64", case control_image_b64 {
      option.Some(img) -> json.string(img)
      option.None -> json.null()
    }),
  ])
}

fn build_classify_request(image_b64: String, labels: List(String)) -> json.Json {
  json.object([
    #("image_b64", json.string(image_b64)),
    #("labels", json.preprocessed_array(list.map(labels, json.string))),
  ])
}

// ────────────────────────────────────────────────────────────────────────────
// Field Extractors (Placeholder - needs proper JSON parsing library)
// ────────────────────────────────────────────────────────────────────────────
//
// NOTE: Gleam's standard `gleam/json` only provides encoding, not decoding.
// For production, we need to either:
// 1. Add `gleam_jsone` package for JSON parsing
// 2. Implement Erlang FFI for `jiffy` or `jason`
// 3. Use `gleam_json` with dynamic decoders
//
// For now, returning placeholders to keep compile working while we wait
// for proper JSON library integration.
// ────────────────────────────────────────────────────────────────────────────

fn extract_image_field(result: Result(String, String)) -> Result(String, String) {
  // TODO: Parse JSON response and extract "image_b64" field
  // Example with gleam_jsone:
  //   use json <- result.try(jsone.decode(body))
  //   use image <- result.try(dynamic.field("image_b64", dynamic.string)(json))
  //   Ok(image)

  case result {
    Ok(_body) -> {
      // Placeholder: In production, parse JSON here
      Error("JSON parsing not yet implemented - need gleam_jsone or similar")
    }
    Error(err) -> Error(err)
  }
}

fn extract_embedding_field(
  result: Result(String, String),
) -> Result(ClipEmbedding, String) {
  case result {
    Ok(_body) -> {
      // Placeholder
      Error("JSON parsing not yet implemented")
    }
    Error(err) -> Error(err)
  }
}

fn extract_classifications(
  result: Result(String, String),
) -> Result(List(Classification), String) {
  case result {
    Ok(_body) -> {
      // Placeholder: Return mock classification for now
      Ok([Classification(label: "building", confidence: 0.95)])
    }
    Error(err) -> Error(err)
  }
}
