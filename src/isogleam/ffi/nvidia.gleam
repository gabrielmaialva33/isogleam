/// IsoGleam FFI - AI Brain Client
/// Connects to the local Python AI Server (running on NVIDIA 4090)
/// Professional implementation using idiomatic Gleam patterns.
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string
import isogleam/core/config.{type Config, type GenerationMode, HuggingFace, Local, Nvidia}
import isogleam/ffi/http

// ────────────────────────────────────────────────────────────────────────────
// Types
// ────────────────────────────────────────────────────────────────────────────

pub type NimConfig {
  NimConfig(
    host: String, 
    port: Int, 
    model: String, 
    timeout_ms: Int,
    mode: GenerationMode,
    api_key: Option(String)
  )
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

/// Create config from core config
pub fn from_core(core: Config) -> NimConfig {
  NimConfig(
    host: "localhost", // Ignored for Cloud
    port: 8000,        // Ignored for Cloud
    model: core.model_id,
    timeout_ms: 60_000,
    mode: core.generation_mode,
    api_key: case core.generation_mode {
      Nvidia -> core.nvidia_api_key
      HuggingFace -> core.hf_token
      Local -> None
    }
  )
}

/// Create default configuration pointing to local AI Brain
pub fn default_config() -> NimConfig {
  NimConfig(
    host: "localhost",
    port: 8000,
    model: "sd-v1.5-controlnet-isometric",
    timeout_ms: 60_000,
    mode: Local,
    api_key: None,
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
  let #(url, headers) = resolve_endpoint(config, "/generate")
  let body_json = build_generate_request(prompt, control_image_b64, seed, config.mode)
  let body_str = json.to_string(body_json)

  http.post_json_with_headers(url, body_str, headers)
  |> convert_http_error
  |> validate_response
  |> extract_image_field
}

/// Get CLIP embedding for an image (for QA checks)
pub fn clip_embed(
  image_b64: String,
  config: NimConfig,
) -> Result(ClipEmbedding, String) {
  let #(url, headers) = resolve_endpoint(config, "/clip/embed")

  let body_json = json.object([#("image_b64", json.string(image_b64))])
  let body_str = json.to_string(body_json)

  http.post_json_with_headers(url, body_str, headers)
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
  let #(url, headers) = resolve_endpoint(config, "/clip/classify")

  let body_json = build_classify_request(image_b64, labels)
  let body_str = json.to_string(body_json)

  http.post_json_with_headers(url, body_str, headers)
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

fn resolve_endpoint(config: NimConfig, path: String) -> #(String, List(#(String, String))) {
  case config.mode {
    Local -> {
      let url = "http://" <> config.host <> ":" <> int.to_string(config.port) <> path
      #(url, [])
    }
    Nvidia -> {
      // NVIDIA NIM Endpoint for SD 3.5
      let url = "https://ai.api.nvidia.com/v1/genai/stabilityai/stable-diffusion-3-5-large"
      let key = option.unwrap(config.api_key, "")
      #(url, [#("Authorization", "Bearer " <> key)])
    }
    HuggingFace -> {
      // HF Inference API
      let url = "https://api-inference.huggingface.co/models/" <> config.model
      let key = option.unwrap(config.api_key, "")
      #(url, [#("Authorization", "Bearer " <> key)])
    }
  }
}

fn base_url(config: NimConfig) -> String {
  // Legacy helper, kept for compatibility if needed, but resolve_endpoint is preferred
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
  mode: GenerationMode,
) -> json.Json {
  case mode {
    Local -> 
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
    
    Nvidia ->
      // NVIDIA Payload format (Standard GenAI)
      json.object([
        #("text_prompts", json.preprocessed_array([
          json.object([#("text", json.string(prompt)), #("weight", json.float(1.0))])
        ])),
        #("cfg_scale", json.float(5.0)),
        #("seed", json.int(seed)),
        #("sampler", json.string("K_EULER_ANCESTRAL")),
        #("steps", json.int(25))
      ])

    HuggingFace ->
      // HF Inference Payload
      json.object([
        #("inputs", json.string(prompt)),
        #("parameters", json.object([
           #("negative_prompt", json.string("blurry, low quality")),
           #("num_inference_steps", json.int(25)),
           #("guidance_scale", json.float(7.5))
        ]))
      ])
  }
}

fn build_classify_request(image_b64: String, labels: List(String)) -> json.Json {
  json.object([
    #("image_b64", json.string(image_b64)),
    #("labels", json.preprocessed_array(list.map(labels, json.string))),
  ])
}

// ────────────────────────────────────────────────────────────────────────────
// Field Extractors & Decoders
// ────────────────────────────────────────────────────────────────────────────

fn extract_image_field(result: Result(String, String)) -> Result(String, String) {
  case result {
    Ok(body) -> {
      let decoder = {
        use image <- decode.field("image_b64", decode.string)
        decode.success(image)
      }

      json.parse(body, decoder)
      |> result.map_error(format_json_error)
    }
    Error(err) -> Error(err)
  }
}

fn extract_embedding_field(
  result: Result(String, String),
) -> Result(ClipEmbedding, String) {
  case result {
    Ok(body) -> {
      let decoder = {
        use vector <- decode.field("embedding", decode.list(decode.float))
        use dimensions <- decode.field("dimensions", decode.int)
        decode.success(ClipEmbedding(vector: vector, dimensions: dimensions))
      }

      json.parse(body, decoder)
      |> result.map_error(format_json_error)
    }
    Error(err) -> Error(err)
  }
}

fn extract_classifications(
  result: Result(String, String),
) -> Result(List(Classification), String) {
  case result {
    Ok(body) -> {
      let decoder =
        decode.list({
          use label <- decode.field("label", decode.string)
          use confidence <- decode.field("confidence", decode.float)
          decode.success(Classification(label: label, confidence: confidence))
        })

      json.parse(body, decoder)
      |> result.map_error(format_json_error)
    }
    Error(err) -> Error(err)
  }
}

fn format_json_error(err: json.DecodeError) -> String {
  case err {
    json.UnexpectedEndOfInput -> "Unexpected end of input"
    json.UnexpectedByte(byte) -> "Unexpected byte: " <> byte
    json.UnexpectedSequence(bytes) -> "Unexpected sequence: " <> bytes
    json.UnableToDecode(errors) -> {
      let error_msgs =
        list.map(errors, fn(err) {
          "Field '"
          <> string.join(err.path, ".")
          <> "': Expected "
          <> err.expected
          <> ", found "
          <> err.found
        })
      "Decode failed: " <> string.join(error_msgs, "; ")
    }
  }
}
