/// Qwen Client - HTTP client para chamar o servidor Python
/// Conecta o Gleam puro com a IA (Qwen-Image-Edit-2511)
/// Usa FFI Erlang httpc direto (sem deps externas)

import gleam/json
import gleam/dynamic/decode
import gleam/option.{type Option, None, Some}
import gleam/int

/// Configuração do servidor
pub type ServerConfig {
  ServerConfig(
    host: String,
    port: Int,
  )
}

/// Configuração padrão (localhost:8100)
pub fn default_config() -> ServerConfig {
  ServerConfig(host: "localhost", port: 8100)
}

/// Response da geração
pub type GenerateResponse {
  GenerateResponse(
    success: Bool,
    output_image_b64: Option(String),
    output_path: Option(String),
    seed_used: Int,
    error: Option(String),
  )
}

/// Status de saúde do servidor
pub type HealthStatus {
  HealthStatus(
    status: String,
    model_loaded: Bool,
    device: String,
    model_id: String,
  )
}

/// Erro do cliente
pub type ClientError {
  ConnectionError(String)
  DecodeError(String)
  ServerError(Int, String)
  HttpStartError
}

/// Request para gerar tile com contexto (infill)
pub type InfillRequest {
  InfillRequest(
    input_image_b64: String,
    prompt: String,
    north_border_b64: Option(String),
    south_border_b64: Option(String),
    east_border_b64: Option(String),
    west_border_b64: Option(String),
    mask_percentage: Float,
    num_steps: Int,
    guidance_scale: Float,
    seed: Int,
    tile_id: String,
  )
}

// Erlang FFI para HTTP
@external(erlang, "inets", "start")
fn start_inets() -> decode.Dynamic

@external(erlang, "httpc_ffi", "post_json")
fn httpc_post(url: String, body: String) -> Result(#(Int, String), String)

@external(erlang, "httpc_ffi", "get")
fn httpc_get(url: String) -> Result(#(Int, String), String)

/// Inicializa o cliente HTTP (chamar uma vez)
pub fn init() -> Result(Nil, ClientError) {
  let _ = start_inets()
  Ok(Nil)
}

/// Monta URL do servidor
fn make_url(config: ServerConfig, path: String) -> String {
  "http://" <> config.host <> ":" <> int.to_string(config.port) <> path
}

/// Verifica saúde do servidor
pub fn health_check(config: ServerConfig) -> Result(HealthStatus, ClientError) {
  let url = make_url(config, "/health")

  case httpc_get(url) {
    Error(msg) -> Error(ConnectionError(msg))
    Ok(#(status, body)) -> {
      case status {
        200 -> decode_health(body)
        _ -> Error(ServerError(status, body))
      }
    }
  }
}

fn health_decoder() -> decode.Decoder(HealthStatus) {
  use status <- decode.field("status", decode.string)
  use model_loaded <- decode.field("model_loaded", decode.bool)
  use device <- decode.field("device", decode.string)
  use model_id <- decode.field("model_id", decode.string)
  decode.success(HealthStatus(status, model_loaded, device, model_id))
}

fn decode_health(body: String) -> Result(HealthStatus, ClientError) {
  case json.parse(body, health_decoder()) {
    Ok(health) -> Ok(health)
    Error(_) -> Error(DecodeError("Failed to decode health response"))
  }
}

/// Gera tile simples
pub fn generate(
  config: ServerConfig,
  image_b64: String,
  prompt: String,
  tile_id: String,
) -> Result(GenerateResponse, ClientError) {
  let url = make_url(config, "/generate")

  let body = json.object([
    #("input_image_b64", json.string(image_b64)),
    #("prompt", json.string(prompt)),
    #("negative_prompt", json.string("blurry, smooth, realistic, antialiased")),
    #("num_steps", json.int(28)),
    #("guidance_scale", json.float(3.5)),
    #("seed", json.int(-1)),
    #("tile_id", json.string(tile_id)),
  ])
  |> json.to_string

  case httpc_post(url, body) {
    Error(msg) -> Error(ConnectionError(msg))
    Ok(#(status, resp_body)) -> {
      case status {
        200 -> decode_generate(resp_body)
        _ -> Error(ServerError(status, resp_body))
      }
    }
  }
}

/// Gera tile com contexto dos vizinhos (infill) - O SEGREDO
pub fn generate_with_context(
  config: ServerConfig,
  req: InfillRequest,
) -> Result(GenerateResponse, ClientError) {
  let url = make_url(config, "/generate_with_context")

  let body = json.object([
    #("input_image_b64", json.string(req.input_image_b64)),
    #("prompt", json.string(req.prompt)),
    #("north_border_b64", option_to_json(req.north_border_b64)),
    #("south_border_b64", option_to_json(req.south_border_b64)),
    #("east_border_b64", option_to_json(req.east_border_b64)),
    #("west_border_b64", option_to_json(req.west_border_b64)),
    #("mask_percentage", json.float(req.mask_percentage)),
    #("num_steps", json.int(req.num_steps)),
    #("guidance_scale", json.float(req.guidance_scale)),
    #("seed", json.int(req.seed)),
    #("tile_id", json.string(req.tile_id)),
  ])
  |> json.to_string

  case httpc_post(url, body) {
    Error(msg) -> Error(ConnectionError(msg))
    Ok(#(status, resp_body)) -> {
      case status {
        200 -> decode_generate(resp_body)
        _ -> Error(ServerError(status, resp_body))
      }
    }
  }
}

fn option_to_json(opt: Option(String)) -> json.Json {
  case opt {
    Some(s) -> json.string(s)
    None -> json.null()
  }
}

fn generate_decoder() -> decode.Decoder(GenerateResponse) {
  use success <- decode.field("success", decode.bool)
  use output_image_b64 <- decode.optional_field("output_image_b64", None, decode.optional(decode.string))
  use output_path <- decode.optional_field("output_path", None, decode.optional(decode.string))
  use seed_used <- decode.field("seed_used", decode.int)
  use error <- decode.optional_field("error", None, decode.optional(decode.string))
  decode.success(GenerateResponse(
    success,
    output_image_b64,
    output_path,
    seed_used,
    error,
  ))
}

fn decode_generate(body: String) -> Result(GenerateResponse, ClientError) {
  case json.parse(body, generate_decoder()) {
    Ok(resp) -> Ok(resp)
    Error(_) -> Error(DecodeError("Failed to decode generate response"))
  }
}

/// Helper: cria request de infill com defaults
pub fn infill_request(
  image_b64: String,
  tile_id: String,
  north: Option(String),
  south: Option(String),
  east: Option(String),
  west: Option(String),
) -> InfillRequest {
  InfillRequest(
    input_image_b64: image_b64,
    prompt: "Convert to isometric pixel art, SimCity 2000 style, 16-bit graphics",
    north_border_b64: north,
    south_border_b64: south,
    east_border_b64: east,
    west_border_b64: west,
    mask_percentage: 0.25,
    num_steps: 28,
    guidance_scale: 3.5,
    seed: -1,
    tile_id: tile_id,
  )
}
