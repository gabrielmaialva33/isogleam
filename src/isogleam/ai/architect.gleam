/// IsoGleam Architect - O cérebro que desenha os prompts
/// Usa LLMs de ponta (Llama 3.1 405B via NVIDIA NIM) para "alucinar" detalhes isométricos.
import gleam/bit_array
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import isogleam/core/config.{type Config, Nvidia}
import isogleam/ffi/http.{type Response}

pub type ArchitectRequest {
  ArchitectRequest(
    tile_type: String,
    neighbors: List(String),
    style: String,
  )
}

/// Gera um prompt rico e detalhado para o Stable Diffusion
pub fn design_prompt(config: Config, req: ArchitectRequest) -> Result(String, String) {
  let model = "meta/llama-3.1-405b-instruct"
  
  let system_prompt = 
    "You are a master isometric pixel artist and prompt engineer. "
    <> "Your goal is to describe a single map tile for a SimCity 2000 style game. "
    <> "The output must be a concise, keyword-heavy prompt optimized for Stable Diffusion 3.5. "
    <> "Focus on: architectural details, lighting (isometric sun from top-left), "
    <> "clean edges, and specific colors from the requested style. "
    <> "Do not include conversational filler. Just the prompt."

  let user_prompt = 
    "Tile Type: " <> req.tile_type <> "\n"
    <> "Style: " <> req.style <> "\n"
    <> "Neighbors: " <> json.to_string(json.array(req.neighbors, json.string)) <> "\n"
    <> "Create the prompt:"

  case config.generation_mode {
    Nvidia -> call_nvidia_nim(config, model, system_prompt, user_prompt)
    _ -> Error("Architect requires NVIDIA Cloud mode (Gemini Banana Level)")
  }
}

fn call_nvidia_nim(
  config: Config, 
  model: String, 
  system: String, 
  user: String
) -> Result(String, String) {
  let url = "https://integrate.api.nvidia.com/v1/chat/completions"
  let key = option.unwrap(config.nvidia_api_key, "")
  let headers = [#("Authorization", "Bearer " <> key), #("Content-Type", "application/json")]

  let body = json.object([
    #("model", json.string(model)),
    #("messages", json.preprocessed_array([
      json.object([#("role", json.string("system")), #("content", json.string(system))]),
      json.object([#("role", json.string("user")), #("content", json.string(user))])
    ])),
    #("temperature", json.float(0.7)),
    #("max_tokens", json.int(150))
  ])

  http.post_json_with_headers(url, json.to_string(body), headers)
  |> result.map_error(fn(e) { "HTTP Error: " <> http_error_string(e) })
  |> result.try(fn(resp: Response) {
    case resp.status {
      200 -> extract_content(resp.body)
      _ -> {
        let body_str = bit_array.to_string(resp.body) |> result.unwrap("Binary body")
        Error("NIM Error " <> int.to_string(resp.status) <> ": " <> body_str)
      }
    }
  })
}

fn extract_content(body: BitArray) -> Result(String, String) {
  use body_str <- result.try(
    bit_array.to_string(body) |> result.replace_error("Invalid UTF-8"),
  )
  let message_decoder = {
    use content <- decode.field("content", decode.string)
    decode.success(content)
  }

  let choice_decoder = {
    use message <- decode.field("message", message_decoder)
    decode.success(message)
  }

  let decoder = {
    use choices <- decode.field("choices", decode.list(choice_decoder))
    case choices {
      [content, ..] -> decode.success(content)
      [] -> decode.success("Error: No choices")
    }
  }

  json.parse(body_str, decoder)
  |> result.map_error(fn(_) { "Failed to parse LLM response" })
}

fn http_error_string(e: http.HttpError) -> String {
  case e {
    http.ConnectionError(msg) -> msg
    http.TimeoutError -> "Timeout"
    http.InvalidResponse(msg) -> msg
  }
}
