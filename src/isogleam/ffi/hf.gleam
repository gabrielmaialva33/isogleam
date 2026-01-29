/// IsoGleam FFI - Hugging Face Client
/// Conecta na Inference API para modelos da comunidade (Flux, SDXL, etc)
import gleam/int
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import isogleam/core/config.{type Config}
import isogleam/ffi/http.{type Response}

pub fn generate_tile(
  prompt: String,
  config: Config,
) -> Result(BitArray, String) {
  let model = "stabilityai/stable-diffusion-2-1"
  let url = "https://api-inference.huggingface.co/models/" <> model
  
  let key = option.unwrap(config.hf_token, "")
  let headers = [#("Authorization", "Bearer " <> key)]

  let body = json.object([
    #("inputs", json.string(prompt)),
    #("parameters", json.object([
      #("negative_prompt", json.string(config.negative_prompt)),
      #("num_inference_steps", json.int(config.num_steps)),
      #("guidance_scale", json.float(config.guidance_scale))
    ]))
  ])

  http.post_json_with_headers(url, json.to_string(body), headers)
  |> result.map_error(fn(e) { "HF Error: " <> http_error_string(e) })
  |> result.try(fn(resp: Response) {
    case resp.status {
      200 -> Ok(resp.body) // HF retorna o binário da imagem ou JSON dependendo do header
      // TODO: Tratar retorno binário (blob) corretamente
      _ -> Error("HF API Error " <> int.to_string(resp.status))
    }
  })
}

fn http_error_string(e: http.HttpError) -> String {
  case e {
    http.ConnectionError(msg) -> msg
    http.TimeoutError -> "Timeout"
    http.InvalidResponse(msg) -> msg
  }
}
