import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Error, toList, Empty as $Empty, CustomType as $CustomType } from "../../gleam.mjs";
import * as $config from "../../isogleam/core/config.mjs";
import { Nvidia } from "../../isogleam/core/config.mjs";
import * as $http from "../../isogleam/ffi/http.mjs";

export class ArchitectRequest extends $CustomType {
  constructor(tile_type, neighbors, style) {
    super();
    this.tile_type = tile_type;
    this.neighbors = neighbors;
    this.style = style;
  }
}
export const ArchitectRequest$ArchitectRequest = (tile_type, neighbors, style) =>
  new ArchitectRequest(tile_type, neighbors, style);
export const ArchitectRequest$isArchitectRequest = (value) =>
  value instanceof ArchitectRequest;
export const ArchitectRequest$ArchitectRequest$tile_type = (value) =>
  value.tile_type;
export const ArchitectRequest$ArchitectRequest$0 = (value) => value.tile_type;
export const ArchitectRequest$ArchitectRequest$neighbors = (value) =>
  value.neighbors;
export const ArchitectRequest$ArchitectRequest$1 = (value) => value.neighbors;
export const ArchitectRequest$ArchitectRequest$style = (value) => value.style;
export const ArchitectRequest$ArchitectRequest$2 = (value) => value.style;

function extract_content(body) {
  return $result.try$(
    (() => {
      let _pipe = $bit_array.to_string(body);
      return $result.replace_error(_pipe, "Invalid UTF-8");
    })(),
    (body_str) => {
      let message_decoder = $decode.field(
        "content",
        $decode.string,
        (content) => { return $decode.success(content); },
      );
      let choice_decoder = $decode.field(
        "message",
        message_decoder,
        (message) => { return $decode.success(message); },
      );
      let decoder = $decode.field(
        "choices",
        $decode.list(choice_decoder),
        (choices) => {
          if (choices instanceof $Empty) {
            return $decode.success("Error: No choices");
          } else {
            let content = choices.head;
            return $decode.success(content);
          }
        },
      );
      let _pipe = $json.parse(body_str, decoder);
      return $result.map_error(
        _pipe,
        (_) => { return "Failed to parse LLM response"; },
      );
    },
  );
}

function http_error_string(e) {
  if (e instanceof $http.ConnectionError) {
    let msg = e[0];
    return msg;
  } else if (e instanceof $http.TimeoutError) {
    return "Timeout";
  } else {
    let msg = e[0];
    return msg;
  }
}

function call_nvidia_nim(config, model, system, user) {
  let url = "https://integrate.api.nvidia.com/v1/chat/completions";
  let key = $option.unwrap(config.nvidia_api_key, "");
  let headers = toList([
    ["Authorization", "Bearer " + key],
    ["Content-Type", "application/json"],
  ]);
  let body = $json.object(
    toList([
      ["model", $json.string(model)],
      [
        "messages",
        $json.preprocessed_array(
          toList([
            $json.object(
              toList([
                ["role", $json.string("system")],
                ["content", $json.string(system)],
              ]),
            ),
            $json.object(
              toList([
                ["role", $json.string("user")],
                ["content", $json.string(user)],
              ]),
            ),
          ]),
        ),
      ],
      ["temperature", $json.float(0.7)],
      ["max_tokens", $json.int(150)],
    ]),
  );
  let _pipe = $http.post_json_with_headers(url, $json.to_string(body), headers);
  let _pipe$1 = $result.map_error(
    _pipe,
    (e) => { return "HTTP Error: " + http_error_string(e); },
  );
  return $result.try$(
    _pipe$1,
    (resp) => {
      let $ = resp.status;
      if ($ === 200) {
        return extract_content(resp.body);
      } else {
        let _block;
        let _pipe$2 = $bit_array.to_string(resp.body);
        _block = $result.unwrap(_pipe$2, "Binary body");
        let body_str = _block;
        return new Error(
          (("NIM Error " + $int.to_string(resp.status)) + ": ") + body_str,
        );
      }
    },
  );
}

/**
 * Gera um prompt rico e detalhado para o Stable Diffusion
 */
export function design_prompt(config, req) {
  let model = "meta/llama-3.1-405b-instruct";
  let system_prompt = (((("You are a master isometric pixel artist and prompt engineer. " + "Your goal is to describe a single map tile for a SimCity 2000 style game. ") + "The output must be a concise, keyword-heavy prompt optimized for Stable Diffusion 3.5. ") + "Focus on: architectural details, lighting (isometric sun from top-left), ") + "clean edges, and specific colors from the requested style. ") + "Do not include conversational filler. Just the prompt.";
  let user_prompt = (((((((("Tile Type: " + req.tile_type) + "\n") + "Style: ") + req.style) + "\n") + "Neighbors: ") + $json.to_string(
    $json.array(req.neighbors, $json.string),
  )) + "\n") + "Create the prompt:";
  let $ = config.generation_mode;
  if ($ instanceof Nvidia) {
    return call_nvidia_nim(config, model, system_prompt, user_prompt);
  } else {
    return new Error(
      "Architect requires NVIDIA Cloud mode (Gemini Banana Level)",
    );
  }
}
