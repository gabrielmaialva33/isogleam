import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $decode from "../../../gleam_stdlib/gleam/dynamic/decode.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, Empty as $Empty, CustomType as $CustomType } from "../../gleam.mjs";
import * as $config from "../../isogleam/core/config.mjs";
import { HuggingFace, Local, Nvidia } from "../../isogleam/core/config.mjs";
import * as $http from "../../isogleam/ffi/http.mjs";

export class NimConfig extends $CustomType {
  constructor(host, port, model, timeout_ms, mode, api_key) {
    super();
    this.host = host;
    this.port = port;
    this.model = model;
    this.timeout_ms = timeout_ms;
    this.mode = mode;
    this.api_key = api_key;
  }
}
export const NimConfig$NimConfig = (host, port, model, timeout_ms, mode, api_key) =>
  new NimConfig(host, port, model, timeout_ms, mode, api_key);
export const NimConfig$isNimConfig = (value) => value instanceof NimConfig;
export const NimConfig$NimConfig$host = (value) => value.host;
export const NimConfig$NimConfig$0 = (value) => value.host;
export const NimConfig$NimConfig$port = (value) => value.port;
export const NimConfig$NimConfig$1 = (value) => value.port;
export const NimConfig$NimConfig$model = (value) => value.model;
export const NimConfig$NimConfig$2 = (value) => value.model;
export const NimConfig$NimConfig$timeout_ms = (value) => value.timeout_ms;
export const NimConfig$NimConfig$3 = (value) => value.timeout_ms;
export const NimConfig$NimConfig$mode = (value) => value.mode;
export const NimConfig$NimConfig$4 = (value) => value.mode;
export const NimConfig$NimConfig$api_key = (value) => value.api_key;
export const NimConfig$NimConfig$5 = (value) => value.api_key;

export class ClipEmbedding extends $CustomType {
  constructor(vector, dimensions) {
    super();
    this.vector = vector;
    this.dimensions = dimensions;
  }
}
export const ClipEmbedding$ClipEmbedding = (vector, dimensions) =>
  new ClipEmbedding(vector, dimensions);
export const ClipEmbedding$isClipEmbedding = (value) =>
  value instanceof ClipEmbedding;
export const ClipEmbedding$ClipEmbedding$vector = (value) => value.vector;
export const ClipEmbedding$ClipEmbedding$0 = (value) => value.vector;
export const ClipEmbedding$ClipEmbedding$dimensions = (value) =>
  value.dimensions;
export const ClipEmbedding$ClipEmbedding$1 = (value) => value.dimensions;

export class Classification extends $CustomType {
  constructor(label, confidence) {
    super();
    this.label = label;
    this.confidence = confidence;
  }
}
export const Classification$Classification = (label, confidence) =>
  new Classification(label, confidence);
export const Classification$isClassification = (value) =>
  value instanceof Classification;
export const Classification$Classification$label = (value) => value.label;
export const Classification$Classification$0 = (value) => value.label;
export const Classification$Classification$confidence = (value) =>
  value.confidence;
export const Classification$Classification$1 = (value) => value.confidence;

/**
 * Create config from core config
 */
export function from_core(core) {
  return new NimConfig(
    "localhost",
    8000,
    core.model_id,
    60_000,
    core.generation_mode,
    (() => {
      let $ = core.generation_mode;
      if ($ instanceof Local) {
        return new None();
      } else if ($ instanceof Nvidia) {
        return core.nvidia_api_key;
      } else {
        return core.hf_token;
      }
    })(),
  );
}

/**
 * Create default configuration pointing to local AI Brain
 */
export function default_config() {
  return new NimConfig(
    "localhost",
    8000,
    "sd-v1.5-controlnet-isometric",
    60_000,
    new Local(),
    new None(),
  );
}

function resolve_endpoint(config, path) {
  let $ = config.mode;
  if ($ instanceof Local) {
    let url = ((("http://" + config.host) + ":") + $int.to_string(config.port)) + path;
    return [url, toList([])];
  } else if ($ instanceof Nvidia) {
    let url = "https://ai.api.nvidia.com/v1/genai/stabilityai/stable-diffusion-3.5-large";
    let key = $option.unwrap(config.api_key, "");
    return [url, toList([["Authorization", "Bearer " + key]])];
  } else {
    let url = "https://api-inference.huggingface.co/models/" + config.model;
    let key = $option.unwrap(config.api_key, "");
    return [url, toList([["Authorization", "Bearer " + key]])];
  }
}

function base_url(config) {
  return (("http://" + config.host) + ":") + $int.to_string(config.port);
}

/**
 * Check if AI Brain is online
 */
export function health_check(config) {
  let url = base_url(config) + "/health";
  return $http.health_check(url);
}

function convert_http_error(result) {
  if (result instanceof Ok) {
    return result;
  } else {
    let $ = result[0];
    if ($ instanceof $http.ConnectionError) {
      let msg = $[0];
      return new Error("Connection error: " + msg);
    } else if ($ instanceof $http.TimeoutError) {
      return new Error("Request timed out");
    } else {
      let msg = $[0];
      return new Error("Invalid response: " + msg);
    }
  }
}

function validate_response(result) {
  if (result instanceof Ok) {
    let resp = result[0];
    if (resp.status === 200) {
      return new Ok(resp.body);
    } else {
      let resp = result[0];
      return new Error("Server returned " + $int.to_string(resp.status));
    }
  } else {
    let err = result[0];
    return new Error("HTTP request failed: " + err);
  }
}

function build_generate_request(prompt, control_image_b64, seed, mode) {
  if (mode instanceof Local) {
    return $json.object(
      toList([
        ["prompt", $json.string(prompt)],
        ["width", $json.int(512)],
        ["height", $json.int(512)],
        ["steps", $json.int(25)],
        ["seed", $json.int(seed)],
        [
          "control_image_b64",
          (() => {
            if (control_image_b64 instanceof $option.Some) {
              let img = control_image_b64[0];
              return $json.string(img);
            } else {
              return $json.null$();
            }
          })(),
        ],
      ]),
    );
  } else if (mode instanceof Nvidia) {
    return $json.object(
      toList([
        [
          "text_prompts",
          $json.preprocessed_array(
            toList([
              $json.object(
                toList([
                  ["text", $json.string(prompt)],
                  ["weight", $json.float(1.0)],
                ]),
              ),
            ]),
          ),
        ],
        ["cfg_scale", $json.float(5.0)],
        ["seed", $json.int(seed)],
        ["sampler", $json.string("K_EULER_ANCESTRAL")],
        ["steps", $json.int(25)],
      ]),
    );
  } else {
    return $json.object(
      toList([
        ["inputs", $json.string(prompt)],
        [
          "parameters",
          $json.object(
            toList([
              ["negative_prompt", $json.string("blurry, low quality")],
              ["num_inference_steps", $json.int(25)],
              ["guidance_scale", $json.float(7.5)],
            ]),
          ),
        ],
      ]),
    );
  }
}

function build_classify_request(image_b64, labels) {
  return $json.object(
    toList([
      ["image_b64", $json.string(image_b64)],
      ["labels", $json.preprocessed_array($list.map(labels, $json.string))],
    ]),
  );
}

function format_json_error(err) {
  if (err instanceof $json.UnexpectedEndOfInput) {
    return "Unexpected end of input";
  } else if (err instanceof $json.UnexpectedByte) {
    let byte = err[0];
    return "Unexpected byte: " + byte;
  } else if (err instanceof $json.UnexpectedSequence) {
    let bytes = err[0];
    return "Unexpected sequence: " + bytes;
  } else {
    let errors = err[0];
    let error_msgs = $list.map(
      errors,
      (err) => {
        return (((("Field '" + $string.join(err.path, ".")) + "': Expected ") + err.expected) + ", found ") + err.found;
      },
    );
    return "Decode failed: " + $string.join(error_msgs, "; ");
  }
}

function extract_image_field(result) {
  if (result instanceof Ok) {
    let body = result[0];
    return $result.try$(
      (() => {
        let _pipe = $bit_array.to_string(body);
        return $result.replace_error(_pipe, "Invalid UTF-8");
      })(),
      (body_str) => {
        let decoder = $decode.field(
          "image_b64",
          $decode.string,
          (image) => { return $decode.success(image); },
        );
        let _pipe = $json.parse(body_str, decoder);
        return $result.map_error(_pipe, format_json_error);
      },
    );
  } else {
    return result;
  }
}

/**
 * Generate a tile using Stable Diffusion + ControlNet
 *
 * ## Parameters
 * - prompt: User description of the tile (e.g., "brick building")
 * - control_image_b64: Optional base64-encoded control image for ControlNet
 * - seed: Random seed (-1 for random)
 * - config: Server configuration
 *
 * ## Returns
 * - Ok(base64_image) on success
 * - Error(message) on failure
 */
export function generate_tile(prompt, control_image_b64, seed, config) {
  let $ = resolve_endpoint(config, "/generate");
  let url;
  let headers;
  url = $[0];
  headers = $[1];
  let body_json = build_generate_request(
    prompt,
    control_image_b64,
    seed,
    config.mode,
  );
  let body_str = $json.to_string(body_json);
  let _pipe = $http.post_json_with_headers(url, body_str, headers);
  let _pipe$1 = convert_http_error(_pipe);
  let _pipe$2 = validate_response(_pipe$1);
  let _pipe$3 = extract_image_field(_pipe$2);
  return $result.try$(
    _pipe$3,
    (b64) => {
      let _pipe$4 = $bit_array.base64_decode(b64);
      return $result.replace_error(_pipe$4, "Failed to decode Base64 image");
    },
  );
}

function extract_embedding_field(result) {
  if (result instanceof Ok) {
    let body = result[0];
    return $result.try$(
      (() => {
        let _pipe = $bit_array.to_string(body);
        return $result.replace_error(_pipe, "Invalid UTF-8");
      })(),
      (body_str) => {
        let decoder = $decode.field(
          "embedding",
          $decode.list($decode.float),
          (vector) => {
            return $decode.field(
              "dimensions",
              $decode.int,
              (dimensions) => {
                return $decode.success(new ClipEmbedding(vector, dimensions));
              },
            );
          },
        );
        let _pipe = $json.parse(body_str, decoder);
        return $result.map_error(_pipe, format_json_error);
      },
    );
  } else {
    return result;
  }
}

/**
 * Get CLIP embedding for an image (for QA checks)
 */
export function clip_embed(image_b64, config) {
  let $ = resolve_endpoint(config, "/clip/embed");
  let url;
  let headers;
  url = $[0];
  headers = $[1];
  let body_json = $json.object(toList([["image_b64", $json.string(image_b64)]]));
  let body_str = $json.to_string(body_json);
  let _pipe = $http.post_json_with_headers(url, body_str, headers);
  let _pipe$1 = convert_http_error(_pipe);
  let _pipe$2 = validate_response(_pipe$1);
  return extract_embedding_field(_pipe$2);
}

function extract_classifications(result) {
  if (result instanceof Ok) {
    let body = result[0];
    return $result.try$(
      (() => {
        let _pipe = $bit_array.to_string(body);
        return $result.replace_error(_pipe, "Invalid UTF-8");
      })(),
      (body_str) => {
        let decoder = $decode.list(
          $decode.field(
            "label",
            $decode.string,
            (label) => {
              return $decode.field(
                "confidence",
                $decode.float,
                (confidence) => {
                  return $decode.success(new Classification(label, confidence));
                },
              );
            },
          ),
        );
        let _pipe = $json.parse(body_str, decoder);
        return $result.map_error(_pipe, format_json_error);
      },
    );
  } else {
    return result;
  }
}

/**
 * Classify image using CLIP (Zero-shot classification)
 */
export function clip_classify(image_b64, labels, config) {
  let $ = resolve_endpoint(config, "/clip/classify");
  let url;
  let headers;
  url = $[0];
  headers = $[1];
  let body_json = build_classify_request(image_b64, labels);
  let body_str = $json.to_string(body_json);
  let _pipe = $http.post_json_with_headers(url, body_str, headers);
  let _pipe$1 = convert_http_error(_pipe);
  let _pipe$2 = validate_response(_pipe$1);
  return extract_classifications(_pipe$2);
}

function extract_chat_content(result) {
  if (result instanceof Ok) {
    let body = result[0];
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
        return $result.map_error(_pipe, format_json_error);
      },
    );
  } else {
    return result;
  }
}

/**
 * Vision QA using Llama 3.2 Vision (NIM)
 * Pergunta para a IA o que ela está vendo na imagem.
 */
export function vision_qa(image_b64, question, config) {
  let url = "https://integrate.api.nvidia.com/v1/chat/completions";
  let model = "meta/llama-3.2-11b-vision-instruct";
  let key = $option.unwrap(config.api_key, "");
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
                ["role", $json.string("user")],
                [
                  "content",
                  $json.preprocessed_array(
                    toList([
                      $json.object(
                        toList([
                          ["type", $json.string("text")],
                          ["text", $json.string(question)],
                        ]),
                      ),
                      $json.object(
                        toList([
                          ["type", $json.string("image_url")],
                          [
                            "image_url",
                            $json.object(
                              toList([
                                [
                                  "url",
                                  $json.string(
                                    "data:image/png;base64," + image_b64,
                                  ),
                                ],
                              ]),
                            ),
                          ],
                        ]),
                      ),
                    ]),
                  ),
                ],
              ]),
            ),
          ]),
        ),
      ],
      ["max_tokens", $json.int(100)],
    ]),
  );
  let _pipe = $http.post_json_with_headers(url, $json.to_string(body), headers);
  let _pipe$1 = convert_http_error(_pipe);
  let _pipe$2 = validate_response(_pipe$1);
  return extract_chat_content(_pipe$2);
}
