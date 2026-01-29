import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error, toList } from "../../gleam.mjs";
import * as $config from "../../isogleam/core/config.mjs";
import * as $http from "../../isogleam/ffi/http.mjs";

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

export function generate_tile(prompt, config) {
  let model = "stabilityai/stable-diffusion-2-1";
  let url = "https://api-inference.huggingface.co/models/" + model;
  let key = $option.unwrap(config.hf_token, "");
  let headers = toList([["Authorization", "Bearer " + key]]);
  let body = $json.object(
    toList([
      ["inputs", $json.string(prompt)],
      [
        "parameters",
        $json.object(
          toList([
            ["negative_prompt", $json.string(config.negative_prompt)],
            ["num_inference_steps", $json.int(config.num_steps)],
            ["guidance_scale", $json.float(config.guidance_scale)],
          ]),
        ),
      ],
    ]),
  );
  let _pipe = $http.post_json_with_headers(url, $json.to_string(body), headers);
  let _pipe$1 = $result.map_error(
    _pipe,
    (e) => { return "HF Error: " + http_error_string(e); },
  );
  return $result.try$(
    _pipe$1,
    (resp) => {
      let $ = resp.status;
      if ($ === 200) {
        return new Ok(resp.body);
      } else {
        return new Error("HF API Error " + $int.to_string(resp.status));
      }
    },
  );
}
