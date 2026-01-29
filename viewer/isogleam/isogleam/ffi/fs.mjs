import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error } from "../../gleam.mjs";
import { write_file as do_write_file } from "./fs_ffi.mjs";

export function write_bytes(path, data) {
  let $ = do_write_file(path, data);
  if ($ instanceof Ok) {
    return new Ok(undefined);
  } else {
    let e = $[0];
    return new Error(((("Failed to write file: " + path) + " (") + e) + ")");
  }
}

export function write_base64_image(path, b64_data) {
  return $result.try$(
    (() => {
      let _pipe = $bit_array.base64_decode(b64_data);
      return $result.replace_error(_pipe, "Failed to decode Base64");
    })(),
    (bytes) => { return write_bytes(path, bytes); },
  );
}
