import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}

function format_number(n) {
  let $ = n >= 1_000_000;
  if ($) {
    return $int.to_string(globalThis.Math.trunc(n / 1_000_000)) + "M";
  } else {
    let $1 = n >= 1000;
    if ($1) {
      return $int.to_string(globalThis.Math.trunc(n / 1000)) + "K";
    } else {
      return $int.to_string(n);
    }
  }
}
