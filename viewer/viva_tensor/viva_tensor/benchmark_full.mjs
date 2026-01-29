import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $awq from "../viva_tensor/awq.mjs";
import * as $compression from "../viva_tensor/compression.mjs";
import * as $metrics from "../viva_tensor/metrics.mjs";
import * as $nf4 from "../viva_tensor/nf4.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

function float_to_str(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}

function pad_float(f) {
  let s = float_to_str(f);
  let len = $string.length(s);
  let padding = 10 - len;
  let $ = padding > 0;
  if ($) {
    return s + $string.repeat(" ", padding);
  } else {
    return $string.slice(s, 0, 10);
  }
}

function pad_ratio(f) {
  let s = float_to_str(f);
  let len = $string.length(s);
  let $ = len < 4;
  if ($) {
    return $string.repeat(" ", 4 - len) + s;
  } else {
    return s;
  }
}

function pad_snr(f) {
  let s = float_to_str(f);
  let len = $string.length(s);
  let $ = len < 6;
  if ($) {
    return $string.repeat(" ", 6 - len) + s;
  } else {
    return $string.slice(s, 0, 6);
  }
}

function pad_gap(f) {
  let s = float_to_str(f);
  let len = $string.length(s);
  let $ = len < 6;
  if ($) {
    return $string.repeat(" ", 6 - len) + s;
  } else {
    return $string.slice(s, 0, 6);
  }
}

function pad_cos(f) {
  let s = float_to_str(f);
  let len = $string.length(s);
  let $ = len < 6;
  if ($) {
    return s + $string.repeat(" ", 6 - len);
  } else {
    return $string.slice(s, 0, 6);
  }
}
