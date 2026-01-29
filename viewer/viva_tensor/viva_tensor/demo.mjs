import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok } from "../gleam.mjs";
import * as $awq from "../viva_tensor/awq.mjs";
import * as $compression from "../viva_tensor/compression.mjs";
import * as $flash_attention from "../viva_tensor/flash_attention.mjs";
import * as $nf4 from "../viva_tensor/nf4.mjs";
import * as $sparsity from "../viva_tensor/sparsity.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

function demo_combined() {
  $io.println("┌─────────────────────────────────────────────────────────────┐");
  $io.println("│ 7. MEMORY MULTIPLICATION - Combining Techniques            │");
  $io.println("└─────────────────────────────────────────────────────────────┘");
  let params = 7_000_000_000;
  let fp16_size = params * 2;
  $io.println("  Model: 7B parameters");
  $io.println(
    ("  FP16 size: " + $int.to_string(
      globalThis.Math.trunc(fp16_size / 1_000_000_000),
    )) + "GB",
  );
  $io.println("");
  $io.println("  ┌─────────────────────────────────────────────────────┐");
  $io.println("  │ Technique         │ Size     │ Fits RTX 4090 24GB  │");
  $io.println("  ├───────────────────┼──────────┼─────────────────────┤");
  $io.println("  │ FP16              │ 14GB     │ [x] Tight           │");
  $io.println("  │ INT8              │ 7GB      │ [x] + KV Cache      │");
  $io.println("  │ NF4               │ 3.5GB    │ [x] + Batch=32      │");
  $io.println("  │ NF4 + 2:4 Sparse  │ 1.75GB   │ [x] Multiple models!│");
  $io.println("  └─────────────────────────────────────────────────────┘");
  let vram = 24;
  $io.println("");
  $io.println("  RTX 4090 24GB VRAM can effectively hold:");
  $io.println(
    ("    - FP16:           " + $int.to_string(
      (globalThis.Math.trunc(vram / 14)) * 7,
    )) + "B params",
  );
  $io.println(
    ("    - INT8:           " + $int.to_string(
      (globalThis.Math.trunc(vram / 7)) * 7,
    )) + "B params",
  );
  $io.println(
    ("    - NF4:            " + $int.to_string(vram * 2)) + "B params",
  );
  $io.println(
    ("    - NF4 + Sparsity: " + $int.to_string(vram * 4)) + "B params",
  );
  return $io.println("");
}

function get_shape(t) {
  if (t instanceof Tensor) {
    let shape = t.shape;
    return shape;
  } else {
    let shape = t.shape;
    return shape;
  }
}

function shape_to_string(shape) {
  return ("[" + (() => {
    let _pipe = $list.map(shape, $int.to_string);
    return $string.join(_pipe, ", ");
  })()) + "]";
}

function result_shape_str(r) {
  if (r instanceof Ok) {
    let t = r[0];
    return shape_to_string(get_shape(t));
  } else {
    return "Error";
  }
}

function float_to_str(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}

function format_bytes(bytes) {
  let b = bytes;
  if (b >= 1_073_741_824) {
    return float_to_str($int.to_float(b) / 1073741824.0) + "GB";
  } else {
    let b = bytes;
    if (b >= 1_048_576) {
      return float_to_str($int.to_float(b) / 1048576.0) + "MB";
    } else {
      let b = bytes;
      if (b >= 1024) {
        return $int.to_string(globalThis.Math.trunc(b / 1024)) + "KB";
      } else {
        let b = bytes;
        return $int.to_string(b) + "B";
      }
    }
  }
}
