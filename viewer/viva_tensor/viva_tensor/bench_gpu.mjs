import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { Ok, Error } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

function safe_glands_check() {
  return new Error(undefined);
}

function check_glands() {
  let $ = safe_glands_check();
  if ($ instanceof Ok) {
    return true;
  } else {
    return false;
  }
}

function bench_similarity_comparative() {
  return $io.println("(Comparativo requer viva_glands carregado)");
}

function bench_dot_comparative() {
  return $io.println("(Comparativo requer viva_glands carregado)");
}

function bench_matmul_comparative() {
  return $io.println("(Comparativo requer viva_glands carregado)");
}

function run_comparative_benchmarks() {
  $io.println("━━━ SIMILARITY BENCHMARK ━━━");
  bench_similarity_comparative();
  $io.println("\n━━━ DOT PRODUCT BENCHMARK ━━━");
  bench_dot_comparative();
  $io.println("\n━━━ MATMUL BENCHMARK ━━━");
  return bench_matmul_comparative();
}

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
