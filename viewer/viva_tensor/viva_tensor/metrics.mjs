import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

export class QuantMetrics extends $CustomType {
  constructor(mse, mae, rmse, cosine_sim, snr_db, sqnr_db, max_error, p99_error, outlier_pct) {
    super();
    this.mse = mse;
    this.mae = mae;
    this.rmse = rmse;
    this.cosine_sim = cosine_sim;
    this.snr_db = snr_db;
    this.sqnr_db = sqnr_db;
    this.max_error = max_error;
    this.p99_error = p99_error;
    this.outlier_pct = outlier_pct;
  }
}
export const QuantMetrics$QuantMetrics = (mse, mae, rmse, cosine_sim, snr_db, sqnr_db, max_error, p99_error, outlier_pct) =>
  new QuantMetrics(mse,
  mae,
  rmse,
  cosine_sim,
  snr_db,
  sqnr_db,
  max_error,
  p99_error,
  outlier_pct);
export const QuantMetrics$isQuantMetrics = (value) =>
  value instanceof QuantMetrics;
export const QuantMetrics$QuantMetrics$mse = (value) => value.mse;
export const QuantMetrics$QuantMetrics$0 = (value) => value.mse;
export const QuantMetrics$QuantMetrics$mae = (value) => value.mae;
export const QuantMetrics$QuantMetrics$1 = (value) => value.mae;
export const QuantMetrics$QuantMetrics$rmse = (value) => value.rmse;
export const QuantMetrics$QuantMetrics$2 = (value) => value.rmse;
export const QuantMetrics$QuantMetrics$cosine_sim = (value) => value.cosine_sim;
export const QuantMetrics$QuantMetrics$3 = (value) => value.cosine_sim;
export const QuantMetrics$QuantMetrics$snr_db = (value) => value.snr_db;
export const QuantMetrics$QuantMetrics$4 = (value) => value.snr_db;
export const QuantMetrics$QuantMetrics$sqnr_db = (value) => value.sqnr_db;
export const QuantMetrics$QuantMetrics$5 = (value) => value.sqnr_db;
export const QuantMetrics$QuantMetrics$max_error = (value) => value.max_error;
export const QuantMetrics$QuantMetrics$6 = (value) => value.max_error;
export const QuantMetrics$QuantMetrics$p99_error = (value) => value.p99_error;
export const QuantMetrics$QuantMetrics$7 = (value) => value.p99_error;
export const QuantMetrics$QuantMetrics$outlier_pct = (value) =>
  value.outlier_pct;
export const QuantMetrics$QuantMetrics$8 = (value) => value.outlier_pct;

export class LayerMetrics extends $CustomType {
  constructor(layer_name, metrics, sensitivity) {
    super();
    this.layer_name = layer_name;
    this.metrics = metrics;
    this.sensitivity = sensitivity;
  }
}
export const LayerMetrics$LayerMetrics = (layer_name, metrics, sensitivity) =>
  new LayerMetrics(layer_name, metrics, sensitivity);
export const LayerMetrics$isLayerMetrics = (value) =>
  value instanceof LayerMetrics;
export const LayerMetrics$LayerMetrics$layer_name = (value) => value.layer_name;
export const LayerMetrics$LayerMetrics$0 = (value) => value.layer_name;
export const LayerMetrics$LayerMetrics$metrics = (value) => value.metrics;
export const LayerMetrics$LayerMetrics$1 = (value) => value.metrics;
export const LayerMetrics$LayerMetrics$sensitivity = (value) =>
  value.sensitivity;
export const LayerMetrics$LayerMetrics$2 = (value) => value.sensitivity;

/**
 * SQNR - Signal-to-Quantization-Noise Ratio
 * Teórico para N bits: SQNR = 6.02 * N + 1.76 dB
 */
export function theoretical_sqnr(bits) {
  return (6.02 * $int.to_float(bits)) + 1.76;
}

/**
 * Identifica top K% de pesos salientes
 */
export function find_salient_weights(saliency, top_pct) {
  let indexed = $list.index_map(saliency, (s, i) => { return [i, s]; });
  let sorted = $list.sort(
    indexed,
    (a, b) => { return $float.compare(b[1], a[1]); },
  );
  let n = $list.length(saliency);
  let _block;
  let _pipe = $float.round((($int.to_float(n) * top_pct)) / 100.0);
  _block = $int.max(_pipe, 1);
  let k = _block;
  let _pipe$1 = $list.take(sorted, k);
  return $list.map(_pipe$1, (pair) => { return pair[0]; });
}

function get_tensor_shape(t) {
  if (t instanceof Tensor) {
    let shape = t.shape;
    return shape;
  } else {
    let shape = t.shape;
    return shape;
  }
}

function approximate_ln(x) {
  let v = x;
  if (v < 0.001) {
    return -7.0;
  } else {
    let v = x;
    if (v < 0.01) {
      return -4.6;
    } else {
      let v = x;
      if (v < 0.1) {
        return -2.3;
      } else {
        let v = x;
        if (v < 1.0) {
          return v - 1.0;
        } else {
          let v = x;
          if (v < 10.0) {
            return (divideFloat((v - 1.0), v)) * 2.0;
          } else {
            let v = x;
            if (v < 100.0) {
              return 2.3 + (divideFloat(((v / 10.0) - 1.0), (v / 10.0)));
            } else {
              return 4.6;
            }
          }
        }
      }
    }
  }
}

function log10(x) {
  let $ = x > 0.0;
  if ($) {
    let ln_10 = 2.302585093;
    return divideFloat(approximate_ln(x), ln_10);
  } else {
    return 0.0;
  }
}

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

function get_at(list, index) {
  let _pipe = list;
  let _pipe$1 = $list.drop(_pipe, index);
  return $list.first(_pipe$1);
}

function result_or(r, default$) {
  if (r instanceof Ok) {
    let v = r[0];
    return v;
  } else {
    return default$;
  }
}

function pad_or_truncate(lst, target_len, default$) {
  let current_len = $list.length(lst);
  let $ = current_len >= target_len;
  if ($) {
    return $list.take(lst, target_len);
  } else {
    let _pipe = lst;
    return $list.append(_pipe, $list.repeat(default$, target_len - current_len));
  }
}
