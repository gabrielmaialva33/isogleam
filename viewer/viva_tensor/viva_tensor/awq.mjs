import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import {
  Ok,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  divideFloat,
  divideInt,
} from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

export class AWQConfig extends $CustomType {
  constructor(bits, group_size, alpha, zero_point) {
    super();
    this.bits = bits;
    this.group_size = group_size;
    this.alpha = alpha;
    this.zero_point = zero_point;
  }
}
export const AWQConfig$AWQConfig = (bits, group_size, alpha, zero_point) =>
  new AWQConfig(bits, group_size, alpha, zero_point);
export const AWQConfig$isAWQConfig = (value) => value instanceof AWQConfig;
export const AWQConfig$AWQConfig$bits = (value) => value.bits;
export const AWQConfig$AWQConfig$0 = (value) => value.bits;
export const AWQConfig$AWQConfig$group_size = (value) => value.group_size;
export const AWQConfig$AWQConfig$1 = (value) => value.group_size;
export const AWQConfig$AWQConfig$alpha = (value) => value.alpha;
export const AWQConfig$AWQConfig$2 = (value) => value.alpha;
export const AWQConfig$AWQConfig$zero_point = (value) => value.zero_point;
export const AWQConfig$AWQConfig$3 = (value) => value.zero_point;

export class AWQScales extends $CustomType {
  constructor(weight_scales, activation_stats, alpha) {
    super();
    this.weight_scales = weight_scales;
    this.activation_stats = activation_stats;
    this.alpha = alpha;
  }
}
export const AWQScales$AWQScales = (weight_scales, activation_stats, alpha) =>
  new AWQScales(weight_scales, activation_stats, alpha);
export const AWQScales$isAWQScales = (value) => value instanceof AWQScales;
export const AWQScales$AWQScales$weight_scales = (value) => value.weight_scales;
export const AWQScales$AWQScales$0 = (value) => value.weight_scales;
export const AWQScales$AWQScales$activation_stats = (value) =>
  value.activation_stats;
export const AWQScales$AWQScales$1 = (value) => value.activation_stats;
export const AWQScales$AWQScales$alpha = (value) => value.alpha;
export const AWQScales$AWQScales$2 = (value) => value.alpha;

export class AWQTensor extends $CustomType {
  constructor(quantized_weights, awq_scales, quant_scales, zero_points, shape, memory_bytes) {
    super();
    this.quantized_weights = quantized_weights;
    this.awq_scales = awq_scales;
    this.quant_scales = quant_scales;
    this.zero_points = zero_points;
    this.shape = shape;
    this.memory_bytes = memory_bytes;
  }
}
export const AWQTensor$AWQTensor = (quantized_weights, awq_scales, quant_scales, zero_points, shape, memory_bytes) =>
  new AWQTensor(quantized_weights,
  awq_scales,
  quant_scales,
  zero_points,
  shape,
  memory_bytes);
export const AWQTensor$isAWQTensor = (value) => value instanceof AWQTensor;
export const AWQTensor$AWQTensor$quantized_weights = (value) =>
  value.quantized_weights;
export const AWQTensor$AWQTensor$0 = (value) => value.quantized_weights;
export const AWQTensor$AWQTensor$awq_scales = (value) => value.awq_scales;
export const AWQTensor$AWQTensor$1 = (value) => value.awq_scales;
export const AWQTensor$AWQTensor$quant_scales = (value) => value.quant_scales;
export const AWQTensor$AWQTensor$2 = (value) => value.quant_scales;
export const AWQTensor$AWQTensor$zero_points = (value) => value.zero_points;
export const AWQTensor$AWQTensor$3 = (value) => value.zero_points;
export const AWQTensor$AWQTensor$shape = (value) => value.shape;
export const AWQTensor$AWQTensor$4 = (value) => value.shape;
export const AWQTensor$AWQTensor$memory_bytes = (value) => value.memory_bytes;
export const AWQTensor$AWQTensor$5 = (value) => value.memory_bytes;

/**
 * Configuração padrão AWQ
 */
export function default_config() {
  return new AWQConfig(4, 128, 0.5, false);
}

/**
 * Coleta estatísticas de ativação de um batch de calibração
 * Retorna média absoluta por canal
 */
export function collect_activation_stats(activations_batch) {
  if (activations_batch instanceof $Empty) {
    return activations_batch;
  } else {
    let first = activations_batch.head;
    let num_channels = $list.length(first);
    let initial = $list.repeat(0.0, num_channels);
    let sums = $list.fold(
      activations_batch,
      initial,
      (acc, activation) => {
        return $list.map2(
          acc,
          activation,
          (sum, act) => { return sum + $float.absolute_value(act); },
        );
      },
    );
    let num_samples = $int.to_float($list.length(activations_batch));
    return $list.map(sums, (sum) => { return divideFloat(sum, num_samples); });
  }
}

/**
 * Aplica transformação equivalente aos pesos
 * W' = W * diag(s)
 * Isso escala canais salientes PARA CIMA
 */
export function apply_weight_transform(weights, scales) {
  return $list.map(
    weights,
    (row) => {
      return $list.map2(row, scales.weight_scales, (w, s) => { return w * s; });
    },
  );
}

/**
 * Aplica transformação inversa às ativações
 * X' = X * diag(1/s)
 * Isso compensa o scaling dos pesos
 */
export function apply_activation_transform(activations, scales) {
  return $list.map2(
    activations,
    scales.weight_scales,
    (x, s) => {
      let $ = s > 0.0;
      if ($) {
        return divideFloat(x, s);
      } else {
        return x;
      }
    },
  );
}

/**
 * Identifica canais salientes (top-k por ativação)
 */
export function identify_salient_channels(activation_stats, top_percent) {
  let n = $list.length(activation_stats);
  let _block;
  let _pipe = $float.round((($int.to_float(n) * top_percent)) / 100.0);
  _block = $int.max(_pipe, 1);
  let k = _block;
  let _pipe$1 = activation_stats;
  let _pipe$2 = $list.index_map(_pipe$1, (stat, idx) => { return [idx, stat]; });
  let _pipe$3 = $list.sort(
    _pipe$2,
    (a, b) => { return $float.compare(b[1], a[1]); },
  );
  let _pipe$4 = $list.take(_pipe$3, k);
  return $list.map(_pipe$4, (pair) => { return pair[0]; });
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

function get_at_index_float(lst, idx, default$) {
  let $ = $list.drop(lst, idx);
  if ($ instanceof $Empty) {
    return default$;
  } else {
    let first = $.head;
    return first;
  }
}

/**
 * Dequantiza tensor AWQ
 */
export function dequantize_awq(awq) {
  let _block;
  let $ = awq.quant_scales;
  if ($ instanceof $Empty) {
    _block = $list.length(awq.quantized_weights);
  } else {
    _block = divideInt(
      $list.length(awq.quantized_weights),
      $list.length(awq.quant_scales)
    );
  }
  let group_size = _block;
  let groups = $list.sized_chunk(awq.quantized_weights, group_size);
  let _block$1;
  let _pipe = $list.index_map(
    groups,
    (group, idx) => {
      let scale = get_at_index_float(awq.quant_scales, idx, 1.0);
      return $list.map(
        group,
        (q) => { return divideFloat($int.to_float(q), scale); },
      );
    },
  );
  _block$1 = $list.flatten(_pipe);
  let dequantized = _block$1;
  let _block$2;
  let $1 = awq.shape;
  if ($1 instanceof $Empty) {
    _block$2 = 1;
  } else {
    let $2 = $1.tail;
    if ($2 instanceof $Empty) {
      _block$2 = 1;
    } else {
      let $3 = $2.tail;
      if ($3 instanceof $Empty) {
        let i = $2.head;
        _block$2 = i;
      } else {
        _block$2 = 1;
      }
    }
  }
  let in_features = _block$2;
  let weight_matrix = $list.sized_chunk(dequantized, in_features);
  let _block$3;
  let _pipe$1 = $list.map(
    weight_matrix,
    (row) => {
      return $list.map2(
        row,
        awq.awq_scales.weight_scales,
        (w, s) => {
          let $2 = s > 0.0;
          if ($2) {
            return divideFloat(w, s);
          } else {
            return w;
          }
        },
      );
    },
  );
  _block$3 = $list.flatten(_pipe$1);
  let restored = _block$3;
  return new Tensor(restored, awq.shape);
}

function float_power(base, exp) {
  let $ = $float.power(base, exp);
  if ($ instanceof Ok) {
    let result = $[0];
    return result;
  } else {
    return 1.0;
  }
}

/**
 * Computa scales AWQ baseado nas estatísticas de ativação
 * scale[i] = activation_stat[i] ^ alpha
 */
export function compute_awq_scales(activation_stats, alpha) {
  let weight_scales = $list.map(
    activation_stats,
    (stat) => {
      let _block;
      let $ = stat > 0.0;
      if ($) {
        _block = stat;
      } else {
        _block = 1.0;
      }
      let safe_stat = _block;
      return float_power(safe_stat, alpha);
    },
  );
  return new AWQScales(weight_scales, activation_stats, alpha);
}

function float_result_to_float(r, default$) {
  if (r instanceof Ok) {
    let v = r[0];
    return v;
  } else {
    return default$;
  }
}

/**
 * Quantização simétrica por grupos
 * 
 * @ignore
 */
function symmetric_group_quantize(values, bits, group_size) {
  let _block;
  let _pipe = $float.power(2.0, $int.to_float(bits - 1));
  let _pipe$1 = float_result_to_float(_pipe, 128.0);
  _block = ((x) => { return x - 1.0; })(_pipe$1);
  let qmax = _block;
  let groups = $list.sized_chunk(values, group_size);
  let $ = $list.fold(
    groups,
    [toList([]), toList([])],
    (acc, group) => {
      let q_acc;
      let s_acc;
      q_acc = acc[0];
      s_acc = acc[1];
      let _block$1;
      let _pipe$2 = group;
      let _pipe$3 = $list.map(_pipe$2, $float.absolute_value);
      _block$1 = $list.fold(_pipe$3, 0.0, $float.max);
      let max_abs = _block$1;
      let _block$2;
      let $1 = max_abs > 0.0;
      if ($1) {
        _block$2 = divideFloat(qmax, max_abs);
      } else {
        _block$2 = 1.0;
      }
      let scale = _block$2;
      let quantized = $list.map(
        group,
        (v) => {
          let scaled = v * scale;
          let clamped = $float.clamp(scaled, -1.0 * qmax, qmax);
          return $float.round(clamped);
        },
      );
      return [$list.append(q_acc, quantized), listPrepend(scale, s_acc)];
    },
  );
  let quantized_groups;
  let scales;
  quantized_groups = $[0];
  scales = $[1];
  return [quantized_groups, $list.reverse(scales)];
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 10000.0)) / 10000.0;
  return $float.to_string(rounded);
}
