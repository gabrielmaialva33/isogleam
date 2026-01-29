import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { Ok, Empty as $Empty, CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

export class FlashConfig extends $CustomType {
  constructor(block_q, block_kv, scale, causal) {
    super();
    this.block_q = block_q;
    this.block_kv = block_kv;
    this.scale = scale;
    this.causal = causal;
  }
}
export const FlashConfig$FlashConfig = (block_q, block_kv, scale, causal) =>
  new FlashConfig(block_q, block_kv, scale, causal);
export const FlashConfig$isFlashConfig = (value) =>
  value instanceof FlashConfig;
export const FlashConfig$FlashConfig$block_q = (value) => value.block_q;
export const FlashConfig$FlashConfig$0 = (value) => value.block_q;
export const FlashConfig$FlashConfig$block_kv = (value) => value.block_kv;
export const FlashConfig$FlashConfig$1 = (value) => value.block_kv;
export const FlashConfig$FlashConfig$scale = (value) => value.scale;
export const FlashConfig$FlashConfig$2 = (value) => value.scale;
export const FlashConfig$FlashConfig$causal = (value) => value.causal;
export const FlashConfig$FlashConfig$3 = (value) => value.causal;

export class OnlineStats extends $CustomType {
  constructor(max_val, sum_exp, output) {
    super();
    this.max_val = max_val;
    this.sum_exp = sum_exp;
    this.output = output;
  }
}
export const OnlineStats$OnlineStats = (max_val, sum_exp, output) =>
  new OnlineStats(max_val, sum_exp, output);
export const OnlineStats$isOnlineStats = (value) =>
  value instanceof OnlineStats;
export const OnlineStats$OnlineStats$max_val = (value) => value.max_val;
export const OnlineStats$OnlineStats$0 = (value) => value.max_val;
export const OnlineStats$OnlineStats$sum_exp = (value) => value.sum_exp;
export const OnlineStats$OnlineStats$1 = (value) => value.sum_exp;
export const OnlineStats$OnlineStats$output = (value) => value.output;
export const OnlineStats$OnlineStats$2 = (value) => value.output;

export class FlashResult extends $CustomType {
  constructor(output, memory_bytes, memory_saved_percent) {
    super();
    this.output = output;
    this.memory_bytes = memory_bytes;
    this.memory_saved_percent = memory_saved_percent;
  }
}
export const FlashResult$FlashResult = (output, memory_bytes, memory_saved_percent) =>
  new FlashResult(output, memory_bytes, memory_saved_percent);
export const FlashResult$isFlashResult = (value) =>
  value instanceof FlashResult;
export const FlashResult$FlashResult$output = (value) => value.output;
export const FlashResult$FlashResult$0 = (value) => value.output;
export const FlashResult$FlashResult$memory_bytes = (value) =>
  value.memory_bytes;
export const FlashResult$FlashResult$1 = (value) => value.memory_bytes;
export const FlashResult$FlashResult$memory_saved_percent = (value) =>
  value.memory_saved_percent;
export const FlashResult$FlashResult$2 = (value) => value.memory_saved_percent;

/**
 * Configuração padrão
 */
export function default_config(head_dim) {
  let _block;
  let $ = $float.square_root($int.to_float(head_dim));
  if ($ instanceof Ok) {
    let sqrt = $[0];
    _block = divideFloat(1.0, sqrt);
  } else {
    _block = 0.125;
  }
  let scale = _block;
  return new FlashConfig(64, 64, scale, false);
}

/**
 * Config para causal (autoregressive)
 */
export function causal_config(head_dim) {
  let _record = default_config(head_dim);
  return new FlashConfig(_record.block_q, _record.block_kv, _record.scale, true);
}

function dot_product(a, b) {
  let _pipe = $list.map2(a, b, (x, y) => { return x * y; });
  return $list.fold(_pipe, 0.0, $float.add);
}

function get_row(matrix, idx) {
  let $ = $list.drop(matrix, idx);
  if ($ instanceof $Empty) {
    return $;
  } else {
    let row = $.head;
    return row;
  }
}

function result_to_float(r, default$) {
  if (r instanceof Ok) {
    let v = r[0];
    return v;
  } else {
    return default$;
  }
}

/**
 * Processa um bloco KV contra queries, atualiza estatísticas online
 * 
 * @ignore
 */
function process_kv_block(stats, q_block, k_block, v_block, config) {
  return $list.map2(
    stats,
    q_block,
    (stat, q_row) => {
      let scores = $list.map(
        k_block,
        (k_row) => { return dot_product(q_row, k_row) * config.scale; },
      );
      let new_max = $list.fold(scores, stat.max_val, $float.max);
      let _block;
      let $ = stat.sum_exp > 0.0;
      if ($) {
        let _pipe = $float.power(2.71828, stat.max_val - new_max);
        _block = result_to_float(_pipe, 1.0);
      } else {
        _block = 1.0;
      }
      let correction = _block;
      let corrected_sum = stat.sum_exp * correction;
      let exp_scores = $list.map(
        scores,
        (s) => {
          let _pipe = $float.power(2.71828, s - new_max);
          return result_to_float(_pipe, 0.0);
        },
      );
      let new_sum = $list.fold(exp_scores, corrected_sum, $float.add);
      let corrected_output = $list.map(
        stat.output,
        (o) => { return o * correction; },
      );
      let new_contribution = $list.index_fold(
        exp_scores,
        $list.repeat(0.0, $list.length(stat.output)),
        (acc, weight, i) => {
          let v_row = get_row(v_block, i);
          return $list.map2(acc, v_row, (a, v) => { return a + (weight * v); });
        },
      );
      let new_output = $list.map2(
        corrected_output,
        new_contribution,
        $float.add,
      );
      return new OnlineStats(new_max, new_sum, new_output);
    },
  );
}

/**
 * Processa um bloco de Q contra todos os blocos KV
 * 
 * @ignore
 */
function process_q_block(
  q_block,
  k_blocks,
  v_blocks,
  config,
  q_block_idx,
  head_dim
) {
  let initial_stats = $list.map(
    q_block,
    (_) => {
      return new OnlineStats(-999999.0, 0.0, $list.repeat(0.0, head_dim));
    },
  );
  let zipped_kv = $list.zip(k_blocks, v_blocks);
  let final_stats = $list.index_fold(
    zipped_kv,
    initial_stats,
    (stats, kv_pair, kv_idx) => {
      let k_block;
      let v_block;
      k_block = kv_pair[0];
      v_block = kv_pair[1];
      let $ = config.causal;
      if ($) {
        let q_start = q_block_idx * config.block_q;
        let kv_start = kv_idx * config.block_kv;
        let $1 = kv_start + $list.length(k_block);
        
        let $2 = kv_start > (q_start + $list.length(q_block));
        if ($2) {
          return stats;
        } else {
          return process_kv_block(stats, q_block, k_block, v_block, config);
        }
      } else {
        return process_kv_block(stats, q_block, k_block, v_block, config);
      }
    },
  );
  return $list.map(
    final_stats,
    (s) => {
      let $ = s.sum_exp > 0.0;
      if ($) {
        return $list.map(s.output, (o) => { return divideFloat(o, s.sum_exp); });
      } else {
        return s.output;
      }
    },
  );
}

function softmax_row(row) {
  let max_val = $list.fold(row, -999999.0, $float.max);
  let exp_vals = $list.map(
    row,
    (x) => {
      let _pipe = $float.power(2.71828, x - max_val);
      return result_to_float(_pipe, 0.0);
    },
  );
  let sum = $list.fold(exp_vals, 0.0, $float.add);
  let $ = sum > 0.0;
  if ($) {
    return $list.map(exp_vals, (e) => { return divideFloat(e, sum); });
  } else {
    return row;
  }
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}

function bytes_to_string(bytes) {
  let b = bytes;
  if (b >= 1_073_741_824) {
    return float_to_string($int.to_float(b) / 1073741824.0) + "GB";
  } else {
    let b = bytes;
    if (b >= 1_048_576) {
      return float_to_string($int.to_float(b) / 1048576.0) + "MB";
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

function get_tensor_shape(t) {
  if (t instanceof Tensor) {
    let shape = t.shape;
    return shape;
  } else {
    let shape = t.shape;
    return shape;
  }
}
