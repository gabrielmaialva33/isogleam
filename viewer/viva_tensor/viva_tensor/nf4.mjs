import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { toList, Empty as $Empty, CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

export class NF4Block extends $CustomType {
  constructor(indices, abs_max, block_size) {
    super();
    this.indices = indices;
    this.abs_max = abs_max;
    this.block_size = block_size;
  }
}
export const NF4Block$NF4Block = (indices, abs_max, block_size) =>
  new NF4Block(indices, abs_max, block_size);
export const NF4Block$isNF4Block = (value) => value instanceof NF4Block;
export const NF4Block$NF4Block$indices = (value) => value.indices;
export const NF4Block$NF4Block$0 = (value) => value.indices;
export const NF4Block$NF4Block$abs_max = (value) => value.abs_max;
export const NF4Block$NF4Block$1 = (value) => value.abs_max;
export const NF4Block$NF4Block$block_size = (value) => value.block_size;
export const NF4Block$NF4Block$2 = (value) => value.block_size;

export class NF4Tensor extends $CustomType {
  constructor(blocks, shape, num_elements, memory_bytes, compression_ratio) {
    super();
    this.blocks = blocks;
    this.shape = shape;
    this.num_elements = num_elements;
    this.memory_bytes = memory_bytes;
    this.compression_ratio = compression_ratio;
  }
}
export const NF4Tensor$NF4Tensor = (blocks, shape, num_elements, memory_bytes, compression_ratio) =>
  new NF4Tensor(blocks, shape, num_elements, memory_bytes, compression_ratio);
export const NF4Tensor$isNF4Tensor = (value) => value instanceof NF4Tensor;
export const NF4Tensor$NF4Tensor$blocks = (value) => value.blocks;
export const NF4Tensor$NF4Tensor$0 = (value) => value.blocks;
export const NF4Tensor$NF4Tensor$shape = (value) => value.shape;
export const NF4Tensor$NF4Tensor$1 = (value) => value.shape;
export const NF4Tensor$NF4Tensor$num_elements = (value) => value.num_elements;
export const NF4Tensor$NF4Tensor$2 = (value) => value.num_elements;
export const NF4Tensor$NF4Tensor$memory_bytes = (value) => value.memory_bytes;
export const NF4Tensor$NF4Tensor$3 = (value) => value.memory_bytes;
export const NF4Tensor$NF4Tensor$compression_ratio = (value) =>
  value.compression_ratio;
export const NF4Tensor$NF4Tensor$4 = (value) => value.compression_ratio;

export class NF4Config extends $CustomType {
  constructor(block_size, double_quant) {
    super();
    this.block_size = block_size;
    this.double_quant = double_quant;
  }
}
export const NF4Config$NF4Config = (block_size, double_quant) =>
  new NF4Config(block_size, double_quant);
export const NF4Config$isNF4Config = (value) => value instanceof NF4Config;
export const NF4Config$NF4Config$block_size = (value) => value.block_size;
export const NF4Config$NF4Config$0 = (value) => value.block_size;
export const NF4Config$NF4Config$double_quant = (value) => value.double_quant;
export const NF4Config$NF4Config$1 = (value) => value.double_quant;

export class DoubleQuantNF4 extends $CustomType {
  constructor(blocks, quantized_scales, scales_scale, shape, num_elements, memory_bytes) {
    super();
    this.blocks = blocks;
    this.quantized_scales = quantized_scales;
    this.scales_scale = scales_scale;
    this.shape = shape;
    this.num_elements = num_elements;
    this.memory_bytes = memory_bytes;
  }
}
export const DoubleQuantNF4$DoubleQuantNF4 = (blocks, quantized_scales, scales_scale, shape, num_elements, memory_bytes) =>
  new DoubleQuantNF4(blocks,
  quantized_scales,
  scales_scale,
  shape,
  num_elements,
  memory_bytes);
export const DoubleQuantNF4$isDoubleQuantNF4 = (value) =>
  value instanceof DoubleQuantNF4;
export const DoubleQuantNF4$DoubleQuantNF4$blocks = (value) => value.blocks;
export const DoubleQuantNF4$DoubleQuantNF4$0 = (value) => value.blocks;
export const DoubleQuantNF4$DoubleQuantNF4$quantized_scales = (value) =>
  value.quantized_scales;
export const DoubleQuantNF4$DoubleQuantNF4$1 = (value) =>
  value.quantized_scales;
export const DoubleQuantNF4$DoubleQuantNF4$scales_scale = (value) =>
  value.scales_scale;
export const DoubleQuantNF4$DoubleQuantNF4$2 = (value) => value.scales_scale;
export const DoubleQuantNF4$DoubleQuantNF4$shape = (value) => value.shape;
export const DoubleQuantNF4$DoubleQuantNF4$3 = (value) => value.shape;
export const DoubleQuantNF4$DoubleQuantNF4$num_elements = (value) =>
  value.num_elements;
export const DoubleQuantNF4$DoubleQuantNF4$4 = (value) => value.num_elements;
export const DoubleQuantNF4$DoubleQuantNF4$memory_bytes = (value) =>
  value.memory_bytes;
export const DoubleQuantNF4$DoubleQuantNF4$5 = (value) => value.memory_bytes;

export class NF4Stats extends $CustomType {
  constructor(original_bytes, compressed_bytes, compression_ratio, mean_error, max_error, num_blocks) {
    super();
    this.original_bytes = original_bytes;
    this.compressed_bytes = compressed_bytes;
    this.compression_ratio = compression_ratio;
    this.mean_error = mean_error;
    this.max_error = max_error;
    this.num_blocks = num_blocks;
  }
}
export const NF4Stats$NF4Stats = (original_bytes, compressed_bytes, compression_ratio, mean_error, max_error, num_blocks) =>
  new NF4Stats(original_bytes,
  compressed_bytes,
  compression_ratio,
  mean_error,
  max_error,
  num_blocks);
export const NF4Stats$isNF4Stats = (value) => value instanceof NF4Stats;
export const NF4Stats$NF4Stats$original_bytes = (value) => value.original_bytes;
export const NF4Stats$NF4Stats$0 = (value) => value.original_bytes;
export const NF4Stats$NF4Stats$compressed_bytes = (value) =>
  value.compressed_bytes;
export const NF4Stats$NF4Stats$1 = (value) => value.compressed_bytes;
export const NF4Stats$NF4Stats$compression_ratio = (value) =>
  value.compression_ratio;
export const NF4Stats$NF4Stats$2 = (value) => value.compression_ratio;
export const NF4Stats$NF4Stats$mean_error = (value) => value.mean_error;
export const NF4Stats$NF4Stats$3 = (value) => value.mean_error;
export const NF4Stats$NF4Stats$max_error = (value) => value.max_error;
export const NF4Stats$NF4Stats$4 = (value) => value.max_error;
export const NF4Stats$NF4Stats$num_blocks = (value) => value.num_blocks;
export const NF4Stats$NF4Stats$5 = (value) => value.num_blocks;

/**
 * Os 16 níveis NF4 são os quantis de N(0,1) normalizados para [-1, 1]
 * Esses valores são hardcoded em bitsandbytes e usados em QLoRA
 */
export function nf4_levels() {
  return toList([
    -1.0,
    -0.6961928009986877,
    -0.5250730514526367,
    -0.39491748809814453,
    -0.28444138169288635,
    -0.18477343022823334,
    -0.09105003625154495,
    0.0,
    0.07958029955625534,
    0.16093020141124725,
    0.24611230194568634,
    0.33791524171829224,
    0.44070982933044434,
    0.5626170039176941,
    0.7229568362236023,
    1.0,
  ]);
}

/**
 * Configuração padrão QLoRA
 */
export function default_config() {
  return new NF4Config(64, false);
}

/**
 * Encontra o índice do nível NF4 mais próximo
 * 
 * @ignore
 */
function find_nearest_nf4_index(value) {
  let levels = nf4_levels();
  let _pipe = levels;
  let _pipe$1 = $list.index_map(
    _pipe,
    (level, idx) => {
      let distance = $float.absolute_value(value - level);
      return [idx, distance];
    },
  );
  let _pipe$2 = $list.fold(
    _pipe$1,
    [0, 999.0],
    (best, current) => {
      let $ = current[1] < best[1];
      if ($) {
        return current;
      } else {
        return best;
      }
    },
  );
  return ((result) => { return result[0]; })(_pipe$2);
}

/**
 * Quantiza um bloco de valores para NF4
 * 
 * @ignore
 */
function quantize_block(values, block_size) {
  let _block;
  let _pipe = values;
  let _pipe$1 = $list.map(_pipe, $float.absolute_value);
  _block = $list.fold(_pipe$1, 0.0, $float.max);
  let abs_max = _block;
  let _block$1;
  let $ = abs_max > 0.0;
  if ($) {
    _block$1 = abs_max;
  } else {
    _block$1 = 1.0;
  }
  let safe_max = _block$1;
  let normalized = $list.map(
    values,
    (v) => { return divideFloat(v, safe_max); },
  );
  let indices = $list.map(normalized, find_nearest_nf4_index);
  return new NF4Block(indices, safe_max, block_size);
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

function get_at_index(lst, idx, default$) {
  let $ = $list.drop(lst, idx);
  if ($ instanceof $Empty) {
    return default$;
  } else {
    let first = $.head;
    return first;
  }
}

/**
 * Dequantiza tensor NF4 de volta para FP32
 */
export function dequantize(nf4) {
  let levels = nf4_levels();
  let data = $list.flat_map(
    nf4.blocks,
    (block) => {
      return $list.map(
        block.indices,
        (idx) => {
          let level = get_at_index(levels, idx, 0.0);
          return level * block.abs_max;
        },
      );
    },
  );
  let truncated = $list.take(data, nf4.num_elements);
  return new Tensor(truncated, nf4.shape);
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 10000.0)) / 10000.0;
  return $float.to_string(rounded);
}
