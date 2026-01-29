import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { Ok, Empty as $Empty, CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

export class MicroBlock extends $CustomType {
  constructor(values, scale, zero_point) {
    super();
    this.values = values;
    this.scale = scale;
    this.zero_point = zero_point;
  }
}
export const MicroBlock$MicroBlock = (values, scale, zero_point) =>
  new MicroBlock(values, scale, zero_point);
export const MicroBlock$isMicroBlock = (value) => value instanceof MicroBlock;
export const MicroBlock$MicroBlock$values = (value) => value.values;
export const MicroBlock$MicroBlock$0 = (value) => value.values;
export const MicroBlock$MicroBlock$scale = (value) => value.scale;
export const MicroBlock$MicroBlock$1 = (value) => value.scale;
export const MicroBlock$MicroBlock$zero_point = (value) => value.zero_point;
export const MicroBlock$MicroBlock$2 = (value) => value.zero_point;

export class BlackwellTensor extends $CustomType {
  constructor(blocks, global_scale, shape, num_elements, memory_bytes, compression_ratio) {
    super();
    this.blocks = blocks;
    this.global_scale = global_scale;
    this.shape = shape;
    this.num_elements = num_elements;
    this.memory_bytes = memory_bytes;
    this.compression_ratio = compression_ratio;
  }
}
export const BlackwellTensor$BlackwellTensor = (blocks, global_scale, shape, num_elements, memory_bytes, compression_ratio) =>
  new BlackwellTensor(blocks,
  global_scale,
  shape,
  num_elements,
  memory_bytes,
  compression_ratio);
export const BlackwellTensor$isBlackwellTensor = (value) =>
  value instanceof BlackwellTensor;
export const BlackwellTensor$BlackwellTensor$blocks = (value) => value.blocks;
export const BlackwellTensor$BlackwellTensor$0 = (value) => value.blocks;
export const BlackwellTensor$BlackwellTensor$global_scale = (value) =>
  value.global_scale;
export const BlackwellTensor$BlackwellTensor$1 = (value) => value.global_scale;
export const BlackwellTensor$BlackwellTensor$shape = (value) => value.shape;
export const BlackwellTensor$BlackwellTensor$2 = (value) => value.shape;
export const BlackwellTensor$BlackwellTensor$num_elements = (value) =>
  value.num_elements;
export const BlackwellTensor$BlackwellTensor$3 = (value) => value.num_elements;
export const BlackwellTensor$BlackwellTensor$memory_bytes = (value) =>
  value.memory_bytes;
export const BlackwellTensor$BlackwellTensor$4 = (value) => value.memory_bytes;
export const BlackwellTensor$BlackwellTensor$compression_ratio = (value) =>
  value.compression_ratio;
export const BlackwellTensor$BlackwellTensor$5 = (value) =>
  value.compression_ratio;

export class CompressionConfig extends $CustomType {
  constructor(block_size, bits_per_value, symmetric, max_error_pct) {
    super();
    this.block_size = block_size;
    this.bits_per_value = bits_per_value;
    this.symmetric = symmetric;
    this.max_error_pct = max_error_pct;
  }
}
export const CompressionConfig$CompressionConfig = (block_size, bits_per_value, symmetric, max_error_pct) =>
  new CompressionConfig(block_size, bits_per_value, symmetric, max_error_pct);
export const CompressionConfig$isCompressionConfig = (value) =>
  value instanceof CompressionConfig;
export const CompressionConfig$CompressionConfig$block_size = (value) =>
  value.block_size;
export const CompressionConfig$CompressionConfig$0 = (value) =>
  value.block_size;
export const CompressionConfig$CompressionConfig$bits_per_value = (value) =>
  value.bits_per_value;
export const CompressionConfig$CompressionConfig$1 = (value) =>
  value.bits_per_value;
export const CompressionConfig$CompressionConfig$symmetric = (value) =>
  value.symmetric;
export const CompressionConfig$CompressionConfig$2 = (value) => value.symmetric;
export const CompressionConfig$CompressionConfig$max_error_pct = (value) =>
  value.max_error_pct;
export const CompressionConfig$CompressionConfig$3 = (value) =>
  value.max_error_pct;

export class CompressionStats extends $CustomType {
  constructor(original_bytes, compressed_bytes, compression_ratio, mean_error, max_error, blocks_processed) {
    super();
    this.original_bytes = original_bytes;
    this.compressed_bytes = compressed_bytes;
    this.compression_ratio = compression_ratio;
    this.mean_error = mean_error;
    this.max_error = max_error;
    this.blocks_processed = blocks_processed;
  }
}
export const CompressionStats$CompressionStats = (original_bytes, compressed_bytes, compression_ratio, mean_error, max_error, blocks_processed) =>
  new CompressionStats(original_bytes,
  compressed_bytes,
  compression_ratio,
  mean_error,
  max_error,
  blocks_processed);
export const CompressionStats$isCompressionStats = (value) =>
  value instanceof CompressionStats;
export const CompressionStats$CompressionStats$original_bytes = (value) =>
  value.original_bytes;
export const CompressionStats$CompressionStats$0 = (value) =>
  value.original_bytes;
export const CompressionStats$CompressionStats$compressed_bytes = (value) =>
  value.compressed_bytes;
export const CompressionStats$CompressionStats$1 = (value) =>
  value.compressed_bytes;
export const CompressionStats$CompressionStats$compression_ratio = (value) =>
  value.compression_ratio;
export const CompressionStats$CompressionStats$2 = (value) =>
  value.compression_ratio;
export const CompressionStats$CompressionStats$mean_error = (value) =>
  value.mean_error;
export const CompressionStats$CompressionStats$3 = (value) => value.mean_error;
export const CompressionStats$CompressionStats$max_error = (value) =>
  value.max_error;
export const CompressionStats$CompressionStats$4 = (value) => value.max_error;
export const CompressionStats$CompressionStats$blocks_processed = (value) =>
  value.blocks_processed;
export const CompressionStats$CompressionStats$5 = (value) =>
  value.blocks_processed;

export class StreamChunk extends $CustomType {
  constructor(id, block, compressed) {
    super();
    this.id = id;
    this.block = block;
    this.compressed = compressed;
  }
}
export const StreamChunk$StreamChunk = (id, block, compressed) =>
  new StreamChunk(id, block, compressed);
export const StreamChunk$isStreamChunk = (value) =>
  value instanceof StreamChunk;
export const StreamChunk$StreamChunk$id = (value) => value.id;
export const StreamChunk$StreamChunk$0 = (value) => value.id;
export const StreamChunk$StreamChunk$block = (value) => value.block;
export const StreamChunk$StreamChunk$1 = (value) => value.block;
export const StreamChunk$StreamChunk$compressed = (value) => value.compressed;
export const StreamChunk$StreamChunk$2 = (value) => value.compressed;

export class StreamState extends $CustomType {
  constructor(config, processed_chunks, total_bytes_in, total_bytes_out) {
    super();
    this.config = config;
    this.processed_chunks = processed_chunks;
    this.total_bytes_in = total_bytes_in;
    this.total_bytes_out = total_bytes_out;
  }
}
export const StreamState$StreamState = (config, processed_chunks, total_bytes_in, total_bytes_out) =>
  new StreamState(config, processed_chunks, total_bytes_in, total_bytes_out);
export const StreamState$isStreamState = (value) =>
  value instanceof StreamState;
export const StreamState$StreamState$config = (value) => value.config;
export const StreamState$StreamState$0 = (value) => value.config;
export const StreamState$StreamState$processed_chunks = (value) =>
  value.processed_chunks;
export const StreamState$StreamState$1 = (value) => value.processed_chunks;
export const StreamState$StreamState$total_bytes_in = (value) =>
  value.total_bytes_in;
export const StreamState$StreamState$2 = (value) => value.total_bytes_in;
export const StreamState$StreamState$total_bytes_out = (value) =>
  value.total_bytes_out;
export const StreamState$StreamState$3 = (value) => value.total_bytes_out;

export class DistributionStats extends $CustomType {
  constructor(mean, std, min_val, max_val, dynamic_range, sparsity) {
    super();
    this.mean = mean;
    this.std = std;
    this.min_val = min_val;
    this.max_val = max_val;
    this.dynamic_range = dynamic_range;
    this.sparsity = sparsity;
  }
}
export const DistributionStats$DistributionStats = (mean, std, min_val, max_val, dynamic_range, sparsity) =>
  new DistributionStats(mean, std, min_val, max_val, dynamic_range, sparsity);
export const DistributionStats$isDistributionStats = (value) =>
  value instanceof DistributionStats;
export const DistributionStats$DistributionStats$mean = (value) => value.mean;
export const DistributionStats$DistributionStats$0 = (value) => value.mean;
export const DistributionStats$DistributionStats$std = (value) => value.std;
export const DistributionStats$DistributionStats$1 = (value) => value.std;
export const DistributionStats$DistributionStats$min_val = (value) =>
  value.min_val;
export const DistributionStats$DistributionStats$2 = (value) => value.min_val;
export const DistributionStats$DistributionStats$max_val = (value) =>
  value.max_val;
export const DistributionStats$DistributionStats$3 = (value) => value.max_val;
export const DistributionStats$DistributionStats$dynamic_range = (value) =>
  value.dynamic_range;
export const DistributionStats$DistributionStats$4 = (value) =>
  value.dynamic_range;
export const DistributionStats$DistributionStats$sparsity = (value) =>
  value.sparsity;
export const DistributionStats$DistributionStats$5 = (value) => value.sparsity;

export class Registers extends $CustomType {}
export const MemoryLevel$Registers = () => new Registers();
export const MemoryLevel$isRegisters = (value) => value instanceof Registers;

export class L1Cache extends $CustomType {}
export const MemoryLevel$L1Cache = () => new L1Cache();
export const MemoryLevel$isL1Cache = (value) => value instanceof L1Cache;

export class L2Cache extends $CustomType {}
export const MemoryLevel$L2Cache = () => new L2Cache();
export const MemoryLevel$isL2Cache = (value) => value instanceof L2Cache;

export class Hbm extends $CustomType {}
export const MemoryLevel$Hbm = () => new Hbm();
export const MemoryLevel$isHbm = (value) => value instanceof Hbm;

export class SystemRam extends $CustomType {}
export const MemoryLevel$SystemRam = () => new SystemRam();
export const MemoryLevel$isSystemRam = (value) => value instanceof SystemRam;

export class Storage extends $CustomType {}
export const MemoryLevel$Storage = () => new Storage();
export const MemoryLevel$isStorage = (value) => value instanceof Storage;

/**
 * Configuração padrão NVFP4 (Blackwell style)
 */
export function nvfp4_config() {
  return new CompressionConfig(16, 4, false, 2.0);
}

/**
 * Configuração INT8 (mais precisa)
 */
export function int8_config() {
  return new CompressionConfig(32, 8, true, 0.5);
}

/**
 * Descomprime tensor Blackwell de volta para FP32
 */
export function decompress(bt) {
  let data = $list.flat_map(
    bt.blocks,
    (block) => {
      return $list.map(
        block.values,
        (q) => {
          let dequant = (divideFloat($int.to_float(q), block.scale)) + block.zero_point;
          return dequant * bt.global_scale;
        },
      );
    },
  );
  let truncated = $list.take(data, bt.num_elements);
  return new Tensor(truncated, bt.shape);
}

/**
 * Cria novo estado de streaming
 */
export function new_stream(config) {
  return new StreamState(config, 0, 0, 0);
}

/**
 * Simula latência de acesso
 */
export function memory_latency_ns(level) {
  if (level instanceof Registers) {
    return 1;
  } else if (level instanceof L1Cache) {
    return 4;
  } else if (level instanceof L2Cache) {
    return 12;
  } else if (level instanceof Hbm) {
    return 200;
  } else if (level instanceof SystemRam) {
    return 100;
  } else {
    return 10_000;
  }
}

/**
 * Simula bandwidth em GB/s
 */
export function memory_bandwidth_gbps(level) {
  if (level instanceof Registers) {
    return 10000.0;
  } else if (level instanceof L1Cache) {
    return 1000.0;
  } else if (level instanceof L2Cache) {
    return 500.0;
  } else if (level instanceof Hbm) {
    return 8000.0;
  } else if (level instanceof SystemRam) {
    return 51.2;
  } else {
    return 7.0;
  }
}

/**
 * Calcula tempo de transferência
 */
export function transfer_time_us(size_mb, level) {
  let bandwidth = memory_bandwidth_gbps(level);
  let size_gb = size_mb / 1024.0;
  let time_s = divideFloat(size_gb, bandwidth);
  return time_s * 1000000.0;
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

function find_max_abs(data) {
  return $list.fold(
    data,
    0.0,
    (acc, v) => {
      let abs_v = $float.absolute_value(v);
      let $ = abs_v > acc;
      if ($) {
        return abs_v;
      } else {
        return acc;
      }
    },
  );
}

function find_max(data) {
  if (data instanceof $Empty) {
    return 0.0;
  } else {
    let first = data.head;
    let rest = data.tail;
    return $list.fold(
      rest,
      first,
      (acc, v) => {
        let $ = v > acc;
        if ($) {
          return v;
        } else {
          return acc;
        }
      },
    );
  }
}

function find_min(data) {
  if (data instanceof $Empty) {
    return 0.0;
  } else {
    let first = data.head;
    let rest = data.tail;
    return $list.fold(
      rest,
      first,
      (acc, v) => {
        let $ = v < acc;
        if ($) {
          return v;
        } else {
          return acc;
        }
      },
    );
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
 * Quantiza um micro-block de 16 valores
 * 
 * @ignore
 */
function quantize_microblock(values, config, global_scale) {
  let normalized = $list.map(
    values,
    (v) => { return divideFloat(v, global_scale); },
  );
  let block_min = find_min(normalized);
  let block_max = find_max(normalized);
  let _block;
  let $1 = config.symmetric;
  if ($1) {
    let max_abs = $float.max(
      $float.absolute_value(block_min),
      $float.absolute_value(block_max),
    );
    let _block$1;
    let _pipe = $float.power(2.0, $int.to_float(config.bits_per_value - 1));
    _block$1 = result_to_float(_pipe, 128.0);
    let max_int = _block$1;
    let _block$2;
    let $2 = max_abs > 0.0;
    if ($2) {
      _block$2 = divideFloat(max_int, max_abs);
    } else {
      _block$2 = 1.0;
    }
    let scale = _block$2;
    _block = [scale, 0.0];
  } else {
    let range = block_max - block_min;
    let _block$1;
    let _pipe = $float.power(2.0, $int.to_float(config.bits_per_value));
    _block$1 = result_to_float(_pipe, 16.0);
    let max_int = _block$1;
    let _block$2;
    let $2 = range > 0.0;
    if ($2) {
      _block$2 = divideFloat((max_int - 1.0), range);
    } else {
      _block$2 = 1.0;
    }
    let scale = _block$2;
    _block = [scale, block_min];
  }
  let $ = _block;
  let scale;
  let zero_point;
  scale = $[0];
  zero_point = $[1];
  let _block$1;
  let _pipe = $float.power(2.0, $int.to_float(config.bits_per_value));
  let _pipe$1 = result_to_float(_pipe, 16.0);
  _block$1 = ((x) => { return x - 1.0; })(_pipe$1);
  let max_val = _block$1;
  let quantized = $list.map(
    normalized,
    (v) => {
      let shifted = (v - zero_point) * scale;
      let clamped = $float.clamp(shifted, 0.0, max_val);
      return $float.round(clamped);
    },
  );
  return new MicroBlock(quantized, scale, zero_point);
}

/**
 * Processa um chunk de dados em streaming
 */
export function process_chunk(state, data) {
  let global_scale = find_max_abs(data);
  let block = quantize_microblock(
    data,
    state.config,
    (() => {
      let $ = global_scale > 0.0;
      if ($) {
        return global_scale;
      } else {
        return 1.0;
      }
    })(),
  );
  let bytes_in = $list.length(data) * 4;
  let bytes_out = (globalThis.Math.trunc(
    (state.config.block_size * state.config.bits_per_value) / 8
  )) + 8;
  let new_state = new StreamState(
    state.config,
    state.processed_chunks + 1,
    state.total_bytes_in + bytes_in,
    state.total_bytes_out + bytes_out,
  );
  return [new_state, block];
}

/**
 * Analisa distribuição dos dados
 * 
 * @ignore
 */
function analyze_distribution(data) {
  let n = $list.length(data);
  let n_float = $int.to_float(n);
  let sum = $list.fold(data, 0.0, (acc, v) => { return acc + v; });
  let _block;
  let $ = n > 0;
  if ($) {
    _block = divideFloat(sum, n_float);
  } else {
    _block = 0.0;
  }
  let mean = _block;
  let _block$1;
  let $1 = n > 0;
  if ($1) {
    let sum_sq = $list.fold(
      data,
      0.0,
      (acc, v) => {
        let diff = v - mean;
        return acc + (diff * diff);
      },
    );
    _block$1 = divideFloat(sum_sq, n_float);
  } else {
    _block$1 = 0.0;
  }
  let variance = _block$1;
  let _block$2;
  let _pipe = $float.square_root(variance);
  _block$2 = result_to_float(_pipe, 0.0);
  let std = _block$2;
  let min_val = find_min(data);
  let max_val = find_max(data);
  let _block$3;
  let $2 = min_val !== 0.0;
  if ($2) {
    _block$3 = $float.absolute_value(divideFloat(max_val, min_val));
  } else {
    _block$3 = $float.absolute_value(max_val);
  }
  let dynamic_range = _block$3;
  let zero_threshold = 0.001;
  let near_zero = $list.filter(
    data,
    (v) => { return $float.absolute_value(v) < zero_threshold; },
  );
  let sparsity = divideFloat($int.to_float($list.length(near_zero)), n_float);
  return new DistributionStats(
    mean,
    std,
    min_val,
    max_val,
    dynamic_range,
    sparsity,
  );
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}
