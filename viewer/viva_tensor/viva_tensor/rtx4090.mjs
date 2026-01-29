import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { Ok, Error, CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $blackwell from "../viva_tensor/blackwell.mjs";
import { compress, int8_config, nvfp4_config } from "../viva_tensor/blackwell.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

export class Rtx4090Specs extends $CustomType {
  constructor(cuda_cores, tensor_cores, vram_gb, vram_available_gb, bandwidth_gbps, tdp_watts, tflops_fp32, tflops_fp16, tops_int8, warp_size, sm_count, l2_cache_mb) {
    super();
    this.cuda_cores = cuda_cores;
    this.tensor_cores = tensor_cores;
    this.vram_gb = vram_gb;
    this.vram_available_gb = vram_available_gb;
    this.bandwidth_gbps = bandwidth_gbps;
    this.tdp_watts = tdp_watts;
    this.tflops_fp32 = tflops_fp32;
    this.tflops_fp16 = tflops_fp16;
    this.tops_int8 = tops_int8;
    this.warp_size = warp_size;
    this.sm_count = sm_count;
    this.l2_cache_mb = l2_cache_mb;
  }
}
export const Rtx4090Specs$Rtx4090Specs = (cuda_cores, tensor_cores, vram_gb, vram_available_gb, bandwidth_gbps, tdp_watts, tflops_fp32, tflops_fp16, tops_int8, warp_size, sm_count, l2_cache_mb) =>
  new Rtx4090Specs(cuda_cores,
  tensor_cores,
  vram_gb,
  vram_available_gb,
  bandwidth_gbps,
  tdp_watts,
  tflops_fp32,
  tflops_fp16,
  tops_int8,
  warp_size,
  sm_count,
  l2_cache_mb);
export const Rtx4090Specs$isRtx4090Specs = (value) =>
  value instanceof Rtx4090Specs;
export const Rtx4090Specs$Rtx4090Specs$cuda_cores = (value) => value.cuda_cores;
export const Rtx4090Specs$Rtx4090Specs$0 = (value) => value.cuda_cores;
export const Rtx4090Specs$Rtx4090Specs$tensor_cores = (value) =>
  value.tensor_cores;
export const Rtx4090Specs$Rtx4090Specs$1 = (value) => value.tensor_cores;
export const Rtx4090Specs$Rtx4090Specs$vram_gb = (value) => value.vram_gb;
export const Rtx4090Specs$Rtx4090Specs$2 = (value) => value.vram_gb;
export const Rtx4090Specs$Rtx4090Specs$vram_available_gb = (value) =>
  value.vram_available_gb;
export const Rtx4090Specs$Rtx4090Specs$3 = (value) => value.vram_available_gb;
export const Rtx4090Specs$Rtx4090Specs$bandwidth_gbps = (value) =>
  value.bandwidth_gbps;
export const Rtx4090Specs$Rtx4090Specs$4 = (value) => value.bandwidth_gbps;
export const Rtx4090Specs$Rtx4090Specs$tdp_watts = (value) => value.tdp_watts;
export const Rtx4090Specs$Rtx4090Specs$5 = (value) => value.tdp_watts;
export const Rtx4090Specs$Rtx4090Specs$tflops_fp32 = (value) =>
  value.tflops_fp32;
export const Rtx4090Specs$Rtx4090Specs$6 = (value) => value.tflops_fp32;
export const Rtx4090Specs$Rtx4090Specs$tflops_fp16 = (value) =>
  value.tflops_fp16;
export const Rtx4090Specs$Rtx4090Specs$7 = (value) => value.tflops_fp16;
export const Rtx4090Specs$Rtx4090Specs$tops_int8 = (value) => value.tops_int8;
export const Rtx4090Specs$Rtx4090Specs$8 = (value) => value.tops_int8;
export const Rtx4090Specs$Rtx4090Specs$warp_size = (value) => value.warp_size;
export const Rtx4090Specs$Rtx4090Specs$9 = (value) => value.warp_size;
export const Rtx4090Specs$Rtx4090Specs$sm_count = (value) => value.sm_count;
export const Rtx4090Specs$Rtx4090Specs$10 = (value) => value.sm_count;
export const Rtx4090Specs$Rtx4090Specs$l2_cache_mb = (value) =>
  value.l2_cache_mb;
export const Rtx4090Specs$Rtx4090Specs$11 = (value) => value.l2_cache_mb;

export class Rtx4090Config extends $CustomType {
  constructor(optimal_batch_size, tensor_core_tile, memory_alignment, threads_per_block, use_tensor_cores, quant_mode) {
    super();
    this.optimal_batch_size = optimal_batch_size;
    this.tensor_core_tile = tensor_core_tile;
    this.memory_alignment = memory_alignment;
    this.threads_per_block = threads_per_block;
    this.use_tensor_cores = use_tensor_cores;
    this.quant_mode = quant_mode;
  }
}
export const Rtx4090Config$Rtx4090Config = (optimal_batch_size, tensor_core_tile, memory_alignment, threads_per_block, use_tensor_cores, quant_mode) =>
  new Rtx4090Config(optimal_batch_size,
  tensor_core_tile,
  memory_alignment,
  threads_per_block,
  use_tensor_cores,
  quant_mode);
export const Rtx4090Config$isRtx4090Config = (value) =>
  value instanceof Rtx4090Config;
export const Rtx4090Config$Rtx4090Config$optimal_batch_size = (value) =>
  value.optimal_batch_size;
export const Rtx4090Config$Rtx4090Config$0 = (value) =>
  value.optimal_batch_size;
export const Rtx4090Config$Rtx4090Config$tensor_core_tile = (value) =>
  value.tensor_core_tile;
export const Rtx4090Config$Rtx4090Config$1 = (value) => value.tensor_core_tile;
export const Rtx4090Config$Rtx4090Config$memory_alignment = (value) =>
  value.memory_alignment;
export const Rtx4090Config$Rtx4090Config$2 = (value) => value.memory_alignment;
export const Rtx4090Config$Rtx4090Config$threads_per_block = (value) =>
  value.threads_per_block;
export const Rtx4090Config$Rtx4090Config$3 = (value) => value.threads_per_block;
export const Rtx4090Config$Rtx4090Config$use_tensor_cores = (value) =>
  value.use_tensor_cores;
export const Rtx4090Config$Rtx4090Config$4 = (value) => value.use_tensor_cores;
export const Rtx4090Config$Rtx4090Config$quant_mode = (value) =>
  value.quant_mode;
export const Rtx4090Config$Rtx4090Config$5 = (value) => value.quant_mode;

export class Fp32Mode extends $CustomType {}
export const QuantMode4090$Fp32Mode = () => new Fp32Mode();
export const QuantMode4090$isFp32Mode = (value) => value instanceof Fp32Mode;

export class Fp16TensorMode extends $CustomType {}
export const QuantMode4090$Fp16TensorMode = () => new Fp16TensorMode();
export const QuantMode4090$isFp16TensorMode = (value) =>
  value instanceof Fp16TensorMode;

export class Int8TensorMode extends $CustomType {}
export const QuantMode4090$Int8TensorMode = () => new Int8TensorMode();
export const QuantMode4090$isInt8TensorMode = (value) =>
  value instanceof Int8TensorMode;

export class MixedPrecisionMode extends $CustomType {}
export const QuantMode4090$MixedPrecisionMode = () => new MixedPrecisionMode();
export const QuantMode4090$isMixedPrecisionMode = (value) =>
  value instanceof MixedPrecisionMode;

export class GpuMemoryState extends $CustomType {
  constructor(total_bytes, used_bytes, free_bytes, allocated_tensors, cached_bytes) {
    super();
    this.total_bytes = total_bytes;
    this.used_bytes = used_bytes;
    this.free_bytes = free_bytes;
    this.allocated_tensors = allocated_tensors;
    this.cached_bytes = cached_bytes;
  }
}
export const GpuMemoryState$GpuMemoryState = (total_bytes, used_bytes, free_bytes, allocated_tensors, cached_bytes) =>
  new GpuMemoryState(total_bytes,
  used_bytes,
  free_bytes,
  allocated_tensors,
  cached_bytes);
export const GpuMemoryState$isGpuMemoryState = (value) =>
  value instanceof GpuMemoryState;
export const GpuMemoryState$GpuMemoryState$total_bytes = (value) =>
  value.total_bytes;
export const GpuMemoryState$GpuMemoryState$0 = (value) => value.total_bytes;
export const GpuMemoryState$GpuMemoryState$used_bytes = (value) =>
  value.used_bytes;
export const GpuMemoryState$GpuMemoryState$1 = (value) => value.used_bytes;
export const GpuMemoryState$GpuMemoryState$free_bytes = (value) =>
  value.free_bytes;
export const GpuMemoryState$GpuMemoryState$2 = (value) => value.free_bytes;
export const GpuMemoryState$GpuMemoryState$allocated_tensors = (value) =>
  value.allocated_tensors;
export const GpuMemoryState$GpuMemoryState$3 = (value) =>
  value.allocated_tensors;
export const GpuMemoryState$GpuMemoryState$cached_bytes = (value) =>
  value.cached_bytes;
export const GpuMemoryState$GpuMemoryState$4 = (value) => value.cached_bytes;

export class BatchResult extends $CustomType {
  constructor(tensors, total_time_ms, throughput_tps, compression_ratio, memory_saved_mb) {
    super();
    this.tensors = tensors;
    this.total_time_ms = total_time_ms;
    this.throughput_tps = throughput_tps;
    this.compression_ratio = compression_ratio;
    this.memory_saved_mb = memory_saved_mb;
  }
}
export const BatchResult$BatchResult = (tensors, total_time_ms, throughput_tps, compression_ratio, memory_saved_mb) =>
  new BatchResult(tensors,
  total_time_ms,
  throughput_tps,
  compression_ratio,
  memory_saved_mb);
export const BatchResult$isBatchResult = (value) =>
  value instanceof BatchResult;
export const BatchResult$BatchResult$tensors = (value) => value.tensors;
export const BatchResult$BatchResult$0 = (value) => value.tensors;
export const BatchResult$BatchResult$total_time_ms = (value) =>
  value.total_time_ms;
export const BatchResult$BatchResult$1 = (value) => value.total_time_ms;
export const BatchResult$BatchResult$throughput_tps = (value) =>
  value.throughput_tps;
export const BatchResult$BatchResult$2 = (value) => value.throughput_tps;
export const BatchResult$BatchResult$compression_ratio = (value) =>
  value.compression_ratio;
export const BatchResult$BatchResult$3 = (value) => value.compression_ratio;
export const BatchResult$BatchResult$memory_saved_mb = (value) =>
  value.memory_saved_mb;
export const BatchResult$BatchResult$4 = (value) => value.memory_saved_mb;

export class PerformanceEstimate extends $CustomType {
  constructor(theoretical_flops, achievable_flops, estimated_time_ms, bottleneck, efficiency_pct) {
    super();
    this.theoretical_flops = theoretical_flops;
    this.achievable_flops = achievable_flops;
    this.estimated_time_ms = estimated_time_ms;
    this.bottleneck = bottleneck;
    this.efficiency_pct = efficiency_pct;
  }
}
export const PerformanceEstimate$PerformanceEstimate = (theoretical_flops, achievable_flops, estimated_time_ms, bottleneck, efficiency_pct) =>
  new PerformanceEstimate(theoretical_flops,
  achievable_flops,
  estimated_time_ms,
  bottleneck,
  efficiency_pct);
export const PerformanceEstimate$isPerformanceEstimate = (value) =>
  value instanceof PerformanceEstimate;
export const PerformanceEstimate$PerformanceEstimate$theoretical_flops = (value) =>
  value.theoretical_flops;
export const PerformanceEstimate$PerformanceEstimate$0 = (value) =>
  value.theoretical_flops;
export const PerformanceEstimate$PerformanceEstimate$achievable_flops = (value) =>
  value.achievable_flops;
export const PerformanceEstimate$PerformanceEstimate$1 = (value) =>
  value.achievable_flops;
export const PerformanceEstimate$PerformanceEstimate$estimated_time_ms = (value) =>
  value.estimated_time_ms;
export const PerformanceEstimate$PerformanceEstimate$2 = (value) =>
  value.estimated_time_ms;
export const PerformanceEstimate$PerformanceEstimate$bottleneck = (value) =>
  value.bottleneck;
export const PerformanceEstimate$PerformanceEstimate$3 = (value) =>
  value.bottleneck;
export const PerformanceEstimate$PerformanceEstimate$efficiency_pct = (value) =>
  value.efficiency_pct;
export const PerformanceEstimate$PerformanceEstimate$4 = (value) =>
  value.efficiency_pct;

export class ComputeBound extends $CustomType {}
export const Bottleneck$ComputeBound = () => new ComputeBound();
export const Bottleneck$isComputeBound = (value) =>
  value instanceof ComputeBound;

export class MemoryBound extends $CustomType {}
export const Bottleneck$MemoryBound = () => new MemoryBound();
export const Bottleneck$isMemoryBound = (value) => value instanceof MemoryBound;

export class LatencyBound extends $CustomType {}
export const Bottleneck$LatencyBound = () => new LatencyBound();
export const Bottleneck$isLatencyBound = (value) =>
  value instanceof LatencyBound;

/**
 * Retorna specs da RTX 4090
 */
export function get_specs() {
  return new Rtx4090Specs(
    16_384,
    512,
    24.0,
    22.0,
    1008.0,
    450,
    82.6,
    330.0,
    661.0,
    32,
    128,
    72,
  );
}

/**
 * Configuração padrão otimizada
 */
export function default_config() {
  let $ = get_specs();
  
  let batch_size = 128;
  return new Rtx4090Config(batch_size, 16, 32, 256, true, new Int8TensorMode());
}

/**
 * Configuração para máxima precisão
 */
export function precision_config() {
  let _record = default_config();
  return new Rtx4090Config(
    _record.optimal_batch_size,
    _record.tensor_core_tile,
    _record.memory_alignment,
    _record.threads_per_block,
    false,
    new Fp32Mode(),
  );
}

/**
 * Configuração para máxima velocidade
 */
export function speed_config() {
  let _record = default_config();
  return new Rtx4090Config(
    256,
    _record.tensor_core_tile,
    _record.memory_alignment,
    _record.threads_per_block,
    true,
    new Int8TensorMode(),
  );
}

/**
 * Cria estado inicial de memória para RTX 4090
 */
export function init_memory() {
  let specs = get_specs();
  let total = $float.round(
    ((specs.vram_available_gb * 1024.0) * 1024.0) * 1024.0,
  );
  return new GpuMemoryState(total, 0, total, 0, 0);
}

/**
 * Calcula memória necessária para tensor
 */
export function tensor_memory_bytes(shape, mode) {
  let elements = $list.fold(shape, 1, (acc, d) => { return acc * d; });
  let _block;
  if (mode instanceof Fp32Mode) {
    _block = 4;
  } else if (mode instanceof Fp16TensorMode) {
    _block = 2;
  } else if (mode instanceof Int8TensorMode) {
    _block = 1;
  } else {
    _block = 2;
  }
  let bytes_per_element = _block;
  return elements * bytes_per_element;
}

/**
 * Verifica se tensor cabe na VRAM
 */
export function can_allocate(state, bytes) {
  return state.free_bytes >= bytes;
}

/**
 * Aloca memória para tensor
 */
export function allocate(state, bytes) {
  let $ = can_allocate(state, bytes);
  if ($) {
    return new Ok(
      new GpuMemoryState(
        state.total_bytes,
        state.used_bytes + bytes,
        state.free_bytes - bytes,
        state.allocated_tensors + 1,
        state.cached_bytes,
      ),
    );
  } else {
    return new Error(
      (((("OOM: Não há VRAM suficiente. Livre: " + $int.to_string(
        globalThis.Math.trunc(
          ((globalThis.Math.trunc(state.free_bytes / 1024))) / 1024
        ),
      )) + "MB, ") + "Necessário: ") + $int.to_string(
        globalThis.Math.trunc(((globalThis.Math.trunc(bytes / 1024))) / 1024),
      )) + "MB",
    );
  }
}

/**
 * Libera memória
 */
export function free(state, bytes) {
  return new GpuMemoryState(
    state.total_bytes,
    $int.max(0, state.used_bytes - bytes),
    $int.min(state.total_bytes, state.free_bytes + bytes),
    $int.max(0, state.allocated_tensors - 1),
    state.cached_bytes,
  );
}

/**
 * Estima performance para operação de tensor
 */
export function estimate_performance(flops_needed, bytes_to_transfer, config) {
  let specs = get_specs();
  let _block;
  let $ = config.quant_mode;
  if ($ instanceof Fp32Mode) {
    _block = specs.tflops_fp32;
  } else if ($ instanceof Fp16TensorMode) {
    _block = specs.tflops_fp16;
  } else if ($ instanceof Int8TensorMode) {
    _block = specs.tops_int8;
  } else {
    _block = specs.tflops_fp16;
  }
  let available_tflops = _block;
  let compute_time = divideFloat(
    flops_needed,
    (available_tflops * 1000000000000.0)
  );
  let memory_time = divideFloat(
    bytes_to_transfer,
    (specs.bandwidth_gbps * 1000000000.0)
  );
  let _block$1;
  let $1 = compute_time > memory_time;
  if ($1) {
    _block$1 = new ComputeBound();
  } else {
    _block$1 = new MemoryBound();
  }
  let bottleneck = _block$1;
  let total_time = $float.max(compute_time, memory_time) * 1.2;
  let theoretical_time = $float.max(compute_time, memory_time);
  let efficiency = (divideFloat(theoretical_time, total_time)) * 100.0;
  return new PerformanceEstimate(
    available_tflops * 1000000000000.0,
    (available_tflops * 1000000000000.0) * (efficiency / 100.0),
    total_time * 1000.0,
    bottleneck,
    efficiency,
  );
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}

function bottleneck_str(b) {
  if (b instanceof ComputeBound) {
    return "compute-bound";
  } else if (b instanceof MemoryBound) {
    return "memory-bound";
  } else {
    return "latency-bound";
  }
}
