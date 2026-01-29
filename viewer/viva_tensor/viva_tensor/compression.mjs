import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import {
  Ok,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  divideFloat,
} from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

export class Fp32 extends $CustomType {}
export const QuantFormat$Fp32 = () => new Fp32();
export const QuantFormat$isFp32 = (value) => value instanceof Fp32;

export class Fp16 extends $CustomType {}
export const QuantFormat$Fp16 = () => new Fp16();
export const QuantFormat$isFp16 = (value) => value instanceof Fp16;

/**
 * Integer 8-bit com escala (1 byte + 1 float per block)
 */
export class Int8 extends $CustomType {
  constructor(scale) {
    super();
    this.scale = scale;
  }
}
export const QuantFormat$Int8 = (scale) => new Int8(scale);
export const QuantFormat$isInt8 = (value) => value instanceof Int8;
export const QuantFormat$Int8$scale = (value) => value.scale;
export const QuantFormat$Int8$0 = (value) => value.scale;

/**
 * 4-bit quantizado (0.5 bytes per value) - GGML style
 */
export class Quant4 extends $CustomType {
  constructor(block_size, scales) {
    super();
    this.block_size = block_size;
    this.scales = scales;
  }
}
export const QuantFormat$Quant4 = (block_size, scales) =>
  new Quant4(block_size, scales);
export const QuantFormat$isQuant4 = (value) => value instanceof Quant4;
export const QuantFormat$Quant4$block_size = (value) => value.block_size;
export const QuantFormat$Quant4$0 = (value) => value.block_size;
export const QuantFormat$Quant4$scales = (value) => value.scales;
export const QuantFormat$Quant4$1 = (value) => value.scales;

/**
 * 4-bit com min/max (mais preciso)
 */
export class Quant4Min extends $CustomType {
  constructor(block_size, scales, mins) {
    super();
    this.block_size = block_size;
    this.scales = scales;
    this.mins = mins;
  }
}
export const QuantFormat$Quant4Min = (block_size, scales, mins) =>
  new Quant4Min(block_size, scales, mins);
export const QuantFormat$isQuant4Min = (value) => value instanceof Quant4Min;
export const QuantFormat$Quant4Min$block_size = (value) => value.block_size;
export const QuantFormat$Quant4Min$0 = (value) => value.block_size;
export const QuantFormat$Quant4Min$scales = (value) => value.scales;
export const QuantFormat$Quant4Min$1 = (value) => value.scales;
export const QuantFormat$Quant4Min$mins = (value) => value.mins;
export const QuantFormat$Quant4Min$2 = (value) => value.mins;

export class CompressedTensor extends $CustomType {
  constructor(data, shape, format, memory_bytes) {
    super();
    this.data = data;
    this.shape = shape;
    this.format = format;
    this.memory_bytes = memory_bytes;
  }
}
export const CompressedTensor$CompressedTensor = (data, shape, format, memory_bytes) =>
  new CompressedTensor(data, shape, format, memory_bytes);
export const CompressedTensor$isCompressedTensor = (value) =>
  value instanceof CompressedTensor;
export const CompressedTensor$CompressedTensor$data = (value) => value.data;
export const CompressedTensor$CompressedTensor$0 = (value) => value.data;
export const CompressedTensor$CompressedTensor$shape = (value) => value.shape;
export const CompressedTensor$CompressedTensor$1 = (value) => value.shape;
export const CompressedTensor$CompressedTensor$format = (value) => value.format;
export const CompressedTensor$CompressedTensor$2 = (value) => value.format;
export const CompressedTensor$CompressedTensor$memory_bytes = (value) =>
  value.memory_bytes;
export const CompressedTensor$CompressedTensor$3 = (value) =>
  value.memory_bytes;

/**
 * Na VRAM da GPU (rápido)
 */
export class OnGpu extends $CustomType {
  constructor(device_id) {
    super();
    this.device_id = device_id;
  }
}
export const TensorLocation$OnGpu = (device_id) => new OnGpu(device_id);
export const TensorLocation$isOnGpu = (value) => value instanceof OnGpu;
export const TensorLocation$OnGpu$device_id = (value) => value.device_id;
export const TensorLocation$OnGpu$0 = (value) => value.device_id;

export class OnRam extends $CustomType {}
export const TensorLocation$OnRam = () => new OnRam();
export const TensorLocation$isOnRam = (value) => value instanceof OnRam;

/**
 * No disco (lento, mas ilimitado)
 */
export class OnDisk extends $CustomType {
  constructor(path) {
    super();
    this.path = path;
  }
}
export const TensorLocation$OnDisk = (path) => new OnDisk(path);
export const TensorLocation$isOnDisk = (value) => value instanceof OnDisk;
export const TensorLocation$OnDisk$path = (value) => value.path;
export const TensorLocation$OnDisk$0 = (value) => value.path;

/**
 * Híbrido: parte GPU, parte RAM
 */
export class Hybrid extends $CustomType {
  constructor(gpu_pct) {
    super();
    this.gpu_pct = gpu_pct;
  }
}
export const TensorLocation$Hybrid = (gpu_pct) => new Hybrid(gpu_pct);
export const TensorLocation$isHybrid = (value) => value instanceof Hybrid;
export const TensorLocation$Hybrid$gpu_pct = (value) => value.gpu_pct;
export const TensorLocation$Hybrid$0 = (value) => value.gpu_pct;

export class MemoryTier extends $CustomType {
  constructor(location, capacity_gb, used_gb, bandwidth_gbps) {
    super();
    this.location = location;
    this.capacity_gb = capacity_gb;
    this.used_gb = used_gb;
    this.bandwidth_gbps = bandwidth_gbps;
  }
}
export const MemoryTier$MemoryTier = (location, capacity_gb, used_gb, bandwidth_gbps) =>
  new MemoryTier(location, capacity_gb, used_gb, bandwidth_gbps);
export const MemoryTier$isMemoryTier = (value) => value instanceof MemoryTier;
export const MemoryTier$MemoryTier$location = (value) => value.location;
export const MemoryTier$MemoryTier$0 = (value) => value.location;
export const MemoryTier$MemoryTier$capacity_gb = (value) => value.capacity_gb;
export const MemoryTier$MemoryTier$1 = (value) => value.capacity_gb;
export const MemoryTier$MemoryTier$used_gb = (value) => value.used_gb;
export const MemoryTier$MemoryTier$2 = (value) => value.used_gb;
export const MemoryTier$MemoryTier$bandwidth_gbps = (value) =>
  value.bandwidth_gbps;
export const MemoryTier$MemoryTier$3 = (value) => value.bandwidth_gbps;

export class MemoryHierarchy extends $CustomType {
  constructor(gpu, ram, disk, total_effective_gb) {
    super();
    this.gpu = gpu;
    this.ram = ram;
    this.disk = disk;
    this.total_effective_gb = total_effective_gb;
  }
}
export const MemoryHierarchy$MemoryHierarchy = (gpu, ram, disk, total_effective_gb) =>
  new MemoryHierarchy(gpu, ram, disk, total_effective_gb);
export const MemoryHierarchy$isMemoryHierarchy = (value) =>
  value instanceof MemoryHierarchy;
export const MemoryHierarchy$MemoryHierarchy$gpu = (value) => value.gpu;
export const MemoryHierarchy$MemoryHierarchy$0 = (value) => value.gpu;
export const MemoryHierarchy$MemoryHierarchy$ram = (value) => value.ram;
export const MemoryHierarchy$MemoryHierarchy$1 = (value) => value.ram;
export const MemoryHierarchy$MemoryHierarchy$disk = (value) => value.disk;
export const MemoryHierarchy$MemoryHierarchy$2 = (value) => value.disk;
export const MemoryHierarchy$MemoryHierarchy$total_effective_gb = (value) =>
  value.total_effective_gb;
export const MemoryHierarchy$MemoryHierarchy$3 = (value) =>
  value.total_effective_gb;

export class KeepOnGpu extends $CustomType {}
export const OffloadPolicy$KeepOnGpu = () => new KeepOnGpu();
export const OffloadPolicy$isKeepOnGpu = (value) => value instanceof KeepOnGpu;

/**
 * Move para RAM quando GPU > threshold
 */
export class OffloadToRam extends $CustomType {
  constructor(threshold_pct) {
    super();
    this.threshold_pct = threshold_pct;
  }
}
export const OffloadPolicy$OffloadToRam = (threshold_pct) =>
  new OffloadToRam(threshold_pct);
export const OffloadPolicy$isOffloadToRam = (value) =>
  value instanceof OffloadToRam;
export const OffloadPolicy$OffloadToRam$threshold_pct = (value) =>
  value.threshold_pct;
export const OffloadPolicy$OffloadToRam$0 = (value) => value.threshold_pct;

/**
 * Move para disco quando RAM > threshold
 */
export class OffloadToDisk extends $CustomType {
  constructor(ram_threshold, disk_path) {
    super();
    this.ram_threshold = ram_threshold;
    this.disk_path = disk_path;
  }
}
export const OffloadPolicy$OffloadToDisk = (ram_threshold, disk_path) =>
  new OffloadToDisk(ram_threshold, disk_path);
export const OffloadPolicy$isOffloadToDisk = (value) =>
  value instanceof OffloadToDisk;
export const OffloadPolicy$OffloadToDisk$ram_threshold = (value) =>
  value.ram_threshold;
export const OffloadPolicy$OffloadToDisk$0 = (value) => value.ram_threshold;
export const OffloadPolicy$OffloadToDisk$disk_path = (value) => value.disk_path;
export const OffloadPolicy$OffloadToDisk$1 = (value) => value.disk_path;

/**
 * Inteligente: prioriza por frequência de acesso
 */
export class SmartOffload extends $CustomType {
  constructor(access_history) {
    super();
    this.access_history = access_history;
  }
}
export const OffloadPolicy$SmartOffload = (access_history) =>
  new SmartOffload(access_history);
export const OffloadPolicy$isSmartOffload = (value) =>
  value instanceof SmartOffload;
export const OffloadPolicy$SmartOffload$access_history = (value) =>
  value.access_history;
export const OffloadPolicy$SmartOffload$0 = (value) => value.access_history;

export class AccessRecord extends $CustomType {
  constructor(tensor_id, timestamp_ms, access_count) {
    super();
    this.tensor_id = tensor_id;
    this.timestamp_ms = timestamp_ms;
    this.access_count = access_count;
  }
}
export const AccessRecord$AccessRecord = (tensor_id, timestamp_ms, access_count) =>
  new AccessRecord(tensor_id, timestamp_ms, access_count);
export const AccessRecord$isAccessRecord = (value) =>
  value instanceof AccessRecord;
export const AccessRecord$AccessRecord$tensor_id = (value) => value.tensor_id;
export const AccessRecord$AccessRecord$0 = (value) => value.tensor_id;
export const AccessRecord$AccessRecord$timestamp_ms = (value) =>
  value.timestamp_ms;
export const AccessRecord$AccessRecord$1 = (value) => value.timestamp_ms;
export const AccessRecord$AccessRecord$access_count = (value) =>
  value.access_count;
export const AccessRecord$AccessRecord$2 = (value) => value.access_count;

export class Checkpoint extends $CustomType {
  constructor(input, forward_fn_id, memory_saved_gb) {
    super();
    this.input = input;
    this.forward_fn_id = forward_fn_id;
    this.memory_saved_gb = memory_saved_gb;
  }
}
export const Checkpoint$Checkpoint = (input, forward_fn_id, memory_saved_gb) =>
  new Checkpoint(input, forward_fn_id, memory_saved_gb);
export const Checkpoint$isCheckpoint = (value) => value instanceof Checkpoint;
export const Checkpoint$Checkpoint$input = (value) => value.input;
export const Checkpoint$Checkpoint$0 = (value) => value.input;
export const Checkpoint$Checkpoint$forward_fn_id = (value) =>
  value.forward_fn_id;
export const Checkpoint$Checkpoint$1 = (value) => value.forward_fn_id;
export const Checkpoint$Checkpoint$memory_saved_gb = (value) =>
  value.memory_saved_gb;
export const Checkpoint$Checkpoint$2 = (value) => value.memory_saved_gb;

export class NoCheckpoint extends $CustomType {}
export const CheckpointStrategy$NoCheckpoint = () => new NoCheckpoint();
export const CheckpointStrategy$isNoCheckpoint = (value) =>
  value instanceof NoCheckpoint;

/**
 * Checkpoint a cada N camadas
 */
export class EveryN extends $CustomType {
  constructor(n) {
    super();
    this.n = n;
  }
}
export const CheckpointStrategy$EveryN = (n) => new EveryN(n);
export const CheckpointStrategy$isEveryN = (value) => value instanceof EveryN;
export const CheckpointStrategy$EveryN$n = (value) => value.n;
export const CheckpointStrategy$EveryN$0 = (value) => value.n;

/**
 * Checkpoint apenas camadas grandes
 */
export class LargeLayersOnly extends $CustomType {
  constructor(threshold_mb) {
    super();
    this.threshold_mb = threshold_mb;
  }
}
export const CheckpointStrategy$LargeLayersOnly = (threshold_mb) =>
  new LargeLayersOnly(threshold_mb);
export const CheckpointStrategy$isLargeLayersOnly = (value) =>
  value instanceof LargeLayersOnly;
export const CheckpointStrategy$LargeLayersOnly$threshold_mb = (value) =>
  value.threshold_mb;
export const CheckpointStrategy$LargeLayersOnly$0 = (value) =>
  value.threshold_mb;

/**
 * Checkpoint adaptativo baseado em pressão de memória
 */
export class Adaptive extends $CustomType {
  constructor(memory_pressure) {
    super();
    this.memory_pressure = memory_pressure;
  }
}
export const CheckpointStrategy$Adaptive = (memory_pressure) =>
  new Adaptive(memory_pressure);
export const CheckpointStrategy$isAdaptive = (value) =>
  value instanceof Adaptive;
export const CheckpointStrategy$Adaptive$memory_pressure = (value) =>
  value.memory_pressure;
export const CheckpointStrategy$Adaptive$0 = (value) => value.memory_pressure;

export class StreamedTensor extends $CustomType {
  constructor(id, shape, chunk_shape, loaded_chunks, total_chunks, format) {
    super();
    this.id = id;
    this.shape = shape;
    this.chunk_shape = chunk_shape;
    this.loaded_chunks = loaded_chunks;
    this.total_chunks = total_chunks;
    this.format = format;
  }
}
export const StreamedTensor$StreamedTensor = (id, shape, chunk_shape, loaded_chunks, total_chunks, format) =>
  new StreamedTensor(id, shape, chunk_shape, loaded_chunks, total_chunks, format);
export const StreamedTensor$isStreamedTensor = (value) =>
  value instanceof StreamedTensor;
export const StreamedTensor$StreamedTensor$id = (value) => value.id;
export const StreamedTensor$StreamedTensor$0 = (value) => value.id;
export const StreamedTensor$StreamedTensor$shape = (value) => value.shape;
export const StreamedTensor$StreamedTensor$1 = (value) => value.shape;
export const StreamedTensor$StreamedTensor$chunk_shape = (value) =>
  value.chunk_shape;
export const StreamedTensor$StreamedTensor$2 = (value) => value.chunk_shape;
export const StreamedTensor$StreamedTensor$loaded_chunks = (value) =>
  value.loaded_chunks;
export const StreamedTensor$StreamedTensor$3 = (value) => value.loaded_chunks;
export const StreamedTensor$StreamedTensor$total_chunks = (value) =>
  value.total_chunks;
export const StreamedTensor$StreamedTensor$4 = (value) => value.total_chunks;
export const StreamedTensor$StreamedTensor$format = (value) => value.format;
export const StreamedTensor$StreamedTensor$5 = (value) => value.format;

export class MemoryPool extends $CustomType {
  constructor(free_buffers, used_buffers, total_allocated) {
    super();
    this.free_buffers = free_buffers;
    this.used_buffers = used_buffers;
    this.total_allocated = total_allocated;
  }
}
export const MemoryPool$MemoryPool = (free_buffers, used_buffers, total_allocated) =>
  new MemoryPool(free_buffers, used_buffers, total_allocated);
export const MemoryPool$isMemoryPool = (value) => value instanceof MemoryPool;
export const MemoryPool$MemoryPool$free_buffers = (value) => value.free_buffers;
export const MemoryPool$MemoryPool$0 = (value) => value.free_buffers;
export const MemoryPool$MemoryPool$used_buffers = (value) => value.used_buffers;
export const MemoryPool$MemoryPool$1 = (value) => value.used_buffers;
export const MemoryPool$MemoryPool$total_allocated = (value) =>
  value.total_allocated;
export const MemoryPool$MemoryPool$2 = (value) => value.total_allocated;

/**
 * Extrai shape de um tensor
 * 
 * @ignore
 */
function get_shape(t) {
  if (t instanceof Tensor) {
    let shape = t.shape;
    return shape;
  } else {
    let shape = t.shape;
    return shape;
  }
}

/**
 * Cria tensor com shape específico
 * 
 * @ignore
 */
function create_tensor(data, shape) {
  return new Tensor(data, shape);
}

/**
 * Acessa elemento em índice específico
 * 
 * @ignore
 */
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
 * Dequantiza de volta para FP32
 */
export function dequantize(ct) {
  let $ = ct.format;
  if ($ instanceof Fp32) {
    return create_tensor($list.map(ct.data, $int.to_float), ct.shape);
  } else if ($ instanceof Fp16) {
    return create_tensor($list.map(ct.data, $int.to_float), ct.shape);
  } else if ($ instanceof Int8) {
    let scale = $.scale;
    let data = $list.map(
      ct.data,
      (q) => { return divideFloat($int.to_float(q), scale); },
    );
    return create_tensor(data, ct.shape);
  } else if ($ instanceof Quant4) {
    let block_size = $.block_size;
    let scales = $.scales;
    let blocks = $list.sized_chunk(ct.data, block_size);
    let _block;
    let _pipe = $list.index_map(
      blocks,
      (block, idx) => {
        let scale = get_at_index(scales, idx, 1.0);
        return $list.map(
          block,
          (q) => { return divideFloat(($int.to_float(q) - 8.0), scale); },
        );
      },
    );
    _block = $list.flatten(_pipe);
    let data = _block;
    return create_tensor(data, ct.shape);
  } else {
    let block_size = $.block_size;
    let scales = $.scales;
    let mins = $.mins;
    let blocks = $list.sized_chunk(ct.data, block_size);
    let _block;
    let _pipe = $list.index_map(
      blocks,
      (block, idx) => {
        let scale = get_at_index(scales, idx, 1.0);
        let min = get_at_index(mins, idx, 0.0);
        return $list.map(
          block,
          (q) => { return (divideFloat($int.to_float(q), scale)) + min; },
        );
      },
    );
    _block = $list.flatten(_pipe);
    let data = _block;
    return create_tensor(data, ct.shape);
  }
}

/**
 * Cria hierarquia de memória para RTX 4090 + 32GB RAM
 */
export function create_memory_hierarchy(vram_gb, ram_gb, disk_path) {
  let gpu_tier = new MemoryTier(new OnGpu(0), vram_gb, 0.0, 1008.0);
  let ram_tier = new MemoryTier(new OnRam(), ram_gb, 0.0, 51.2);
  let _block;
  if (disk_path instanceof Some) {
    let path = disk_path[0];
    _block = new Some(new MemoryTier(new OnDisk(path), 1000.0, 0.0, 7.0));
  } else {
    _block = disk_path;
  }
  let disk_tier = _block;
  let effective = ((vram_gb * 4.0) + (ram_gb * 4.0));
  return new MemoryHierarchy(gpu_tier, ram_tier, disk_tier, effective);
}

/**
 * Decide onde colocar um tensor
 */
export function allocate_tensor(hierarchy, tensor_size_gb, policy) {
  if (policy instanceof KeepOnGpu) {
    let gpu_free = hierarchy.gpu.capacity_gb - hierarchy.gpu.used_gb;
    let $ = tensor_size_gb <= gpu_free;
    if ($) {
      let _block;
      let _record = hierarchy.gpu;
      _block = new MemoryTier(
        _record.location,
        _record.capacity_gb,
        hierarchy.gpu.used_gb + tensor_size_gb,
        _record.bandwidth_gbps,
      );
      let new_gpu = _block;
      return [
        new OnGpu(0),
        new MemoryHierarchy(
          new_gpu,
          hierarchy.ram,
          hierarchy.disk,
          hierarchy.total_effective_gb,
        ),
      ];
    } else {
      let _block;
      let _record = hierarchy.ram;
      _block = new MemoryTier(
        _record.location,
        _record.capacity_gb,
        hierarchy.ram.used_gb + tensor_size_gb,
        _record.bandwidth_gbps,
      );
      let new_ram = _block;
      return [
        new OnRam(),
        new MemoryHierarchy(
          hierarchy.gpu,
          new_ram,
          hierarchy.disk,
          hierarchy.total_effective_gb,
        ),
      ];
    }
  } else if (policy instanceof OffloadToRam) {
    let threshold = policy.threshold_pct;
    let gpu_usage = divideFloat(
      hierarchy.gpu.used_gb,
      hierarchy.gpu.capacity_gb
    );
    let $ = gpu_usage < threshold;
    if ($) {
      let _block;
      let _record = hierarchy.gpu;
      _block = new MemoryTier(
        _record.location,
        _record.capacity_gb,
        hierarchy.gpu.used_gb + tensor_size_gb,
        _record.bandwidth_gbps,
      );
      let new_gpu = _block;
      return [
        new OnGpu(0),
        new MemoryHierarchy(
          new_gpu,
          hierarchy.ram,
          hierarchy.disk,
          hierarchy.total_effective_gb,
        ),
      ];
    } else {
      let _block;
      let _record = hierarchy.ram;
      _block = new MemoryTier(
        _record.location,
        _record.capacity_gb,
        hierarchy.ram.used_gb + tensor_size_gb,
        _record.bandwidth_gbps,
      );
      let new_ram = _block;
      return [
        new OnRam(),
        new MemoryHierarchy(
          hierarchy.gpu,
          new_ram,
          hierarchy.disk,
          hierarchy.total_effective_gb,
        ),
      ];
    }
  } else if (policy instanceof OffloadToDisk) {
    let ram_threshold = policy.ram_threshold;
    let disk_path = policy.disk_path;
    let gpu_free = hierarchy.gpu.capacity_gb - hierarchy.gpu.used_gb;
    let ram_usage = divideFloat(
      hierarchy.ram.used_gb,
      hierarchy.ram.capacity_gb
    );
    let $ = tensor_size_gb <= gpu_free;
    if ($) {
      let _block;
      let _record = hierarchy.gpu;
      _block = new MemoryTier(
        _record.location,
        _record.capacity_gb,
        hierarchy.gpu.used_gb + tensor_size_gb,
        _record.bandwidth_gbps,
      );
      let new_gpu = _block;
      return [
        new OnGpu(0),
        new MemoryHierarchy(
          new_gpu,
          hierarchy.ram,
          hierarchy.disk,
          hierarchy.total_effective_gb,
        ),
      ];
    } else {
      let $1 = ram_usage < ram_threshold;
      if ($1) {
        let _block;
        let _record = hierarchy.ram;
        _block = new MemoryTier(
          _record.location,
          _record.capacity_gb,
          hierarchy.ram.used_gb + tensor_size_gb,
          _record.bandwidth_gbps,
        );
        let new_ram = _block;
        return [
          new OnRam(),
          new MemoryHierarchy(
            hierarchy.gpu,
            new_ram,
            hierarchy.disk,
            hierarchy.total_effective_gb,
          ),
        ];
      } else {
        return [new OnDisk(disk_path), hierarchy];
      }
    }
  } else {
    return [new OnGpu(0), hierarchy];
  }
}

/**
 * Calcula economia de memória com checkpointing
 */
export function checkpoint_savings(num_layers, layer_size_mb, strategy) {
  let total_mb = $int.to_float(num_layers) * layer_size_mb;
  if (strategy instanceof NoCheckpoint) {
    return 0.0;
  } else if (strategy instanceof EveryN) {
    let n = strategy.n;
    let checkpoint_pct = 1.0 - (divideFloat(1.0, $int.to_float(n)));
    return total_mb * checkpoint_pct;
  } else if (strategy instanceof LargeLayersOnly) {
    let threshold = strategy.threshold_mb;
    let $ = layer_size_mb > threshold;
    if ($) {
      return total_mb * 0.7;
    } else {
      return 0.0;
    }
  } else {
    let pressure = strategy.memory_pressure;
    return total_mb * pressure;
  }
}

/**
 * Carrega um chunk específico
 */
export function load_chunk(st, chunk_idx) {
  let $ = $list.contains(st.loaded_chunks, chunk_idx);
  if ($) {
    return st;
  } else {
    return new StreamedTensor(
      st.id,
      st.shape,
      st.chunk_shape,
      listPrepend(chunk_idx, st.loaded_chunks),
      st.total_chunks,
      st.format,
    );
  }
}

/**
 * Descarrega chunk (libera memória)
 */
export function unload_chunk(st, chunk_idx) {
  return new StreamedTensor(
    st.id,
    st.shape,
    st.chunk_shape,
    $list.filter(st.loaded_chunks, (c) => { return c !== chunk_idx; }),
    st.total_chunks,
    st.format,
  );
}

/**
 * Cria pool de memória
 */
export function create_pool() {
  return new MemoryPool(toList([]), 0, 0);
}

/**
 * Aloca do pool (reutiliza se possível)
 */
export function pool_alloc(pool, size) {
  let found = $list.find(
    pool.free_buffers,
    (b) => {
      let s;
      let count;
      s = b[0];
      count = b[1];
      return (s === size) && (count > 0);
    },
  );
  if (found instanceof Ok) {
    let s = found[0][0];
    let new_buffers = $list.map(
      pool.free_buffers,
      (b) => {
        let bs;
        let bc;
        bs = b[0];
        bc = b[1];
        let $ = bs === s;
        if ($) {
          return [bs, bc - 1];
        } else {
          return b;
        }
      },
    );
    return [
      new MemoryPool(new_buffers, pool.used_buffers + 1, pool.total_allocated),
      true,
    ];
  } else {
    let new_buffers = listPrepend([size, 0], pool.free_buffers);
    return [
      new MemoryPool(
        new_buffers,
        pool.used_buffers + 1,
        pool.total_allocated + size,
      ),
      false,
    ];
  }
}

/**
 * Devolve buffer ao pool
 */
export function pool_free(pool, size) {
  let _block;
  let $ = $list.find(
    pool.free_buffers,
    (b) => {
      let s;
      s = b[0];
      return s === size;
    },
  );
  if ($ instanceof Ok) {
    _block = $list.map(
      pool.free_buffers,
      (b) => {
        let bs;
        let bc;
        bs = b[0];
        bc = b[1];
        let $1 = bs === size;
        if ($1) {
          return [bs, bc + 1];
        } else {
          return b;
        }
      },
    );
  } else {
    _block = listPrepend([size, 1], pool.free_buffers);
  }
  let new_buffers = _block;
  return new MemoryPool(
    new_buffers,
    pool.used_buffers - 1,
    pool.total_allocated,
  );
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

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}

function location_to_string(loc) {
  if (loc instanceof OnGpu) {
    let id = loc.device_id;
    return "GPU #" + $int.to_string(id);
  } else if (loc instanceof OnRam) {
    return "RAM";
  } else if (loc instanceof OnDisk) {
    let path = loc.path;
    return ("Disk(" + path) + ")";
  } else {
    let pct = loc.gpu_pct;
    return ("Hybrid(" + float_to_string(pct * 100.0)) + "% GPU)";
  }
}
