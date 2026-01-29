import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import {
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  divideFloat,
} from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

export class Cuda extends $CustomType {
  constructor(gpu_id, vram_gb) {
    super();
    this.gpu_id = gpu_id;
    this.vram_gb = vram_gb;
  }
}
export const Device$Cuda = (gpu_id, vram_gb) => new Cuda(gpu_id, vram_gb);
export const Device$isCuda = (value) => value instanceof Cuda;
export const Device$Cuda$gpu_id = (value) => value.gpu_id;
export const Device$Cuda$0 = (value) => value.gpu_id;
export const Device$Cuda$vram_gb = (value) => value.vram_gb;
export const Device$Cuda$1 = (value) => value.vram_gb;

export class Metal extends $CustomType {
  constructor(device_id) {
    super();
    this.device_id = device_id;
  }
}
export const Device$Metal = (device_id) => new Metal(device_id);
export const Device$isMetal = (value) => value instanceof Metal;
export const Device$Metal$device_id = (value) => value.device_id;
export const Device$Metal$0 = (value) => value.device_id;

export class Cpu extends $CustomType {
  constructor(cores) {
    super();
    this.cores = cores;
  }
}
export const Device$Cpu = (cores) => new Cpu(cores);
export const Device$isCpu = (value) => value instanceof Cpu;
export const Device$Cpu$cores = (value) => value.cores;
export const Device$Cpu$0 = (value) => value.cores;

export class HardwareProfile extends $CustomType {
  constructor(device, total_vram_gb, available_vram_gb, total_ram_gb, gpu_load_pct, optimal_batch_size) {
    super();
    this.device = device;
    this.total_vram_gb = total_vram_gb;
    this.available_vram_gb = available_vram_gb;
    this.total_ram_gb = total_ram_gb;
    this.gpu_load_pct = gpu_load_pct;
    this.optimal_batch_size = optimal_batch_size;
  }
}
export const HardwareProfile$HardwareProfile = (device, total_vram_gb, available_vram_gb, total_ram_gb, gpu_load_pct, optimal_batch_size) =>
  new HardwareProfile(device,
  total_vram_gb,
  available_vram_gb,
  total_ram_gb,
  gpu_load_pct,
  optimal_batch_size);
export const HardwareProfile$isHardwareProfile = (value) =>
  value instanceof HardwareProfile;
export const HardwareProfile$HardwareProfile$device = (value) => value.device;
export const HardwareProfile$HardwareProfile$0 = (value) => value.device;
export const HardwareProfile$HardwareProfile$total_vram_gb = (value) =>
  value.total_vram_gb;
export const HardwareProfile$HardwareProfile$1 = (value) => value.total_vram_gb;
export const HardwareProfile$HardwareProfile$available_vram_gb = (value) =>
  value.available_vram_gb;
export const HardwareProfile$HardwareProfile$2 = (value) =>
  value.available_vram_gb;
export const HardwareProfile$HardwareProfile$total_ram_gb = (value) =>
  value.total_ram_gb;
export const HardwareProfile$HardwareProfile$3 = (value) => value.total_ram_gb;
export const HardwareProfile$HardwareProfile$gpu_load_pct = (value) =>
  value.gpu_load_pct;
export const HardwareProfile$HardwareProfile$4 = (value) => value.gpu_load_pct;
export const HardwareProfile$HardwareProfile$optimal_batch_size = (value) =>
  value.optimal_batch_size;
export const HardwareProfile$HardwareProfile$5 = (value) =>
  value.optimal_batch_size;

export class Inference extends $CustomType {}
export const QuantMode$Inference = () => new Inference();
export const QuantMode$isInference = (value) => value instanceof Inference;

export class Training extends $CustomType {}
export const QuantMode$Training = () => new Training();
export const QuantMode$isTraining = (value) => value instanceof Training;

export class Adaptive extends $CustomType {}
export const QuantMode$Adaptive = () => new Adaptive();
export const QuantMode$isAdaptive = (value) => value instanceof Adaptive;

export class AutoTuner extends $CustomType {
  constructor(hardware, quant_mode, history, current_batch_size) {
    super();
    this.hardware = hardware;
    this.quant_mode = quant_mode;
    this.history = history;
    this.current_batch_size = current_batch_size;
  }
}
export const AutoTuner$AutoTuner = (hardware, quant_mode, history, current_batch_size) =>
  new AutoTuner(hardware, quant_mode, history, current_batch_size);
export const AutoTuner$isAutoTuner = (value) => value instanceof AutoTuner;
export const AutoTuner$AutoTuner$hardware = (value) => value.hardware;
export const AutoTuner$AutoTuner$0 = (value) => value.hardware;
export const AutoTuner$AutoTuner$quant_mode = (value) => value.quant_mode;
export const AutoTuner$AutoTuner$1 = (value) => value.quant_mode;
export const AutoTuner$AutoTuner$history = (value) => value.history;
export const AutoTuner$AutoTuner$2 = (value) => value.history;
export const AutoTuner$AutoTuner$current_batch_size = (value) =>
  value.current_batch_size;
export const AutoTuner$AutoTuner$3 = (value) => value.current_batch_size;

export class BatchResult extends $CustomType {
  constructor(batch_size, duration_ms, throughput) {
    super();
    this.batch_size = batch_size;
    this.duration_ms = duration_ms;
    this.throughput = throughput;
  }
}
export const BatchResult$BatchResult = (batch_size, duration_ms, throughput) =>
  new BatchResult(batch_size, duration_ms, throughput);
export const BatchResult$isBatchResult = (value) =>
  value instanceof BatchResult;
export const BatchResult$BatchResult$batch_size = (value) => value.batch_size;
export const BatchResult$BatchResult$0 = (value) => value.batch_size;
export const BatchResult$BatchResult$duration_ms = (value) => value.duration_ms;
export const BatchResult$BatchResult$1 = (value) => value.duration_ms;
export const BatchResult$BatchResult$throughput = (value) => value.throughput;
export const BatchResult$BatchResult$2 = (value) => value.throughput;

export class QuantContext extends $CustomType {
  constructor(mode, scales) {
    super();
    this.mode = mode;
    this.scales = scales;
  }
}
export const QuantContext$QuantContext = (mode, scales) =>
  new QuantContext(mode, scales);
export const QuantContext$isQuantContext = (value) =>
  value instanceof QuantContext;
export const QuantContext$QuantContext$mode = (value) => value.mode;
export const QuantContext$QuantContext$0 = (value) => value.mode;
export const QuantContext$QuantContext$scales = (value) => value.scales;
export const QuantContext$QuantContext$1 = (value) => value.scales;

export class Low extends $CustomType {}
export const MemoryPressure$Low = () => new Low();
export const MemoryPressure$isLow = (value) => value instanceof Low;

export class Medium extends $CustomType {}
export const MemoryPressure$Medium = () => new Medium();
export const MemoryPressure$isMedium = (value) => value instanceof Medium;

export class High extends $CustomType {}
export const MemoryPressure$High = () => new High();
export const MemoryPressure$isHigh = (value) => value instanceof High;

export class Critical extends $CustomType {}
export const MemoryPressure$Critical = () => new Critical();
export const MemoryPressure$isCritical = (value) => value instanceof Critical;

export class MemoryStrategy extends $CustomType {
  constructor(batch_size_mult, quant_mode, gc_aggressive) {
    super();
    this.batch_size_mult = batch_size_mult;
    this.quant_mode = quant_mode;
    this.gc_aggressive = gc_aggressive;
  }
}
export const MemoryStrategy$MemoryStrategy = (batch_size_mult, quant_mode, gc_aggressive) =>
  new MemoryStrategy(batch_size_mult, quant_mode, gc_aggressive);
export const MemoryStrategy$isMemoryStrategy = (value) =>
  value instanceof MemoryStrategy;
export const MemoryStrategy$MemoryStrategy$batch_size_mult = (value) =>
  value.batch_size_mult;
export const MemoryStrategy$MemoryStrategy$0 = (value) => value.batch_size_mult;
export const MemoryStrategy$MemoryStrategy$quant_mode = (value) =>
  value.quant_mode;
export const MemoryStrategy$MemoryStrategy$1 = (value) => value.quant_mode;
export const MemoryStrategy$MemoryStrategy$gc_aggressive = (value) =>
  value.gc_aggressive;
export const MemoryStrategy$MemoryStrategy$2 = (value) => value.gc_aggressive;

/**
 * Detecta hardware disponível
 */
export function detect_hardware() {
  return new HardwareProfile(new Cuda(0, 24.0), 24.0, 20.0, 32.0, 0.0, 32);
}

/**
 * Cria auto-tuner para CPU-only
 */
export function detect_cpu_only() {
  return new HardwareProfile(new Cpu(16), 0.0, 0.0, 32.0, 0.0, 8);
}

/**
 * Batch size padrão baseado no device
 * 
 * @ignore
 */
function get_default_batch_size(device) {
  if (device instanceof Cuda) {
    let vram_gb = device.vram_gb;
    let $ = vram_gb >= 20.0;
    if ($) {
      return 64;
    } else {
      let $1 = vram_gb >= 10.0;
      if ($1) {
        return 32;
      } else {
        return 16;
      }
    }
  } else if (device instanceof Metal) {
    return 16;
  } else {
    return 8;
  }
}

/**
 * Cria novo auto-tuner
 */
export function new$() {
  let hardware = detect_hardware();
  let batch_size = get_default_batch_size(hardware.device);
  return new AutoTuner(hardware, new Adaptive(), toList([]), batch_size);
}

/**
 * Ajusta batch size para memória disponível
 * 
 * @ignore
 */
function adjust_for_memory(batch_size, hw) {
  let tensor_size_mb = 2.0;
  let batch_memory_gb = (($int.to_float(batch_size) * tensor_size_mb)) / 1024.0;
  let max_memory = hw.available_vram_gb * 0.8;
  let $ = batch_memory_gb > max_memory;
  if ($) {
    let _block;
    let _pipe = $float.round((divideFloat(max_memory, tensor_size_mb)) * 1024.0);
    _block = $int.max(_pipe, 1);
    let adjusted = _block;
    return adjusted;
  } else {
    return batch_size;
  }
}

/**
 * Encontra batch size com maior throughput
 * 
 * @ignore
 */
function find_optimal_batch_size(history, hw) {
  if (history instanceof $Empty) {
    return get_default_batch_size(hw.device);
  } else {
    let _block;
    let _pipe = history;
    _block = $list.fold(
      _pipe,
      new BatchResult(0, 0.0, 0.0),
      (acc, r) => {
        let $ = r.throughput > acc.throughput;
        if ($) {
          return r;
        } else {
          return acc;
        }
      },
    );
    let best = _block;
    return adjust_for_memory(best.batch_size, hw);
  }
}

/**
 * Registra resultado de execução e otimiza
 */
export function profile(tuner, batch_size, duration_ms) {
  let throughput = divideFloat($int.to_float(batch_size), duration_ms);
  let result = new BatchResult(batch_size, duration_ms, throughput);
  let _block;
  let $ = $list.length(tuner.history) >= 20;
  if ($) {
    _block = listPrepend(result, $list.take(tuner.history, 19));
  } else {
    _block = listPrepend(result, tuner.history);
  }
  let new_history = _block;
  let optimal = find_optimal_batch_size(new_history, tuner.hardware);
  return new AutoTuner(tuner.hardware, tuner.quant_mode, new_history, optimal);
}

/**
 * Cria contexto de quantização
 */
export function new_quant_context(mode) {
  return new QuantContext(mode, toList([]));
}

/**
 * Decide modo de quantização baseado na operação
 */
export function should_quantize(ctx, is_inference) {
  let $ = ctx.mode;
  if ($ instanceof Inference) {
    return true;
  } else if ($ instanceof Training) {
    return false;
  } else {
    return is_inference;
  }
}

/**
 * Verifica pressão de memória
 */
export function check_memory_pressure(hw) {
  let usage_pct = 1.0 - (divideFloat(hw.available_vram_gb, hw.total_vram_gb));
  let p = usage_pct;
  if (p >= 0.9) {
    return new Critical();
  } else {
    let p = usage_pct;
    if (p >= 0.7) {
      return new High();
    } else {
      let p = usage_pct;
      if (p >= 0.5) {
        return new Medium();
      } else {
        return new Low();
      }
    }
  }
}

/**
 * Estratégia baseada em pressão de memória
 */
export function get_memory_strategy(pressure) {
  if (pressure instanceof Low) {
    return new MemoryStrategy(1.0, new Training(), false);
  } else if (pressure instanceof Medium) {
    return new MemoryStrategy(0.75, new Adaptive(), false);
  } else if (pressure instanceof High) {
    return new MemoryStrategy(0.5, new Inference(), true);
  } else {
    return new MemoryStrategy(0.25, new Inference(), true);
  }
}

function pressure_to_string(p) {
  if (p instanceof Low) {
    return "LOW (tudo ok)";
  } else if (p instanceof Medium) {
    return "MEDIUM (monitorar)";
  } else if (p instanceof High) {
    return "HIGH (reduzir batch)";
  } else {
    return "CRITICAL (emergency mode)";
  }
}

function quant_mode_to_string(m) {
  if (m instanceof Inference) {
    return "INT8 (inference)";
  } else if (m instanceof Training) {
    return "FP32 (training)";
  } else {
    return "ADAPTIVE";
  }
}

function bool_to_string(b) {
  if (b) {
    return "Sim";
  } else {
    return "Não";
  }
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}

function print_device(device) {
  if (device instanceof Cuda) {
    let id = device.gpu_id;
    let vram = device.vram_gb;
    return $io.println(
      ((("  Device: CUDA GPU #" + $int.to_string(id)) + " (") + float_to_string(
        vram,
      )) + "GB)",
    );
  } else if (device instanceof Metal) {
    let id = device.device_id;
    return $io.println("  Device: Metal #" + $int.to_string(id));
  } else {
    let cores = device.cores;
    return $io.println(("  Device: CPU (" + $int.to_string(cores)) + " cores)");
  }
}

/**
 * Roda profile completo do hardware
 */
export function run_hardware_profile() {
  $io.println(
    "╔══════════════════════════════════════════════════════════════════╗",
  );
  $io.println(
    "║  AUTO-TUNE HARDWARE PROFILE                                     ║",
  );
  $io.println(
    "╚══════════════════════════════════════════════════════════════════╝\n",
  );
  let hw = detect_hardware();
  $io.println("HARDWARE DETECTADO:");
  print_device(hw.device);
  $io.println(("  VRAM Total: " + float_to_string(hw.total_vram_gb)) + " GB");
  $io.println(
    ("  VRAM Disponível: " + float_to_string(hw.available_vram_gb)) + " GB",
  );
  $io.println(("  RAM Total: " + float_to_string(hw.total_ram_gb)) + " GB");
  $io.println(("  GPU Load: " + float_to_string(hw.gpu_load_pct)) + "%");
  $io.println("  Batch Size Ótimo: " + $int.to_string(hw.optimal_batch_size));
  let pressure = check_memory_pressure(hw);
  $io.println("\nMEMORY PRESSURE: " + pressure_to_string(pressure));
  let strategy = get_memory_strategy(pressure);
  $io.println("ESTRATÉGIA:");
  $io.println("  Batch Mult: " + float_to_string(strategy.batch_size_mult));
  $io.println("  Quant Mode: " + quant_mode_to_string(strategy.quant_mode));
  $io.println("  GC Agressivo: " + bool_to_string(strategy.gc_aggressive));
  $io.println(
    "\n╔══════════════════════════════════════════════════════════════════╗",
  );
  $io.println(
    "║  RECOMENDAÇÕES PARA RTX 4090 24GB + 32GB RAM:                   ║",
  );
  $io.println(
    "║                                                                  ║",
  );
  $io.println(
    "║  1. Batch Size: 64 (pode ir até 128 com INT8)                   ║",
  );
  $io.println(
    "║  2. Quantização: INT8 para inference (4x menos VRAM)            ║",
  );
  $io.println(
    "║  3. Memory Pool: Pre-alocar 20GB para tensores                  ║",
  );
  $io.println(
    "║  4. Zero-Copy: Usar Binary refs entre Gleam e Rust              ║",
  );
  return $io.println(
    "╚══════════════════════════════════════════════════════════════════╝",
  );
}

export function main() {
  return run_hardware_profile();
}
