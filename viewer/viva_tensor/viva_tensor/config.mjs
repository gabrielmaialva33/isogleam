import { Empty as $Empty, CustomType as $CustomType } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { StridedTensor, Tensor } from "../viva_tensor/tensor.mjs";

export class Sequential extends $CustomType {}
export const OperationType$Sequential = () => new Sequential();
export const OperationType$isSequential = (value) =>
  value instanceof Sequential;

export class RandomAccess extends $CustomType {}
export const OperationType$RandomAccess = () => new RandomAccess();
export const OperationType$isRandomAccess = (value) =>
  value instanceof RandomAccess;

export class MatrixOp extends $CustomType {}
export const OperationType$MatrixOp = () => new MatrixOp();
export const OperationType$isMatrixOp = (value) => value instanceof MatrixOp;

export class TensorConfig extends $CustomType {
  constructor(strided_threshold_random, strided_threshold_matmul, force_strided, force_list) {
    super();
    this.strided_threshold_random = strided_threshold_random;
    this.strided_threshold_matmul = strided_threshold_matmul;
    this.force_strided = force_strided;
    this.force_list = force_list;
  }
}
export const TensorConfig$TensorConfig = (strided_threshold_random, strided_threshold_matmul, force_strided, force_list) =>
  new TensorConfig(strided_threshold_random,
  strided_threshold_matmul,
  force_strided,
  force_list);
export const TensorConfig$isTensorConfig = (value) =>
  value instanceof TensorConfig;
export const TensorConfig$TensorConfig$strided_threshold_random = (value) =>
  value.strided_threshold_random;
export const TensorConfig$TensorConfig$0 = (value) =>
  value.strided_threshold_random;
export const TensorConfig$TensorConfig$strided_threshold_matmul = (value) =>
  value.strided_threshold_matmul;
export const TensorConfig$TensorConfig$1 = (value) =>
  value.strided_threshold_matmul;
export const TensorConfig$TensorConfig$force_strided = (value) =>
  value.force_strided;
export const TensorConfig$TensorConfig$2 = (value) => value.force_strided;
export const TensorConfig$TensorConfig$force_list = (value) => value.force_list;
export const TensorConfig$TensorConfig$3 = (value) => value.force_list;

/**
 * Default configuration based on benchmarks
 */
export function default_config() {
  return new TensorConfig(500, 64, false, false);
}

/**
 * High-performance config (prefer strided for large tensors)
 */
export function performance_config() {
  return new TensorConfig(100, 32, false, false);
}

/**
 * Memory-efficient config (prefer lists)
 */
export function memory_config() {
  return new TensorConfig(5000, 256, false, false);
}

/**
 * GPU-optimized config (always strided for batched ops)
 */
export function gpu_config() {
  return new TensorConfig(64, 16, false, false);
}

/**
 * Get shape from tensor (helper for pattern matching)
 * 
 * @ignore
 */
function get_tensor_shape(t) {
  if (t instanceof Tensor) {
    let shape = t.shape;
    return shape;
  } else {
    let shape = t.shape;
    return shape;
  }
}

/**
 * Check if should use strided backend for given operation
 */
export function should_use_strided(t, op, config) {
  let $ = config.force_strided;
  let $1 = config.force_list;
  if ($) {
    return $;
  } else if ($1) {
    return false;
  } else {
    let tensor_size = $tensor.size(t);
    if (op instanceof Sequential) {
      return false;
    } else if (op instanceof RandomAccess) {
      return tensor_size >= config.strided_threshold_random;
    } else {
      let shape = get_tensor_shape(t);
      if (shape instanceof $Empty) {
        return tensor_size >= config.strided_threshold_matmul;
      } else {
        let $2 = shape.tail;
        if ($2 instanceof $Empty) {
          return tensor_size >= config.strided_threshold_matmul;
        } else {
          let $3 = $2.tail;
          if ($3 instanceof $Empty) {
            let rows = shape.head;
            let cols = $2.head;
            return rows * cols >= config.strided_threshold_matmul;
          } else {
            return tensor_size >= config.strided_threshold_matmul;
          }
        }
      }
    }
  }
}
