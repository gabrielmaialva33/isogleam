import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  remainderInt,
  divideInt,
  isEqual,
} from "../gleam.mjs";

export class Tensor extends $CustomType {
  constructor(data, shape) {
    super();
    this.data = data;
    this.shape = shape;
  }
}
export const Tensor$Tensor = (data, shape) => new Tensor(data, shape);
export const Tensor$isTensor = (value) => value instanceof Tensor;
export const Tensor$Tensor$data = (value) => value.data;
export const Tensor$Tensor$0 = (value) => value.data;
export const Tensor$Tensor$shape = (value) => value.shape;
export const Tensor$Tensor$1 = (value) => value.shape;

export class StridedTensor extends $CustomType {
  constructor(storage, shape, strides, offset) {
    super();
    this.storage = storage;
    this.shape = shape;
    this.strides = strides;
    this.offset = offset;
  }
}
export const Tensor$StridedTensor = (storage, shape, strides, offset) =>
  new StridedTensor(storage, shape, strides, offset);
export const Tensor$isStridedTensor = (value) => value instanceof StridedTensor;
export const Tensor$StridedTensor$storage = (value) => value.storage;
export const Tensor$StridedTensor$0 = (value) => value.storage;
export const Tensor$StridedTensor$shape = (value) => value.shape;
export const Tensor$StridedTensor$1 = (value) => value.shape;
export const Tensor$StridedTensor$strides = (value) => value.strides;
export const Tensor$StridedTensor$2 = (value) => value.strides;
export const Tensor$StridedTensor$offset = (value) => value.offset;
export const Tensor$StridedTensor$3 = (value) => value.offset;

export const Tensor$shape = (value) => value.shape;

export class ShapeMismatch extends $CustomType {
  constructor(expected, got) {
    super();
    this.expected = expected;
    this.got = got;
  }
}
export const TensorError$ShapeMismatch = (expected, got) =>
  new ShapeMismatch(expected, got);
export const TensorError$isShapeMismatch = (value) =>
  value instanceof ShapeMismatch;
export const TensorError$ShapeMismatch$expected = (value) => value.expected;
export const TensorError$ShapeMismatch$0 = (value) => value.expected;
export const TensorError$ShapeMismatch$got = (value) => value.got;
export const TensorError$ShapeMismatch$1 = (value) => value.got;

export class InvalidShape extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const TensorError$InvalidShape = (reason) => new InvalidShape(reason);
export const TensorError$isInvalidShape = (value) =>
  value instanceof InvalidShape;
export const TensorError$InvalidShape$reason = (value) => value.reason;
export const TensorError$InvalidShape$0 = (value) => value.reason;

export class DimensionError extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const TensorError$DimensionError = (reason) =>
  new DimensionError(reason);
export const TensorError$isDimensionError = (value) =>
  value instanceof DimensionError;
export const TensorError$DimensionError$reason = (value) => value.reason;
export const TensorError$DimensionError$0 = (value) => value.reason;

export class BroadcastError extends $CustomType {
  constructor(a, b) {
    super();
    this.a = a;
    this.b = b;
  }
}
export const TensorError$BroadcastError = (a, b) => new BroadcastError(a, b);
export const TensorError$isBroadcastError = (value) =>
  value instanceof BroadcastError;
export const TensorError$BroadcastError$a = (value) => value.a;
export const TensorError$BroadcastError$0 = (value) => value.a;
export const TensorError$BroadcastError$b = (value) => value.b;
export const TensorError$BroadcastError$1 = (value) => value.b;

export class Conv2dConfig extends $CustomType {
  constructor(kernel_h, kernel_w, stride_h, stride_w, padding_h, padding_w) {
    super();
    this.kernel_h = kernel_h;
    this.kernel_w = kernel_w;
    this.stride_h = stride_h;
    this.stride_w = stride_w;
    this.padding_h = padding_h;
    this.padding_w = padding_w;
  }
}
export const Conv2dConfig$Conv2dConfig = (kernel_h, kernel_w, stride_h, stride_w, padding_h, padding_w) =>
  new Conv2dConfig(kernel_h, kernel_w, stride_h, stride_w, padding_h, padding_w);
export const Conv2dConfig$isConv2dConfig = (value) =>
  value instanceof Conv2dConfig;
export const Conv2dConfig$Conv2dConfig$kernel_h = (value) => value.kernel_h;
export const Conv2dConfig$Conv2dConfig$0 = (value) => value.kernel_h;
export const Conv2dConfig$Conv2dConfig$kernel_w = (value) => value.kernel_w;
export const Conv2dConfig$Conv2dConfig$1 = (value) => value.kernel_w;
export const Conv2dConfig$Conv2dConfig$stride_h = (value) => value.stride_h;
export const Conv2dConfig$Conv2dConfig$2 = (value) => value.stride_h;
export const Conv2dConfig$Conv2dConfig$stride_w = (value) => value.stride_w;
export const Conv2dConfig$Conv2dConfig$3 = (value) => value.stride_w;
export const Conv2dConfig$Conv2dConfig$padding_h = (value) => value.padding_h;
export const Conv2dConfig$Conv2dConfig$4 = (value) => value.padding_h;
export const Conv2dConfig$Conv2dConfig$padding_w = (value) => value.padding_w;
export const Conv2dConfig$Conv2dConfig$5 = (value) => value.padding_w;

/**
 * Create tensor of zeros
 */
export function zeros(shape) {
  let size$1 = $list.fold(shape, 1, (acc, dim) => { return acc * dim; });
  return new Tensor($list.repeat(0.0, size$1), shape);
}

/**
 * Create tensor of ones
 */
export function ones(shape) {
  let size$1 = $list.fold(shape, 1, (acc, dim) => { return acc * dim; });
  return new Tensor($list.repeat(1.0, size$1), shape);
}

/**
 * Create tensor filled with value
 */
export function fill(shape, value) {
  let size$1 = $list.fold(shape, 1, (acc, dim) => { return acc * dim; });
  return new Tensor($list.repeat(value, size$1), shape);
}

/**
 * Create tensor from list (1D)
 */
export function from_list(data) {
  return new Tensor(data, toList([$list.length(data)]));
}

/**
 * Create 2D tensor (matrix) from list of lists
 */
export function from_list2d(rows) {
  if (rows instanceof $Empty) {
    return new Ok(new Tensor(toList([]), toList([0, 0])));
  } else {
    let first = rows.head;
    let rest = rows.tail;
    let cols$1 = $list.length(first);
    let valid = $list.all(
      rest,
      (row) => { return $list.length(row) === cols$1; },
    );
    if (valid) {
      let data = $list.flatten(rows);
      let num_rows = $list.length(rows);
      return new Ok(new Tensor(data, toList([num_rows, cols$1])));
    } else {
      return new Error(new InvalidShape("Rows have different lengths"));
    }
  }
}

/**
 * Create vector (1D tensor)
 */
export function vector(data) {
  return from_list(data);
}

/**
 * Create matrix (2D tensor) with explicit dimensions
 */
export function matrix(rows, cols, data) {
  let expected_size = rows * cols;
  let actual_size = $list.length(data);
  let $ = expected_size === actual_size;
  if ($) {
    return new Ok(new Tensor(data, toList([rows, cols])));
  } else {
    return new Error(
      new InvalidShape(
        (("Expected " + $int.to_string(expected_size)) + " elements, got ") + $int.to_string(
          actual_size,
        ),
      ),
    );
  }
}

/**
 * Get tensor shape
 */
export function shape(t) {
  if (t instanceof Tensor) {
    let s = t.shape;
    return s;
  } else {
    let s = t.shape;
    return s;
  }
}

/**
 * Total number of elements
 */
export function size(t) {
  if (t instanceof Tensor) {
    let data = t.data;
    return $list.length(data);
  } else {
    let shape$1 = t.shape;
    return $list.fold(shape$1, 1, (acc, dim) => { return acc * dim; });
  }
}

/**
 * Number of dimensions (rank)
 */
export function rank(t) {
  return $list.length(t.shape);
}

/**
 * Return number of rows (for matrices)
 */
export function rows(t) {
  let $ = t.shape;
  if ($ instanceof $Empty) {
    return 0;
  } else {
    let r = $.head;
    return r;
  }
}

/**
 * Return number of columns (for matrices)
 */
export function cols(t) {
  let $ = t.shape;
  if ($ instanceof $Empty) {
    return 0;
  } else {
    let $1 = $.tail;
    if ($1 instanceof $Empty) {
      let n = $.head;
      return n;
    } else {
      let c = $1.head;
      return c;
    }
  }
}

/**
 * Remove element at index from list
 * 
 * @ignore
 */
function remove_at_index(lst, idx) {
  let _pipe = lst;
  let _pipe$1 = $list.index_map(_pipe, (item, i) => { return [item, i]; });
  let _pipe$2 = $list.filter(_pipe$1, (pair) => { return pair[1] !== idx; });
  return $list.map(_pipe$2, (pair) => { return pair[0]; });
}

/**
 * Check if two shapes can be broadcast together
 */
export function can_broadcast(a, b) {
  let _block;
  let $1 = $list.length(a) >= $list.length(b);
  if ($1) {
    _block = [a, b];
  } else {
    _block = [b, a];
  }
  let $ = _block;
  let longer;
  let shorter;
  longer = $[0];
  shorter = $[1];
  let diff = $list.length(longer) - $list.length(shorter);
  let padded = $list.append($list.repeat(1, diff), shorter);
  let _pipe = $list.zip(longer, padded);
  return $list.all(
    _pipe,
    (pair) => {
      let dim_a;
      let dim_b;
      dim_a = pair[0];
      dim_b = pair[1];
      return ((dim_a === dim_b) || (dim_a === 1)) || (dim_b === 1);
    },
  );
}

/**
 * Compute broadcast shape
 */
export function broadcast_shape(a, b) {
  let $ = can_broadcast(a, b);
  if ($) {
    let max_rank = $int.max($list.length(a), $list.length(b));
    let diff_a = max_rank - $list.length(a);
    let diff_b = max_rank - $list.length(b);
    let padded_a = $list.append($list.repeat(1, diff_a), a);
    let padded_b = $list.append($list.repeat(1, diff_b), b);
    let _block;
    let _pipe = $list.zip(padded_a, padded_b);
    _block = $list.map(
      _pipe,
      (pair) => {
        let dim_a;
        let dim_b;
        dim_a = pair[0];
        dim_b = pair[1];
        return $int.max(dim_a, dim_b);
      },
    );
    let result_shape = _block;
    return new Ok(result_shape);
  } else {
    return new Error(new BroadcastError(a, b));
  }
}

function list_at_int(lst, index) {
  let $ = index < 0;
  if ($) {
    return new Error(undefined);
  } else {
    let _pipe = lst;
    let _pipe$1 = $list.drop(_pipe, index);
    return $list.first(_pipe$1);
  }
}

/**
 * Specific dimension
 */
export function dim(t, axis) {
  let _pipe = list_at_int(t.shape, axis);
  return $result.map_error(
    _pipe,
    (_) => {
      return new DimensionError(
        ("Axis " + $int.to_string(axis)) + " out of bounds",
      );
    },
  );
}

function list_at_float(lst, index) {
  let $ = index < 0;
  if ($) {
    return new Error(undefined);
  } else {
    let _pipe = lst;
    let _pipe$1 = $list.drop(_pipe, index);
    return $list.first(_pipe$1);
  }
}

function flat_to_multi(flat, shape) {
  let reversed = $list.reverse(shape);
  let $ = $list.fold(
    reversed,
    [toList([]), flat],
    (acc, dim) => {
      let idxs;
      let remaining;
      idxs = acc[0];
      remaining = acc[1];
      let idx = remainderInt(remaining, dim);
      let next = divideInt(remaining, dim);
      return [listPrepend(idx, idxs), next];
    },
  );
  let indices;
  indices = $[0];
  return indices;
}

function compute_strides(shape) {
  let reversed = $list.reverse(shape);
  let $ = $list.fold(
    reversed,
    [toList([]), 1],
    (acc, dim) => {
      let s;
      let running;
      s = acc[0];
      running = acc[1];
      return [listPrepend(running, s), running * dim];
    },
  );
  let strides;
  strides = $[0];
  return strides;
}

/**
 * Compute flat index when summing along an axis
 * 
 * @ignore
 */
function compute_index_with_axis(shape, out_idx, axis_idx, axis_pos) {
  let strides = compute_strides(shape);
  let $ = $list.length(shape);
  
  let shape_without_axis = remove_at_index(shape, axis_idx);
  let strides_without_axis = compute_strides(shape_without_axis);
  let _block;
  let _pipe = $list.range(0, $list.length(shape_without_axis) - 1);
  _block = $list.map(
    _pipe,
    (i) => {
      let _block$1;
      let $1 = (() => {
        let _pipe$1 = $list.drop(strides_without_axis, i);
        return $list.first(_pipe$1);
      })();
      if ($1 instanceof Ok) {
        let s = $1[0];
        _block$1 = s;
      } else {
        _block$1 = 1;
      }
      let stride = _block$1;
      return remainderInt(
        (divideInt(out_idx, stride)),
        (() => {
          let $2 = (() => {
            let _pipe$1 = $list.drop(shape_without_axis, i);
            return $list.first(_pipe$1);
          })();
          if ($2 instanceof Ok) {
            let d = $2[0];
            return d;
          } else {
            return 1;
          }
        })()
      );
    },
  );
  let out_coords = _block;
  let $1 = $list.split(out_coords, axis_idx);
  let before;
  let after;
  before = $1[0];
  after = $1[1];
  let full_coords = $list.flatten(toList([before, toList([axis_pos]), after]));
  let _pipe$1 = $list.zip(full_coords, strides);
  return $list.fold(
    _pipe$1,
    0,
    (acc, pair) => {
      let coord;
      let stride;
      coord = pair[0];
      stride = pair[1];
      return acc + coord * stride;
    },
  );
}

/**
 * Check if tensor is contiguous in memory
 */
export function is_contiguous(t) {
  if (t instanceof Tensor) {
    return true;
  } else {
    let shape$1 = t.shape;
    let strides = t.strides;
    let expected_strides = compute_strides(shape$1);
    return isEqual(strides, expected_strides);
  }
}

function multi_to_flat(indices, shape) {
  let strides = compute_strides(shape);
  let _pipe = $list.zip(indices, strides);
  return $list.fold(
    _pipe,
    0,
    (acc, pair) => {
      let idx;
      let stride;
      idx = pair[0];
      stride = pair[1];
      return acc + idx * stride;
    },
  );
}

/**
 * Default conv2d config (3x3 kernel, stride 1, no padding)
 */
export function conv2d_config() {
  return new Conv2dConfig(3, 3, 1, 1, 0, 0);
}

/**
 * Conv2d config with "same" padding (output same size as input)
 */
export function conv2d_same(kernel_h, kernel_w) {
  return new Conv2dConfig(
    kernel_h,
    kernel_w,
    1,
    1,
    globalThis.Math.trunc(kernel_h / 2),
    globalThis.Math.trunc(kernel_w / 2),
  );
}

function string_join(strings, sep) {
  if (strings instanceof $Empty) {
    return "";
  } else {
    let $ = strings.tail;
    if ($ instanceof $Empty) {
      let s = strings.head;
      return s;
    } else {
      let s = strings.head;
      let rest = $;
      return (s + sep) + string_join(rest, sep);
    }
  }
}

/**
 * Helper to convert shape to string for error messages
 * 
 * @ignore
 */
function shape_to_string(shp) {
  return ("[" + (() => {
    let _pipe = $list.map(shp, $int.to_string);
    return string_join(_pipe, ", ");
  })()) + "]";
}
