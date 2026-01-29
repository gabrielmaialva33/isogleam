import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { Ok, Error, Empty as $Empty, CustomType as $CustomType } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";
import { Tensor } from "../viva_tensor/tensor.mjs";

export class Sparse24Block extends $CustomType {
  constructor(values, positions) {
    super();
    this.values = values;
    this.positions = positions;
  }
}
export const Sparse24Block$Sparse24Block = (values, positions) =>
  new Sparse24Block(values, positions);
export const Sparse24Block$isSparse24Block = (value) =>
  value instanceof Sparse24Block;
export const Sparse24Block$Sparse24Block$values = (value) => value.values;
export const Sparse24Block$Sparse24Block$0 = (value) => value.values;
export const Sparse24Block$Sparse24Block$positions = (value) => value.positions;
export const Sparse24Block$Sparse24Block$1 = (value) => value.positions;

export class Sparse24Tensor extends $CustomType {
  constructor(blocks, shape, num_elements, memory_bytes, sparsity_percent) {
    super();
    this.blocks = blocks;
    this.shape = shape;
    this.num_elements = num_elements;
    this.memory_bytes = memory_bytes;
    this.sparsity_percent = sparsity_percent;
  }
}
export const Sparse24Tensor$Sparse24Tensor = (blocks, shape, num_elements, memory_bytes, sparsity_percent) =>
  new Sparse24Tensor(blocks, shape, num_elements, memory_bytes, sparsity_percent);
export const Sparse24Tensor$isSparse24Tensor = (value) =>
  value instanceof Sparse24Tensor;
export const Sparse24Tensor$Sparse24Tensor$blocks = (value) => value.blocks;
export const Sparse24Tensor$Sparse24Tensor$0 = (value) => value.blocks;
export const Sparse24Tensor$Sparse24Tensor$shape = (value) => value.shape;
export const Sparse24Tensor$Sparse24Tensor$1 = (value) => value.shape;
export const Sparse24Tensor$Sparse24Tensor$num_elements = (value) =>
  value.num_elements;
export const Sparse24Tensor$Sparse24Tensor$2 = (value) => value.num_elements;
export const Sparse24Tensor$Sparse24Tensor$memory_bytes = (value) =>
  value.memory_bytes;
export const Sparse24Tensor$Sparse24Tensor$3 = (value) => value.memory_bytes;
export const Sparse24Tensor$Sparse24Tensor$sparsity_percent = (value) =>
  value.sparsity_percent;
export const Sparse24Tensor$Sparse24Tensor$4 = (value) =>
  value.sparsity_percent;

export class PruneMetrics extends $CustomType {
  constructor(pruned_count, total_count, approximation_error, kept_magnitude_mean, pruned_magnitude_mean) {
    super();
    this.pruned_count = pruned_count;
    this.total_count = total_count;
    this.approximation_error = approximation_error;
    this.kept_magnitude_mean = kept_magnitude_mean;
    this.pruned_magnitude_mean = pruned_magnitude_mean;
  }
}
export const PruneMetrics$PruneMetrics = (pruned_count, total_count, approximation_error, kept_magnitude_mean, pruned_magnitude_mean) =>
  new PruneMetrics(pruned_count,
  total_count,
  approximation_error,
  kept_magnitude_mean,
  pruned_magnitude_mean);
export const PruneMetrics$isPruneMetrics = (value) =>
  value instanceof PruneMetrics;
export const PruneMetrics$PruneMetrics$pruned_count = (value) =>
  value.pruned_count;
export const PruneMetrics$PruneMetrics$0 = (value) => value.pruned_count;
export const PruneMetrics$PruneMetrics$total_count = (value) =>
  value.total_count;
export const PruneMetrics$PruneMetrics$1 = (value) => value.total_count;
export const PruneMetrics$PruneMetrics$approximation_error = (value) =>
  value.approximation_error;
export const PruneMetrics$PruneMetrics$2 = (value) => value.approximation_error;
export const PruneMetrics$PruneMetrics$kept_magnitude_mean = (value) =>
  value.kept_magnitude_mean;
export const PruneMetrics$PruneMetrics$3 = (value) => value.kept_magnitude_mean;
export const PruneMetrics$PruneMetrics$pruned_magnitude_mean = (value) =>
  value.pruned_magnitude_mean;
export const PruneMetrics$PruneMetrics$4 = (value) =>
  value.pruned_magnitude_mean;

/**
 * Poda um grupo de 4 elementos, retornando Sparse24Block
 * 
 * @ignore
 */
function prune_group_magnitude(group) {
  let indexed = $list.index_map(
    group,
    (val, idx) => { return [idx, val, $float.absolute_value(val)]; },
  );
  let sorted = $list.sort(
    indexed,
    (a, b) => { return $float.compare(b[2], a[2]); },
  );
  if (sorted instanceof $Empty) {
    return new Sparse24Block([0.0, 0.0], [0, 1]);
  } else {
    let $ = sorted.tail;
    if ($ instanceof $Empty) {
      return new Sparse24Block([0.0, 0.0], [0, 1]);
    } else {
      let first = sorted.head;
      let second = $.head;
      let pos1;
      let val1;
      pos1 = first[0];
      val1 = first[1];
      let pos2;
      let val2;
      pos2 = second[0];
      val2 = second[1];
      let _block;
      let $2 = pos1 < pos2;
      if ($2) {
        _block = [pos1, val1, pos2, val2];
      } else {
        _block = [pos2, val2, pos1, val1];
      }
      let $1 = _block;
      let p1;
      let v1;
      let p2;
      let v2;
      p1 = $1[0];
      v1 = $1[1];
      p2 = $1[2];
      v2 = $1[3];
      return new Sparse24Block([v1, v2], [p1, p2]);
    }
  }
}

/**
 * Pad grupo para ter exatamente 4 elementos
 * 
 * @ignore
 */
function pad_group(group) {
  let len = $list.length(group);
  let $ = len < 4;
  if ($) {
    return $list.append(group, $list.repeat(0.0, 4 - len));
  } else {
    return $list.take(group, 4);
  }
}

function prune_group_by_importance(weights, importance) {
  let _block;
  let _pipe = $list.zip($list.range(0, 3), $list.zip(weights, importance));
  _block = $list.map(
    _pipe,
    (x) => {
      let idx;
      let w;
      let i;
      idx = x[0];
      w = x[1][0];
      i = x[1][1];
      return [idx, w, i];
    },
  );
  let indexed = _block;
  let sorted = $list.sort(
    indexed,
    (a, b) => { return $float.compare(b[2], a[2]); },
  );
  if (sorted instanceof $Empty) {
    return new Sparse24Block([0.0, 0.0], [0, 1]);
  } else {
    let $ = sorted.tail;
    if ($ instanceof $Empty) {
      return new Sparse24Block([0.0, 0.0], [0, 1]);
    } else {
      let first = sorted.head;
      let second = $.head;
      let pos1;
      let val1;
      pos1 = first[0];
      val1 = first[1];
      let pos2;
      let val2;
      pos2 = second[0];
      val2 = second[1];
      let _block$1;
      let $2 = pos1 < pos2;
      if ($2) {
        _block$1 = [pos1, val1, pos2, val2];
      } else {
        _block$1 = [pos2, val2, pos1, val1];
      }
      let $1 = _block$1;
      let p1;
      let v1;
      let p2;
      let v2;
      p1 = $1[0];
      v1 = $1[1];
      p2 = $1[2];
      v2 = $1[3];
      return new Sparse24Block([v1, v2], [p1, p2]);
    }
  }
}

/**
 * Reconstrói tensor denso a partir de 2:4 sparse
 */
export function decompress(sparse) {
  let data = $list.flat_map(
    sparse.blocks,
    (block) => {
      let $ = block.values;
      let v1;
      let v2;
      v1 = $[0];
      v2 = $[1];
      let $1 = block.positions;
      let p1;
      let p2;
      p1 = $1[0];
      p2 = $1[1];
      let _pipe = $list.range(0, 3);
      return $list.map(
        _pipe,
        (i) => {
          let $2 = i === p1;
          if ($2) {
            return v1;
          } else {
            let $3 = i === p2;
            if ($3) {
              return v2;
            } else {
              return 0.0;
            }
          }
        },
      );
    },
  );
  let truncated = $list.take(data, sparse.num_elements);
  return new Tensor(truncated, sparse.shape);
}

function transpose_matrix(m) {
  if (m instanceof $Empty) {
    return m;
  } else {
    let first = m.head;
    let n_cols = $list.length(first);
    let _pipe = $list.range(0, n_cols - 1);
    return $list.map(
      _pipe,
      (col_idx) => {
        return $list.filter_map(
          m,
          (row) => {
            let $ = $list.drop(row, col_idx);
            if ($ instanceof $Empty) {
              return new Error(undefined);
            } else {
              let x = $.head;
              return new Ok(x);
            }
          },
        );
      },
    );
  }
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 10000.0)) / 10000.0;
  return $float.to_string(rounded);
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
