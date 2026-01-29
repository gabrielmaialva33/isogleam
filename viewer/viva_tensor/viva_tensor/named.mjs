import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, Empty as $Empty, CustomType as $CustomType } from "../gleam.mjs";
import * as $axis from "../viva_tensor/axis.mjs";
import { Anon, AxisSpec, equals as axis_equals, to_string as axis_to_string } from "../viva_tensor/axis.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

export class NamedTensor extends $CustomType {
  constructor(data, axes) {
    super();
    this.data = data;
    this.axes = axes;
  }
}
export const NamedTensor$NamedTensor = (data, axes) =>
  new NamedTensor(data, axes);
export const NamedTensor$isNamedTensor = (value) =>
  value instanceof NamedTensor;
export const NamedTensor$NamedTensor$data = (value) => value.data;
export const NamedTensor$NamedTensor$0 = (value) => value.data;
export const NamedTensor$NamedTensor$axes = (value) => value.axes;
export const NamedTensor$NamedTensor$1 = (value) => value.axes;

/**
 * Axis not found
 */
export class AxisNotFound extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const NamedTensorError$AxisNotFound = (name) => new AxisNotFound(name);
export const NamedTensorError$isAxisNotFound = (value) =>
  value instanceof AxisNotFound;
export const NamedTensorError$AxisNotFound$name = (value) => value.name;
export const NamedTensorError$AxisNotFound$0 = (value) => value.name;

/**
 * Duplicate axis name
 */
export class DuplicateAxis extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const NamedTensorError$DuplicateAxis = (name) => new DuplicateAxis(name);
export const NamedTensorError$isDuplicateAxis = (value) =>
  value instanceof DuplicateAxis;
export const NamedTensorError$DuplicateAxis$name = (value) => value.name;
export const NamedTensorError$DuplicateAxis$0 = (value) => value.name;

/**
 * Axis mismatch in operation
 */
export class AxisMismatch extends $CustomType {
  constructor(expected, got) {
    super();
    this.expected = expected;
    this.got = got;
  }
}
export const NamedTensorError$AxisMismatch = (expected, got) =>
  new AxisMismatch(expected, got);
export const NamedTensorError$isAxisMismatch = (value) =>
  value instanceof AxisMismatch;
export const NamedTensorError$AxisMismatch$expected = (value) => value.expected;
export const NamedTensorError$AxisMismatch$0 = (value) => value.expected;
export const NamedTensorError$AxisMismatch$got = (value) => value.got;
export const NamedTensorError$AxisMismatch$1 = (value) => value.got;

/**
 * Size mismatch for same axis
 */
export class SizeMismatch extends $CustomType {
  constructor(axis, expected, got) {
    super();
    this.axis = axis;
    this.expected = expected;
    this.got = got;
  }
}
export const NamedTensorError$SizeMismatch = (axis, expected, got) =>
  new SizeMismatch(axis, expected, got);
export const NamedTensorError$isSizeMismatch = (value) =>
  value instanceof SizeMismatch;
export const NamedTensorError$SizeMismatch$axis = (value) => value.axis;
export const NamedTensorError$SizeMismatch$0 = (value) => value.axis;
export const NamedTensorError$SizeMismatch$expected = (value) => value.expected;
export const NamedTensorError$SizeMismatch$1 = (value) => value.expected;
export const NamedTensorError$SizeMismatch$got = (value) => value.got;
export const NamedTensorError$SizeMismatch$2 = (value) => value.got;

/**
 * Cannot broadcast axes
 */
export class BroadcastErr extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const NamedTensorError$BroadcastErr = (reason) =>
  new BroadcastErr(reason);
export const NamedTensorError$isBroadcastErr = (value) =>
  value instanceof BroadcastErr;
export const NamedTensorError$BroadcastErr$reason = (value) => value.reason;
export const NamedTensorError$BroadcastErr$0 = (value) => value.reason;

/**
 * Underlying tensor error
 */
export class TensorErr extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const NamedTensorError$TensorErr = ($0) => new TensorErr($0);
export const NamedTensorError$isTensorErr = (value) =>
  value instanceof TensorErr;
export const NamedTensorError$TensorErr$0 = (value) => value[0];

/**
 * Invalid operation
 */
export class InvalidOp extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const NamedTensorError$InvalidOp = (reason) => new InvalidOp(reason);
export const NamedTensorError$isInvalidOp = (value) =>
  value instanceof InvalidOp;
export const NamedTensorError$InvalidOp$reason = (value) => value.reason;
export const NamedTensorError$InvalidOp$0 = (value) => value.reason;

/**
 * Create from tensor with inferred anonymous axes
 */
export function from_tensor(t) {
  let axes = $list.map(
    t.shape,
    (size) => { return new AxisSpec(new Anon(), size); },
  );
  return new NamedTensor(t, axes);
}

/**
 * Create named tensor of zeros
 */
export function zeros(axes) {
  let shape$1 = $list.map(axes, (a) => { return a.size; });
  let data = $tensor.zeros(shape$1);
  return new NamedTensor(data, axes);
}

/**
 * Create named tensor of ones
 */
export function ones(axes) {
  let shape$1 = $list.map(axes, (a) => { return a.size; });
  let data = $tensor.ones(shape$1);
  return new NamedTensor(data, axes);
}

function find_axis_in_list(loop$axes, loop$name, loop$idx) {
  while (true) {
    let axes = loop$axes;
    let name = loop$name;
    let idx = loop$idx;
    if (axes instanceof $Empty) {
      return new Error(new AxisNotFound(name));
    } else {
      let first = axes.head;
      let rest = axes.tail;
      let $ = axis_equals(first.name, name);
      if ($) {
        return new Ok(idx);
      } else {
        loop$axes = rest;
        loop$name = name;
        loop$idx = idx + 1;
      }
    }
  }
}

/**
 * Find axis index by name
 */
export function find_axis(t, name) {
  return find_axis_in_list(t.axes, name, 0);
}

/**
 * Check if tensor has axis
 */
export function has_axis(t, name) {
  let $ = find_axis(t, name);
  if ($ instanceof Ok) {
    return true;
  } else {
    return false;
  }
}

/**
 * Get all axis names
 */
export function axis_names(t) {
  return $list.map(t.axes, (a) => { return a.name; });
}

/**
 * Get shape as list
 */
export function shape(t) {
  return t.data.shape;
}

/**
 * Get rank (number of dimensions)
 */
export function rank(t) {
  return $list.length(t.axes);
}

/**
 * Total number of elements
 */
export function size(t) {
  return $tensor.size(t.data);
}

/**
 * Rename an axis
 */
export function rename_axis(t, from, to) {
  let $ = find_axis(t, from);
  if ($ instanceof Ok) {
    let idx = $[0];
    let new_axes = $list.index_map(
      t.axes,
      (spec, i) => {
        let $1 = i === idx;
        if ($1) {
          return new AxisSpec(to, spec.size);
        } else {
          return spec;
        }
      },
    );
    return new Ok(new NamedTensor(t.data, new_axes));
  } else {
    return $;
  }
}

/**
 * Convert to plain tensor (drop names)
 */
export function to_tensor(t) {
  return t.data;
}

/**
 * Pretty print tensor info
 */
export function describe(t) {
  let _block;
  let _pipe = t.axes;
  let _pipe$1 = $list.map(
    _pipe,
    (a) => { return (axis_to_string(a.name) + ":") + $int.to_string(a.size); },
  );
  _block = $string.join(_pipe$1, ", ");
  let axes_str = _block;
  return ("NamedTensor[" + axes_str) + "]";
}

function validate_sizes(loop$shape, loop$axes) {
  while (true) {
    let shape = loop$shape;
    let axes = loop$axes;
    if (shape instanceof $Empty) {
      if (axes instanceof $Empty) {
        return new Ok(undefined);
      } else {
        return new Error(new InvalidOp("Shape and axes length mismatch"));
      }
    } else if (axes instanceof $Empty) {
      return new Error(new InvalidOp("Shape and axes length mismatch"));
    } else {
      let s = shape.head;
      let s_rest = shape.tail;
      let a = axes.head;
      let a_rest = axes.tail;
      let $ = s === a.size;
      if ($) {
        loop$shape = s_rest;
        loop$axes = a_rest;
      } else {
        return new Error(new SizeMismatch(a.name, a.size, s));
      }
    }
  }
}

function has_duplicates(loop$items) {
  while (true) {
    let items = loop$items;
    if (items instanceof $Empty) {
      return false;
    } else {
      let first = items.head;
      let rest = items.tail;
      let $ = $list.any(rest, (x) => { return axis_equals(x, first); });
      if ($) {
        return $;
      } else {
        loop$items = rest;
      }
    }
  }
}

function validate_unique_names(axes) {
  let named_axes = $list.filter(
    axes,
    (a) => {
      let $ = a.name;
      if ($ instanceof Anon) {
        return false;
      } else {
        return true;
      }
    },
  );
  let names = $list.map(named_axes, (a) => { return a.name; });
  let $ = has_duplicates(names);
  if ($) {
    return new Error(new DuplicateAxis(new Anon()));
  } else {
    return new Ok(undefined);
  }
}

/**
 * Create named tensor from data and axis specs
 */
export function new$(data, axes) {
  let data_rank = $tensor.rank(data);
  let axes_count = $list.length(axes);
  let $ = data_rank === axes_count;
  if ($) {
    let $1 = validate_sizes(data.shape, axes);
    if ($1 instanceof Ok) {
      let $2 = validate_unique_names(axes);
      if ($2 instanceof Ok) {
        return new Ok(new NamedTensor(data, axes));
      } else {
        return $2;
      }
    } else {
      return $1;
    }
  } else {
    return new Error(
      new InvalidOp(
        ((("Axis count (" + $int.to_string(axes_count)) + ") doesn't match tensor rank (") + $int.to_string(
          data_rank,
        )) + ")",
      ),
    );
  }
}

function list_at(lst, idx) {
  let _pipe = lst;
  let _pipe$1 = $list.drop(_pipe, idx);
  return $list.first(_pipe$1);
}

/**
 * Get axis size by name
 */
export function axis_size(t, name) {
  let $ = find_axis(t, name);
  if ($ instanceof Ok) {
    let idx = $[0];
    let $1 = list_at(t.axes, idx);
    if ($1 instanceof Ok) {
      let spec = $1[0];
      return new Ok(spec.size);
    } else {
      return new Error(new AxisNotFound(name));
    }
  } else {
    return $;
  }
}

function remove_at(lst, idx) {
  let _pipe = lst;
  let _pipe$1 = $list.index_map(_pipe, (item, i) => { return [item, i]; });
  let _pipe$2 = $list.filter(_pipe$1, (pair) => { return pair[1] !== idx; });
  return $list.map(_pipe$2, (pair) => { return pair[0]; });
}
