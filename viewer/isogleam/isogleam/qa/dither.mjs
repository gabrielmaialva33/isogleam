import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  remainderInt,
  divideFloat,
  divideInt,
} from "../../gleam.mjs";

/**
 * 8x8 Bayer ordered dithering matrix
 * Each value is 0-63, representing threshold levels
 * 
 * @ignore
 */
const bayer_8x8 = /* @__PURE__ */ toList([
  /* @__PURE__ */ toList([0, 32, 8, 40, 2, 34, 10, 42]),
  /* @__PURE__ */ toList([48, 16, 56, 24, 50, 18, 58, 26]),
  /* @__PURE__ */ toList([12, 44, 4, 36, 14, 46, 6, 38]),
  /* @__PURE__ */ toList([60, 28, 52, 20, 62, 30, 54, 22]),
  /* @__PURE__ */ toList([3, 35, 11, 43, 1, 33, 9, 41]),
  /* @__PURE__ */ toList([51, 19, 59, 27, 49, 17, 57, 25]),
  /* @__PURE__ */ toList([15, 47, 7, 39, 13, 45, 5, 37]),
  /* @__PURE__ */ toList([63, 31, 55, 23, 61, 29, 53, 21]),
]);

/**
 * 4x4 Bayer matrix (for smaller patterns)
 * 
 * @ignore
 */
const bayer_4x4 = /* @__PURE__ */ toList([
  /* @__PURE__ */ toList([0, 8, 2, 10]),
  /* @__PURE__ */ toList([12, 4, 14, 6]),
  /* @__PURE__ */ toList([3, 11, 1, 9]),
  /* @__PURE__ */ toList([15, 7, 13, 5]),
]);

function do_list_at(loop$l, loop$index) {
  while (true) {
    let l = loop$l;
    let index = loop$index;
    if (l instanceof $Empty) {
      return new Error(undefined);
    } else {
      let first = l.head;
      let rest = l.tail;
      let $ = index === 0;
      if ($) {
        return new Ok(first);
      } else {
        loop$l = rest;
        loop$index = index - 1;
      }
    }
  }
}

function list_at(l, index) {
  return do_list_at(l, index);
}

function float_abs(x) {
  let $ = x < 0.0;
  if ($) {
    return 0.0 - x;
  } else {
    return x;
  }
}

/**
 * Detect if image uses smooth gradients (anti-dithering)
 * Returns percentage of smooth gradient areas (0.0 = all dithered, 1.0 = all smooth)
 */
export function detect_smooth_gradient(values, width) {
  let smooth_count = $list.index_fold(
    values,
    0,
    (acc, value, idx) => {
      let x = remainderInt(idx, width);
      let $ = x > 0;
      if ($) {
        let $1 = list_at(values, idx - 1);
        if ($1 instanceof Ok) {
          let prev_value = $1[0];
          let diff = float_abs(value - prev_value);
          let $2 = (diff > 0.001) && (diff < 0.1);
          if ($2) {
            return acc + 1;
          } else {
            return acc;
          }
        } else {
          return acc;
        }
      } else {
        return acc;
      }
    },
  );
  let total = $list.length(values) - width;
  let $ = total > 0;
  if ($) {
    return divideFloat($int.to_float(smooth_count), $int.to_float(total));
  } else {
    return 0.0;
  }
}

/**
 * Get Bayer threshold at position
 */
export function get_threshold_8x8(x, y) {
  let row_idx = y % 8;
  let col_idx = x % 8;
  let $ = list_at(bayer_8x8, row_idx);
  if ($ instanceof Ok) {
    let row = $[0];
    let $1 = list_at(row, col_idx);
    if ($1 instanceof Ok) {
      let val = $1[0];
      return $int.to_float(val + 1) / 64.0;
    } else {
      return 0.5;
    }
  } else {
    return 0.5;
  }
}

/**
 * Check if a color transition follows Bayer dithering pattern
 * Returns a score from 0.0 (no dithering) to 1.0 (perfect dithering)
 */
export function detect_bayer_pattern(intensities, width) {
  let matches = $list.index_fold(
    intensities,
    0,
    (acc, intensity, idx) => {
      let x = remainderInt(idx, width);
      let y = divideInt(idx, width);
      let threshold = get_threshold_8x8(x, y);
      let expected_on = intensity > 0.5;
      let would_be_on = intensity > threshold;
      let $ = expected_on === would_be_on;
      if ($) {
        return acc + 1;
      } else {
        return acc;
      }
    },
  );
  let total = $list.length(intensities);
  if (total === 0) {
    return 0.0;
  } else {
    return divideFloat($int.to_float(matches), $int.to_float(total));
  }
}

/**
 * Apply Bayer dithering to a value (for generation)
 */
export function dither_value(value, x, y) {
  let threshold = get_threshold_8x8(x, y);
  return value > threshold;
}

/**
 * Get 4x4 Bayer threshold
 */
export function get_threshold_4x4(x, y) {
  let row_idx = y % 4;
  let col_idx = x % 4;
  let $ = list_at(bayer_4x4, row_idx);
  if ($ instanceof Ok) {
    let row = $[0];
    let $1 = list_at(row, col_idx);
    if ($1 instanceof Ok) {
      let val = $1[0];
      return $int.to_float(val + 1) / 16.0;
    } else {
      return 0.5;
    }
  } else {
    return 0.5;
  }
}
