import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { Ok, CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $checker from "../isogleam/qa/checker.mjs";
import * as $color from "../isogleam/qa/color.mjs";
import * as $palette from "../isogleam/qa/palette.mjs";
import * as $pixel from "../isogleam/qa/pixel.mjs";

export class BatchSummary extends $CustomType {
  constructor(total, passed, failed, errors, avg_score) {
    super();
    this.total = total;
    this.passed = passed;
    this.failed = failed;
    this.errors = errors;
    this.avg_score = avg_score;
  }
}
export const BatchSummary$BatchSummary = (total, passed, failed, errors, avg_score) =>
  new BatchSummary(total, passed, failed, errors, avg_score);
export const BatchSummary$isBatchSummary = (value) =>
  value instanceof BatchSummary;
export const BatchSummary$BatchSummary$total = (value) => value.total;
export const BatchSummary$BatchSummary$0 = (value) => value.total;
export const BatchSummary$BatchSummary$passed = (value) => value.passed;
export const BatchSummary$BatchSummary$1 = (value) => value.passed;
export const BatchSummary$BatchSummary$failed = (value) => value.failed;
export const BatchSummary$BatchSummary$2 = (value) => value.failed;
export const BatchSummary$BatchSummary$errors = (value) => value.errors;
export const BatchSummary$BatchSummary$3 = (value) => value.errors;
export const BatchSummary$BatchSummary$avg_score = (value) => value.avg_score;
export const BatchSummary$BatchSummary$4 = (value) => value.avg_score;

/**
 * Check image data directly
 */
export function check_image(img, config) {
  let pal = $palette.isogleam_palette();
  return $checker.check(img, pal, config);
}

/**
 * Check file with custom config
 */
export function check_file_with_config(path, config) {
  let $ = $pixel.read_png(path);
  if ($ instanceof Ok) {
    let img = $[0];
    return new Ok(check_image(img, config));
  } else {
    return $;
  }
}

/**
 * Check a PNG file against IsoGleam standards
 */
export function check_file(path) {
  return check_file_with_config(path, $checker.default_config());
}

/**
 * Check image with custom palette
 */
export function check_with_palette(img, pal, config) {
  return $checker.check(img, pal, config);
}

/**
 * Quick check - returns just pass/fail
 */
export function quick_check(path) {
  let $ = check_file(path);
  if ($ instanceof Ok) {
    let result = $[0];
    return result.passed;
  } else {
    return false;
  }
}

/**
 * Get score only
 */
export function score(path) {
  let $ = check_file(path);
  if ($ instanceof Ok) {
    let result = $[0];
    return new Ok(result.score);
  } else {
    return $;
  }
}

/**
 * Format result for display
 */
export function format(result) {
  return $checker.format_result(result);
}

/**
 * Batch check multiple files
 */
export function batch_check(paths) {
  return $list.map(paths, (path) => { return [path, check_file(path)]; });
}

export function batch_summary(results) {
  let total = $list.length(results);
  let $ = $list.fold(
    results,
    [0, 0, 0, 0.0],
    (acc, item) => {
      let p;
      let f;
      let e;
      let s;
      p = acc[0];
      f = acc[1];
      e = acc[2];
      s = acc[3];
      let res;
      res = item[1];
      if (res instanceof Ok) {
        let qa_result = res[0];
        let $1 = qa_result.passed;
        if ($1) {
          return [p + 1, f, e, s + qa_result.score];
        } else {
          return [p, f + 1, e, s + qa_result.score];
        }
      } else {
        return [p, f, e + 1, s];
      }
    },
  );
  let passed;
  let failed;
  let errors;
  let score_sum;
  passed = $[0];
  failed = $[1];
  errors = $[2];
  score_sum = $[3];
  let _block;
  let $1 = total - errors;
  if ($1 === 0) {
    _block = 0.0;
  } else {
    let n = $1;
    _block = divideFloat(score_sum, $int.to_float(n));
  }
  let avg = _block;
  return new BatchSummary(total, passed, failed, errors, avg);
}

export function format_summary(summary) {
  return (((((((((((((("Batch QA Summary:\n" + "  Total:  ") + $int.to_string(
    summary.total,
  )) + "\n") + "  Passed: ") + $int.to_string(summary.passed)) + "\n") + "  Failed: ") + $int.to_string(
    summary.failed,
  )) + "\n") + "  Errors: ") + $int.to_string(summary.errors)) + "\n") + "  Avg Score: ") + $float.to_string(
    summary.avg_score * 100.0,
  )) + "%";
}

function opposite_side(side) {
  if (side instanceof $pixel.North) {
    return new $pixel.South();
  } else if (side instanceof $pixel.South) {
    return new $pixel.North();
  } else if (side instanceof $pixel.East) {
    return new $pixel.West();
  } else {
    return new $pixel.East();
  }
}

function compare_borders(b1, b2) {
  let pairs = $list.zip(b1, b2);
  let total = $list.length(pairs);
  if (total === 0) {
    return 1.0;
  } else {
    let matching = $list.count(
      pairs,
      (p) => { return $color.is_similar(p[0], p[1], 5.0); },
    );
    return divideFloat($int.to_float(matching), $int.to_float(total));
  }
}

/**
 * Seamless border checking between two tiles
 */
export function check_seamless(tile1_path, tile2_path, side, thickness) {
  let $ = $pixel.read_png(tile1_path);
  let $1 = $pixel.read_png(tile2_path);
  if ($ instanceof Ok) {
    if ($1 instanceof Ok) {
      let img1 = $[0];
      let img2 = $1[0];
      let border1 = $pixel.get_border(img1, side, thickness);
      let opposite = opposite_side(side);
      let border2 = $pixel.get_border(img2, opposite, thickness);
      let score_val = compare_borders(border1, border2);
      return new Ok(score_val);
    } else {
      return $1;
    }
  } else {
    return $;
  }
}
