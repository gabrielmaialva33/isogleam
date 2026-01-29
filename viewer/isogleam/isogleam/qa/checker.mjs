import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  toList,
  Empty as $Empty,
  CustomType as $CustomType,
  remainderInt,
  divideFloat,
  divideInt,
} from "../../gleam.mjs";
import * as $color from "../../isogleam/qa/color.mjs";
import * as $palette from "../../isogleam/qa/palette.mjs";
import * as $pixel from "../../isogleam/qa/pixel.mjs";

export class QAResult extends $CustomType {
  constructor(passed, score, checks, errors, warnings) {
    super();
    this.passed = passed;
    this.score = score;
    this.checks = checks;
    this.errors = errors;
    this.warnings = warnings;
  }
}
export const QAResult$QAResult = (passed, score, checks, errors, warnings) =>
  new QAResult(passed, score, checks, errors, warnings);
export const QAResult$isQAResult = (value) => value instanceof QAResult;
export const QAResult$QAResult$passed = (value) => value.passed;
export const QAResult$QAResult$0 = (value) => value.passed;
export const QAResult$QAResult$score = (value) => value.score;
export const QAResult$QAResult$1 = (value) => value.score;
export const QAResult$QAResult$checks = (value) => value.checks;
export const QAResult$QAResult$2 = (value) => value.checks;
export const QAResult$QAResult$errors = (value) => value.errors;
export const QAResult$QAResult$3 = (value) => value.errors;
export const QAResult$QAResult$warnings = (value) => value.warnings;
export const QAResult$QAResult$4 = (value) => value.warnings;

export class Check extends $CustomType {
  constructor(name, passed, score, message) {
    super();
    this.name = name;
    this.passed = passed;
    this.score = score;
    this.message = message;
  }
}
export const Check$Check = (name, passed, score, message) =>
  new Check(name, passed, score, message);
export const Check$isCheck = (value) => value instanceof Check;
export const Check$Check$name = (value) => value.name;
export const Check$Check$0 = (value) => value.name;
export const Check$Check$passed = (value) => value.passed;
export const Check$Check$1 = (value) => value.passed;
export const Check$Check$score = (value) => value.score;
export const Check$Check$2 = (value) => value.score;
export const Check$Check$message = (value) => value.message;
export const Check$Check$3 = (value) => value.message;

export class QAConfig extends $CustomType {
  constructor(expected_width, expected_height, max_colors, color_tolerance, min_score, check_antialiasing, check_gradients) {
    super();
    this.expected_width = expected_width;
    this.expected_height = expected_height;
    this.max_colors = max_colors;
    this.color_tolerance = color_tolerance;
    this.min_score = min_score;
    this.check_antialiasing = check_antialiasing;
    this.check_gradients = check_gradients;
  }
}
export const QAConfig$QAConfig = (expected_width, expected_height, max_colors, color_tolerance, min_score, check_antialiasing, check_gradients) =>
  new QAConfig(expected_width,
  expected_height,
  max_colors,
  color_tolerance,
  min_score,
  check_antialiasing,
  check_gradients);
export const QAConfig$isQAConfig = (value) => value instanceof QAConfig;
export const QAConfig$QAConfig$expected_width = (value) => value.expected_width;
export const QAConfig$QAConfig$0 = (value) => value.expected_width;
export const QAConfig$QAConfig$expected_height = (value) =>
  value.expected_height;
export const QAConfig$QAConfig$1 = (value) => value.expected_height;
export const QAConfig$QAConfig$max_colors = (value) => value.max_colors;
export const QAConfig$QAConfig$2 = (value) => value.max_colors;
export const QAConfig$QAConfig$color_tolerance = (value) =>
  value.color_tolerance;
export const QAConfig$QAConfig$3 = (value) => value.color_tolerance;
export const QAConfig$QAConfig$min_score = (value) => value.min_score;
export const QAConfig$QAConfig$4 = (value) => value.min_score;
export const QAConfig$QAConfig$check_antialiasing = (value) =>
  value.check_antialiasing;
export const QAConfig$QAConfig$5 = (value) => value.check_antialiasing;
export const QAConfig$QAConfig$check_gradients = (value) =>
  value.check_gradients;
export const QAConfig$QAConfig$6 = (value) => value.check_gradients;

/**
 * Default QA config for IsoGleam tiles
 */
export function default_config() {
  return new QAConfig(128, 64, 64, 15.0, 0.85, true, true);
}

/**
 * Check 2: Palette compliance
 * 
 * @ignore
 */
function check_palette_compliance(img, pal, config) {
  let analysis = $palette.analyze(img.pixels, pal, config.color_tolerance);
  let ok = analysis.colors_outside_palette === 0;
  let score = analysis.score;
  let _block;
  if (ok) {
    _block = ("All " + $int.to_string(analysis.unique_colors)) + " colors in palette";
  } else {
    _block = $int.to_string(analysis.colors_outside_palette) + " colors outside palette";
  }
  let msg = _block;
  return new Check("palette", ok, score, msg);
}

/**
 * Check 3: Color count
 * 
 * @ignore
 */
function check_color_count(img, config) {
  let analysis = $palette.analyze(img.pixels, $palette.isogleam_palette(), 0.0);
  let ok = analysis.unique_colors <= config.max_colors;
  let _block;
  if (ok) {
    _block = 1.0;
  } else {
    _block = divideFloat(
      $int.to_float(config.max_colors),
      $int.to_float(analysis.unique_colors)
    );
  }
  let score = _block;
  let msg = (($int.to_string(analysis.unique_colors) + " unique colors (max ") + $int.to_string(
    config.max_colors,
  )) + ")";
  return new Check("color_count", ok, score, msg);
}

function count_aa_pixels(img) {
  let threshold = 30.0;
  return $list.index_fold(
    img.pixels,
    0,
    (acc, pixel, idx) => {
      let x = remainderInt(idx, img.width);
      let y = divideInt(idx, img.width);
      let $ = (x > 0) && (x < (img.width - 1));
      if ($) {
        let $1 = $pixel.get_pixel(img, x - 1, y);
        let $2 = $pixel.get_pixel(img, x + 1, y);
        if ($1 instanceof Ok && $2 instanceof Ok) {
          let left = $1[0];
          let right = $2[0];
          let dist_lr = $color.distance(left, right);
          let dist_lc = $color.distance(left, pixel);
          let dist_rc = $color.distance(right, pixel);
          let $3 = ((dist_lr > (threshold * 2.0)) && (dist_lc < threshold)) && (dist_rc < threshold);
          if ($3) {
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
}

/**
 * Detect antialiasing by checking for intermediate colors between neighbors
 * 
 * @ignore
 */
function detect_antialiasing(img) {
  let total = img.width * img.height;
  if (total === 0) {
    return 0.0;
  } else {
    let aa_pixels = count_aa_pixels(img);
    return divideFloat($int.to_float(aa_pixels), $int.to_float(total));
  }
}

/**
 * Check 4: Antialiasing detection
 * 
 * @ignore
 */
function check_antialiasing(img, config) {
  let $ = config.check_antialiasing;
  if ($) {
    let aa_score = detect_antialiasing(img);
    let ok = aa_score < 0.1;
    let _block;
    if (ok) {
      _block = "No antialiasing detected";
    } else {
      _block = ("Antialiasing detected (" + $float.to_string(aa_score * 100.0)) + "%)";
    }
    let msg = _block;
    return new Check("antialiasing", ok, 1.0 - aa_score, msg);
  } else {
    return new Check("antialiasing", true, 1.0, "Skipped");
  }
}

function count_gradient_runs(img) {
  return $list.index_fold(
    img.pixels,
    0,
    (acc, pixel, idx) => {
      let x = remainderInt(idx, img.width);
      let $ = x > 1;
      if ($) {
        let y = divideInt(idx, img.width);
        let $1 = $pixel.get_pixel(img, x - 1, y);
        let $2 = $pixel.get_pixel(img, x - 2, y);
        if ($1 instanceof Ok && $2 instanceof Ok) {
          let p1 = $1[0];
          let p2 = $2[0];
          let d1 = $color.distance(p2, p1);
          let d2 = $color.distance(p1, pixel);
          let $3 = (((d1 > 1.0) && (d1 < 5.0)) && (d2 > 1.0)) && (d2 < 5.0);
          if ($3) {
            let _block;
            let $4 = d1 > d2;
            if ($4) {
              _block = divideFloat(d2, d1);
            } else {
              _block = divideFloat(d1, d2);
            }
            let ratio = _block;
            let $5 = ratio > 0.8;
            if ($5) {
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
      } else {
        return acc;
      }
    },
  );
}

/**
 * Detect smooth gradients (non-dithered color transitions)
 * 
 * @ignore
 */
function detect_smooth_gradients(img) {
  let total = img.width * img.height;
  if (total === 0) {
    return 0.0;
  } else {
    let gradient_pixels = count_gradient_runs(img);
    return divideFloat($int.to_float(gradient_pixels), $int.to_float(total));
  }
}

/**
 * Check 5: Gradient detection
 * 
 * @ignore
 */
function check_gradients(img, config) {
  let $ = config.check_gradients;
  if ($) {
    let grad_score = detect_smooth_gradients(img);
    let ok = grad_score < 0.05;
    let _block;
    if (ok) {
      _block = "No smooth gradients detected";
    } else {
      _block = ("Smooth gradients detected (" + $float.to_string(
        grad_score * 100.0,
      )) + "%)";
    }
    let msg = _block;
    return new Check("gradients", ok, 1.0 - grad_score, msg);
  } else {
    return new Check("gradients", true, 1.0, "Skipped");
  }
}

function bool_to_score(b) {
  if (b) {
    return 1.0;
  } else {
    return 0.0;
  }
}

/**
 * Check 1: Dimensions
 * 
 * @ignore
 */
function check_dimensions(img, config) {
  let ok = (img.width === config.expected_width) && (img.height === config.expected_height);
  let _block;
  if (ok) {
    _block = (("OK: " + $int.to_string(img.width)) + "x") + $int.to_string(
      img.height,
    );
  } else {
    _block = (((((("Expected " + $int.to_string(config.expected_width)) + "x") + $int.to_string(
      config.expected_height,
    )) + ", got ") + $int.to_string(img.width)) + "x") + $int.to_string(
      img.height,
    );
  }
  let msg = _block;
  return new Check("dimensions", ok, bool_to_score(ok), msg);
}

/**
 * Run full QA on image
 */
export function check(img, pal, config) {
  let checks = toList([
    check_dimensions(img, config),
    check_palette_compliance(img, pal, config),
    check_color_count(img, config),
    check_antialiasing(img, config),
    check_gradients(img, config),
  ]);
  let passed_count = $list.count(checks, (c) => { return c.passed; });
  let total = $list.length(checks);
  let avg_score = divideFloat(
    $list.fold(checks, 0.0, (acc, c) => { return acc + c.score; }),
    $int.to_float(total)
  );
  let _block;
  let _pipe = $list.filter(checks, (c) => { return !c.passed; });
  _block = $list.map(_pipe, (c) => { return (c.name + ": ") + c.message; });
  let errors = _block;
  let _block$1;
  let _pipe$1 = $list.filter(
    checks,
    (c) => { return c.passed && (c.score < 1.0); },
  );
  _block$1 = $list.map(_pipe$1, (c) => { return (c.name + ": ") + c.message; });
  let warnings = _block$1;
  let passed = (passed_count === total) && (avg_score >= config.min_score);
  return new QAResult(passed, avg_score, checks, errors, warnings);
}

/**
 * Format QA result as string
 */
export function format_result(result) {
  let _block;
  let $ = result.passed;
  if ($) {
    _block = "PASSED";
  } else {
    _block = "FAILED";
  }
  let status = _block;
  let header = ((("QA Result: " + status) + " (score: ") + $float.to_string(
    result.score * 100.0,
  )) + "%)\n";
  let _block$1;
  let _pipe = $list.map(
    result.checks,
    (c) => {
      let _block$2;
      let $1 = c.passed;
      if ($1) {
        _block$2 = "[OK]";
      } else {
        _block$2 = "[X]";
      }
      let status_char = _block$2;
      return (((("  " + status_char) + " ") + c.name) + ": ") + c.message;
    },
  );
  _block$1 = $string.join(_pipe, "\n");
  let checks_str = _block$1;
  let _block$2;
  let $1 = result.errors;
  if ($1 instanceof $Empty) {
    _block$2 = "";
  } else {
    let errs = $1;
    _block$2 = "\nErrors:\n  " + $string.join(errs, "\n  ");
  }
  let errors_str = _block$2;
  let _block$3;
  let $2 = result.warnings;
  if ($2 instanceof $Empty) {
    _block$3 = "";
  } else {
    let warns = $2;
    _block$3 = "\nWarnings:\n  " + $string.join(warns, "\n  ");
  }
  let warnings_str = _block$3;
  return ((header + checks_str) + errors_str) + warnings_str;
}
