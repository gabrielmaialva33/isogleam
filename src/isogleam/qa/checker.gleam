/// IsoGleam QA - Checker Module
/// Main QA checker that combines all checks

import gleam/list
import gleam/int
import gleam/float
import gleam/string
import isogleam/qa/color
import isogleam/qa/pixel.{type ImageData}
import isogleam/qa/palette.{type Palette}

/// QA Result
pub type QAResult {
  QAResult(
    passed: Bool,
    score: Float,
    checks: List(Check),
    errors: List(String),
    warnings: List(String),
  )
}

/// Individual check result
pub type Check {
  Check(
    name: String,
    passed: Bool,
    score: Float,
    message: String,
  )
}

/// QA Configuration
pub type QAConfig {
  QAConfig(
    expected_width: Int,
    expected_height: Int,
    max_colors: Int,
    color_tolerance: Float,
    min_score: Float,
    check_antialiasing: Bool,
    check_gradients: Bool,
  )
}

/// Default QA config for IsoGleam tiles
pub fn default_config() -> QAConfig {
  QAConfig(
    expected_width: 128,
    expected_height: 64,
    max_colors: 64,
    color_tolerance: 15.0,
    min_score: 0.85,
    check_antialiasing: True,
    check_gradients: True,
  )
}

/// Run full QA on image
pub fn check(img: ImageData, pal: Palette, config: QAConfig) -> QAResult {
  let checks = [
    check_dimensions(img, config),
    check_palette_compliance(img, pal, config),
    check_color_count(img, config),
    check_antialiasing(img, config),
    check_gradients(img, config),
  ]

  let passed_count = list.count(checks, fn(c) { c.passed })
  let total = list.length(checks)

  let avg_score =
    list.fold(checks, 0.0, fn(acc, c) { acc +. c.score })
    /. int.to_float(total)

  let errors =
    list.filter(checks, fn(c) { !c.passed })
    |> list.map(fn(c) { c.name <> ": " <> c.message })

  let warnings =
    list.filter(checks, fn(c) { c.passed && c.score <. 1.0 })
    |> list.map(fn(c) { c.name <> ": " <> c.message })

  let passed = passed_count == total && avg_score >=. config.min_score

  QAResult(
    passed: passed,
    score: avg_score,
    checks: checks,
    errors: errors,
    warnings: warnings,
  )
}

/// Check 1: Dimensions
fn check_dimensions(img: ImageData, config: QAConfig) -> Check {
  let ok =
    img.width == config.expected_width && img.height == config.expected_height

  let msg = case ok {
    True ->
      "OK: "
      <> int.to_string(img.width)
      <> "x"
      <> int.to_string(img.height)
    False ->
      "Expected "
      <> int.to_string(config.expected_width)
      <> "x"
      <> int.to_string(config.expected_height)
      <> ", got "
      <> int.to_string(img.width)
      <> "x"
      <> int.to_string(img.height)
  }

  Check(name: "dimensions", passed: ok, score: bool_to_score(ok), message: msg)
}

/// Check 2: Palette compliance
fn check_palette_compliance(
  img: ImageData,
  pal: Palette,
  config: QAConfig,
) -> Check {
  let analysis = palette.analyze(img.pixels, pal, config.color_tolerance)

  let ok = analysis.colors_outside_palette == 0
  let score = analysis.score

  let msg = case ok {
    True ->
      "All "
      <> int.to_string(analysis.unique_colors)
      <> " colors in palette"
    False ->
      int.to_string(analysis.colors_outside_palette)
      <> " colors outside palette"
  }

  Check(name: "palette", passed: ok, score: score, message: msg)
}

/// Check 3: Color count
fn check_color_count(img: ImageData, config: QAConfig) -> Check {
  let analysis = palette.analyze(img.pixels, palette.isogleam_palette(), 0.0)

  let ok = analysis.unique_colors <= config.max_colors
  let score = case ok {
    True -> 1.0
    False ->
      int.to_float(config.max_colors)
      /. int.to_float(analysis.unique_colors)
  }

  let msg =
    int.to_string(analysis.unique_colors)
    <> " unique colors (max "
    <> int.to_string(config.max_colors)
    <> ")"

  Check(name: "color_count", passed: ok, score: score, message: msg)
}

/// Check 4: Antialiasing detection
fn check_antialiasing(img: ImageData, config: QAConfig) -> Check {
  case config.check_antialiasing {
    False -> Check(name: "antialiasing", passed: True, score: 1.0, message: "Skipped")
    True -> {
      let aa_score = detect_antialiasing(img)
      let ok = aa_score <. 0.1  // Less than 10% antialiased pixels

      let msg = case ok {
        True -> "No antialiasing detected"
        False ->
          "Antialiasing detected ("
          <> float.to_string(aa_score *. 100.0)
          <> "%)"
      }

      Check(
        name: "antialiasing",
        passed: ok,
        score: 1.0 -. aa_score,
        message: msg,
      )
    }
  }
}

/// Check 5: Gradient detection
fn check_gradients(img: ImageData, config: QAConfig) -> Check {
  case config.check_gradients {
    False -> Check(name: "gradients", passed: True, score: 1.0, message: "Skipped")
    True -> {
      let grad_score = detect_smooth_gradients(img)
      let ok = grad_score <. 0.05  // Less than 5% smooth gradient areas

      let msg = case ok {
        True -> "No smooth gradients detected"
        False ->
          "Smooth gradients detected ("
          <> float.to_string(grad_score *. 100.0)
          <> "%)"
      }

      Check(
        name: "gradients",
        passed: ok,
        score: 1.0 -. grad_score,
        message: msg,
      )
    }
  }
}

/// Detect antialiasing by checking for intermediate colors between neighbors
fn detect_antialiasing(img: ImageData) -> Float {
  // Sample pixels and check for intermediate colors
  let total = img.width * img.height
  case total {
    0 -> 0.0
    _ -> {
      // Simplified: check adjacent pixel color distances
      // Real AA detection would be more sophisticated
      let aa_pixels = count_aa_pixels(img)
      int.to_float(aa_pixels) /. int.to_float(total)
    }
  }
}

fn count_aa_pixels(img: ImageData) -> Int {
  // Count pixels that look like antialiasing
  // (intermediate colors between very different neighbors)
  let threshold = 30.0  // Color distance threshold

  list.index_fold(img.pixels, 0, fn(acc, pixel, idx) {
    let x = idx % img.width
    let y = idx / img.width

    // Check if pixel is intermediate between neighbors
    case x > 0 && x < img.width - 1 {
      False -> acc
      True -> {
        case pixel.get_pixel(img, x - 1, y), pixel.get_pixel(img, x + 1, y) {
          Ok(left), Ok(right) -> {
            let dist_lr = color.distance(left, right)
            let dist_lc = color.distance(left, pixel)
            let dist_rc = color.distance(right, pixel)

            // If neighbors are very different but current is in between
            case
              dist_lr >. threshold *. 2.0
              && dist_lc <. threshold
              && dist_rc <. threshold
            {
              True -> acc + 1
              False -> acc
            }
          }
          _, _ -> acc
        }
      }
    }
  })
}

/// Detect smooth gradients (non-dithered color transitions)
fn detect_smooth_gradients(img: ImageData) -> Float {
  // Count runs of incrementally changing colors
  let total = img.width * img.height
  case total {
    0 -> 0.0
    _ -> {
      let gradient_pixels = count_gradient_runs(img)
      int.to_float(gradient_pixels) /. int.to_float(total)
    }
  }
}

fn count_gradient_runs(img: ImageData) -> Int {
  // Simplified gradient detection
  // Real implementation would look for consistent color increments
  list.index_fold(img.pixels, 0, fn(acc, pixel, idx) {
    let x = idx % img.width

    case x > 1 {
      False -> acc
      True -> {
        let y = idx / img.width
        case
          pixel.get_pixel(img, x - 1, y),
          pixel.get_pixel(img, x - 2, y)
        {
          Ok(p1), Ok(p2) -> {
            // Check for consistent gradient
            let d1 = color.distance(p2, p1)
            let d2 = color.distance(p1, pixel)

            // If distances are similar and small, might be gradient
            case d1 >. 1.0 && d1 <. 5.0 && d2 >. 1.0 && d2 <. 5.0 {
              True -> {
                let ratio = case d1 >. d2 {
                  True -> d2 /. d1
                  False -> d1 /. d2
                }
                case ratio >. 0.8 {
                  True -> acc + 1
                  False -> acc
                }
              }
              False -> acc
            }
          }
          _, _ -> acc
        }
      }
    }
  })
}

fn bool_to_score(b: Bool) -> Float {
  case b {
    True -> 1.0
    False -> 0.0
  }
}

/// Format QA result as string
pub fn format_result(result: QAResult) -> String {
  let status = case result.passed {
    True -> "PASSED"
    False -> "FAILED"
  }

  let header =
    "QA Result: "
    <> status
    <> " (score: "
    <> float.to_string(result.score *. 100.0)
    <> "%)\n"

  let checks_str =
    list.map(result.checks, fn(c) {
      let status_char = case c.passed {
        True -> "[OK]"
        False -> "[X]"
      }
      "  "
      <> status_char
      <> " "
      <> c.name
      <> ": "
      <> c.message
    })
    |> string.join("\n")

  let errors_str = case result.errors {
    [] -> ""
    errs -> "\nErrors:\n  " <> string.join(errs, "\n  ")
  }

  let warnings_str = case result.warnings {
    [] -> ""
    warns -> "\nWarnings:\n  " <> string.join(warns, "\n  ")
  }

  header <> checks_str <> errors_str <> warnings_str
}
