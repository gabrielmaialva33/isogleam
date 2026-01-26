/// IsoGleam QA - Quality Assurance Module
/// Pure Gleam implementation for pixel art validation

import gleam/list
import gleam/int
import gleam/float
import isogleam/qa/color.{type RGB}
import isogleam/qa/pixel.{type ImageData, type ImageError}
import isogleam/qa/palette.{type Palette}
import isogleam/qa/checker.{type QAResult, type QAConfig}

/// Check a PNG file against IsoGleam standards
pub fn check_file(path: String) -> Result(QAResult, ImageError) {
  check_file_with_config(path, checker.default_config())
}

/// Check file with custom config
pub fn check_file_with_config(
  path: String,
  config: QAConfig,
) -> Result(QAResult, ImageError) {
  case pixel.read_png(path) {
    Error(e) -> Error(e)
    Ok(img) -> Ok(check_image(img, config))
  }
}

/// Check image data directly
pub fn check_image(img: ImageData, config: QAConfig) -> QAResult {
  let pal = palette.isogleam_palette()
  checker.check(img, pal, config)
}

/// Check image with custom palette
pub fn check_with_palette(
  img: ImageData,
  pal: Palette,
  config: QAConfig,
) -> QAResult {
  checker.check(img, pal, config)
}

/// Quick check - returns just pass/fail
pub fn quick_check(path: String) -> Bool {
  case check_file(path) {
    Ok(result) -> result.passed
    Error(_) -> False
  }
}

/// Get score only
pub fn score(path: String) -> Result(Float, ImageError) {
  case check_file(path) {
    Ok(result) -> Ok(result.score)
    Error(e) -> Error(e)
  }
}

/// Format result for display
pub fn format(result: QAResult) -> String {
  checker.format_result(result)
}

/// Batch check multiple files
pub fn batch_check(paths: List(String)) -> List(#(String, Result(QAResult, ImageError))) {
  list.map(paths, fn(path) { #(path, check_file(path)) })
}

/// Summary stats for batch
pub type BatchSummary {
  BatchSummary(
    total: Int,
    passed: Int,
    failed: Int,
    errors: Int,
    avg_score: Float,
  )
}

pub fn batch_summary(results: List(#(String, Result(QAResult, ImageError)))) -> BatchSummary {
  let total = list.length(results)

  let #(passed, failed, errors, score_sum) =
    list.fold(results, #(0, 0, 0, 0.0), fn(acc, item) {
      let #(p, f, e, s) = acc
      let #(_path, res) = item
      case res {
        Ok(qa_result) -> {
          case qa_result.passed {
            True -> #(p + 1, f, e, s +. qa_result.score)
            False -> #(p, f + 1, e, s +. qa_result.score)
          }
        }
        Error(_) -> #(p, f, e + 1, s)
      }
    })

  let avg = case total - errors {
    0 -> 0.0
    n -> score_sum /. int.to_float(n)
  }

  BatchSummary(
    total: total,
    passed: passed,
    failed: failed,
    errors: errors,
    avg_score: avg,
  )
}

pub fn format_summary(summary: BatchSummary) -> String {
  "Batch QA Summary:\n"
  <> "  Total:  " <> int.to_string(summary.total) <> "\n"
  <> "  Passed: " <> int.to_string(summary.passed) <> "\n"
  <> "  Failed: " <> int.to_string(summary.failed) <> "\n"
  <> "  Errors: " <> int.to_string(summary.errors) <> "\n"
  <> "  Avg Score: " <> float.to_string(summary.avg_score *. 100.0) <> "%"
}

/// Seamless border checking between two tiles
pub fn check_seamless(
  tile1_path: String,
  tile2_path: String,
  side: pixel.BorderSide,
  thickness: Int,
) -> Result(Float, ImageError) {
  case pixel.read_png(tile1_path), pixel.read_png(tile2_path) {
    Ok(img1), Ok(img2) -> {
      let border1 = pixel.get_border(img1, side, thickness)
      let opposite = opposite_side(side)
      let border2 = pixel.get_border(img2, opposite, thickness)
      let score_val = compare_borders(border1, border2)
      Ok(score_val)
    }
    Error(e), _ -> Error(e)
    _, Error(e) -> Error(e)
  }
}

fn opposite_side(side: pixel.BorderSide) -> pixel.BorderSide {
  case side {
    pixel.North -> pixel.South
    pixel.South -> pixel.North
    pixel.East -> pixel.West
    pixel.West -> pixel.East
  }
}

fn compare_borders(b1: List(RGB), b2: List(RGB)) -> Float {
  let pairs = list.zip(b1, b2)
  let total = list.length(pairs)

  case total {
    0 -> 1.0
    _ -> {
      let matching =
        list.count(pairs, fn(p) {
          color.is_similar(p.0, p.1, 5.0)
        })
      int.to_float(matching) /. int.to_float(total)
    }
  }
}
