/// IsoGleam QA - Dithering Module
/// Bayer matrix dithering detection and validation
/// Reference: RetroArch bayer-matrix-dithering.glsl by Martins Upitis

import gleam/list
import gleam/int

/// 8x8 Bayer ordered dithering matrix
/// Each value is 0-63, representing threshold levels
const bayer_8x8: List(List(Int)) = [
  [0, 32, 8, 40, 2, 34, 10, 42],
  [48, 16, 56, 24, 50, 18, 58, 26],
  [12, 44, 4, 36, 14, 46, 6, 38],
  [60, 28, 52, 20, 62, 30, 54, 22],
  [3, 35, 11, 43, 1, 33, 9, 41],
  [51, 19, 59, 27, 49, 17, 57, 25],
  [15, 47, 7, 39, 13, 45, 5, 37],
  [63, 31, 55, 23, 61, 29, 53, 21],
]

/// 4x4 Bayer matrix (for smaller patterns)
const bayer_4x4: List(List(Int)) = [
  [0, 8, 2, 10],
  [12, 4, 14, 6],
  [3, 11, 1, 9],
  [15, 7, 13, 5],
]

/// Get Bayer threshold at position
pub fn get_threshold_8x8(x: Int, y: Int) -> Float {
  let row_idx = y % 8
  let col_idx = x % 8

  case list_at(bayer_8x8, row_idx) {
    Ok(row) -> {
      case list_at(row, col_idx) {
        Ok(val) -> int.to_float(val + 1) /. 64.0
        Error(_) -> 0.5
      }
    }
    Error(_) -> 0.5
  }
}

/// Get 4x4 Bayer threshold
pub fn get_threshold_4x4(x: Int, y: Int) -> Float {
  let row_idx = y % 4
  let col_idx = x % 4

  case list_at(bayer_4x4, row_idx) {
    Ok(row) -> {
      case list_at(row, col_idx) {
        Ok(val) -> int.to_float(val + 1) /. 16.0
        Error(_) -> 0.5
      }
    }
    Error(_) -> 0.5
  }
}

/// Check if a color transition follows Bayer dithering pattern
/// Returns a score from 0.0 (no dithering) to 1.0 (perfect dithering)
pub fn detect_bayer_pattern(
  intensities: List(Float),
  width: Int,
) -> Float {
  // For each pixel, check if its intensity matches Bayer threshold
  let matches = list.index_fold(intensities, 0, fn(acc, intensity, idx) {
    let x = idx % width
    let y = idx / width
    let threshold = get_threshold_8x8(x, y)

    // Check if pixel is "on" (above threshold) or "off" (below)
    // In dithered images, pixels follow the Bayer pattern
    let expected_on = intensity >. 0.5
    let would_be_on = intensity >. threshold

    case expected_on == would_be_on {
      True -> acc + 1
      False -> acc
    }
  })

  let total = list.length(intensities)
  case total {
    0 -> 0.0
    _ -> int.to_float(matches) /. int.to_float(total)
  }
}

/// Apply Bayer dithering to a value (for generation)
pub fn dither_value(value: Float, x: Int, y: Int) -> Bool {
  let threshold = get_threshold_8x8(x, y)
  value >. threshold
}

/// Detect if image uses smooth gradients (anti-dithering)
/// Returns percentage of smooth gradient areas (0.0 = all dithered, 1.0 = all smooth)
pub fn detect_smooth_gradient(
  values: List(Float),
  width: Int,
) -> Float {
  // Smooth gradients have consecutive values with small differences
  let smooth_count = list.index_fold(values, 0, fn(acc, value, idx) {
    let x = idx % width

    case x > 0 {
      False -> acc
      True -> {
        case list_at(values, idx - 1) {
          Ok(prev_value) -> {
            let diff = float_abs(value -. prev_value)
            // If difference is small but non-zero, it's a smooth gradient
            case diff >. 0.001 && diff <. 0.1 {
              True -> acc + 1
              False -> acc
            }
          }
          Error(_) -> acc
        }
      }
    }
  })

  let total = list.length(values) - width  // Exclude first column
  case total > 0 {
    True -> int.to_float(smooth_count) /. int.to_float(total)
    False -> 0.0
  }
}

// Helper functions
fn list_at(l: List(a), index: Int) -> Result(a, Nil) {
  do_list_at(l, index)
}

fn do_list_at(l: List(a), index: Int) -> Result(a, Nil) {
  case l {
    [] -> Error(Nil)
    [first, ..rest] -> {
      case index == 0 {
        True -> Ok(first)
        False -> do_list_at(rest, index - 1)
      }
    }
  }
}

fn float_abs(x: Float) -> Float {
  case x <. 0.0 {
    True -> 0.0 -. x
    False -> x
  }
}
