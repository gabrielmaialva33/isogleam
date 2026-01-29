/// IsoGleam QA - Color Module
/// Pure Gleam color types and operations
import gleam/int
import gleam/list
import gleam/string

/// RGB color (0-255 per channel)
pub type RGB {
  RGB(r: Int, g: Int, b: Int)
}

/// RGBA color with alpha
pub type RGBA {
  RGBA(r: Int, g: Int, b: Int, a: Int)
}

/// Hex color string
pub type Hex =
  String

/// Create RGB from tuple
pub fn from_tuple(t: #(Int, Int, Int)) -> RGB {
  RGB(t.0, t.1, t.2)
}

/// Convert RGB to tuple
pub fn to_tuple(c: RGB) -> #(Int, Int, Int) {
  #(c.r, c.g, c.b)
}

/// Convert RGB to hex string
pub fn to_hex(c: RGB) -> Hex {
  "#" <> int_to_hex(c.r) <> int_to_hex(c.g) <> int_to_hex(c.b)
}

/// Convert hex string to RGB
pub fn from_hex(hex: Hex) -> Result(RGB, Nil) {
  let clean = case string.starts_with(hex, "#") {
    True -> string.drop_start(hex, 1)
    False -> hex
  }

  case string.length(clean) {
    6 -> {
      let r_hex = string.slice(clean, 0, 2)
      let g_hex = string.slice(clean, 2, 2)
      let b_hex = string.slice(clean, 4, 2)

      case hex_to_int(r_hex), hex_to_int(g_hex), hex_to_int(b_hex) {
        Ok(r), Ok(g), Ok(b) -> Ok(RGB(r, g, b))
        _, _, _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// Calculate color distance (Euclidean in RGB space)
pub fn distance(c1: RGB, c2: RGB) -> Float {
  let dr = int.to_float(c1.r - c2.r)
  let dg = int.to_float(c1.g - c2.g)
  let db = int.to_float(c1.b - c2.b)
  sqrt(dr *. dr +. dg *. dg +. db *. db)
}

/// Check if two colors are similar (within threshold)
/// Uses <= to include exact matches when threshold is 0.0
pub fn is_similar(c1: RGB, c2: RGB, threshold: Float) -> Bool {
  distance(c1, c2) <=. threshold
}

/// Check if color is in palette (within threshold)
pub fn in_palette(color: RGB, palette: List(RGB), threshold: Float) -> Bool {
  list.any(palette, fn(p) { is_similar(color, p, threshold) })
}

/// Find closest color in palette
pub fn closest_in_palette(color: RGB, palette: List(RGB)) -> Result(RGB, Nil) {
  case palette {
    [] -> Error(Nil)
    [first, ..rest] -> {
      let closest =
        list.fold(rest, #(first, distance(color, first)), fn(acc, p) {
          let d = distance(color, p)
          case d <. acc.1 {
            True -> #(p, d)
            False -> acc
          }
        })
      Ok(closest.0)
    }
  }
}

/// Check if color is grayscale
pub fn is_grayscale(c: RGB) -> Bool {
  c.r == c.g && c.g == c.b
}

/// Calculate luminance (0.0 - 1.0)
pub fn luminance(c: RGB) -> Float {
  let r = int.to_float(c.r) /. 255.0
  let g = int.to_float(c.g) /. 255.0
  let b = int.to_float(c.b) /. 255.0
  0.2126 *. r +. 0.7152 *. g +. 0.0722 *. b
}

// --- Internal helpers ---

fn int_to_hex(n: Int) -> String {
  let hex_chars = "0123456789ABCDEF"
  let high = n / 16
  let low = n % 16
  string.slice(hex_chars, high, 1) <> string.slice(hex_chars, low, 1)
}

fn hex_to_int(hex: String) -> Result(Int, Nil) {
  let chars = string.uppercase(hex)
  case string.to_graphemes(chars) {
    [h, l] -> {
      case hex_char_to_int(h), hex_char_to_int(l) {
        Ok(high), Ok(low) -> Ok(high * 16 + low)
        _, _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

fn hex_char_to_int(c: String) -> Result(Int, Nil) {
  case c {
    "0" -> Ok(0)
    "1" -> Ok(1)
    "2" -> Ok(2)
    "3" -> Ok(3)
    "4" -> Ok(4)
    "5" -> Ok(5)
    "6" -> Ok(6)
    "7" -> Ok(7)
    "8" -> Ok(8)
    "9" -> Ok(9)
    "A" -> Ok(10)
    "B" -> Ok(11)
    "C" -> Ok(12)
    "D" -> Ok(13)
    "E" -> Ok(14)
    "F" -> Ok(15)
    _ -> Error(Nil)
  }
}

// FFI for sqrt
@external(erlang, "math", "sqrt")
@external(javascript, "../qa/color_ffi.mjs", "sqrt")
fn sqrt(x: Float) -> Float
