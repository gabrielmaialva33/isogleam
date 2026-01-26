/// IsoGleam QA - Palette Module
/// Palette analysis and validation

import gleam/dict.{type Dict}
import gleam/list
import gleam/int
import isogleam/qa/color.{type RGB, RGB}

/// Palette definition
pub type Palette {
  Palette(
    name: String,
    colors: List(RGB),
    max_colors: Int,
  )
}

/// Palette analysis result
pub type PaletteAnalysis {
  PaletteAnalysis(
    total_pixels: Int,
    unique_colors: Int,
    colors_in_palette: Int,
    colors_outside_palette: Int,
    color_histogram: Dict(String, Int),
    invalid_colors: List(RGB),
    score: Float,
  )
}

/// Create IsoGleam default palette from palette.json
pub fn isogleam_palette() -> Palette {
  let colors = [
    // Terrain - Grass
    RGB(74, 112, 35), RGB(92, 138, 47), RGB(61, 92, 28),
    // Terrain - Dirt
    RGB(139, 115, 85), RGB(107, 83, 68), RGB(160, 128, 96),
    // Terrain - Asphalt
    RGB(60, 60, 60), RGB(74, 74, 74), RGB(46, 46, 46),
    // Terrain - Sidewalk
    RGB(184, 184, 184), RGB(158, 158, 158), RGB(202, 202, 202),
    // Buildings - Roof Colonial
    RGB(184, 76, 46), RGB(139, 58, 34), RGB(212, 90, 53),
    // Buildings - Wall White
    RGB(245, 245, 240), RGB(232, 232, 224), RGB(255, 255, 255),
    // Buildings - Wall Yellow
    RGB(232, 209, 116), RGB(196, 168, 53), RGB(240, 224, 144),
    // Buildings - Wood
    RGB(107, 68, 35), RGB(74, 48, 24), RGB(139, 90, 43),
    // Vegetation - Tree Dark
    RGB(45, 80, 22), RGB(30, 59, 15), RGB(58, 102, 32),
    // Vegetation - Tree Light
    RGB(74, 128, 40), RGB(92, 154, 53), RGB(58, 106, 28),
    // Vegetation - Mata Atlantica
    RGB(30, 77, 43), RGB(43, 94, 56), RGB(21, 61, 32),
    // Water - River
    RGB(46, 91, 138), RGB(30, 74, 117), RGB(62, 107, 154), RGB(26, 58, 92),
    // Effects
    RGB(0, 0, 0),
  ]

  Palette(name: "isogleam-capao-v1", colors: colors, max_colors: 64)
}

/// Create palette from hex list
pub fn from_hex_list(name: String, hexes: List(String)) -> Palette {
  let colors =
    list.filter_map(hexes, fn(hex) {
      color.from_hex(hex)
    })

  Palette(name: name, colors: colors, max_colors: list.length(colors))
}

/// Analyze image colors against palette
pub fn analyze(
  pixels: List(RGB),
  palette: Palette,
  tolerance: Float,
) -> PaletteAnalysis {
  let total = list.length(pixels)

  // Build histogram
  let histogram =
    list.fold(pixels, dict.new(), fn(acc, pixel) {
      let key = color.to_hex(pixel)
      let count = case dict.get(acc, key) {
        Ok(n) -> n + 1
        Error(_) -> 1
      }
      dict.insert(acc, key, count)
    })

  let unique = dict.size(histogram)

  // Check each unique color against palette
  let unique_colors = dict.keys(histogram)
    |> list.filter_map(fn(hex) {
      color.from_hex(hex)
    })

  let #(in_palette, outside) =
    list.partition(unique_colors, fn(c) {
      color.in_palette(c, palette.colors, tolerance)
    })

  let in_count = list.length(in_palette)
  let out_count = list.length(outside)

  // Score: percentage of colors in palette
  let score = case unique {
    0 -> 1.0
    _ -> int.to_float(in_count) /. int.to_float(unique)
  }

  PaletteAnalysis(
    total_pixels: total,
    unique_colors: unique,
    colors_in_palette: in_count,
    colors_outside_palette: out_count,
    color_histogram: histogram,
    invalid_colors: outside,
    score: score,
  )
}

/// Check if palette is valid (within max colors)
pub fn is_valid(analysis: PaletteAnalysis, max_colors: Int) -> Bool {
  analysis.unique_colors <= max_colors && analysis.colors_outside_palette == 0
}

/// Get top N most used colors
pub fn top_colors(analysis: PaletteAnalysis, n: Int) -> List(#(String, Int)) {
  dict.to_list(analysis.color_histogram)
  |> list.sort(fn(a, b) { int.compare(b.1, a.1) })
  |> list.take(n)
}
