import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { Ok, toList, CustomType as $CustomType, divideFloat } from "../../gleam.mjs";
import * as $color from "../../isogleam/qa/color.mjs";
import { RGB } from "../../isogleam/qa/color.mjs";

export class Palette extends $CustomType {
  constructor(name, colors, max_colors) {
    super();
    this.name = name;
    this.colors = colors;
    this.max_colors = max_colors;
  }
}
export const Palette$Palette = (name, colors, max_colors) =>
  new Palette(name, colors, max_colors);
export const Palette$isPalette = (value) => value instanceof Palette;
export const Palette$Palette$name = (value) => value.name;
export const Palette$Palette$0 = (value) => value.name;
export const Palette$Palette$colors = (value) => value.colors;
export const Palette$Palette$1 = (value) => value.colors;
export const Palette$Palette$max_colors = (value) => value.max_colors;
export const Palette$Palette$2 = (value) => value.max_colors;

export class PaletteAnalysis extends $CustomType {
  constructor(total_pixels, unique_colors, colors_in_palette, colors_outside_palette, color_histogram, invalid_colors, score) {
    super();
    this.total_pixels = total_pixels;
    this.unique_colors = unique_colors;
    this.colors_in_palette = colors_in_palette;
    this.colors_outside_palette = colors_outside_palette;
    this.color_histogram = color_histogram;
    this.invalid_colors = invalid_colors;
    this.score = score;
  }
}
export const PaletteAnalysis$PaletteAnalysis = (total_pixels, unique_colors, colors_in_palette, colors_outside_palette, color_histogram, invalid_colors, score) =>
  new PaletteAnalysis(total_pixels,
  unique_colors,
  colors_in_palette,
  colors_outside_palette,
  color_histogram,
  invalid_colors,
  score);
export const PaletteAnalysis$isPaletteAnalysis = (value) =>
  value instanceof PaletteAnalysis;
export const PaletteAnalysis$PaletteAnalysis$total_pixels = (value) =>
  value.total_pixels;
export const PaletteAnalysis$PaletteAnalysis$0 = (value) => value.total_pixels;
export const PaletteAnalysis$PaletteAnalysis$unique_colors = (value) =>
  value.unique_colors;
export const PaletteAnalysis$PaletteAnalysis$1 = (value) => value.unique_colors;
export const PaletteAnalysis$PaletteAnalysis$colors_in_palette = (value) =>
  value.colors_in_palette;
export const PaletteAnalysis$PaletteAnalysis$2 = (value) =>
  value.colors_in_palette;
export const PaletteAnalysis$PaletteAnalysis$colors_outside_palette = (value) =>
  value.colors_outside_palette;
export const PaletteAnalysis$PaletteAnalysis$3 = (value) =>
  value.colors_outside_palette;
export const PaletteAnalysis$PaletteAnalysis$color_histogram = (value) =>
  value.color_histogram;
export const PaletteAnalysis$PaletteAnalysis$4 = (value) =>
  value.color_histogram;
export const PaletteAnalysis$PaletteAnalysis$invalid_colors = (value) =>
  value.invalid_colors;
export const PaletteAnalysis$PaletteAnalysis$5 = (value) =>
  value.invalid_colors;
export const PaletteAnalysis$PaletteAnalysis$score = (value) => value.score;
export const PaletteAnalysis$PaletteAnalysis$6 = (value) => value.score;

/**
 * Create IsoGleam default palette from palette.json
 */
export function isogleam_palette() {
  let colors = toList([
    new RGB(74, 112, 35),
    new RGB(92, 138, 47),
    new RGB(61, 92, 28),
    new RGB(139, 115, 85),
    new RGB(107, 83, 68),
    new RGB(160, 128, 96),
    new RGB(60, 60, 60),
    new RGB(74, 74, 74),
    new RGB(46, 46, 46),
    new RGB(184, 184, 184),
    new RGB(158, 158, 158),
    new RGB(202, 202, 202),
    new RGB(184, 76, 46),
    new RGB(139, 58, 34),
    new RGB(212, 90, 53),
    new RGB(245, 245, 240),
    new RGB(232, 232, 224),
    new RGB(255, 255, 255),
    new RGB(232, 209, 116),
    new RGB(196, 168, 53),
    new RGB(240, 224, 144),
    new RGB(107, 68, 35),
    new RGB(74, 48, 24),
    new RGB(139, 90, 43),
    new RGB(45, 80, 22),
    new RGB(30, 59, 15),
    new RGB(58, 102, 32),
    new RGB(74, 128, 40),
    new RGB(92, 154, 53),
    new RGB(58, 106, 28),
    new RGB(30, 77, 43),
    new RGB(43, 94, 56),
    new RGB(21, 61, 32),
    new RGB(46, 91, 138),
    new RGB(30, 74, 117),
    new RGB(62, 107, 154),
    new RGB(26, 58, 92),
    new RGB(0, 0, 0),
  ]);
  return new Palette("isogleam-capao-v1", colors, 64);
}

/**
 * Create palette from hex list
 */
export function from_hex_list(name, hexes) {
  let colors = $list.filter_map(
    hexes,
    (hex) => { return $color.from_hex(hex); },
  );
  return new Palette(name, colors, $list.length(colors));
}

/**
 * Analyze image colors against palette
 */
export function analyze(pixels, palette, tolerance) {
  let total = $list.length(pixels);
  let histogram = $list.fold(
    pixels,
    $dict.new$(),
    (acc, pixel) => {
      let key = $color.to_hex(pixel);
      let _block;
      let $ = $dict.get(acc, key);
      if ($ instanceof Ok) {
        let n = $[0];
        _block = n + 1;
      } else {
        _block = 1;
      }
      let count = _block;
      return $dict.insert(acc, key, count);
    },
  );
  let unique = $dict.size(histogram);
  let _block;
  let _pipe = $dict.keys(histogram);
  _block = $list.filter_map(_pipe, (hex) => { return $color.from_hex(hex); });
  let unique_colors = _block;
  let $ = $list.partition(
    unique_colors,
    (c) => { return $color.in_palette(c, palette.colors, tolerance); },
  );
  let in_palette;
  let outside;
  in_palette = $[0];
  outside = $[1];
  let in_count = $list.length(in_palette);
  let out_count = $list.length(outside);
  let _block$1;
  if (unique === 0) {
    _block$1 = 1.0;
  } else {
    _block$1 = divideFloat($int.to_float(in_count), $int.to_float(unique));
  }
  let score = _block$1;
  return new PaletteAnalysis(
    total,
    unique,
    in_count,
    out_count,
    histogram,
    outside,
    score,
  );
}

/**
 * Check if palette is valid (within max colors)
 */
export function is_valid(analysis, max_colors) {
  return (analysis.unique_colors <= max_colors) && (analysis.colors_outside_palette === 0);
}

/**
 * Get top N most used colors
 */
export function top_colors(analysis, n) {
  let _pipe = $dict.to_list(analysis.color_histogram);
  let _pipe$1 = $list.sort(
    _pipe,
    (a, b) => { return $int.compare(b[1], a[1]); },
  );
  return $list.take(_pipe$1, n);
}
