import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, Empty as $Empty, CustomType as $CustomType } from "../../gleam.mjs";
import { sqrt } from "../qa/color_ffi.mjs";

export class RGB extends $CustomType {
  constructor(r, g, b) {
    super();
    this.r = r;
    this.g = g;
    this.b = b;
  }
}
export const RGB$RGB = (r, g, b) => new RGB(r, g, b);
export const RGB$isRGB = (value) => value instanceof RGB;
export const RGB$RGB$r = (value) => value.r;
export const RGB$RGB$0 = (value) => value.r;
export const RGB$RGB$g = (value) => value.g;
export const RGB$RGB$1 = (value) => value.g;
export const RGB$RGB$b = (value) => value.b;
export const RGB$RGB$2 = (value) => value.b;

export class RGBA extends $CustomType {
  constructor(r, g, b, a) {
    super();
    this.r = r;
    this.g = g;
    this.b = b;
    this.a = a;
  }
}
export const RGBA$RGBA = (r, g, b, a) => new RGBA(r, g, b, a);
export const RGBA$isRGBA = (value) => value instanceof RGBA;
export const RGBA$RGBA$r = (value) => value.r;
export const RGBA$RGBA$0 = (value) => value.r;
export const RGBA$RGBA$g = (value) => value.g;
export const RGBA$RGBA$1 = (value) => value.g;
export const RGBA$RGBA$b = (value) => value.b;
export const RGBA$RGBA$2 = (value) => value.b;
export const RGBA$RGBA$a = (value) => value.a;
export const RGBA$RGBA$3 = (value) => value.a;

/**
 * Create RGB from tuple
 */
export function from_tuple(t) {
  return new RGB(t[0], t[1], t[2]);
}

/**
 * Convert RGB to tuple
 */
export function to_tuple(c) {
  return [c.r, c.g, c.b];
}

/**
 * Check if color is grayscale
 */
export function is_grayscale(c) {
  return (c.r === c.g) && (c.g === c.b);
}

/**
 * Calculate luminance (0.0 - 1.0)
 */
export function luminance(c) {
  let r = $int.to_float(c.r) / 255.0;
  let g = $int.to_float(c.g) / 255.0;
  let b = $int.to_float(c.b) / 255.0;
  return ((0.2126 * r) + (0.7152 * g)) + (0.0722 * b);
}

function int_to_hex(n) {
  let hex_chars = "0123456789ABCDEF";
  let high = globalThis.Math.trunc(n / 16);
  let low = n % 16;
  return $string.slice(hex_chars, high, 1) + $string.slice(hex_chars, low, 1);
}

/**
 * Convert RGB to hex string
 */
export function to_hex(c) {
  return (("#" + int_to_hex(c.r)) + int_to_hex(c.g)) + int_to_hex(c.b);
}

function hex_char_to_int(c) {
  if (c === "0") {
    return new Ok(0);
  } else if (c === "1") {
    return new Ok(1);
  } else if (c === "2") {
    return new Ok(2);
  } else if (c === "3") {
    return new Ok(3);
  } else if (c === "4") {
    return new Ok(4);
  } else if (c === "5") {
    return new Ok(5);
  } else if (c === "6") {
    return new Ok(6);
  } else if (c === "7") {
    return new Ok(7);
  } else if (c === "8") {
    return new Ok(8);
  } else if (c === "9") {
    return new Ok(9);
  } else if (c === "A") {
    return new Ok(10);
  } else if (c === "B") {
    return new Ok(11);
  } else if (c === "C") {
    return new Ok(12);
  } else if (c === "D") {
    return new Ok(13);
  } else if (c === "E") {
    return new Ok(14);
  } else if (c === "F") {
    return new Ok(15);
  } else {
    return new Error(undefined);
  }
}

function hex_to_int(hex) {
  let chars = $string.uppercase(hex);
  let $ = $string.to_graphemes(chars);
  if ($ instanceof $Empty) {
    return new Error(undefined);
  } else {
    let $1 = $.tail;
    if ($1 instanceof $Empty) {
      return new Error(undefined);
    } else {
      let $2 = $1.tail;
      if ($2 instanceof $Empty) {
        let h = $.head;
        let l = $1.head;
        let $3 = hex_char_to_int(h);
        let $4 = hex_char_to_int(l);
        if ($3 instanceof Ok && $4 instanceof Ok) {
          let high = $3[0];
          let low = $4[0];
          return new Ok(high * 16 + low);
        } else {
          return new Error(undefined);
        }
      } else {
        return new Error(undefined);
      }
    }
  }
}

/**
 * Convert hex string to RGB
 */
export function from_hex(hex) {
  let _block;
  let $ = $string.starts_with(hex, "#");
  if ($) {
    _block = $string.drop_start(hex, 1);
  } else {
    _block = hex;
  }
  let clean = _block;
  let $1 = $string.length(clean);
  if ($1 === 6) {
    let r_hex = $string.slice(clean, 0, 2);
    let g_hex = $string.slice(clean, 2, 2);
    let b_hex = $string.slice(clean, 4, 2);
    let $2 = hex_to_int(r_hex);
    let $3 = hex_to_int(g_hex);
    let $4 = hex_to_int(b_hex);
    if ($2 instanceof Ok && $3 instanceof Ok && $4 instanceof Ok) {
      let r = $2[0];
      let g = $3[0];
      let b = $4[0];
      return new Ok(new RGB(r, g, b));
    } else {
      return new Error(undefined);
    }
  } else {
    return new Error(undefined);
  }
}

/**
 * Calculate color distance (Euclidean in RGB space)
 */
export function distance(c1, c2) {
  let dr = $int.to_float(c1.r - c2.r);
  let dg = $int.to_float(c1.g - c2.g);
  let db = $int.to_float(c1.b - c2.b);
  return sqrt(((dr * dr) + (dg * dg)) + (db * db));
}

/**
 * Check if two colors are similar (within threshold)
 * Uses <= to include exact matches when threshold is 0.0
 */
export function is_similar(c1, c2, threshold) {
  return distance(c1, c2) <= threshold;
}

/**
 * Check if color is in palette (within threshold)
 */
export function in_palette(color, palette, threshold) {
  return $list.any(palette, (p) => { return is_similar(color, p, threshold); });
}

/**
 * Find closest color in palette
 */
export function closest_in_palette(color, palette) {
  if (palette instanceof $Empty) {
    return new Error(undefined);
  } else {
    let first = palette.head;
    let rest = palette.tail;
    let closest = $list.fold(
      rest,
      [first, distance(color, first)],
      (acc, p) => {
        let d = distance(color, p);
        let $ = d < acc[1];
        if ($) {
          return [p, d];
        } else {
          return acc;
        }
      },
    );
    return new Ok(closest[0]);
  }
}
