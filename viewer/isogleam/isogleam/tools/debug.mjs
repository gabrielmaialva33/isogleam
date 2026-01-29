import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  CustomType as $CustomType,
  remainderInt,
  divideFloat,
  divideInt,
} from "../../gleam.mjs";
import * as $checker from "../../isogleam/qa/checker.mjs";
import * as $color from "../../isogleam/qa/color.mjs";
import { RGB } from "../../isogleam/qa/color.mjs";
import * as $pixel from "../../isogleam/qa/pixel.mjs";

export class TerminalAnsi extends $CustomType {}
export const DebugFormat$TerminalAnsi = () => new TerminalAnsi();
export const DebugFormat$isTerminalAnsi = (value) =>
  value instanceof TerminalAnsi;

export class HtmlReport extends $CustomType {}
export const DebugFormat$HtmlReport = () => new HtmlReport();
export const DebugFormat$isHtmlReport = (value) => value instanceof HtmlReport;

export class JsonData extends $CustomType {}
export const DebugFormat$JsonData = () => new JsonData();
export const DebugFormat$isJsonData = (value) => value instanceof JsonData;

export class PlainText extends $CustomType {}
export const DebugFormat$PlainText = () => new PlainText();
export const DebugFormat$isPlainText = (value) => value instanceof PlainText;

export class HistogramEntry extends $CustomType {
  constructor(color, count, percentage) {
    super();
    this.color = color;
    this.count = count;
    this.percentage = percentage;
  }
}
export const HistogramEntry$HistogramEntry = (color, count, percentage) =>
  new HistogramEntry(color, count, percentage);
export const HistogramEntry$isHistogramEntry = (value) =>
  value instanceof HistogramEntry;
export const HistogramEntry$HistogramEntry$color = (value) => value.color;
export const HistogramEntry$HistogramEntry$0 = (value) => value.color;
export const HistogramEntry$HistogramEntry$count = (value) => value.count;
export const HistogramEntry$HistogramEntry$1 = (value) => value.count;
export const HistogramEntry$HistogramEntry$percentage = (value) =>
  value.percentage;
export const HistogramEntry$HistogramEntry$2 = (value) => value.percentage;

function sample_border_colors(img, start_x, start_y, width, height) {
  let step = $int.max(1, globalThis.Math.trunc(width * height / 10));
  return $list.filter_map(
    $list.range(0, 9),
    (i) => {
      let offset = i * step;
      let x = start_x + (remainderInt(offset, width));
      let y = start_y + (divideInt(offset, width));
      return $pixel.get_pixel(img, x, y);
    },
  );
}

function format_checks_plain(checks) {
  let _pipe = $list.map(
    checks,
    (c) => {
      let _block;
      let $ = c.passed;
      if ($) {
        _block = "[OK]";
      } else {
        _block = "[FAIL]";
      }
      let status = _block;
      return ((((("  " + status) + " ") + c.name) + ": ") + c.message) + "\n";
    },
  );
  return $string.concat(_pipe);
}

function format_checks_ansi(checks) {
  let reset = "\u{001b}[0m";
  let _pipe = $list.map(
    checks,
    (c) => {
      let _block;
      let $ = c.passed;
      if ($) {
        _block = "\u{001b}[32m[OK]\u{001b}[0m";
      } else {
        _block = "\u{001b}[31m[FAIL]\u{001b}[0m";
      }
      let status = _block;
      return (((((("  " + status) + " ") + c.name) + ": ") + c.message) + reset) + "\n";
    },
  );
  return $string.concat(_pipe);
}

function format_errors_plain(errors) {
  let $ = $list.length(errors);
  if ($ === 0) {
    return "  (none)\n";
  } else {
    let _pipe = $list.map(errors, (e) => { return ("  - " + e) + "\n"; });
    return $string.concat(_pipe);
  }
}

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
  let $ = index < 0;
  if ($) {
    return new Error(undefined);
  } else {
    return do_list_at(l, index);
  }
}

/**
 * Generate ASCII art preview of tile
 */
export function ascii_preview(img, scale) {
  let chars = " .:-=+*#%@";
  let char_list = $string.to_graphemes(chars);
  let _pipe = $list.index_map(
    img.pixels,
    (px, idx) => {
      let x = remainderInt(idx, img.width);
      let $ = divideInt(idx, img.width);
      
      let $1 = ((remainderInt(x, scale)) === 0) && ((remainderInt(
        idx,
        img.width * scale
      )) < img.width);
      if ($1) {
        let brightness = globalThis.Math.trunc(((px.r + px.g) + px.b) / 3);
        let char_idx = globalThis.Math.trunc((brightness * 9) / 255);
        let $2 = list_at(char_list, char_idx);
        if ($2 instanceof Ok) {
          let c = $2[0];
          return c;
        } else {
          return " ";
        }
      } else {
        return "";
      }
    },
  );
  let _pipe$1 = $list.filter(_pipe, (s) => { return s !== ""; });
  let _pipe$2 = $list.index_map(
    _pipe$1,
    (c, i) => {
      let $ = (remainderInt(i, (divideInt(img.width, scale)))) === ((divideInt(
        img.width,
        scale
      )) - 1);
      if ($) {
        return c + "\n";
      } else {
        return c;
      }
    },
  );
  return $string.concat(_pipe$2);
}

function int_to_str(n) {
  return $int.to_string(n);
}

function colors_to_ansi(colors) {
  let _pipe = $list.map(
    colors,
    (c) => {
      return ((((("\u{001b}[48;2;" + int_to_str(c.r)) + ";") + int_to_str(c.g)) + ";") + int_to_str(
        c.b,
      )) + "m  \u{001b}[0m";
    },
  );
  return $string.concat(_pipe);
}

/**
 * Generate border visualization
 */
export function visualize_borders(img) {
  let w = img.width;
  let h = img.height;
  let north = sample_border_colors(img, 0, 0, w, 1);
  let south = sample_border_colors(img, 0, h - 1, w, 1);
  let east = sample_border_colors(img, w - 1, 0, 1, h);
  let west = sample_border_colors(img, 0, 0, 1, h);
  return $string.concat(
    toList([
      "North: ",
      colors_to_ansi(north),
      "\n",
      "South: ",
      colors_to_ansi(south),
      "\n",
      "East:  ",
      colors_to_ansi(east),
      "\n",
      "West:  ",
      colors_to_ansi(west),
      "\n",
    ]),
  );
}

function rgb_to_key(c) {
  return (((int_to_str(c.r) + ",") + int_to_str(c.g)) + ",") + int_to_str(c.b);
}

function str_to_int(s) {
  let $ = $int.parse(s);
  if ($ instanceof Ok) {
    let n = $[0];
    return n;
  } else {
    return 0;
  }
}

function key_to_rgb(key) {
  let $ = $string.split(key, ",");
  if ($ instanceof $Empty) {
    return new RGB(0, 0, 0);
  } else {
    let $1 = $.tail;
    if ($1 instanceof $Empty) {
      return new RGB(0, 0, 0);
    } else {
      let $2 = $1.tail;
      if ($2 instanceof $Empty) {
        return new RGB(0, 0, 0);
      } else {
        let $3 = $2.tail;
        if ($3 instanceof $Empty) {
          let r_str = $.head;
          let g_str = $1.head;
          let b_str = $2.head;
          let r = str_to_int(r_str);
          let g = str_to_int(g_str);
          let b = str_to_int(b_str);
          return new RGB(r, g, b);
        } else {
          return new RGB(0, 0, 0);
        }
      }
    }
  }
}

/**
 * Generate color histogram
 */
export function color_histogram(img) {
  let counts = $list.fold(
    img.pixels,
    $dict.new$(),
    (acc, px) => {
      let key = rgb_to_key(px);
      let _block;
      let $ = $dict.get(acc, key);
      if ($ instanceof Ok) {
        let n = $[0];
        _block = n;
      } else {
        _block = 0;
      }
      let current = _block;
      return $dict.insert(acc, key, current + 1);
    },
  );
  let total = $list.length(img.pixels);
  let _pipe = $dict.to_list(counts);
  let _pipe$1 = $list.map(
    _pipe,
    (item) => {
      let key;
      let count;
      key = item[0];
      count = item[1];
      let color = key_to_rgb(key);
      let _block;
      if (total === 0) {
        _block = 0.0;
      } else {
        let n = total;
        _block = (divideFloat($int.to_float(count), $int.to_float(n))) * 100.0;
      }
      let pct = _block;
      return new HistogramEntry(color, count, pct);
    },
  );
  return $list.sort(
    _pipe$1,
    (a, b) => { return $int.compare(b.count, a.count); },
  );
}

function float_to_str(f) {
  let int_part = $float.truncate(f);
  let dec_part = $float.truncate((f - $int.to_float(int_part)) * 100.0);
  return (int_to_str(int_part) + ".") + int_to_str(
    $int.absolute_value(dec_part),
  );
}

function format_qa_plain(result) {
  let _block;
  let $ = result.passed;
  if ($) {
    _block = "PASSED";
  } else {
    _block = "FAILED";
  }
  let status = _block;
  return $string.concat(
    toList([
      "=== QA Result ===\n",
      "Status: ",
      status,
      "\n",
      "Overall Score: ",
      float_to_str(result.score * 100.0),
      "%\n",
      "\nChecks:\n",
      format_checks_plain(result.checks),
      "\nErrors:\n",
      format_errors_plain(result.errors),
    ]),
  );
}

function score_to_ansi(score) {
  let reset = "\u{001b}[0m";
  let _block;
  let $ = score >= 0.9;
  if ($) {
    _block = "\u{001b}[32m";
  } else {
    let $1 = score >= 0.7;
    if ($1) {
      _block = "\u{001b}[33m";
    } else {
      _block = "\u{001b}[31m";
    }
  }
  let color = _block;
  return ((color + float_to_str(score * 100.0)) + "%") + reset;
}

function format_qa_ansi(result) {
  let _block;
  let $ = result.passed;
  if ($) {
    _block = "\u{001b}[32m";
  } else {
    _block = "\u{001b}[31m";
  }
  let status_color = _block;
  let reset = "\u{001b}[0m";
  let _block$1;
  let $1 = result.passed;
  if ($1) {
    _block$1 = "PASSED";
  } else {
    _block$1 = "FAILED";
  }
  let status = _block$1;
  return $string.concat(
    toList([
      "=== QA Result ===\n",
      "Status: ",
      status_color,
      status,
      reset,
      "\n",
      "Score: ",
      score_to_ansi(result.score),
      "\n",
      "\nChecks:\n",
      format_checks_ansi(result.checks),
    ]),
  );
}

function format_qa_html(result) {
  let _block;
  let $ = result.passed;
  if ($) {
    _block = "passed";
  } else {
    _block = "failed";
  }
  let status_class = _block;
  return $string.concat(
    toList([
      "<div class=\"qa-result ",
      status_class,
      "\">\n",
      "  <h2>QA Result</h2>\n",
      "  <p class=\"score\">Score: ",
      float_to_str(result.score * 100.0),
      "%</p>\n",
      "</div>\n",
    ]),
  );
}

/**
 * Tile diff visualization
 */
export function diff_tiles(a, b) {
  let total = $int.min($list.length(a.pixels), $list.length(b.pixels));
  let diffs = $list.map2(
    a.pixels,
    b.pixels,
    (px_a, px_b) => {
      let dr = $int.absolute_value(px_a.r - px_b.r);
      let dg = $int.absolute_value(px_a.g - px_b.g);
      let db = $int.absolute_value(px_a.b - px_b.b);
      return globalThis.Math.trunc(((dr + dg) + db) / 3);
    },
  );
  let total_diff = $list.fold(diffs, 0, (acc, d) => { return acc + d; });
  let _block;
  if (total === 0) {
    _block = 0.0;
  } else {
    let n = total;
    _block = divideFloat($int.to_float(total_diff), $int.to_float(n));
  }
  let avg_diff = _block;
  let changed_pixels = $list.fold(
    diffs,
    0,
    (acc, d) => {
      let $ = d > 10;
      if ($) {
        return acc + 1;
      } else {
        return acc;
      }
    },
  );
  return $string.concat(
    toList([
      "Tile Diff:\n",
      "  Pixels compared: ",
      int_to_str(total),
      "\n",
      "  Average diff: ",
      float_to_str(avg_diff),
      "\n",
      "  Changed pixels: ",
      int_to_str(changed_pixels),
      " (",
      float_to_str(
        (divideFloat($int.to_float(changed_pixels), $int.to_float(total))) * 100.0,
      ),
      "%)\n",
    ]),
  );
}

function bool_to_str(b) {
  if (b) {
    return "true";
  } else {
    return "false";
  }
}

function format_qa_json(result) {
  return $string.concat(
    toList([
      "{\"passed\":",
      bool_to_str(result.passed),
      ",",
      "\"score\":",
      float_to_str(result.score),
      ",",
      "\"checks\":",
      int_to_str($list.length(result.checks)),
      ",",
      "\"errors\":",
      int_to_str($list.length(result.errors)),
      "}",
    ]),
  );
}

/**
 * Generate QA result summary
 */
export function format_qa_result(result, format) {
  if (format instanceof TerminalAnsi) {
    return format_qa_ansi(result);
  } else if (format instanceof HtmlReport) {
    return format_qa_html(result);
  } else if (format instanceof JsonData) {
    return format_qa_json(result);
  } else {
    return format_qa_plain(result);
  }
}
