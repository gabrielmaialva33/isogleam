/// IsoGleam Tools - Debug Module
/// Micro-tools for tile inspection and debugging
/// Inspired by Isometric NYC's approach of building many small tools
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/string
import isogleam/qa/checker.{type Check, type QAResult}
import isogleam/qa/color.{type RGB, RGB}
import isogleam/qa/pixel.{type ImageData}

/// Debug output formats
pub type DebugFormat {
  /// Terminal ANSI output
  TerminalAnsi
  /// HTML report
  HtmlReport
  /// JSON data
  JsonData
  /// Plain text
  PlainText
}

/// Color histogram entry
pub type HistogramEntry {
  HistogramEntry(color: RGB, count: Int, percentage: Float)
}

/// Generate color histogram
pub fn color_histogram(img: ImageData) -> List(HistogramEntry) {
  let counts =
    list.fold(img.pixels, dict.new(), fn(acc, px) {
      let key = rgb_to_key(px)
      let current = case dict.get(acc, key) {
        Ok(n) -> n
        Error(_) -> 0
      }
      dict.insert(acc, key, current + 1)
    })

  let total = list.length(img.pixels)

  dict.to_list(counts)
  |> list.map(fn(item) {
    let #(key, count) = item
    let color = key_to_rgb(key)
    let pct = case total {
      0 -> 0.0
      n -> int.to_float(count) /. int.to_float(n) *. 100.0
    }
    HistogramEntry(color, count, pct)
  })
  |> list.sort(fn(a, b) { int.compare(b.count, a.count) })
}

/// Generate ASCII art preview of tile
pub fn ascii_preview(img: ImageData, scale: Int) -> String {
  let chars = " .:-=+*#%@"
  let char_list = string.to_graphemes(chars)

  list.index_map(img.pixels, fn(px, idx) {
    let x = idx % img.width
    let _y = idx / img.width

    // Only sample every N pixels
    case x % scale == 0 && idx % { img.width * scale } < img.width {
      False -> ""
      True -> {
        // Brightness to char
        let brightness = { px.r + px.g + px.b } / 3
        let char_idx = brightness * 9 / 255
        case list_at(char_list, char_idx) {
          Ok(c) -> c
          Error(_) -> " "
        }
      }
    }
  })
  |> list.filter(fn(s) { s != "" })
  |> list.index_map(fn(c, i) {
    case i % { img.width / scale } == { img.width / scale - 1 } {
      True -> c <> "\n"
      False -> c
    }
  })
  |> string.concat
}

/// Generate border visualization
pub fn visualize_borders(img: ImageData) -> String {
  let w = img.width
  let h = img.height

  let north = sample_border_colors(img, 0, 0, w, 1)
  let south = sample_border_colors(img, 0, h - 1, w, 1)
  let east = sample_border_colors(img, w - 1, 0, 1, h)
  let west = sample_border_colors(img, 0, 0, 1, h)

  string.concat([
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
  ])
}

fn sample_border_colors(
  img: ImageData,
  start_x: Int,
  start_y: Int,
  width: Int,
  height: Int,
) -> List(RGB) {
  // Sample 10 evenly spaced colors
  let step = int.max(1, { width * height } / 10)
  list.filter_map(list.range(0, 9), fn(i) {
    let offset = i * step
    let x = start_x + { offset % width }
    let y = start_y + { offset / width }
    pixel.get_pixel(img, x, y)
  })
}

fn colors_to_ansi(colors: List(RGB)) -> String {
  list.map(colors, fn(c) {
    // ANSI 24-bit color block
    "\u{001b}[48;2;"
    <> int_to_str(c.r)
    <> ";"
    <> int_to_str(c.g)
    <> ";"
    <> int_to_str(c.b)
    <> "m  \u{001b}[0m"
  })
  |> string.concat
}

/// Generate QA result summary
pub fn format_qa_result(result: QAResult, format: DebugFormat) -> String {
  case format {
    PlainText -> format_qa_plain(result)
    TerminalAnsi -> format_qa_ansi(result)
    HtmlReport -> format_qa_html(result)
    JsonData -> format_qa_json(result)
  }
}

fn format_qa_plain(result: QAResult) -> String {
  let status = case result.passed {
    True -> "PASSED"
    False -> "FAILED"
  }

  string.concat([
    "=== QA Result ===\n",
    "Status: ",
    status,
    "\n",
    "Overall Score: ",
    float_to_str(result.score *. 100.0),
    "%\n",
    "\nChecks:\n",
    format_checks_plain(result.checks),
    "\nErrors:\n",
    format_errors_plain(result.errors),
  ])
}

fn format_qa_ansi(result: QAResult) -> String {
  let status_color = case result.passed {
    True -> "\u{001b}[32m"
    // Green
    False -> "\u{001b}[31m"
    // Red
  }
  let reset = "\u{001b}[0m"

  let status = case result.passed {
    True -> "PASSED"
    False -> "FAILED"
  }

  string.concat([
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
  ])
}

fn score_to_ansi(score: Float) -> String {
  let reset = "\u{001b}[0m"
  let color = case score >=. 0.9 {
    True -> "\u{001b}[32m"
    // Green
    False ->
      case score >=. 0.7 {
        True -> "\u{001b}[33m"
        // Yellow
        False -> "\u{001b}[31m"
        // Red
      }
  }
  color <> float_to_str(score *. 100.0) <> "%" <> reset
}

fn format_qa_html(result: QAResult) -> String {
  let status_class = case result.passed {
    True -> "passed"
    False -> "failed"
  }

  string.concat([
    "<div class=\"qa-result ",
    status_class,
    "\">\n",
    "  <h2>QA Result</h2>\n",
    "  <p class=\"score\">Score: ",
    float_to_str(result.score *. 100.0),
    "%</p>\n",
    "</div>\n",
  ])
}

fn format_qa_json(result: QAResult) -> String {
  string.concat([
    "{\"passed\":",
    bool_to_str(result.passed),
    ",",
    "\"score\":",
    float_to_str(result.score),
    ",",
    "\"checks\":",
    int_to_str(list.length(result.checks)),
    ",",
    "\"errors\":",
    int_to_str(list.length(result.errors)),
    "}",
  ])
}

fn format_checks_plain(checks: List(Check)) -> String {
  list.map(checks, fn(c) {
    let status = case c.passed {
      True -> "[OK]"
      False -> "[FAIL]"
    }
    "  " <> status <> " " <> c.name <> ": " <> c.message <> "\n"
  })
  |> string.concat
}

fn format_checks_ansi(checks: List(Check)) -> String {
  let reset = "\u{001b}[0m"

  list.map(checks, fn(c) {
    let status = case c.passed {
      True -> "\u{001b}[32m[OK]\u{001b}[0m"
      False -> "\u{001b}[31m[FAIL]\u{001b}[0m"
    }
    "  " <> status <> " " <> c.name <> ": " <> c.message <> reset <> "\n"
  })
  |> string.concat
}

fn format_errors_plain(errors: List(String)) -> String {
  case list.length(errors) {
    0 -> "  (none)\n"
    _ -> {
      list.map(errors, fn(e) { "  - " <> e <> "\n" })
      |> string.concat
    }
  }
}

/// Tile diff visualization
pub fn diff_tiles(a: ImageData, b: ImageData) -> String {
  let total = int.min(list.length(a.pixels), list.length(b.pixels))

  let diffs =
    list.map2(a.pixels, b.pixels, fn(px_a, px_b) {
      let dr = int.absolute_value(px_a.r - px_b.r)
      let dg = int.absolute_value(px_a.g - px_b.g)
      let db = int.absolute_value(px_a.b - px_b.b)
      { dr + dg + db } / 3
    })

  let total_diff = list.fold(diffs, 0, fn(acc, d) { acc + d })
  let avg_diff = case total {
    0 -> 0.0
    n -> int.to_float(total_diff) /. int.to_float(n)
  }

  let changed_pixels =
    list.fold(diffs, 0, fn(acc, d) {
      case d > 10 {
        True -> acc + 1
        False -> acc
      }
    })

  string.concat([
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
    float_to_str(int.to_float(changed_pixels) /. int.to_float(total) *. 100.0),
    "%)\n",
  ])
}

// Helpers
fn rgb_to_key(c: RGB) -> String {
  int_to_str(c.r) <> "," <> int_to_str(c.g) <> "," <> int_to_str(c.b)
}

fn key_to_rgb(key: String) -> RGB {
  case string.split(key, ",") {
    [r_str, g_str, b_str] -> {
      let r = str_to_int(r_str)
      let g = str_to_int(g_str)
      let b = str_to_int(b_str)
      RGB(r, g, b)
    }
    _ -> RGB(0, 0, 0)
  }
}

fn list_at(l: List(a), index: Int) -> Result(a, Nil) {
  case index < 0 {
    True -> Error(Nil)
    False -> do_list_at(l, index)
  }
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

// Replaced externals with pure Gleam
fn int_to_str(n: Int) -> String {
  int.to_string(n)
}

fn str_to_int(s: String) -> Int {
  case int.parse(s) {
    Ok(n) -> n
    Error(_) -> 0
  }
}

fn float_to_str(f: Float) -> String {
  let int_part = float.truncate(f)
  let dec_part = float.truncate({ f -. int.to_float(int_part) } *. 100.0)
  int_to_str(int_part) <> "." <> int_to_str(int.absolute_value(dec_part))
}

fn bool_to_str(b: Bool) -> String {
  case b {
    True -> "true"
    False -> "false"
  }
}
