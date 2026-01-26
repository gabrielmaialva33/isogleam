/// IsoGleam QA - Pixel Module
/// PNG reading and pixel extraction via Erlang FFI

import gleam/list
import isogleam/qa/color.{type RGB, RGB}

/// Image dimensions
pub type ImageSize {
  ImageSize(width: Int, height: Int)
}

/// Image data
pub type ImageData {
  ImageData(
    width: Int,
    height: Int,
    pixels: List(RGB),
    raw_bytes: BitArray,
  )
}

/// Error types
pub type ImageError {
  FileNotFound
  InvalidFormat
  DecodeFailed(String)
}

/// Read PNG file and extract pixels
pub fn read_png(path: String) -> Result(ImageData, ImageError) {
  case read_file(path) {
    Error(_) -> Error(FileNotFound)
    Ok(bytes) -> decode_png(bytes)
  }
}

/// Decode PNG bytes to image data
pub fn decode_png(bytes: BitArray) -> Result(ImageData, ImageError) {
  case png_decode(bytes) {
    Error(reason) -> Error(DecodeFailed(reason))
    Ok(#(width, height, color_type, pixel_bytes)) -> {
      // ColorType: 0=Gray, 2=RGB, 4=GrayA, 6=RGBA
      let bpp = case color_type {
        0 -> 1  // Grayscale
        2 -> 3  // RGB
        4 -> 2  // Grayscale + Alpha
        6 -> 4  // RGBA
        _ -> 3  // Default to RGB
      }
      let pixels = bytes_to_rgb_list(pixel_bytes, bpp)
      Ok(ImageData(
        width: width,
        height: height,
        pixels: pixels,
        raw_bytes: pixel_bytes,
      ))
    }
  }
}

/// Get pixel at coordinates
pub fn get_pixel(img: ImageData, x: Int, y: Int) -> Result(RGB, Nil) {
  case x >= 0 && x < img.width && y >= 0 && y < img.height {
    False -> Error(Nil)
    True -> {
      let index = y * img.width + x
      list_at(img.pixels, index)
    }
  }
}

/// Get image dimensions
pub fn size(img: ImageData) -> ImageSize {
  ImageSize(img.width, img.height)
}

/// Check if image has correct dimensions
pub fn check_size(img: ImageData, expected_width: Int, expected_height: Int) -> Bool {
  img.width == expected_width && img.height == expected_height
}

/// Extract border pixels (for seamless checking)
pub fn get_border(img: ImageData, side: BorderSide, thickness: Int) -> List(RGB) {
  case side {
    North -> get_north_border(img, thickness)
    South -> get_south_border(img, thickness)
    East -> get_east_border(img, thickness)
    West -> get_west_border(img, thickness)
  }
}

pub type BorderSide {
  North
  South
  East
  West
}

fn get_north_border(img: ImageData, thickness: Int) -> List(RGB) {
  list.flat_map(list.range(0, thickness - 1), fn(y) {
    list.filter_map(list.range(0, img.width - 1), fn(x) {
      get_pixel(img, x, y)     })
  })
}

fn get_south_border(img: ImageData, thickness: Int) -> List(RGB) {
  let start_y = img.height - thickness
  list.flat_map(list.range(start_y, img.height - 1), fn(y) {
    list.filter_map(list.range(0, img.width - 1), fn(x) {
      get_pixel(img, x, y)     })
  })
}

fn get_east_border(img: ImageData, thickness: Int) -> List(RGB) {
  let start_x = img.width - thickness
  list.flat_map(list.range(0, img.height - 1), fn(y) {
    list.filter_map(list.range(start_x, img.width - 1), fn(x) {
      get_pixel(img, x, y)     })
  })
}

fn get_west_border(img: ImageData, thickness: Int) -> List(RGB) {
  list.flat_map(list.range(0, img.height - 1), fn(y) {
    list.filter_map(list.range(0, thickness - 1), fn(x) {
      get_pixel(img, x, y)     })
  })
}

/// Convert raw bytes to RGB list based on bytes per pixel
fn bytes_to_rgb_list(bytes: BitArray, bpp: Int) -> List(RGB) {
  case bpp {
    1 -> do_bytes_to_rgb_gray(bytes, [])
    2 -> do_bytes_to_rgb_graya(bytes, [])
    3 -> do_bytes_to_rgb_rgb(bytes, [])
    4 -> do_bytes_to_rgb_rgba(bytes, [])
    _ -> do_bytes_to_rgb_rgb(bytes, [])  // Default
  }
  |> list.reverse
}

fn do_bytes_to_rgb_gray(bytes: BitArray, acc: List(RGB)) -> List(RGB) {
  case bytes {
    <<g:8, rest:bits>> -> do_bytes_to_rgb_gray(rest, [RGB(g, g, g), ..acc])
    _ -> acc
  }
}

fn do_bytes_to_rgb_graya(bytes: BitArray, acc: List(RGB)) -> List(RGB) {
  case bytes {
    <<g:8, _a:8, rest:bits>> -> do_bytes_to_rgb_graya(rest, [RGB(g, g, g), ..acc])
    _ -> acc
  }
}

fn do_bytes_to_rgb_rgb(bytes: BitArray, acc: List(RGB)) -> List(RGB) {
  case bytes {
    <<r:8, g:8, b:8, rest:bits>> -> do_bytes_to_rgb_rgb(rest, [RGB(r, g, b), ..acc])
    _ -> acc
  }
}

fn do_bytes_to_rgb_rgba(bytes: BitArray, acc: List(RGB)) -> List(RGB) {
  case bytes {
    <<r:8, g:8, b:8, _a:8, rest:bits>> -> do_bytes_to_rgb_rgba(rest, [RGB(r, g, b), ..acc])
    _ -> acc
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

// --- Erlang FFI ---

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, String)

@external(erlang, "png_ffi", "decode")
fn png_decode(bytes: BitArray) -> Result(#(Int, Int, Int, BitArray), String)
