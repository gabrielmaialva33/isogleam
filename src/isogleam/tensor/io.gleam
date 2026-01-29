//// IsoGleam Tensor IO
//// Load/save imagens PNG <-> viva_tensor usando ansel (libvips)
////
//// Pipeline: PNG file → ansel.Image → pixel matrix → viva_tensor.Tensor
////
//// Este módulo conecta o ecossistema de imagem (ansel/vips) com
//// o ecossistema de tensor (viva_tensor) - tudo em Pure Gleam!

import ansel
import ansel/color
import ansel/image
import gleam/int
import gleam/list
import gleam/result
import snag
import viva_tensor as t
import viva_tensor/tensor.{type Tensor, type TensorError}

/// Erro de IO de imagem
pub type ImageError {
  FileNotFound(path: String)
  InvalidFormat(reason: String)
  ReadError(reason: String)
  WriteError(reason: String)
  ConversionError(reason: String)
}

/// Converte snag.Snag para ImageError
fn snag_to_error(s: snag.Snag) -> ImageError {
  ReadError(snag.pretty_print(s))
}

// =============================================================================
// LOAD: PNG → Tensor
// =============================================================================

/// Carrega PNG para Tensor [H, W, 3]
/// Usa ansel (libvips) para decode eficiente
pub fn load_png(path: String) -> Result(Tensor, ImageError) {
  // 1. Ler imagem com ansel
  use img <- result.try(
    image.read(path)
    |> result.map_error(snag_to_error),
  )

  // 2. Converter para tensor
  image_to_tensor(img)
}

/// Converte ansel.Image para viva_tensor.Tensor [H, W, 3]
pub fn image_to_tensor(img: ansel.Image) -> Result(Tensor, ImageError) {
  // Pegar dimensões
  let width = image.get_width(img)
  let height = image.get_height(img)

  // Converter para matriz de pixels RGB
  use pixel_matrix <- result.try(
    image.to_pixel_matrix(img)
    |> result.map_error(snag_to_error),
  )

  // Achatar matriz para lista de floats [R, G, B, R, G, B, ...]
  let flat_data =
    pixel_matrix
    |> list.flatten()
    |> list.flat_map(fn(c) {
      let #(r, g, b) = color.to_rgb_tuple(c)
      [int.to_float(r), int.to_float(g), int.to_float(b)]
    })

  // Criar tensor [H, W, 3]
  let flat_tensor = t.from_list(flat_data)
  case t.reshape(flat_tensor, [height, width, 3]) {
    Ok(tensor) -> Ok(tensor)
    Error(e) -> Error(ConversionError(tensor_error_to_string(e)))
  }
}

// =============================================================================
// SAVE: Tensor → PNG
// =============================================================================

/// Salva Tensor [H, W, 3] como PNG
pub fn save_png(tensor: Tensor, path: String) -> Result(String, ImageError) {
  // 1. Converter tensor para imagem
  use img <- result.try(tensor_to_image(tensor))

  // 2. Remover extensão .png se presente (ansel adiciona automaticamente)
  let base_path = case strip_png_extension(path) {
    Ok(base) -> base
    Error(_) -> path
  }

  // 3. Salvar como PNG
  image.write(img, to: base_path, in: image.PNG)
  |> result.map_error(snag_to_error)
}

/// Remove extensão .png do path se presente
fn strip_png_extension(path: String) -> Result(String, Nil) {
  case string_ends_with(path, ".png") {
    True -> Ok(string_drop_right(path, 4))
    False -> Error(Nil)
  }
}

fn string_ends_with(s: String, suffix: String) -> Bool {
  let s_len = string_length(s)
  let suffix_len = string_length(suffix)
  case s_len >= suffix_len {
    True -> string_slice(s, s_len - suffix_len, suffix_len) == suffix
    False -> False
  }
}

@external(erlang, "string", "length")
fn string_length(s: String) -> Int

@external(erlang, "string", "slice")
fn string_slice(s: String, start: Int, length: Int) -> String

fn string_drop_right(s: String, n: Int) -> String {
  let len = string_length(s)
  string_slice(s, 0, len - n)
}

/// Converte viva_tensor.Tensor [H, W, 3] para ansel.Image
pub fn tensor_to_image(tensor: Tensor) -> Result(ansel.Image, ImageError) {
  let shape = t.shape(tensor)

  case shape {
    [height, width, 3] -> {
      // Converter tensor para lista de floats
      let data = t.to_list(tensor)

      // Converter para matriz de pixels
      let pixel_matrix = floats_to_pixel_matrix(data, width, height)

      // Criar imagem ansel
      image.from_pixel_matrix(pixel_matrix)
      |> result.map_error(snag_to_error)
    }
    [height, width] -> {
      // Grayscale - converter para RGB
      let data = t.to_list(tensor)
      let rgb_data =
        list.flat_map(data, fn(v) {
          let clamped = clamp_float(v, 0.0, 255.0)
          [clamped, clamped, clamped]
        })

      let pixel_matrix = floats_to_pixel_matrix(rgb_data, width, height)

      image.from_pixel_matrix(pixel_matrix)
      |> result.map_error(snag_to_error)
    }
    _ ->
      Error(ConversionError(
        "Expected [H, W, 3] or [H, W], got: " <> shape_to_string(shape),
      ))
  }
}

/// Converte lista flat de floats RGB para matriz de pixels
fn floats_to_pixel_matrix(
  data: List(Float),
  width: Int,
  height: Int,
) -> List(List(color.Color)) {
  // Converter para lista de cores RGB
  let colors = floats_to_colors(data, [])

  // Dividir em linhas de width pixels
  chunk_list(colors, width)
  |> list.take(height)
}

/// Converte [R, G, B, R, G, B, ...] para [RGB, RGB, ...]
fn floats_to_colors(
  data: List(Float),
  acc: List(color.Color),
) -> List(color.Color) {
  case data {
    [r, g, b, ..rest] -> {
      let color =
        color.RGB(
          r: float_to_byte(r),
          g: float_to_byte(g),
          b: float_to_byte(b),
        )
      floats_to_colors(rest, [color, ..acc])
    }
    _ -> list.reverse(acc)
  }
}

/// Converte float [0, 255] para int [0, 255]
fn float_to_byte(f: Float) -> Int {
  let clamped = clamp_float(f, 0.0, 255.0)
  float_round(clamped)
}

/// Clamp float entre min e max
fn clamp_float(v: Float, min: Float, max: Float) -> Float {
  case v <. min {
    True -> min
    False ->
      case v >. max {
        True -> max
        False -> v
      }
  }
}

/// Round float para int
fn float_round(f: Float) -> Int {
  // Erlang float_to_integer com round
  float_round_ffi(f)
}

@external(erlang, "erlang", "round")
fn float_round_ffi(f: Float) -> Int

/// Divide lista em chunks de tamanho n
fn chunk_list(items: List(a), n: Int) -> List(List(a)) {
  case items {
    [] -> []
    _ -> {
      let #(chunk, rest) = list.split(items, n)
      [chunk, ..chunk_list(rest, n)]
    }
  }
}

// =============================================================================
// HELPERS
// =============================================================================

/// Carrega imagem e normaliza para [0, 1]
pub fn load_normalized(path: String) -> Result(Tensor, ImageError) {
  use img <- result.try(load_png(path))
  Ok(t.scale(img, 1.0 /. 255.0))
}

/// Salva tensor normalizado [0, 1] como PNG
pub fn save_normalized(tensor: Tensor, path: String) -> Result(String, ImageError) {
  let denorm = t.scale(tensor, 255.0)
  let clamped = t.clamp(denorm, 0.0, 255.0)
  save_png(clamped, path)
}

/// Cria imagem vazia (zeros) com dimensões especificadas
pub fn empty_image(width: Int, height: Int, channels: Int) -> Tensor {
  t.zeros([height, width, channels])
}

/// Cria imagem preenchida com cor sólida RGB
pub fn solid_color(
  width: Int,
  height: Int,
  r: Float,
  g: Float,
  b: Float,
) -> Tensor {
  let pixel_count = width * height
  let pixels =
    list.repeat(#(r, g, b), pixel_count)
    |> list.flat_map(fn(p) {
      let #(r, g, b) = p
      [r, g, b]
    })

  let flat = t.from_list(pixels)
  case t.reshape(flat, [height, width, 3]) {
    Ok(tensor) -> tensor
    Error(_) -> empty_image(width, height, 3)
  }
}

/// Cria grid de teste (útil para debug)
pub fn test_grid(size: Int) -> Tensor {
  let pixels =
    list.range(0, size * size - 1)
    |> list.flat_map(fn(i) {
      let x = i % size
      let y = i / size
      let is_white = { x + y } % 2 == 0
      case is_white {
        True -> [255.0, 255.0, 255.0]
        False -> [0.0, 0.0, 0.0]
      }
    })

  let flat = t.from_list(pixels)
  case t.reshape(flat, [size, size, 3]) {
    Ok(tensor) -> tensor
    Error(_) -> empty_image(size, size, 3)
  }
}

/// Converte TensorError para String
fn tensor_error_to_string(e: TensorError) -> String {
  case e {
    tensor.ShapeMismatch(expected, got) ->
      "Shape mismatch: expected "
      <> shape_to_string(expected)
      <> ", got "
      <> shape_to_string(got)
    tensor.InvalidShape(reason) -> "Invalid shape: " <> reason
    tensor.DimensionError(reason) -> "Dimension error: " <> reason
    tensor.BroadcastError(a, b) ->
      "Broadcast error: " <> shape_to_string(a) <> " vs " <> shape_to_string(b)
  }
}

/// Converte shape para string
fn shape_to_string(shape: List(Int)) -> String {
  "["
  <> list.map(shape, int.to_string) |> string_join(", ")
  <> "]"
}

fn string_join(items: List(String), sep: String) -> String {
  case items {
    [] -> ""
    [x] -> x
    [x, ..rest] -> x <> sep <> string_join(rest, sep)
  }
}
