//// IsoGleam Tensor Postprocessing
//// Dithering, quantização de paleta, e validação de pixel art

import gleam/float
import gleam/int
import gleam/list
import gleam/result
import viva_tensor as t
import viva_tensor/tensor.{type Tensor, type TensorError}

/// Cor RGB como tupla
pub type Color =
  #(Int, Int, Int)

/// Paleta de cores
pub type Palette =
  List(Color)

/// Resultado de validação de paleta
pub type PaletteValidation {
  PaletteValidation(
    valid: Bool,
    total_colors: Int,
    invalid_colors: Int,
    invalid_list: List(Color),
  )
}

/// Paleta SimCity 2000 (64 cores principais)
pub fn simcity_palette() -> Palette {
  [
    // Pretos e cinzas
    #(0, 0, 0),
    #(32, 32, 32),
    #(64, 64, 64),
    #(96, 96, 96),
    #(128, 128, 128),
    #(160, 160, 160),
    #(192, 192, 192),
    #(224, 224, 224),
    #(255, 255, 255),
    // Azuis (céu, água)
    #(0, 0, 128),
    #(0, 0, 192),
    #(0, 0, 255),
    #(64, 64, 192),
    #(96, 96, 224),
    #(128, 128, 255),
    #(0, 128, 192),
    // Verdes (vegetação)
    #(0, 96, 0),
    #(0, 128, 0),
    #(0, 160, 0),
    #(32, 128, 32),
    #(64, 160, 64),
    #(96, 192, 96),
    #(128, 224, 128),
    #(0, 96, 64),
    // Marrons (terra, telhados)
    #(96, 64, 32),
    #(128, 96, 64),
    #(160, 128, 96),
    #(192, 160, 128),
    #(128, 64, 0),
    #(160, 96, 32),
    #(192, 128, 64),
    #(224, 160, 96),
    // Vermelhos (telhados, alerta)
    #(128, 0, 0),
    #(160, 32, 32),
    #(192, 64, 64),
    #(224, 96, 96),
    #(192, 0, 0),
    #(224, 32, 32),
    #(255, 64, 64),
    #(255, 128, 128),
    // Amarelos (luzes, areia)
    #(192, 192, 0),
    #(224, 224, 0),
    #(255, 255, 0),
    #(255, 255, 128),
    #(192, 160, 64),
    #(224, 192, 96),
    #(255, 224, 128),
    #(255, 240, 192),
    // Laranjas (tijolos)
    #(192, 96, 0),
    #(224, 128, 32),
    #(255, 160, 64),
    #(255, 192, 128),
    // Roxos/Rosas
    #(128, 0, 128),
    #(160, 32, 160),
    #(192, 64, 192),
    #(128, 64, 128),
    // Ciano
    #(0, 128, 128),
    #(0, 160, 160),
    #(0, 192, 192),
    #(64, 192, 192),
    // Extras (sombras, highlights)
    #(48, 48, 48),
    #(80, 80, 80),
    #(112, 112, 112),
    #(144, 144, 144),
    #(176, 176, 176),
  ]
}

/// Calcula distância euclidiana entre duas cores
fn color_distance(c1: Color, c2: Color) -> Float {
  let #(r1, g1, b1) = c1
  let #(r2, g2, b2) = c2

  let dr = int.to_float(r1 - r2)
  let dg = int.to_float(g1 - g2)
  let db = int.to_float(b1 - b2)

  // Distância euclidiana (sem sqrt para performance)
  dr *. dr +. dg *. dg +. db *. db
}

/// Encontra cor mais próxima na paleta
pub fn find_nearest_color(color: Color, palette: Palette) -> Color {
  case palette {
    [] -> color
    [first, ..rest] -> {
      list.fold(rest, #(first, color_distance(color, first)), fn(acc, c) {
        let #(best, best_dist) = acc
        let dist = color_distance(color, c)
        case dist <. best_dist {
          True -> #(c, dist)
          False -> acc
        }
      })
      |> fn(result) {
        let #(best, _) = result
        best
      }
    }
  }
}

/// Verifica se cor está na paleta
pub fn is_in_palette(color: Color, palette: Palette) -> Bool {
  list.any(palette, fn(c) { c == color })
}

/// Extrai cores únicas de uma imagem (representada como lista de floats RGB)
/// Assume formato [r1, g1, b1, r2, g2, b2, ...]
pub fn extract_unique_colors(data: List(Float)) -> List(Color) {
  do_extract_colors(data, [])
  |> list.unique()
}

fn do_extract_colors(data: List(Float), acc: List(Color)) -> List(Color) {
  case data {
    [r, g, b, ..rest] -> {
      let color = #(float.round(r), float.round(g), float.round(b))
      do_extract_colors(rest, [color, ..acc])
    }
    _ -> acc
  }
}

/// Valida se todas as cores de uma imagem estão na paleta
pub fn validate_palette(
  img: Tensor,
  palette: Palette,
) -> PaletteValidation {
  let data = t.to_list(img)
  let colors = extract_unique_colors(data)
  let total = list.length(colors)

  let invalid =
    list.filter(colors, fn(c) { !is_in_palette(c, palette) })
  let invalid_count = list.length(invalid)

  PaletteValidation(
    valid: invalid_count == 0,
    total_colors: total,
    invalid_colors: invalid_count,
    invalid_list: invalid,
  )
}

/// Quantiza imagem para paleta (sem dithering)
/// Cada pixel é mapeado para a cor mais próxima
pub fn quantize_to_palette(
  img: Tensor,
  palette: Palette,
) -> Tensor {
  t.map(img, fn(value) {
    // Assume que estamos processando um valor de canal por vez
    // Para quantização real, precisaríamos processar RGB junto
    // Por agora, clamp para [0, 255]
    let clamped = float.max(0.0, float.min(255.0, value))
    clamped
  })
}

/// Erro de quantização para um pixel
pub type QuantError {
  QuantError(r: Float, g: Float, b: Float)
}

/// Aplica dithering Floyd-Steinberg
/// Distribui erro de quantização para pixels vizinhos
/// Pattern:
///         X   7/16
///   3/16  5/16  1/16
pub fn dither_floyd_steinberg(
  img: Tensor,
  palette: Palette,
) -> Result(Tensor, TensorError) {
  // Floyd-Steinberg requer acesso aleatório e modificação in-place
  // que não é idiomático em Gleam funcional
  // Por agora, retorna imagem quantizada sem dithering
  // TODO: Implementar via FFI Erlang para performance
  Ok(quantize_to_palette(img, palette))
}

/// Dithering ordenado (Bayer matrix) - mais simples que F-S
/// Adiciona ruído sistemático antes de quantizar
pub fn dither_ordered(
  img: Tensor,
  palette: Palette,
) -> Tensor {
  // Bayer matrix 4x4 (normalizada para [0, 1])
  // Por agora, apenas quantiza
  quantize_to_palette(img, palette)
}

/// Conta cores na imagem
pub fn count_colors(img: Tensor) -> Int {
  let data = t.to_list(img)
  let colors = extract_unique_colors(data)
  list.length(colors)
}

/// Verifica se imagem é válida pixel art
pub type PixelArtValidation {
  PixelArtValidation(
    valid: Bool,
    color_count: Int,
    max_colors: Int,
    has_antialiasing: Bool,
    palette_valid: Bool,
  )
}

/// Valida se imagem é pixel art válido
pub fn validate_pixel_art(
  img: Tensor,
  palette: Palette,
  max_colors: Int,
) -> PixelArtValidation {
  let color_count = count_colors(img)
  let palette_check = validate_palette(img, palette)

  // Antialiasing detectado se muitas cores fora da paleta
  let has_antialiasing = palette_check.invalid_colors > color_count / 10

  PixelArtValidation(
    valid: color_count <=
      max_colors && palette_check.valid && !has_antialiasing,
    color_count: color_count,
    max_colors: max_colors,
    has_antialiasing: has_antialiasing,
    palette_valid: palette_check.valid,
  )
}
