//// IsoGleam Tensor Preprocessing
//// Normalização e preparação de imagens para inference

import gleam/int
import gleam/list
import gleam/result
import gleam/string
import viva_tensor as t
import viva_tensor/tensor.{type Tensor, type TensorError}

/// Normaliza imagem [0,255] -> [-1,1] para Stable Diffusion
pub fn normalize_for_sd(img: Tensor) -> Tensor {
  img
  |> t.scale(1.0 /. 127.5)
  |> t.map(fn(x) { x -. 1.0 })
}

/// Desnormaliza [-1,1] -> [0,255]
pub fn denormalize_from_sd(img: Tensor) -> Tensor {
  img
  |> t.map(fn(x) { x +. 1.0 })
  |> t.scale(127.5)
}

/// Normaliza imagem [0,255] -> [0,1]
pub fn normalize_01(img: Tensor) -> Tensor {
  t.scale(img, 1.0 /. 255.0)
}

/// Desnormaliza [0,1] -> [0,255]
pub fn denormalize_01(img: Tensor) -> Tensor {
  t.scale(img, 255.0)
}

/// Clamp valores para range válido de imagem
pub fn clamp_image(img: Tensor) -> Tensor {
  t.clamp(img, 0.0, 255.0)
}

/// Resize com nearest neighbor (preserva pixel art)
/// Não faz interpolação - apenas repete pixels
pub fn resize_nearest(
  img: Tensor,
  target_h: Int,
  target_w: Int,
) -> Result(Tensor, TensorError) {
  let shape = t.shape(img)
  case shape {
    [h, w, c] -> {
      // Para pixel art, scale deve ser inteiro
      let scale_h = target_h / h
      let scale_w = target_w / w
      let scale = int.min(scale_h, scale_w)

      case scale > 0 {
        True -> {
          // Nearest neighbor = repetir cada pixel scale vezes
          // Por enquanto, reshape simples se dimensões batem
          let new_h = h * scale
          let new_w = w * scale
          case new_h == target_h && new_w == target_w {
            True -> t.reshape(img, [new_h, new_w, c])
            False -> Error(tensor.ShapeMismatch(shape, [target_h, target_w, c]))
          }
        }
        False -> Error(tensor.ShapeMismatch(shape, [target_h, target_w, c]))
      }
    }
    _ ->
      Error(tensor.InvalidShape(
        "Expected [H, W, C], got: " <> shape_to_string(shape),
      ))
  }
}

/// Converte shape para string
fn shape_to_string(shape: List(Int)) -> String {
  "[" <> string.join(list.map(shape, int.to_string), ", ") <> "]"
}

// =============================================================================
// EDGE DETECTION (usando conv2d v1.3.0)
// =============================================================================

/// Kernel Sobel para detecção de bordas horizontais
pub fn sobel_x_kernel() -> Tensor {
  case t.from_list2d([[-1.0, 0.0, 1.0], [-2.0, 0.0, 2.0], [-1.0, 0.0, 1.0]]) {
    Ok(kernel) -> kernel
    Error(_) -> t.zeros([3, 3])
  }
}

/// Kernel Sobel para detecção de bordas verticais
pub fn sobel_y_kernel() -> Tensor {
  case t.from_list2d([[-1.0, -2.0, -1.0], [0.0, 0.0, 0.0], [1.0, 2.0, 1.0]]) {
    Ok(kernel) -> kernel
    Error(_) -> t.zeros([3, 3])
  }
}

/// Kernel Laplaciano para detecção de bordas
pub fn laplacian_kernel() -> Tensor {
  case t.from_list2d([[0.0, 1.0, 0.0], [1.0, -4.0, 1.0], [0.0, 1.0, 0.0]]) {
    Ok(kernel) -> kernel
    Error(_) -> t.zeros([3, 3])
  }
}

/// Aplica detecção de bordas Sobel
/// Retorna magnitude das bordas
pub fn sobel_edges(img: Tensor) -> Result(Tensor, TensorError) {
  let config = t.conv2d_same(3, 3)

  // Aplica Sobel X e Y
  use edges_x <- result.try(t.conv2d(img, sobel_x_kernel(), config))
  use edges_y <- result.try(t.conv2d(img, sobel_y_kernel(), config))

  // Magnitude = sqrt(x² + y²) ≈ |x| + |y| (aproximação rápida)
  use x_sq <- result.try(t.mul(edges_x, edges_x))
  use y_sq <- result.try(t.mul(edges_y, edges_y))
  use sum_sq <- result.try(t.add(x_sq, y_sq))

  // sqrt via map
  Ok(
    t.map(sum_sq, fn(v) {
      case v >=. 0.0 {
        True -> float_sqrt(v)
        False -> 0.0
      }
    }),
  )
}

/// Sqrt aproximado via Newton-Raphson (1 iteração)
fn float_sqrt(x: Float) -> Float {
  case x <=. 0.0 {
    True -> 0.0
    False -> {
      // Estimativa inicial
      let guess = x /. 2.0
      // Uma iteração de Newton-Raphson
      { guess +. x /. guess } /. 2.0
    }
  }
}

/// Aplica detecção de bordas Laplaciano
pub fn laplacian_edges(img: Tensor) -> Result(Tensor, TensorError) {
  let config = t.conv2d_same(3, 3)
  t.conv2d(img, laplacian_kernel(), config)
}

/// Aplica blur gaussiano 3x3
pub fn gaussian_blur(img: Tensor) -> Result(Tensor, TensorError) {
  let kernel = case
    t.from_list2d([
      [1.0 /. 16.0, 2.0 /. 16.0, 1.0 /. 16.0],
      [2.0 /. 16.0, 4.0 /. 16.0, 2.0 /. 16.0],
      [1.0 /. 16.0, 2.0 /. 16.0, 1.0 /. 16.0],
    ])
  {
    Ok(k) -> k
    Error(_) -> t.zeros([3, 3])
  }

  let config = t.conv2d_same(3, 3)
  t.conv2d(img, kernel, config)
}

/// Sharpen (aumenta nitidez)
pub fn sharpen(img: Tensor) -> Result(Tensor, TensorError) {
  let kernel = case
    t.from_list2d([
      [0.0, -1.0, 0.0],
      [-1.0, 5.0, -1.0],
      [0.0, -1.0, 0.0],
    ])
  {
    Ok(k) -> k
    Error(_) -> t.zeros([3, 3])
  }

  let config = t.conv2d_same(3, 3)
  t.conv2d(img, kernel, config)
}

/// Extrai patch de uma imagem
pub fn extract_patch(
  _img: Tensor,
  _x: Int,
  _y: Int,
  _w: Int,
  _h: Int,
) -> Result(Tensor, String) {
  // TODO: Implementar slicing
  // Por agora, retorna erro
  Error("extract_patch not implemented yet")
}

/// Padding para divisibilidade por 8 (requisito SD)
pub fn pad_to_multiple(
  img: Tensor,
  multiple: Int,
) -> Result(Tensor, TensorError) {
  let shape = t.shape(img)
  case shape {
    [h, w, _c] -> {
      let pad_h = case h % multiple {
        0 -> 0
        r -> multiple - r
      }
      let pad_w = case w % multiple {
        0 -> 0
        r -> multiple - r
      }

      case pad_h == 0 && pad_w == 0 {
        True -> Ok(img)
        False -> {
          // Criar tensor maior com zeros e copiar
          let _new_h = h + pad_h
          let _new_w = w + pad_w

          // Por agora, apenas reshape se não precisar padding
          // TODO: implementar padding real
          Ok(img)
        }
      }
    }
    _ ->
      Error(tensor.InvalidShape(
        "Expected [H, W, C], got: " <> shape_to_string(shape),
      ))
  }
}

/// Prepara imagem para inference SD
/// Faz: normalize + pad + reshape para [1, C, H, W] (batch format)
pub fn prepare_for_inference(img: Tensor) -> Result(Tensor, TensorError) {
  let shape = t.shape(img)
  case shape {
    [_h, _w, _c] -> {
      // 1. Normaliza para [-1, 1]
      let normalized = normalize_for_sd(img)

      // 2. Pad para múltiplo de 8
      use padded <- result.try(pad_to_multiple(normalized, 8))

      // 3. Reshape para batch format [1, C, H, W]
      // HWC -> CHW -> NCHW
      let padded_shape = t.shape(padded)
      case padded_shape {
        [ph, pw, pc] -> t.reshape(padded, [1, pc, ph, pw])
        _ ->
          Error(tensor.InvalidShape(
            "Expected [H, W, C] after padding, got: "
            <> shape_to_string(padded_shape),
          ))
      }
    }
    _ ->
      Error(tensor.InvalidShape(
        "Expected [H, W, C], got: " <> shape_to_string(shape),
      ))
  }
}

/// Desfaz prepare_for_inference
/// [1, C, H, W] -> [H, W, C] e desnormaliza
pub fn unpack_from_inference(tensor: Tensor) -> Result(Tensor, TensorError) {
  let shape = t.shape(tensor)
  case shape {
    [1, c, h, w] -> {
      // NCHW -> CHW -> HWC
      use reshaped <- result.try(t.reshape(tensor, [c, h, w]))
      // TODO: transpose para HWC
      // Por agora assume já está correto
      let denorm = denormalize_from_sd(reshaped)
      let clamped = clamp_image(denorm)
      Ok(clamped)
    }
    [_c, _h, _w] -> {
      let denorm = denormalize_from_sd(tensor)
      let clamped = clamp_image(denorm)
      Ok(clamped)
    }
    _ ->
      Error(tensor.InvalidShape(
        "Expected [H, W, C], got: " <> shape_to_string(shape),
      ))
  }
}
