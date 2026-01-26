/// Infill - Algoritmo de geração com contexto dos vizinhos
/// SEGREDO do Isometric NYC: nunca gera tile isolado!
///
/// Estratégia:
/// 1. Começa pelos cantos (menos vizinhos)
/// 2. Expande em espiral para o centro
/// 3. Cada tile gerado inclui bordas dos vizinhos como contexto
/// 4. Máscara de 25% nas bordas para blending seamless

import gleam/list
import gleam/int
import gleam/option.{type Option, None, Some}
import isogleam/tile.{type Tile, type IsoCoord, IsoCoord, Generated}
import isogleam/grid.{type Grid}

/// Estratégia de geração
pub type GenerationStrategy {
  /// Começa pelo canto e expande (padrão)
  SpiralFromCorner
  /// Começa pelo centro e expande
  SpiralFromCenter
  /// Ordem aleatória (não recomendado)
  Random
  /// Por linhas (mais simples, menos qualidade)
  RowByRow
}

/// Contexto de infill para um tile
pub type InfillContext {
  InfillContext(
    target: Tile,
    north_border: Option(BorderData),
    south_border: Option(BorderData),
    east_border: Option(BorderData),
    west_border: Option(BorderData),
    mask_percentage: Float,
  )
}

/// Dados da borda de um tile vizinho
pub type BorderData {
  BorderData(
    image_path: String,
    edge: Edge,
    width: Int,
  )
}

/// Qual borda do vizinho usar
pub type Edge {
  TopEdge
  BottomEdge
  LeftEdge
  RightEdge
}

/// Ordem de geração dos tiles usando estratégia spiral
pub fn generation_order(g: Grid, strategy: GenerationStrategy) -> List(IsoCoord) {
  case strategy {
    SpiralFromCorner -> spiral_from_corner(g.width, g.height)
    SpiralFromCenter -> spiral_from_center(g.width, g.height)
    RowByRow -> row_by_row(g.width, g.height)
    Random -> row_by_row(g.width, g.height)  // TODO: shuffle
  }
}

/// Gera ordem espiral começando do canto (0,0)
fn spiral_from_corner(width: Int, height: Int) -> List(IsoCoord) {
  // Implementação simplificada: prioriza tiles com menor x+y (distância do canto)
  let all_coords =
    list.range(0, width - 1)
    |> list.flat_map(fn(x) {
      list.range(0, height - 1)
      |> list.map(fn(y) { IsoCoord(x, y) })
    })

  // Ordena por distância Manhattan do canto (0,0)
  list.sort(all_coords, fn(a, b) {
    let dist_a = a.x + a.y
    let dist_b = b.x + b.y
    int.compare(dist_a, dist_b)
  })
}

/// Gera ordem espiral começando do centro
fn spiral_from_center(width: Int, height: Int) -> List(IsoCoord) {
  let center_x = width / 2
  let center_y = height / 2

  let all_coords =
    list.range(0, width - 1)
    |> list.flat_map(fn(x) {
      list.range(0, height - 1)
      |> list.map(fn(y) { IsoCoord(x, y) })
    })

  // Ordena por distância do centro
  list.sort(all_coords, fn(a, b) {
    let dist_a = abs(a.x - center_x) + abs(a.y - center_y)
    let dist_b = abs(b.x - center_x) + abs(b.y - center_y)
    int.compare(dist_a, dist_b)
  })
}

fn abs(n: Int) -> Int {
  case n < 0 {
    True -> -n
    False -> n
  }
}

/// Gera ordem linha por linha
fn row_by_row(width: Int, height: Int) -> List(IsoCoord) {
  list.range(0, height - 1)
  |> list.flat_map(fn(y) {
    list.range(0, width - 1)
    |> list.map(fn(x) { IsoCoord(x, y) })
  })
}

/// Cria contexto de infill para um tile
pub fn create_context(g: Grid, coord: IsoCoord, mask_pct: Float) -> Option(InfillContext) {
  case grid.get_tile(g, coord) {
    None -> None
    Some(target) -> {
      // Buscar bordas dos vizinhos gerados
      let north = get_neighbor_border(g, target.neighbors.north, BottomEdge)
      let south = get_neighbor_border(g, target.neighbors.south, TopEdge)
      let east = get_neighbor_border(g, target.neighbors.east, LeftEdge)
      let west = get_neighbor_border(g, target.neighbors.west, RightEdge)

      Some(InfillContext(
        target: target,
        north_border: north,
        south_border: south,
        east_border: east,
        west_border: west,
        mask_percentage: mask_pct,
      ))
    }
  }
}

/// Busca borda de um vizinho se ele já foi gerado
fn get_neighbor_border(g: Grid, maybe_coord: Option(IsoCoord), edge: Edge) -> Option(BorderData) {
  case maybe_coord {
    None -> None
    Some(coord) -> {
      case grid.get_tile(g, coord) {
        None -> None
        Some(neighbor) -> {
          case neighbor.status {
            Generated -> {
              case neighbor.image_path {
                None -> None
                Some(path) -> Some(BorderData(
                  image_path: path,
                  edge: edge,
                  width: 128,  // 25% de 512px
                ))
              }
            }
            _ -> None
          }
        }
      }
    }
  }
}

/// Conta quantas bordas de contexto um tile tem
pub fn context_count(ctx: InfillContext) -> Int {
  let count = fn(b: Option(BorderData)) -> Int {
    case b {
      Some(_) -> 1
      None -> 0
    }
  }

  count(ctx.north_border)
  + count(ctx.south_border)
  + count(ctx.east_border)
  + count(ctx.west_border)
}

/// Verifica se tile pode ser gerado (tem contexto suficiente ou é canto)
pub fn can_generate(g: Grid, coord: IsoCoord) -> Bool {
  case grid.get_tile(g, coord) {
    None -> False
    Some(t) -> {
      // Cantos sempre podem gerar (não têm todos os vizinhos)
      let is_corner =
        { coord.x == 0 || coord.x == g.width - 1 }
        && { coord.y == 0 || coord.y == g.height - 1 }

      case is_corner {
        True -> True
        False -> {
          // Precisa de pelo menos 1 vizinho gerado
          grid.count_generated_neighbors(g, t) >= 1
        }
      }
    }
  }
}

/// Próximo tile a gerar baseado na estratégia de infill
pub fn next_to_generate(g: Grid) -> Option(IsoCoord) {
  let order = generation_order(g, SpiralFromCorner)

  // Encontrar primeiro tile que pode ser gerado
  list.find(order, fn(coord) {
    case grid.get_tile(g, coord) {
      None -> False
      Some(t) -> {
        case t.status {
          tile.Pending -> can_generate(g, coord)
          _ -> False
        }
      }
    }
  })
  |> option.from_result
}
