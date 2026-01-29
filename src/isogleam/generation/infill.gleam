/// Infill - Generation algorithm with neighbor context
/// SECRET SAUCE of Isometric NYC: never generate isolated tiles!
///
/// Strategy:
/// 1. Start from corners (fewest neighbors)
/// 2. Spiral expansion towards the center
/// 3. Each generated tile includes borders from neighbors as context
/// 4. 25% mask on borders for seamless blending
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import isogleam/core/grid.{type Grid}
import isogleam/core/tile.{type IsoCoord, type Tile, Generated, IsoCoord}

/// Generation Strategy
pub type GenerationStrategy {
  /// Start from corner and expand (default)
  SpiralFromCorner
  /// Start from center and expand
  SpiralFromCenter
  /// Random order (not recommended)
  Random
  /// Row by row (simpler, lower quality)
  RowByRow
}

/// Infill Context for a tile
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

/// Border data from a neighbor tile
pub type BorderData {
  BorderData(image_path: String, edge: Edge, width: Int)
}

/// Which edge of the neighbor to use
pub type Edge {
  TopEdge
  BottomEdge
  LeftEdge
  RightEdge
}

/// Generation order using spiral strategy
pub fn generation_order(g: Grid, strategy: GenerationStrategy) -> List(IsoCoord) {
  case strategy {
    SpiralFromCorner -> spiral_from_corner(g.width, g.height)
    SpiralFromCenter -> spiral_from_center(g.width, g.height)
    RowByRow -> row_by_row(g.width, g.height)
    Random -> row_by_row(g.width, g.height)
    // TODO: shuffle
  }
}

/// Generate spiral order starting from corner (0,0)
fn spiral_from_corner(width: Int, height: Int) -> List(IsoCoord) {
  // Simplified implementation: prioritize tiles with lowest x+y (Manhattan distance from corner)
  let all_coords =
    list.range(0, width - 1)
    |> list.flat_map(fn(x) {
      list.range(0, height - 1)
      |> list.map(fn(y) { IsoCoord(x, y) })
    })

  // Sort by Manhattan distance from corner (0,0)
  list.sort(all_coords, fn(a, b) {
    let dist_a = a.x + a.y
    let dist_b = b.x + b.y
    int.compare(dist_a, dist_b)
  })
}

/// Generate spiral order starting from center
fn spiral_from_center(width: Int, height: Int) -> List(IsoCoord) {
  let center_x = width / 2
  let center_y = height / 2

  let all_coords =
    list.range(0, width - 1)
    |> list.flat_map(fn(x) {
      list.range(0, height - 1)
      |> list.map(fn(y) { IsoCoord(x, y) })
    })

  // Sort by distance from center
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

/// Generate row by row order
fn row_by_row(width: Int, height: Int) -> List(IsoCoord) {
  list.range(0, height - 1)
  |> list.flat_map(fn(y) {
    list.range(0, width - 1)
    |> list.map(fn(x) { IsoCoord(x, y) })
  })
}

/// Create infill context for a tile
pub fn create_context(
  g: Grid,
  coord: IsoCoord,
  mask_pct: Float,
) -> Option(InfillContext) {
  case grid.get_tile(g, coord) {
    None -> None
    Some(target) -> {
      // Get generated neighbor borders
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

/// Get neighbor border if generated
fn get_neighbor_border(
  g: Grid,
  maybe_coord: Option(IsoCoord),
  edge: Edge,
) -> Option(BorderData) {
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
                Some(path) ->
                  Some(BorderData(
                    image_path: path,
                    edge: edge,
                    width: 128,
                    // 25% of 512px
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

/// Count how many context borders a tile has
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

/// Check if tile can be generated (has enough context or is corner)
pub fn can_generate(g: Grid, coord: IsoCoord) -> Bool {
  case grid.get_tile(g, coord) {
    None -> False
    Some(t) -> {
      // Corners can always generate (they don't have full neighbors)
      let is_corner =
        { coord.x == 0 || coord.x == g.width - 1 }
        && { coord.y == 0 || coord.y == g.height - 1 }

      case is_corner {
        True -> True
        False -> {
          // Needs at least 1 generated neighbor
          grid.count_generated_neighbors(g, t) >= 1
        }
      }
    }
  }
}

/// Next tile to generate based on infill strategy
pub fn next_to_generate(g: Grid) -> Option(IsoCoord) {
  let order = generation_order(g, SpiralFromCorner)

  // Find first tile that can be generated
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
