/// Grid - Isometric tile grid management
/// Inspired by memory/world.gleam from VIVA
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import isogleam/core/tile.{
  type IsoCoord, type Tile, type TileStatus, Generated, IsoCoord, Pending,
}

/// Tile Grid
pub type Grid {
  Grid(
    tiles: Dict(String, Tile),
    width: Int,
    height: Int,
    tile_size: Int,
    generated_count: Int,
    pending_count: Int,
  )
}

/// Create empty grid
pub fn new(width: Int, height: Int, tile_size: Int) -> Grid {
  let tiles =
    list.range(0, width - 1)
    |> list.flat_map(fn(x) {
      list.range(0, height - 1)
      |> list.map(fn(y) {
        let t = tile.new(x, y)
        let t_with_neighbors = set_tile_neighbors(t, width, height)
        #(tile.id(t_with_neighbors), t_with_neighbors)
      })
    })
    |> dict.from_list

  Grid(
    tiles: tiles,
    width: width,
    height: height,
    tile_size: tile_size,
    generated_count: 0,
    pending_count: width * height,
  )
}

/// Set neighbors for a tile based on grid boundaries
fn set_tile_neighbors(t: Tile, width: Int, height: Int) -> Tile {
  let x = t.coord.x
  let y = t.coord.y

  let north = case y > 0 {
    True -> Some(IsoCoord(x, y - 1))
    False -> None
  }
  let south = case y < height - 1 {
    True -> Some(IsoCoord(x, y + 1))
    False -> None
  }
  let east = case x < width - 1 {
    True -> Some(IsoCoord(x + 1, y))
    False -> None
  }
  let west = case x > 0 {
    True -> Some(IsoCoord(x - 1, y))
    False -> None
  }

  tile.set_neighbors(t, north, south, east, west)
}

/// Convert coordinate to dictionary key
fn coord_to_key(c: IsoCoord) -> String {
  "tile_" <> int.to_string(c.x) <> "_" <> int.to_string(c.y)
}

/// Get tile by coordinate
pub fn get_tile(grid: Grid, coord: IsoCoord) -> Option(Tile) {
  dict.get(grid.tiles, coord_to_key(coord))
  |> option.from_result
}

/// Get tile by x, y
pub fn get(grid: Grid, x: Int, y: Int) -> Option(Tile) {
  get_tile(grid, IsoCoord(x, y))
}

/// Update tile in grid
pub fn update_tile(grid: Grid, t: Tile) -> Grid {
  let key = tile.id(t)
  let old_tile = dict.get(grid.tiles, key)

  let #(gen_delta, pend_delta) = case old_tile {
    Ok(old) -> {
      let was_generated = case old.status {
        Generated -> True
        _ -> False
      }
      let is_generated = case t.status {
        Generated -> True
        _ -> False
      }
      case was_generated, is_generated {
        False, True -> #(1, -1)
        True, False -> #(-1, 1)
        _, _ -> #(0, 0)
      }
    }
    Error(_) -> #(0, 0)
  }

  Grid(
    ..grid,
    tiles: dict.insert(grid.tiles, key, t),
    generated_count: grid.generated_count + gen_delta,
    pending_count: grid.pending_count + pend_delta,
  )
}

/// List pending tiles
pub fn pending_tiles(grid: Grid) -> List(Tile) {
  dict.values(grid.tiles)
  |> list.filter(fn(t) {
    case t.status {
      Pending -> True
      _ -> False
    }
  })
}

/// List generated tiles
pub fn generated_tiles(grid: Grid) -> List(Tile) {
  dict.values(grid.tiles)
  |> list.filter(fn(t) {
    case t.status {
      Generated -> True
      _ -> False
    }
  })
}

/// Generation progress (0.0 to 1.0)
pub fn progress(grid: Grid) -> Float {
  let total = grid.width * grid.height
  case total {
    0 -> 1.0
    _ -> int.to_float(grid.generated_count) /. int.to_float(total)
  }
}

/// Total tiles
pub fn total_tiles(grid: Grid) -> Int {
  grid.width * grid.height
}

/// Check if grid is complete
pub fn is_complete(grid: Grid) -> Bool {
  grid.pending_count == 0
}

/// Get next tile to generate (prioritizing tiles with more generated neighbors)
pub fn next_tile_to_generate(grid: Grid) -> Option(Tile) {
  let pending = pending_tiles(grid)

  // Sort by number of generated neighbors (more neighbors = higher priority)
  let scored =
    list.map(pending, fn(t) {
      let score = count_generated_neighbors(grid, t)
      #(score, t)
    })

  let sorted =
    list.sort(scored, fn(a, b) {
      int.compare(b.0, a.0)
      // Descending
    })

  case sorted {
    [#(_, t), ..] -> Some(t)
    [] -> None
  }
}

/// Count generated neighbors of a tile
pub fn count_generated_neighbors(grid: Grid, t: Tile) -> Int {
  let check = fn(maybe_coord: Option(IsoCoord)) -> Int {
    case maybe_coord {
      None -> 0
      Some(c) -> {
        case get_tile(grid, c) {
          None -> 0
          Some(neighbor) -> {
            case neighbor.status {
              Generated -> 1
              _ -> 0
            }
          }
        }
      }
    }
  }

  check(t.neighbors.north)
  + check(t.neighbors.south)
  + check(t.neighbors.east)
  + check(t.neighbors.west)
}

/// Serialize grid to JSON (for persistence)
pub fn to_json(grid: Grid) -> String {
  let tiles_json =
    dict.values(grid.tiles)
    |> list.map(fn(t) {
      "{\"x\":"
      <> int.to_string(t.coord.x)
      <> ",\"y\":"
      <> int.to_string(t.coord.y)
      <> ",\"status\":\""
      <> status_to_string(t.status)
      <> "\"}"
    })
    |> list.intersperse(",")
    |> list.fold("", fn(acc, s) { acc <> s })

  "{\"width\":"
  <> int.to_string(grid.width)
  <> ",\"height\":"
  <> int.to_string(grid.height)
  <> ",\"tiles\":["
  <> tiles_json
  <> "]}"
}

fn status_to_string(s: TileStatus) -> String {
  case s {
    Pending -> "pending"
    tile.Generating -> "generating"
    Generated -> "generated"
    tile.Failed(_) -> "failed"
  }
}
