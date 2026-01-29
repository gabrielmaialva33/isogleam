/// Tile - Estrutura de dados para tiles isométricos
/// Baseado no sistema de coordenadas 2:1 do SimCity 2000

import gleam/int
import gleam/option.{type Option, None, Some}

/// Status de geração do tile
pub type TileStatus {
  Pending
  Generating
  Generated
  Failed(reason: String)
}

/// Tipo de terreno
pub type TerrainType {
  Ground
  Water
  Road
  Building
  Vegetation
  Empty
}

/// Coordenada isométrica (x, y no grid)
pub type IsoCoord {
  IsoCoord(x: Int, y: Int)
}

/// Tile individual no mapa
pub type Tile {
  Tile(
    coord: IsoCoord,
    status: TileStatus,
    terrain: TerrainType,
    image_path: Option(String),
    input_path: Option(String),
    seed: Option(Int),
    neighbors: Neighbors,
  )
}

/// Vizinhos do tile (para infill)
pub type Neighbors {
  Neighbors(
    north: Option(IsoCoord),
    south: Option(IsoCoord),
    east: Option(IsoCoord),
    west: Option(IsoCoord),
  )
}

/// Cria um tile vazio
pub fn new(x: Int, y: Int) -> Tile {
  Tile(
    coord: IsoCoord(x, y),
    status: Pending,
    terrain: Empty,
    image_path: None,
    input_path: None,
    seed: None,
    neighbors: Neighbors(None, None, None, None),
  )
}

/// Cria coordenada isométrica
pub fn coord(x: Int, y: Int) -> IsoCoord {
  IsoCoord(x, y)
}

/// Converte coordenada iso para screen (pixels)
/// Projeção 2:1 isométrica padrão
pub fn iso_to_screen(c: IsoCoord, tile_width: Int, tile_height: Int) -> #(Int, Int) {
  let screen_x = { c.x - c.y } * { tile_width / 2 }
  let screen_y = { c.x + c.y } * { tile_height / 2 }
  #(screen_x, screen_y)
}

/// Converte coordenada screen para iso
pub fn screen_to_iso(screen_x: Int, screen_y: Int, tile_width: Int, tile_height: Int) -> IsoCoord {
  let half_w = tile_width / 2
  let half_h = tile_height / 2
  let x = { screen_x / half_w + screen_y / half_h } / 2
  let y = { screen_y / half_h - screen_x / half_w } / 2
  IsoCoord(x, y)
}

/// Atualiza status do tile
pub fn set_status(tile: Tile, status: TileStatus) -> Tile {
  Tile(..tile, status: status)
}

/// Atualiza terreno do tile
pub fn set_terrain(tile: Tile, terrain: TerrainType) -> Tile {
  Tile(..tile, terrain: terrain)
}

/// Define caminho da imagem gerada
pub fn set_image(tile: Tile, path: String) -> Tile {
  Tile(..tile, image_path: Some(path), status: Generated)
}

/// Define vizinhos do tile
pub fn set_neighbors(tile: Tile, n: Option(IsoCoord), s: Option(IsoCoord), e: Option(IsoCoord), w: Option(IsoCoord)) -> Tile {
  Tile(..tile, neighbors: Neighbors(n, s, e, w))
}

/// Verifica se tile tem todos os vizinhos gerados
pub fn has_all_neighbors_generated(tile: Tile, get_tile: fn(IsoCoord) -> Option(Tile)) -> Bool {
  let check = fn(maybe_coord: Option(IsoCoord)) -> Bool {
    case maybe_coord {
      None -> True  // Borda do mapa, não precisa
      Some(c) -> {
        case get_tile(c) {
          None -> True
          Some(t) -> {
            case t.status {
              Generated -> True
              _ -> False
            }
          }
        }
      }
    }
  }

  check(tile.neighbors.north)
  && check(tile.neighbors.south)
  && check(tile.neighbors.east)
  && check(tile.neighbors.west)
}

/// ID único do tile
pub fn id(tile: Tile) -> String {
  "tile_" <> int.to_string(tile.coord.x) <> "_" <> int.to_string(tile.coord.y)
}
