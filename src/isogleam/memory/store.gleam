/// IsoGleam Memory - Tile Storage
/// Persistent storage for generated tiles using SQLite/Qdrant

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import isogleam/tile.{type Tile, type IsoCoord, IsoCoord}

/// In-memory tile store (for dev/testing)
/// Production would use SQLite + Qdrant
pub opaque type TileStore {
  TileStore(
    tiles: Dict(String, Tile),
    metadata: Dict(String, TileMetadata),
  )
}

/// Tile metadata for search/retrieval
pub type TileMetadata {
  TileMetadata(
    coord: IsoCoord,
    tile_type: String,
    score: Float,
    generated_at: Int,  // Unix timestamp
    version: Int,
  )
}

/// Create empty store
pub fn new() -> TileStore {
  TileStore(
    tiles: dict.new(),
    metadata: dict.new(),
  )
}

/// Store a tile
pub fn put(store: TileStore, tile: Tile, meta: TileMetadata) -> TileStore {
  let key = coord_to_key(meta.coord)
  TileStore(
    tiles: dict.insert(store.tiles, key, tile),
    metadata: dict.insert(store.metadata, key, meta),
  )
}

/// Get a tile by coordinate
pub fn get(store: TileStore, coord: IsoCoord) -> Option(Tile) {
  let key = coord_to_key(coord)
  case dict.get(store.tiles, key) {
    Ok(tile) -> Some(tile)
    Error(_) -> None
  }
}

/// Get metadata
pub fn get_meta(store: TileStore, coord: IsoCoord) -> Option(TileMetadata) {
  let key = coord_to_key(coord)
  case dict.get(store.metadata, key) {
    Ok(meta) -> Some(meta)
    Error(_) -> None
  }
}

/// Check if tile exists
pub fn has(store: TileStore, coord: IsoCoord) -> Bool {
  let key = coord_to_key(coord)
  dict.has_key(store.tiles, key)
}

/// Delete a tile
pub fn delete(store: TileStore, coord: IsoCoord) -> TileStore {
  let key = coord_to_key(coord)
  TileStore(
    tiles: dict.delete(store.tiles, key),
    metadata: dict.delete(store.metadata, key),
  )
}

/// Count tiles
pub fn count(store: TileStore) -> Int {
  dict.size(store.tiles)
}

/// List all coordinates
pub fn list_coords(store: TileStore) -> List(IsoCoord) {
  dict.values(store.metadata)
  |> list.map(fn(m) { m.coord })
}

/// Find tiles by type
pub fn find_by_type(store: TileStore, tile_type: String) -> List(Tile) {
  dict.to_list(store.metadata)
  |> list.filter_map(fn(item) {
    let #(key, meta) = item
    case meta.tile_type == tile_type {
      True -> dict.get(store.tiles, key)
      False -> Error(Nil)
    }
  })
}

/// Find tiles with score above threshold
pub fn find_by_score(store: TileStore, min_score: Float) -> List(Tile) {
  dict.to_list(store.metadata)
  |> list.filter_map(fn(item) {
    let #(key, meta) = item
    case meta.score >=. min_score {
      True -> dict.get(store.tiles, key)
      False -> Error(Nil)
    }
  })
}

/// Get neighbors of a tile (for infill)
pub fn get_neighbors(
  store: TileStore,
  coord: IsoCoord,
) -> Dict(String, Option(Tile)) {
  let north = IsoCoord(x: coord.x, y: coord.y - 1)
  let south = IsoCoord(x: coord.x, y: coord.y + 1)
  let east = IsoCoord(x: coord.x + 1, y: coord.y)
  let west = IsoCoord(x: coord.x - 1, y: coord.y)

  dict.new()
  |> dict.insert("north", get(store, north))
  |> dict.insert("south", get(store, south))
  |> dict.insert("east", get(store, east))
  |> dict.insert("west", get(store, west))
}

// Helper
fn coord_to_key(coord: IsoCoord) -> String {
  int_to_string(coord.x) <> "_" <> int_to_string(coord.y)
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String
