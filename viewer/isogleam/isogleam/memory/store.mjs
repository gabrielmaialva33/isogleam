import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import { Ok, Error, CustomType as $CustomType } from "../../gleam.mjs";
import * as $tile from "../../isogleam/core/tile.mjs";
import { IsoCoord } from "../../isogleam/core/tile.mjs";

class TileStore extends $CustomType {
  constructor(tiles, metadata) {
    super();
    this.tiles = tiles;
    this.metadata = metadata;
  }
}

export class TileMetadata extends $CustomType {
  constructor(coord, tile_type, score, generated_at, version) {
    super();
    this.coord = coord;
    this.tile_type = tile_type;
    this.score = score;
    this.generated_at = generated_at;
    this.version = version;
  }
}
export const TileMetadata$TileMetadata = (coord, tile_type, score, generated_at, version) =>
  new TileMetadata(coord, tile_type, score, generated_at, version);
export const TileMetadata$isTileMetadata = (value) =>
  value instanceof TileMetadata;
export const TileMetadata$TileMetadata$coord = (value) => value.coord;
export const TileMetadata$TileMetadata$0 = (value) => value.coord;
export const TileMetadata$TileMetadata$tile_type = (value) => value.tile_type;
export const TileMetadata$TileMetadata$1 = (value) => value.tile_type;
export const TileMetadata$TileMetadata$score = (value) => value.score;
export const TileMetadata$TileMetadata$2 = (value) => value.score;
export const TileMetadata$TileMetadata$generated_at = (value) =>
  value.generated_at;
export const TileMetadata$TileMetadata$3 = (value) => value.generated_at;
export const TileMetadata$TileMetadata$version = (value) => value.version;
export const TileMetadata$TileMetadata$4 = (value) => value.version;

/**
 * Create empty store
 */
export function new$() {
  return new TileStore($dict.new$(), $dict.new$());
}

/**
 * Count tiles
 */
export function count(store) {
  return $dict.size(store.tiles);
}

/**
 * List all coordinates
 */
export function list_coords(store) {
  let _pipe = $dict.values(store.metadata);
  return $list.map(_pipe, (m) => { return m.coord; });
}

/**
 * Find tiles by type
 */
export function find_by_type(store, tile_type) {
  let _pipe = $dict.to_list(store.metadata);
  return $list.filter_map(
    _pipe,
    (item) => {
      let key;
      let meta;
      key = item[0];
      meta = item[1];
      let $ = meta.tile_type === tile_type;
      if ($) {
        return $dict.get(store.tiles, key);
      } else {
        return new Error(undefined);
      }
    },
  );
}

/**
 * Find tiles with score above threshold
 */
export function find_by_score(store, min_score) {
  let _pipe = $dict.to_list(store.metadata);
  return $list.filter_map(
    _pipe,
    (item) => {
      let key;
      let meta;
      key = item[0];
      meta = item[1];
      let $ = meta.score >= min_score;
      if ($) {
        return $dict.get(store.tiles, key);
      } else {
        return new Error(undefined);
      }
    },
  );
}

function coord_to_key(coord) {
  return ($int.to_string(coord.x) + "_") + $int.to_string(coord.y);
}

/**
 * Store a tile
 */
export function put(store, tile, meta) {
  let key = coord_to_key(meta.coord);
  return new TileStore(
    $dict.insert(store.tiles, key, tile),
    $dict.insert(store.metadata, key, meta),
  );
}

/**
 * Get a tile by coordinate
 */
export function get(store, coord) {
  let key = coord_to_key(coord);
  let $ = $dict.get(store.tiles, key);
  if ($ instanceof Ok) {
    let tile = $[0];
    return new Some(tile);
  } else {
    return new None();
  }
}

/**
 * Get metadata
 */
export function get_meta(store, coord) {
  let key = coord_to_key(coord);
  let $ = $dict.get(store.metadata, key);
  if ($ instanceof Ok) {
    let meta = $[0];
    return new Some(meta);
  } else {
    return new None();
  }
}

/**
 * Check if tile exists
 */
export function has(store, coord) {
  let key = coord_to_key(coord);
  return $dict.has_key(store.tiles, key);
}

/**
 * Delete a tile
 */
export function delete$(store, coord) {
  let key = coord_to_key(coord);
  return new TileStore(
    $dict.delete$(store.tiles, key),
    $dict.delete$(store.metadata, key),
  );
}

/**
 * Get neighbors of a tile (for infill)
 */
export function get_neighbors(store, coord) {
  let north = new IsoCoord(coord.x, coord.y - 1);
  let south = new IsoCoord(coord.x, coord.y + 1);
  let east = new IsoCoord(coord.x + 1, coord.y);
  let west = new IsoCoord(coord.x - 1, coord.y);
  let _pipe = $dict.new$();
  let _pipe$1 = $dict.insert(_pipe, "north", get(store, north));
  let _pipe$2 = $dict.insert(_pipe$1, "south", get(store, south));
  let _pipe$3 = $dict.insert(_pipe$2, "east", get(store, east));
  return $dict.insert(_pipe$3, "west", get(store, west));
}
