import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType, divideInt } from "../../gleam.mjs";

export class Pending extends $CustomType {}
export const TileStatus$Pending = () => new Pending();
export const TileStatus$isPending = (value) => value instanceof Pending;

export class Generating extends $CustomType {}
export const TileStatus$Generating = () => new Generating();
export const TileStatus$isGenerating = (value) => value instanceof Generating;

export class Generated extends $CustomType {}
export const TileStatus$Generated = () => new Generated();
export const TileStatus$isGenerated = (value) => value instanceof Generated;

export class Failed extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}
export const TileStatus$Failed = (reason) => new Failed(reason);
export const TileStatus$isFailed = (value) => value instanceof Failed;
export const TileStatus$Failed$reason = (value) => value.reason;
export const TileStatus$Failed$0 = (value) => value.reason;

export class Ground extends $CustomType {}
export const TerrainType$Ground = () => new Ground();
export const TerrainType$isGround = (value) => value instanceof Ground;

export class Water extends $CustomType {}
export const TerrainType$Water = () => new Water();
export const TerrainType$isWater = (value) => value instanceof Water;

export class Road extends $CustomType {}
export const TerrainType$Road = () => new Road();
export const TerrainType$isRoad = (value) => value instanceof Road;

export class Building extends $CustomType {}
export const TerrainType$Building = () => new Building();
export const TerrainType$isBuilding = (value) => value instanceof Building;

export class Vegetation extends $CustomType {}
export const TerrainType$Vegetation = () => new Vegetation();
export const TerrainType$isVegetation = (value) => value instanceof Vegetation;

export class Empty extends $CustomType {}
export const TerrainType$Empty = () => new Empty();
export const TerrainType$isEmpty = (value) => value instanceof Empty;

export class IsoCoord extends $CustomType {
  constructor(x, y) {
    super();
    this.x = x;
    this.y = y;
  }
}
export const IsoCoord$IsoCoord = (x, y) => new IsoCoord(x, y);
export const IsoCoord$isIsoCoord = (value) => value instanceof IsoCoord;
export const IsoCoord$IsoCoord$x = (value) => value.x;
export const IsoCoord$IsoCoord$0 = (value) => value.x;
export const IsoCoord$IsoCoord$y = (value) => value.y;
export const IsoCoord$IsoCoord$1 = (value) => value.y;

export class Tile extends $CustomType {
  constructor(coord, status, terrain, image_path, input_path, seed, neighbors) {
    super();
    this.coord = coord;
    this.status = status;
    this.terrain = terrain;
    this.image_path = image_path;
    this.input_path = input_path;
    this.seed = seed;
    this.neighbors = neighbors;
  }
}
export const Tile$Tile = (coord, status, terrain, image_path, input_path, seed, neighbors) =>
  new Tile(coord, status, terrain, image_path, input_path, seed, neighbors);
export const Tile$isTile = (value) => value instanceof Tile;
export const Tile$Tile$coord = (value) => value.coord;
export const Tile$Tile$0 = (value) => value.coord;
export const Tile$Tile$status = (value) => value.status;
export const Tile$Tile$1 = (value) => value.status;
export const Tile$Tile$terrain = (value) => value.terrain;
export const Tile$Tile$2 = (value) => value.terrain;
export const Tile$Tile$image_path = (value) => value.image_path;
export const Tile$Tile$3 = (value) => value.image_path;
export const Tile$Tile$input_path = (value) => value.input_path;
export const Tile$Tile$4 = (value) => value.input_path;
export const Tile$Tile$seed = (value) => value.seed;
export const Tile$Tile$5 = (value) => value.seed;
export const Tile$Tile$neighbors = (value) => value.neighbors;
export const Tile$Tile$6 = (value) => value.neighbors;

export class Neighbors extends $CustomType {
  constructor(north, south, east, west) {
    super();
    this.north = north;
    this.south = south;
    this.east = east;
    this.west = west;
  }
}
export const Neighbors$Neighbors = (north, south, east, west) =>
  new Neighbors(north, south, east, west);
export const Neighbors$isNeighbors = (value) => value instanceof Neighbors;
export const Neighbors$Neighbors$north = (value) => value.north;
export const Neighbors$Neighbors$0 = (value) => value.north;
export const Neighbors$Neighbors$south = (value) => value.south;
export const Neighbors$Neighbors$1 = (value) => value.south;
export const Neighbors$Neighbors$east = (value) => value.east;
export const Neighbors$Neighbors$2 = (value) => value.east;
export const Neighbors$Neighbors$west = (value) => value.west;
export const Neighbors$Neighbors$3 = (value) => value.west;

/**
 * Cria um tile vazio
 */
export function new$(x, y) {
  return new Tile(
    new IsoCoord(x, y),
    new Pending(),
    new Empty(),
    new None(),
    new None(),
    new None(),
    new Neighbors(new None(), new None(), new None(), new None()),
  );
}

/**
 * Cria coordenada isométrica
 */
export function coord(x, y) {
  return new IsoCoord(x, y);
}

/**
 * Converte coordenada iso para screen (pixels)
 * Projeção 2:1 isométrica padrão
 */
export function iso_to_screen(c, tile_width, tile_height) {
  let screen_x = (c.x - c.y) * (globalThis.Math.trunc(tile_width / 2));
  let screen_y = (c.x + c.y) * (globalThis.Math.trunc(tile_height / 2));
  return [screen_x, screen_y];
}

/**
 * Converte coordenada screen para iso
 */
export function screen_to_iso(screen_x, screen_y, tile_width, tile_height) {
  let half_w = globalThis.Math.trunc(tile_width / 2);
  let half_h = globalThis.Math.trunc(tile_height / 2);
  let x = globalThis.Math.trunc(
    ((divideInt(screen_x, half_w)) + (divideInt(screen_y, half_h))) / 2
  );
  let y = globalThis.Math.trunc(
    ((divideInt(screen_y, half_h)) - (divideInt(screen_x, half_w))) / 2
  );
  return new IsoCoord(x, y);
}

/**
 * Atualiza status do tile
 */
export function set_status(tile, status) {
  return new Tile(
    tile.coord,
    status,
    tile.terrain,
    tile.image_path,
    tile.input_path,
    tile.seed,
    tile.neighbors,
  );
}

/**
 * Atualiza terreno do tile
 */
export function set_terrain(tile, terrain) {
  return new Tile(
    tile.coord,
    tile.status,
    terrain,
    tile.image_path,
    tile.input_path,
    tile.seed,
    tile.neighbors,
  );
}

/**
 * Define caminho da imagem gerada
 */
export function set_image(tile, path) {
  return new Tile(
    tile.coord,
    new Generated(),
    tile.terrain,
    new Some(path),
    tile.input_path,
    tile.seed,
    tile.neighbors,
  );
}

/**
 * Define vizinhos do tile
 */
export function set_neighbors(tile, n, s, e, w) {
  return new Tile(
    tile.coord,
    tile.status,
    tile.terrain,
    tile.image_path,
    tile.input_path,
    tile.seed,
    new Neighbors(n, s, e, w),
  );
}

/**
 * Verifica se tile tem todos os vizinhos gerados
 */
export function has_all_neighbors_generated(tile, get_tile) {
  let check = (maybe_coord) => {
    if (maybe_coord instanceof Some) {
      let c = maybe_coord[0];
      let $ = get_tile(c);
      if ($ instanceof Some) {
        let t = $[0];
        let $1 = t.status;
        if ($1 instanceof Generated) {
          return true;
        } else {
          return false;
        }
      } else {
        return true;
      }
    } else {
      return true;
    }
  };
  return ((check(tile.neighbors.north) && check(tile.neighbors.south)) && check(
    tile.neighbors.east,
  )) && check(tile.neighbors.west);
}

/**
 * ID único do tile
 */
export function id(tile) {
  return (("tile_" + $int.to_string(tile.coord.x)) + "_") + $int.to_string(
    tile.coord.y,
  );
}
