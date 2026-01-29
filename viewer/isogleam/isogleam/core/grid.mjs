import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import { Ok, Empty as $Empty, CustomType as $CustomType, divideFloat } from "../../gleam.mjs";
import * as $tile from "../../isogleam/core/tile.mjs";
import { Generated, IsoCoord, Pending } from "../../isogleam/core/tile.mjs";

export class Grid extends $CustomType {
  constructor(tiles, width, height, tile_size, generated_count, pending_count) {
    super();
    this.tiles = tiles;
    this.width = width;
    this.height = height;
    this.tile_size = tile_size;
    this.generated_count = generated_count;
    this.pending_count = pending_count;
  }
}
export const Grid$Grid = (tiles, width, height, tile_size, generated_count, pending_count) =>
  new Grid(tiles, width, height, tile_size, generated_count, pending_count);
export const Grid$isGrid = (value) => value instanceof Grid;
export const Grid$Grid$tiles = (value) => value.tiles;
export const Grid$Grid$0 = (value) => value.tiles;
export const Grid$Grid$width = (value) => value.width;
export const Grid$Grid$1 = (value) => value.width;
export const Grid$Grid$height = (value) => value.height;
export const Grid$Grid$2 = (value) => value.height;
export const Grid$Grid$tile_size = (value) => value.tile_size;
export const Grid$Grid$3 = (value) => value.tile_size;
export const Grid$Grid$generated_count = (value) => value.generated_count;
export const Grid$Grid$4 = (value) => value.generated_count;
export const Grid$Grid$pending_count = (value) => value.pending_count;
export const Grid$Grid$5 = (value) => value.pending_count;

/**
 * Set neighbors for a tile based on grid boundaries
 * 
 * @ignore
 */
function set_tile_neighbors(t, width, height) {
  let x = t.coord.x;
  let y = t.coord.y;
  let _block;
  let $ = y > 0;
  if ($) {
    _block = new Some(new IsoCoord(x, y - 1));
  } else {
    _block = new None();
  }
  let north = _block;
  let _block$1;
  let $1 = y < (height - 1);
  if ($1) {
    _block$1 = new Some(new IsoCoord(x, y + 1));
  } else {
    _block$1 = new None();
  }
  let south = _block$1;
  let _block$2;
  let $2 = x < (width - 1);
  if ($2) {
    _block$2 = new Some(new IsoCoord(x + 1, y));
  } else {
    _block$2 = new None();
  }
  let east = _block$2;
  let _block$3;
  let $3 = x > 0;
  if ($3) {
    _block$3 = new Some(new IsoCoord(x - 1, y));
  } else {
    _block$3 = new None();
  }
  let west = _block$3;
  return $tile.set_neighbors(t, north, south, east, west);
}

/**
 * Create empty grid
 */
export function new$(width, height, tile_size) {
  let _block;
  let _pipe = $list.range(0, width - 1);
  let _pipe$1 = $list.flat_map(
    _pipe,
    (x) => {
      let _pipe$1 = $list.range(0, height - 1);
      return $list.map(
        _pipe$1,
        (y) => {
          let t = $tile.new$(x, y);
          let t_with_neighbors = set_tile_neighbors(t, width, height);
          return [$tile.id(t_with_neighbors), t_with_neighbors];
        },
      );
    },
  );
  _block = $dict.from_list(_pipe$1);
  let tiles = _block;
  return new Grid(tiles, width, height, tile_size, 0, width * height);
}

/**
 * Convert coordinate to dictionary key
 * 
 * @ignore
 */
function coord_to_key(c) {
  return (("tile_" + $int.to_string(c.x)) + "_") + $int.to_string(c.y);
}

/**
 * Get tile by coordinate
 */
export function get_tile(grid, coord) {
  let _pipe = $dict.get(grid.tiles, coord_to_key(coord));
  return $option.from_result(_pipe);
}

/**
 * Get tile by x, y
 */
export function get(grid, x, y) {
  return get_tile(grid, new IsoCoord(x, y));
}

/**
 * Update tile in grid
 */
export function update_tile(grid, t) {
  let key = $tile.id(t);
  let old_tile = $dict.get(grid.tiles, key);
  let _block;
  if (old_tile instanceof Ok) {
    let old = old_tile[0];
    let _block$1;
    let $1 = old.status;
    if ($1 instanceof Generated) {
      _block$1 = true;
    } else {
      _block$1 = false;
    }
    let was_generated = _block$1;
    let _block$2;
    let $2 = t.status;
    if ($2 instanceof Generated) {
      _block$2 = true;
    } else {
      _block$2 = false;
    }
    let is_generated = _block$2;
    if (was_generated) {
      if (!is_generated) {
        _block = [-1, 1];
      } else {
        _block = [0, 0];
      }
    } else if (is_generated) {
      _block = [1, -1];
    } else {
      _block = [0, 0];
    }
  } else {
    _block = [0, 0];
  }
  let $ = _block;
  let gen_delta;
  let pend_delta;
  gen_delta = $[0];
  pend_delta = $[1];
  return new Grid(
    $dict.insert(grid.tiles, key, t),
    grid.width,
    grid.height,
    grid.tile_size,
    grid.generated_count + gen_delta,
    grid.pending_count + pend_delta,
  );
}

/**
 * List pending tiles
 */
export function pending_tiles(grid) {
  let _pipe = $dict.values(grid.tiles);
  return $list.filter(
    _pipe,
    (t) => {
      let $ = t.status;
      if ($ instanceof Pending) {
        return true;
      } else {
        return false;
      }
    },
  );
}

/**
 * List generated tiles
 */
export function generated_tiles(grid) {
  let _pipe = $dict.values(grid.tiles);
  return $list.filter(
    _pipe,
    (t) => {
      let $ = t.status;
      if ($ instanceof Generated) {
        return true;
      } else {
        return false;
      }
    },
  );
}

/**
 * Generation progress (0.0 to 1.0)
 */
export function progress(grid) {
  let total = grid.width * grid.height;
  if (total === 0) {
    return 1.0;
  } else {
    return divideFloat(
      $int.to_float(grid.generated_count),
      $int.to_float(total)
    );
  }
}

/**
 * Total tiles
 */
export function total_tiles(grid) {
  return grid.width * grid.height;
}

/**
 * Check if grid is complete
 */
export function is_complete(grid) {
  return grid.pending_count === 0;
}

/**
 * Count generated neighbors of a tile
 */
export function count_generated_neighbors(grid, t) {
  let check = (maybe_coord) => {
    if (maybe_coord instanceof Some) {
      let c = maybe_coord[0];
      let $ = get_tile(grid, c);
      if ($ instanceof Some) {
        let neighbor = $[0];
        let $1 = neighbor.status;
        if ($1 instanceof Generated) {
          return 1;
        } else {
          return 0;
        }
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  };
  return ((check(t.neighbors.north) + check(t.neighbors.south)) + check(
    t.neighbors.east,
  )) + check(t.neighbors.west);
}

/**
 * Get next tile to generate (prioritizing tiles with more generated neighbors)
 */
export function next_tile_to_generate(grid) {
  let pending = pending_tiles(grid);
  let scored = $list.map(
    pending,
    (t) => {
      let score = count_generated_neighbors(grid, t);
      return [score, t];
    },
  );
  let sorted = $list.sort(
    scored,
    (a, b) => { return $int.compare(b[0], a[0]); },
  );
  if (sorted instanceof $Empty) {
    return new None();
  } else {
    let t = sorted.head[1];
    return new Some(t);
  }
}

function status_to_string(s) {
  if (s instanceof Pending) {
    return "pending";
  } else if (s instanceof $tile.Generating) {
    return "generating";
  } else if (s instanceof Generated) {
    return "generated";
  } else {
    return "failed";
  }
}

/**
 * Serialize grid to JSON (for persistence)
 */
export function to_json(grid) {
  let _block;
  let _pipe = $dict.values(grid.tiles);
  let _pipe$1 = $list.map(
    _pipe,
    (t) => {
      return ((((("{\"x\":" + $int.to_string(t.coord.x)) + ",\"y\":") + $int.to_string(
        t.coord.y,
      )) + ",\"status\":\"") + status_to_string(t.status)) + "\"}";
    },
  );
  let _pipe$2 = $list.intersperse(_pipe$1, ",");
  _block = $list.fold(_pipe$2, "", (acc, s) => { return acc + s; });
  let tiles_json = _block;
  return ((((("{\"width\":" + $int.to_string(grid.width)) + ",\"height\":") + $int.to_string(
    grid.height,
  )) + ",\"tiles\":[") + tiles_json) + "]}";
}
