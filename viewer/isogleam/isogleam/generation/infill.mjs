import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import * as $grid from "../../isogleam/core/grid.mjs";
import * as $tile from "../../isogleam/core/tile.mjs";
import { Generated, IsoCoord } from "../../isogleam/core/tile.mjs";

export class SpiralFromCorner extends $CustomType {}
export const GenerationStrategy$SpiralFromCorner = () => new SpiralFromCorner();
export const GenerationStrategy$isSpiralFromCorner = (value) =>
  value instanceof SpiralFromCorner;

export class SpiralFromCenter extends $CustomType {}
export const GenerationStrategy$SpiralFromCenter = () => new SpiralFromCenter();
export const GenerationStrategy$isSpiralFromCenter = (value) =>
  value instanceof SpiralFromCenter;

export class Random extends $CustomType {}
export const GenerationStrategy$Random = () => new Random();
export const GenerationStrategy$isRandom = (value) => value instanceof Random;

export class RowByRow extends $CustomType {}
export const GenerationStrategy$RowByRow = () => new RowByRow();
export const GenerationStrategy$isRowByRow = (value) =>
  value instanceof RowByRow;

export class InfillContext extends $CustomType {
  constructor(target, north_border, south_border, east_border, west_border, mask_percentage) {
    super();
    this.target = target;
    this.north_border = north_border;
    this.south_border = south_border;
    this.east_border = east_border;
    this.west_border = west_border;
    this.mask_percentage = mask_percentage;
  }
}
export const InfillContext$InfillContext = (target, north_border, south_border, east_border, west_border, mask_percentage) =>
  new InfillContext(target,
  north_border,
  south_border,
  east_border,
  west_border,
  mask_percentage);
export const InfillContext$isInfillContext = (value) =>
  value instanceof InfillContext;
export const InfillContext$InfillContext$target = (value) => value.target;
export const InfillContext$InfillContext$0 = (value) => value.target;
export const InfillContext$InfillContext$north_border = (value) =>
  value.north_border;
export const InfillContext$InfillContext$1 = (value) => value.north_border;
export const InfillContext$InfillContext$south_border = (value) =>
  value.south_border;
export const InfillContext$InfillContext$2 = (value) => value.south_border;
export const InfillContext$InfillContext$east_border = (value) =>
  value.east_border;
export const InfillContext$InfillContext$3 = (value) => value.east_border;
export const InfillContext$InfillContext$west_border = (value) =>
  value.west_border;
export const InfillContext$InfillContext$4 = (value) => value.west_border;
export const InfillContext$InfillContext$mask_percentage = (value) =>
  value.mask_percentage;
export const InfillContext$InfillContext$5 = (value) => value.mask_percentage;

export class BorderData extends $CustomType {
  constructor(image_path, edge, width) {
    super();
    this.image_path = image_path;
    this.edge = edge;
    this.width = width;
  }
}
export const BorderData$BorderData = (image_path, edge, width) =>
  new BorderData(image_path, edge, width);
export const BorderData$isBorderData = (value) => value instanceof BorderData;
export const BorderData$BorderData$image_path = (value) => value.image_path;
export const BorderData$BorderData$0 = (value) => value.image_path;
export const BorderData$BorderData$edge = (value) => value.edge;
export const BorderData$BorderData$1 = (value) => value.edge;
export const BorderData$BorderData$width = (value) => value.width;
export const BorderData$BorderData$2 = (value) => value.width;

export class TopEdge extends $CustomType {}
export const Edge$TopEdge = () => new TopEdge();
export const Edge$isTopEdge = (value) => value instanceof TopEdge;

export class BottomEdge extends $CustomType {}
export const Edge$BottomEdge = () => new BottomEdge();
export const Edge$isBottomEdge = (value) => value instanceof BottomEdge;

export class LeftEdge extends $CustomType {}
export const Edge$LeftEdge = () => new LeftEdge();
export const Edge$isLeftEdge = (value) => value instanceof LeftEdge;

export class RightEdge extends $CustomType {}
export const Edge$RightEdge = () => new RightEdge();
export const Edge$isRightEdge = (value) => value instanceof RightEdge;

/**
 * Generate spiral order starting from corner (0,0)
 * 
 * @ignore
 */
function spiral_from_corner(width, height) {
  let _block;
  let _pipe = $list.range(0, width - 1);
  _block = $list.flat_map(
    _pipe,
    (x) => {
      let _pipe$1 = $list.range(0, height - 1);
      return $list.map(_pipe$1, (y) => { return new IsoCoord(x, y); });
    },
  );
  let all_coords = _block;
  return $list.sort(
    all_coords,
    (a, b) => {
      let dist_a = a.x + a.y;
      let dist_b = b.x + b.y;
      return $int.compare(dist_a, dist_b);
    },
  );
}

function abs(n) {
  let $ = n < 0;
  if ($) {
    return - n;
  } else {
    return n;
  }
}

/**
 * Generate spiral order starting from center
 * 
 * @ignore
 */
function spiral_from_center(width, height) {
  let center_x = globalThis.Math.trunc(width / 2);
  let center_y = globalThis.Math.trunc(height / 2);
  let _block;
  let _pipe = $list.range(0, width - 1);
  _block = $list.flat_map(
    _pipe,
    (x) => {
      let _pipe$1 = $list.range(0, height - 1);
      return $list.map(_pipe$1, (y) => { return new IsoCoord(x, y); });
    },
  );
  let all_coords = _block;
  return $list.sort(
    all_coords,
    (a, b) => {
      let dist_a = abs(a.x - center_x) + abs(a.y - center_y);
      let dist_b = abs(b.x - center_x) + abs(b.y - center_y);
      return $int.compare(dist_a, dist_b);
    },
  );
}

/**
 * Generate row by row order
 * 
 * @ignore
 */
function row_by_row(width, height) {
  let _pipe = $list.range(0, height - 1);
  return $list.flat_map(
    _pipe,
    (y) => {
      let _pipe$1 = $list.range(0, width - 1);
      return $list.map(_pipe$1, (x) => { return new IsoCoord(x, y); });
    },
  );
}

/**
 * Generation order using spiral strategy
 */
export function generation_order(g, strategy) {
  if (strategy instanceof SpiralFromCorner) {
    return spiral_from_corner(g.width, g.height);
  } else if (strategy instanceof SpiralFromCenter) {
    return spiral_from_center(g.width, g.height);
  } else if (strategy instanceof Random) {
    return row_by_row(g.width, g.height);
  } else {
    return row_by_row(g.width, g.height);
  }
}

/**
 * Get neighbor border if generated
 * 
 * @ignore
 */
function get_neighbor_border(g, maybe_coord, edge) {
  if (maybe_coord instanceof Some) {
    let coord = maybe_coord[0];
    let $ = $grid.get_tile(g, coord);
    if ($ instanceof Some) {
      let neighbor = $[0];
      let $1 = neighbor.status;
      if ($1 instanceof Generated) {
        let $2 = neighbor.image_path;
        if ($2 instanceof Some) {
          let path = $2[0];
          return new Some(new BorderData(path, edge, 128));
        } else {
          return $2;
        }
      } else {
        return new None();
      }
    } else {
      return $;
    }
  } else {
    return maybe_coord;
  }
}

/**
 * Create infill context for a tile
 */
export function create_context(g, coord, mask_pct) {
  let $ = $grid.get_tile(g, coord);
  if ($ instanceof Some) {
    let target = $[0];
    let north = get_neighbor_border(g, target.neighbors.north, new BottomEdge());
    let south = get_neighbor_border(g, target.neighbors.south, new TopEdge());
    let east = get_neighbor_border(g, target.neighbors.east, new LeftEdge());
    let west = get_neighbor_border(g, target.neighbors.west, new RightEdge());
    return new Some(
      new InfillContext(target, north, south, east, west, mask_pct),
    );
  } else {
    return $;
  }
}

/**
 * Count how many context borders a tile has
 */
export function context_count(ctx) {
  let count = (b) => {
    if (b instanceof Some) {
      return 1;
    } else {
      return 0;
    }
  };
  return ((count(ctx.north_border) + count(ctx.south_border)) + count(
    ctx.east_border,
  )) + count(ctx.west_border);
}

/**
 * Check if tile can be generated (has enough context or is corner)
 */
export function can_generate(g, coord) {
  let $ = $grid.get_tile(g, coord);
  if ($ instanceof Some) {
    let t = $[0];
    let is_corner = ((coord.x === 0) || (coord.x === (g.width - 1))) && ((coord.y === 0) || (coord.y === (g.height - 1)));
    if (is_corner) {
      return is_corner;
    } else {
      return $grid.count_generated_neighbors(g, t) >= 1;
    }
  } else {
    return false;
  }
}

/**
 * Next tile to generate based on infill strategy
 */
export function next_to_generate(g) {
  let order = generation_order(g, new SpiralFromCorner());
  let _pipe = $list.find(
    order,
    (coord) => {
      let $ = $grid.get_tile(g, coord);
      if ($ instanceof Some) {
        let t = $[0];
        let $1 = t.status;
        if ($1 instanceof $tile.Pending) {
          return can_generate(g, coord);
        } else {
          return false;
        }
      } else {
        return false;
      }
    },
  );
  return $option.from_result(_pipe);
}
