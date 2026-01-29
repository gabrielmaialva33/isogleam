import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import { toList, Empty as $Empty, CustomType as $CustomType, divideFloat } from "../../gleam.mjs";
import * as $color from "../../isogleam/qa/color.mjs";
import { distance } from "../../isogleam/qa/color.mjs";
import * as $pixel from "../../isogleam/qa/pixel.mjs";
import { North, South, East, West } from "../../isogleam/qa/pixel.mjs";

/**
 * Overlap tiles by N pixels, mask and regenerate
 */
export class OverlapMask extends $CustomType {
  constructor(overlap_pixels) {
    super();
    this.overlap_pixels = overlap_pixels;
  }
}
export const InfillStrategy$OverlapMask = (overlap_pixels) =>
  new OverlapMask(overlap_pixels);
export const InfillStrategy$isOverlapMask = (value) =>
  value instanceof OverlapMask;
export const InfillStrategy$OverlapMask$overlap_pixels = (value) =>
  value.overlap_pixels;
export const InfillStrategy$OverlapMask$0 = (value) => value.overlap_pixels;

/**
 * Blend borders with neighboring tiles
 */
export class BorderBlend extends $CustomType {
  constructor(blend_width) {
    super();
    this.blend_width = blend_width;
  }
}
export const InfillStrategy$BorderBlend = (blend_width) =>
  new BorderBlend(blend_width);
export const InfillStrategy$isBorderBlend = (value) =>
  value instanceof BorderBlend;
export const InfillStrategy$BorderBlend$blend_width = (value) =>
  value.blend_width;
export const InfillStrategy$BorderBlend$0 = (value) => value.blend_width;

/**
 * Feather edges for smooth transition
 */
export class FeatherEdge extends $CustomType {
  constructor(feather_pixels) {
    super();
    this.feather_pixels = feather_pixels;
  }
}
export const InfillStrategy$FeatherEdge = (feather_pixels) =>
  new FeatherEdge(feather_pixels);
export const InfillStrategy$isFeatherEdge = (value) =>
  value instanceof FeatherEdge;
export const InfillStrategy$FeatherEdge$feather_pixels = (value) =>
  value.feather_pixels;
export const InfillStrategy$FeatherEdge$0 = (value) => value.feather_pixels;

export class BorderMatch extends $CustomType {
  constructor(side, similarity, mismatched_pixels, total_pixels) {
    super();
    this.side = side;
    this.similarity = similarity;
    this.mismatched_pixels = mismatched_pixels;
    this.total_pixels = total_pixels;
  }
}
export const BorderMatch$BorderMatch = (side, similarity, mismatched_pixels, total_pixels) =>
  new BorderMatch(side, similarity, mismatched_pixels, total_pixels);
export const BorderMatch$isBorderMatch = (value) =>
  value instanceof BorderMatch;
export const BorderMatch$BorderMatch$side = (value) => value.side;
export const BorderMatch$BorderMatch$0 = (value) => value.side;
export const BorderMatch$BorderMatch$similarity = (value) => value.similarity;
export const BorderMatch$BorderMatch$1 = (value) => value.similarity;
export const BorderMatch$BorderMatch$mismatched_pixels = (value) =>
  value.mismatched_pixels;
export const BorderMatch$BorderMatch$2 = (value) => value.mismatched_pixels;
export const BorderMatch$BorderMatch$total_pixels = (value) =>
  value.total_pixels;
export const BorderMatch$BorderMatch$3 = (value) => value.total_pixels;

export class InfillAnalysis extends $CustomType {
  constructor(north, south, east, west, overall_score, needs_regeneration) {
    super();
    this.north = north;
    this.south = south;
    this.east = east;
    this.west = west;
    this.overall_score = overall_score;
    this.needs_regeneration = needs_regeneration;
  }
}
export const InfillAnalysis$InfillAnalysis = (north, south, east, west, overall_score, needs_regeneration) =>
  new InfillAnalysis(north, south, east, west, overall_score, needs_regeneration);
export const InfillAnalysis$isInfillAnalysis = (value) =>
  value instanceof InfillAnalysis;
export const InfillAnalysis$InfillAnalysis$north = (value) => value.north;
export const InfillAnalysis$InfillAnalysis$0 = (value) => value.north;
export const InfillAnalysis$InfillAnalysis$south = (value) => value.south;
export const InfillAnalysis$InfillAnalysis$1 = (value) => value.south;
export const InfillAnalysis$InfillAnalysis$east = (value) => value.east;
export const InfillAnalysis$InfillAnalysis$2 = (value) => value.east;
export const InfillAnalysis$InfillAnalysis$west = (value) => value.west;
export const InfillAnalysis$InfillAnalysis$3 = (value) => value.west;
export const InfillAnalysis$InfillAnalysis$overall_score = (value) =>
  value.overall_score;
export const InfillAnalysis$InfillAnalysis$4 = (value) => value.overall_score;
export const InfillAnalysis$InfillAnalysis$needs_regeneration = (value) =>
  value.needs_regeneration;
export const InfillAnalysis$InfillAnalysis$5 = (value) =>
  value.needs_regeneration;

export class InfillConfig extends $CustomType {
  constructor(strategy, tolerance, min_similarity, border_thickness) {
    super();
    this.strategy = strategy;
    this.tolerance = tolerance;
    this.min_similarity = min_similarity;
    this.border_thickness = border_thickness;
  }
}
export const InfillConfig$InfillConfig = (strategy, tolerance, min_similarity, border_thickness) =>
  new InfillConfig(strategy, tolerance, min_similarity, border_thickness);
export const InfillConfig$isInfillConfig = (value) =>
  value instanceof InfillConfig;
export const InfillConfig$InfillConfig$strategy = (value) => value.strategy;
export const InfillConfig$InfillConfig$0 = (value) => value.strategy;
export const InfillConfig$InfillConfig$tolerance = (value) => value.tolerance;
export const InfillConfig$InfillConfig$1 = (value) => value.tolerance;
export const InfillConfig$InfillConfig$min_similarity = (value) =>
  value.min_similarity;
export const InfillConfig$InfillConfig$2 = (value) => value.min_similarity;
export const InfillConfig$InfillConfig$border_thickness = (value) =>
  value.border_thickness;
export const InfillConfig$InfillConfig$3 = (value) => value.border_thickness;

export class MaskPoint extends $CustomType {
  constructor(x, y, weight) {
    super();
    this.x = x;
    this.y = y;
    this.weight = weight;
  }
}
export const MaskPoint$MaskPoint = (x, y, weight) =>
  new MaskPoint(x, y, weight);
export const MaskPoint$isMaskPoint = (value) => value instanceof MaskPoint;
export const MaskPoint$MaskPoint$x = (value) => value.x;
export const MaskPoint$MaskPoint$0 = (value) => value.x;
export const MaskPoint$MaskPoint$y = (value) => value.y;
export const MaskPoint$MaskPoint$1 = (value) => value.y;
export const MaskPoint$MaskPoint$weight = (value) => value.weight;
export const MaskPoint$MaskPoint$2 = (value) => value.weight;

export class WaterTile extends $CustomType {
  constructor(confidence) {
    super();
    this.confidence = confidence;
  }
}
export const TileFlag$WaterTile = (confidence) => new WaterTile(confidence);
export const TileFlag$isWaterTile = (value) => value instanceof WaterTile;
export const TileFlag$WaterTile$confidence = (value) => value.confidence;
export const TileFlag$WaterTile$0 = (value) => value.confidence;

export class VegetationTile extends $CustomType {
  constructor(confidence) {
    super();
    this.confidence = confidence;
  }
}
export const TileFlag$VegetationTile = (confidence) =>
  new VegetationTile(confidence);
export const TileFlag$isVegetationTile = (value) =>
  value instanceof VegetationTile;
export const TileFlag$VegetationTile$confidence = (value) => value.confidence;
export const TileFlag$VegetationTile$0 = (value) => value.confidence;

export class BuildingTile extends $CustomType {
  constructor(confidence) {
    super();
    this.confidence = confidence;
  }
}
export const TileFlag$BuildingTile = (confidence) =>
  new BuildingTile(confidence);
export const TileFlag$isBuildingTile = (value) => value instanceof BuildingTile;
export const TileFlag$BuildingTile$confidence = (value) => value.confidence;
export const TileFlag$BuildingTile$0 = (value) => value.confidence;

export class MixedTile extends $CustomType {}
export const TileFlag$MixedTile = () => new MixedTile();
export const TileFlag$isMixedTile = (value) => value instanceof MixedTile;

export class UnknownTile extends $CustomType {}
export const TileFlag$UnknownTile = () => new UnknownTile();
export const TileFlag$isUnknownTile = (value) => value instanceof UnknownTile;

/**
 * Default config (tuned for pixel art)
 */
export function default_config() {
  return new InfillConfig(new OverlapMask(32), 30.0, 0.85, 4);
}

function count_mismatches(loop$a, loop$b, loop$tolerance, loop$acc) {
  while (true) {
    let a = loop$a;
    let b = loop$b;
    let tolerance = loop$tolerance;
    let acc = loop$acc;
    if (a instanceof $Empty) {
      return acc;
    } else if (b instanceof $Empty) {
      return acc;
    } else {
      let px_a = a.head;
      let rest_a = a.tail;
      let px_b = b.head;
      let rest_b = b.tail;
      let dist = distance(px_a, px_b);
      let _block;
      let $ = dist > tolerance;
      if ($) {
        _block = acc + 1;
      } else {
        _block = acc;
      }
      let new_acc = _block;
      loop$a = rest_a;
      loop$b = rest_b;
      loop$tolerance = tolerance;
      loop$acc = new_acc;
    }
  }
}

/**
 * Compare two tile borders
 * Returns similarity score 0.0-1.0
 */
export function compare_borders(border_a, border_b, tolerance) {
  let total = $int.min($list.length(border_a), $list.length(border_b));
  if (total === 0) {
    return new BorderMatch(new North(), 0.0, 0, 0);
  } else {
    let mismatches = count_mismatches(border_a, border_b, tolerance, 0);
    let similarity = 1.0 - (divideFloat(
      $int.to_float(mismatches),
      $int.to_float(total)
    ));
    return new BorderMatch(new North(), similarity, mismatches, total);
  }
}

/**
 * Analyze if a tile needs infill with its neighbors
 */
export function analyze_infill(
  tile,
  north_neighbor,
  south_neighbor,
  east_neighbor,
  west_neighbor,
  config
) {
  let thickness = config.border_thickness;
  let tol = config.tolerance;
  let tile_north = $pixel.get_border(tile, new North(), thickness);
  let tile_south = $pixel.get_border(tile, new South(), thickness);
  let tile_east = $pixel.get_border(tile, new East(), thickness);
  let tile_west = $pixel.get_border(tile, new West(), thickness);
  let neighbor_south = $pixel.get_border(north_neighbor, new South(), thickness);
  let neighbor_north = $pixel.get_border(south_neighbor, new North(), thickness);
  let neighbor_west = $pixel.get_border(east_neighbor, new West(), thickness);
  let neighbor_east = $pixel.get_border(west_neighbor, new East(), thickness);
  let _block;
  let _record = compare_borders(tile_north, neighbor_south, tol);
  _block = new BorderMatch(
    new North(),
    _record.similarity,
    _record.mismatched_pixels,
    _record.total_pixels,
  );
  let north_match = _block;
  let _block$1;
  let _record$1 = compare_borders(tile_south, neighbor_north, tol);
  _block$1 = new BorderMatch(
    new South(),
    _record$1.similarity,
    _record$1.mismatched_pixels,
    _record$1.total_pixels,
  );
  let south_match = _block$1;
  let _block$2;
  let _record$2 = compare_borders(tile_east, neighbor_west, tol);
  _block$2 = new BorderMatch(
    new East(),
    _record$2.similarity,
    _record$2.mismatched_pixels,
    _record$2.total_pixels,
  );
  let east_match = _block$2;
  let _block$3;
  let _record$3 = compare_borders(tile_west, neighbor_east, tol);
  _block$3 = new BorderMatch(
    new West(),
    _record$3.similarity,
    _record$3.mismatched_pixels,
    _record$3.total_pixels,
  );
  let west_match = _block$3;
  let overall = (((north_match.similarity + south_match.similarity) + east_match.similarity) + west_match.similarity) / 4.0;
  return new InfillAnalysis(
    north_match,
    south_match,
    east_match,
    west_match,
    overall,
    overall < config.min_similarity,
  );
}

function generate_edge_mask(width, height, side, depth) {
  if (side instanceof North) {
    return $list.flat_map(
      $list.range(0, depth - 1),
      (y) => {
        return $list.map(
          $list.range(0, width - 1),
          (x) => {
            let weight = 1.0 - (divideFloat(
              $int.to_float(y),
              $int.to_float(depth)
            ));
            return new MaskPoint(x, y, weight);
          },
        );
      },
    );
  } else if (side instanceof South) {
    let start_y = height - depth;
    return $list.flat_map(
      $list.range(0, depth - 1),
      (dy) => {
        let y = start_y + dy;
        return $list.map(
          $list.range(0, width - 1),
          (x) => {
            let weight = divideFloat($int.to_float(dy), $int.to_float(depth));
            return new MaskPoint(x, y, weight);
          },
        );
      },
    );
  } else if (side instanceof East) {
    let start_x = width - depth;
    return $list.flat_map(
      $list.range(0, height - 1),
      (y) => {
        return $list.map(
          $list.range(0, depth - 1),
          (dx) => {
            let x = start_x + dx;
            let weight = divideFloat($int.to_float(dx), $int.to_float(depth));
            return new MaskPoint(x, y, weight);
          },
        );
      },
    );
  } else {
    return $list.flat_map(
      $list.range(0, height - 1),
      (y) => {
        return $list.map(
          $list.range(0, depth - 1),
          (x) => {
            let weight = 1.0 - (divideFloat(
              $int.to_float(x),
              $int.to_float(depth)
            ));
            return new MaskPoint(x, y, weight);
          },
        );
      },
    );
  }
}

function merge_mask_points(points) {
  return points;
}

export function generate_infill_mask(width, height, analysis, config) {
  let _block;
  let $ = config.strategy;
  if ($ instanceof OverlapMask) {
    let n = $.overlap_pixels;
    _block = n;
  } else if ($ instanceof BorderBlend) {
    let n = $.blend_width;
    _block = n;
  } else {
    let n = $.feather_pixels;
    _block = n;
  }
  let overlap = _block;
  let _block$1;
  let $1 = analysis.north.similarity < config.min_similarity;
  if ($1) {
    _block$1 = generate_edge_mask(width, height, new North(), overlap);
  } else {
    _block$1 = toList([]);
  }
  let north_mask = _block$1;
  let _block$2;
  let $2 = analysis.south.similarity < config.min_similarity;
  if ($2) {
    _block$2 = generate_edge_mask(width, height, new South(), overlap);
  } else {
    _block$2 = toList([]);
  }
  let south_mask = _block$2;
  let _block$3;
  let $3 = analysis.east.similarity < config.min_similarity;
  if ($3) {
    _block$3 = generate_edge_mask(width, height, new East(), overlap);
  } else {
    _block$3 = toList([]);
  }
  let east_mask = _block$3;
  let _block$4;
  let $4 = analysis.west.similarity < config.min_similarity;
  if ($4) {
    _block$4 = generate_edge_mask(width, height, new West(), overlap);
  } else {
    _block$4 = toList([]);
  }
  let west_mask = _block$4;
  let _pipe = $list.flatten(
    toList([north_mask, south_mask, east_mask, west_mask]),
  );
  return merge_mask_points(_pipe);
}

/**
 * Detect water tiles (Isometric NYC's pain point #1)
 * Water has specific characteristics: horizontal bands, limited palette
 */
export function detect_water(img) {
  let pixels = img.pixels;
  let $ = img.width;
  
  let blue_count = $list.fold(
    pixels,
    0,
    (acc, px) => {
      let $1 = (px.b > px.r) && (px.b > px.g);
      if ($1) {
        return acc + 1;
      } else {
        return acc;
      }
    },
  );
  let total = $list.length(pixels);
  if (total === 0) {
    return 0.0;
  } else {
    return divideFloat($int.to_float(blue_count), $int.to_float(total));
  }
}

/**
 * Detect vegetation/trees (Isometric NYC's pain point #2)
 * Trees have: green colors, irregular patterns, vertical elements
 */
export function detect_vegetation(img) {
  let pixels = img.pixels;
  let green_count = $list.fold(
    pixels,
    0,
    (acc, px) => {
      let $ = (px.g > px.r) && (px.g > px.b);
      if ($) {
        return acc + 1;
      } else {
        return acc;
      }
    },
  );
  let total = $list.length(pixels);
  if (total === 0) {
    return 0.0;
  } else {
    return divideFloat($int.to_float(green_count), $int.to_float(total));
  }
}

function detect_building(img) {
  let pixels = img.pixels;
  let neutral_count = $list.fold(
    pixels,
    0,
    (acc, px) => {
      let max_diff = $int.max(
        $int.absolute_value(px.r - px.g),
        $int.max(
          $int.absolute_value(px.g - px.b),
          $int.absolute_value(px.r - px.b),
        ),
      );
      let $ = max_diff < 40;
      if ($) {
        return acc + 1;
      } else {
        return acc;
      }
    },
  );
  let total = $list.length(pixels);
  if (total === 0) {
    return 0.0;
  } else {
    return divideFloat($int.to_float(neutral_count), $int.to_float(total));
  }
}

/**
 * Classify tile type for infill strategy selection
 */
export function classify_tile(img) {
  let water_score = detect_water(img);
  let veg_score = detect_vegetation(img);
  let building_score = detect_building(img);
  let $ = water_score > 0.5;
  let $1 = veg_score > 0.4;
  let $2 = building_score > 0.3;
  if ($) {
    if (!$1 && !$2) {
      return new WaterTile(water_score);
    } else {
      return new MixedTile();
    }
  } else if ($1) {
    if (!$2) {
      return new VegetationTile(veg_score);
    } else {
      return new MixedTile();
    }
  } else if ($2) {
    return new BuildingTile(building_score);
  } else {
    return new MixedTile();
  }
}
