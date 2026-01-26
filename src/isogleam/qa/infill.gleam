/// IsoGleam QA - Infill Module
/// Seamless tile border matching strategy
/// Inspired by Isometric NYC but with automated QA

import gleam/list
import gleam/int
import isogleam/qa/color.{type RGB, distance}
import isogleam/qa/pixel.{type ImageData, type BorderSide, North, South, East, West}

/// Infill strategy - how to handle tile borders
pub type InfillStrategy {
  /// Overlap tiles by N pixels, mask and regenerate
  OverlapMask(overlap_pixels: Int)
  /// Blend borders with neighboring tiles
  BorderBlend(blend_width: Int)
  /// Feather edges for smooth transition
  FeatherEdge(feather_pixels: Int)
}

/// Border match result
pub type BorderMatch {
  BorderMatch(
    side: BorderSide,
    similarity: Float,  // 0.0 = mismatch, 1.0 = perfect
    mismatched_pixels: Int,
    total_pixels: Int,
  )
}

/// Infill analysis result
pub type InfillAnalysis {
  InfillAnalysis(
    north: BorderMatch,
    south: BorderMatch,
    east: BorderMatch,
    west: BorderMatch,
    overall_score: Float,
    needs_regeneration: Bool,
  )
}

/// Default infill config
pub type InfillConfig {
  InfillConfig(
    strategy: InfillStrategy,
    tolerance: Float,      // Max color distance for "match"
    min_similarity: Float, // Below this = needs regen
    border_thickness: Int,
  )
}

/// Default config (tuned for pixel art)
pub fn default_config() -> InfillConfig {
  InfillConfig(
    strategy: OverlapMask(overlap_pixels: 32),
    tolerance: 30.0,       // RGB distance
    min_similarity: 0.85,  // 85% match required
    border_thickness: 4,
  )
}

/// Compare two tile borders
/// Returns similarity score 0.0-1.0
pub fn compare_borders(
  border_a: List(RGB),
  border_b: List(RGB),
  tolerance: Float,
) -> BorderMatch {
  let total = int.min(list.length(border_a), list.length(border_b))

  case total {
    0 -> BorderMatch(
      side: North,  // placeholder
      similarity: 0.0,
      mismatched_pixels: 0,
      total_pixels: 0,
    )
    _ -> {
      let mismatches = count_mismatches(border_a, border_b, tolerance, 0)
      let similarity = 1.0 -. int.to_float(mismatches) /. int.to_float(total)

      BorderMatch(
        side: North,
        similarity: similarity,
        mismatched_pixels: mismatches,
        total_pixels: total,
      )
    }
  }
}

fn count_mismatches(
  a: List(RGB),
  b: List(RGB),
  tolerance: Float,
  acc: Int,
) -> Int {
  case a, b {
    [], _ -> acc
    _, [] -> acc
    [px_a, ..rest_a], [px_b, ..rest_b] -> {
      let dist = distance(px_a, px_b)
      let new_acc = case dist >. tolerance {
        True -> acc + 1
        False -> acc
      }
      count_mismatches(rest_a, rest_b, tolerance, new_acc)
    }
  }
}

/// Analyze if a tile needs infill with its neighbors
pub fn analyze_infill(
  tile: ImageData,
  north_neighbor: ImageData,
  south_neighbor: ImageData,
  east_neighbor: ImageData,
  west_neighbor: ImageData,
  config: InfillConfig,
) -> InfillAnalysis {
  let thickness = config.border_thickness
  let tol = config.tolerance

  // Get borders from current tile
  let tile_north = pixel.get_border(tile, North, thickness)
  let tile_south = pixel.get_border(tile, South, thickness)
  let tile_east = pixel.get_border(tile, East, thickness)
  let tile_west = pixel.get_border(tile, West, thickness)

  // Get matching borders from neighbors
  let neighbor_south = pixel.get_border(north_neighbor, South, thickness)
  let neighbor_north = pixel.get_border(south_neighbor, North, thickness)
  let neighbor_west = pixel.get_border(east_neighbor, West, thickness)
  let neighbor_east = pixel.get_border(west_neighbor, East, thickness)

  // Compare each border
  let north_match = BorderMatch(
    ..compare_borders(tile_north, neighbor_south, tol),
    side: North,
  )
  let south_match = BorderMatch(
    ..compare_borders(tile_south, neighbor_north, tol),
    side: South,
  )
  let east_match = BorderMatch(
    ..compare_borders(tile_east, neighbor_west, tol),
    side: East,
  )
  let west_match = BorderMatch(
    ..compare_borders(tile_west, neighbor_east, tol),
    side: West,
  )

  // Overall score is average
  let overall = {
    north_match.similarity +. south_match.similarity +.
    east_match.similarity +. west_match.similarity
  } /. 4.0

  InfillAnalysis(
    north: north_match,
    south: south_match,
    east: east_match,
    west: west_match,
    overall_score: overall,
    needs_regeneration: overall <. config.min_similarity,
  )
}

/// Generate infill mask for regeneration
/// Returns list of (x, y, weight) where weight = how much to regenerate
pub type MaskPoint {
  MaskPoint(x: Int, y: Int, weight: Float)
}

pub fn generate_infill_mask(
  width: Int,
  height: Int,
  analysis: InfillAnalysis,
  config: InfillConfig,
) -> List(MaskPoint) {
  let overlap = case config.strategy {
    OverlapMask(n) -> n
    BorderBlend(n) -> n
    FeatherEdge(n) -> n
  }

  // Generate mask points for edges that need fixing
  let north_mask = case analysis.north.similarity <. config.min_similarity {
    True -> generate_edge_mask(width, height, North, overlap)
    False -> []
  }

  let south_mask = case analysis.south.similarity <. config.min_similarity {
    True -> generate_edge_mask(width, height, South, overlap)
    False -> []
  }

  let east_mask = case analysis.east.similarity <. config.min_similarity {
    True -> generate_edge_mask(width, height, East, overlap)
    False -> []
  }

  let west_mask = case analysis.west.similarity <. config.min_similarity {
    True -> generate_edge_mask(width, height, West, overlap)
    False -> []
  }

  // Merge masks (max weight wins)
  list.flatten([north_mask, south_mask, east_mask, west_mask])
  |> merge_mask_points
}

fn generate_edge_mask(
  width: Int,
  height: Int,
  side: BorderSide,
  depth: Int,
) -> List(MaskPoint) {
  case side {
    North -> {
      list.flat_map(list.range(0, depth - 1), fn(y) {
        list.map(list.range(0, width - 1), fn(x) {
          let weight = 1.0 -. int.to_float(y) /. int.to_float(depth)
          MaskPoint(x, y, weight)
        })
      })
    }
    South -> {
      let start_y = height - depth
      list.flat_map(list.range(0, depth - 1), fn(dy) {
        let y = start_y + dy
        list.map(list.range(0, width - 1), fn(x) {
          let weight = int.to_float(dy) /. int.to_float(depth)
          MaskPoint(x, y, weight)
        })
      })
    }
    East -> {
      let start_x = width - depth
      list.flat_map(list.range(0, height - 1), fn(y) {
        list.map(list.range(0, depth - 1), fn(dx) {
          let x = start_x + dx
          let weight = int.to_float(dx) /. int.to_float(depth)
          MaskPoint(x, y, weight)
        })
      })
    }
    West -> {
      list.flat_map(list.range(0, height - 1), fn(y) {
        list.map(list.range(0, depth - 1), fn(x) {
          let weight = 1.0 -. int.to_float(x) /. int.to_float(depth)
          MaskPoint(x, y, weight)
        })
      })
    }
  }
}

fn merge_mask_points(points: List(MaskPoint)) -> List(MaskPoint) {
  // Simple merge - just return as-is for now
  // In production: group by (x,y) and take max weight
  points
}

/// Detect water tiles (Isometric NYC's pain point #1)
/// Water has specific characteristics: horizontal bands, limited palette
pub fn detect_water(img: ImageData) -> Float {
  let pixels = img.pixels
  let _width = img.width

  // Water characteristics:
  // 1. Blue-dominant colors
  // 2. Horizontal banding pattern (waves)
  // 3. Limited color variation

  let blue_count = list.fold(pixels, 0, fn(acc, px) {
    case px.b > px.r && px.b > px.g {
      True -> acc + 1
      False -> acc
    }
  })

  let total = list.length(pixels)
  case total {
    0 -> 0.0
    _ -> int.to_float(blue_count) /. int.to_float(total)
  }
}

/// Detect vegetation/trees (Isometric NYC's pain point #2)
/// Trees have: green colors, irregular patterns, vertical elements
pub fn detect_vegetation(img: ImageData) -> Float {
  let pixels = img.pixels

  // Green-dominant colors
  let green_count = list.fold(pixels, 0, fn(acc, px) {
    case px.g > px.r && px.g > px.b {
      True -> acc + 1
      False -> acc
    }
  })

  let total = list.length(pixels)
  case total {
    0 -> 0.0
    _ -> int.to_float(green_count) /. int.to_float(total)
  }
}

/// Flag tiles that need special handling
pub type TileFlag {
  WaterTile(confidence: Float)
  VegetationTile(confidence: Float)
  BuildingTile(confidence: Float)
  MixedTile
  UnknownTile
}

/// Classify tile type for infill strategy selection
pub fn classify_tile(img: ImageData) -> TileFlag {
  let water_score = detect_water(img)
  let veg_score = detect_vegetation(img)

  // Building detection: gray/brown dominant, geometric patterns
  let building_score = detect_building(img)

  case water_score >. 0.5, veg_score >. 0.4, building_score >. 0.3 {
    True, False, False -> WaterTile(water_score)
    False, True, False -> VegetationTile(veg_score)
    False, False, True -> BuildingTile(building_score)
    _, _, _ -> MixedTile
  }
}

fn detect_building(img: ImageData) -> Float {
  let pixels = img.pixels

  // Buildings: gray, brown, beige - neutral colors
  let neutral_count = list.fold(pixels, 0, fn(acc, px) {
    let max_diff = int.max(
      int.absolute_value(px.r - px.g),
      int.max(
        int.absolute_value(px.g - px.b),
        int.absolute_value(px.r - px.b),
      ),
    )
    // Neutral if all channels similar
    case max_diff < 40 {
      True -> acc + 1
      False -> acc
    }
  })

  let total = list.length(pixels)
  case total {
    0 -> 0.0
    _ -> int.to_float(neutral_count) /. int.to_float(total)
  }
}
