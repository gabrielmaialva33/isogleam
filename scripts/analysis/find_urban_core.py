#!/usr/bin/env python3
"""
Find the actual urban core using density-based clustering
"""

import geopandas as gpd
import json
from pathlib import Path
import numpy as np
from collections import Counter

DATA_DIR = Path(__file__).parent.parent.parent / "data" / "inputs" / "osm"

def main():
    print("=== Finding Urban Core ===\n")

    # Load buildings
    buildings = gpd.read_file(DATA_DIR / "buildings.geojson")
    buildings_utm = buildings.to_crs(epsg=32723)

    print(f"Total buildings: {len(buildings)}")

    # Get centroids
    centroids = buildings_utm.geometry.centroid
    coords = np.array([[p.x, p.y] for p in centroids])

    # Create a grid and count buildings per cell (1km cells)
    cell_size = 500  # 500m cells for finer resolution

    min_x, min_y = coords.min(axis=0)
    max_x, max_y = coords.max(axis=0)

    print(f"Extent: {(max_x-min_x)/1000:.1f}km x {(max_y-min_y)/1000:.1f}km")

    # Grid cell indices for each building
    cell_x = ((coords[:, 0] - min_x) / cell_size).astype(int)
    cell_y = ((coords[:, 1] - min_y) / cell_size).astype(int)
    cells = list(zip(cell_x, cell_y))

    # Count buildings per cell
    cell_counts = Counter(cells)

    # Find densest cells
    densest = cell_counts.most_common(20)
    print("\nDensest 500m cells:")
    for (cx, cy), count in densest[:10]:
        cell_center_x = min_x + (cx + 0.5) * cell_size
        cell_center_y = min_y + (cy + 0.5) * cell_size
        print(f"  Cell ({cx},{cy}): {count} buildings")

    # Find cluster of dense cells (urban core)
    # Use the densest cell as seed and expand
    seed_cell = densest[0][0]
    seed_x, seed_y = seed_cell

    # Find all cells within 2km of seed with >5 buildings
    core_cells = []
    for (cx, cy), count in cell_counts.items():
        dist = np.sqrt((cx - seed_x)**2 + (cy - seed_y)**2) * cell_size
        if dist < 2500 and count >= 2:  # 2.5km radius, min 2 buildings
            core_cells.append((cx, cy, count))

    print(f"\nUrban core cells (2.5km radius, ≥2 buildings): {len(core_cells)}")
    total_core_buildings = sum(c[2] for c in core_cells)
    print(f"Buildings in core: {total_core_buildings}")

    # Calculate core bounds
    core_x = [c[0] for c in core_cells]
    core_y = [c[1] for c in core_cells]

    core_min_x = min_x + min(core_x) * cell_size
    core_max_x = min_x + (max(core_x) + 1) * cell_size
    core_min_y = min_y + min(core_y) * cell_size
    core_max_y = min_y + (max(core_y) + 1) * cell_size

    # Convert center to WGS84
    from shapely.geometry import Point, box
    core_center_x = (core_min_x + core_max_x) / 2
    core_center_y = (core_min_y + core_max_y) / 2

    center_point = gpd.GeoSeries([Point(core_center_x, core_center_y)], crs="EPSG:32723")
    center_wgs84 = center_point.to_crs(epsg=4326)

    print(f"\nCore center (WGS84): {center_wgs84.iloc[0].y:.6f}, {center_wgs84.iloc[0].x:.6f}")
    print(f"Core size: {(core_max_x-core_min_x)/1000:.1f}km x {(core_max_y-core_min_y)/1000:.1f}km")

    # Convert bounds to WGS84
    core_box = gpd.GeoSeries([box(core_min_x, core_min_y, core_max_x, core_max_y)], crs="EPSG:32723")
    core_box_wgs84 = core_box.to_crs(epsg=4326)
    bounds = core_box_wgs84.total_bounds

    print(f"Core bounds (WGS84): {bounds[1]:.6f},{bounds[0]:.6f} → {bounds[3]:.6f},{bounds[2]:.6f}")

    # Now create tile grid for the core area
    # Using 100m tiles = 512px
    tile_size_m = 100

    n_tiles_x = int((core_max_x - core_min_x) / tile_size_m)
    n_tiles_y = int((core_max_y - core_min_y) / tile_size_m)

    print(f"\nTile grid: {n_tiles_x} x {n_tiles_y} = {n_tiles_x * n_tiles_y} tiles")
    print(f"Tile size: {tile_size_m}m → 512px")

    # Load other layers
    roads = gpd.read_file(DATA_DIR / "roads.geojson").to_crs(epsg=32723)
    water = gpd.read_file(DATA_DIR / "water.geojson").to_crs(epsg=32723)
    amenity = gpd.read_file(DATA_DIR / "amenity.geojson").to_crs(epsg=32723)

    # Generate tiles with context
    tiles = []
    for i in range(n_tiles_x):
        for j in range(n_tiles_y):
            tile_min_x = core_min_x + i * tile_size_m
            tile_max_x = tile_min_x + tile_size_m
            tile_min_y = core_min_y + j * tile_size_m
            tile_max_y = tile_min_y + tile_size_m

            # Count features
            tile_buildings = buildings_utm.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_roads = roads.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_water = water.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_amenity = amenity.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]

            # Get building types
            building_types = []
            if len(tile_buildings) > 0 and 'building' in tile_buildings.columns:
                building_types = tile_buildings['building'].dropna().unique().tolist()

            # Get amenity types
            amenity_types = []
            if len(tile_amenity) > 0 and 'amenity' in tile_amenity.columns:
                amenity_types = tile_amenity['amenity'].dropna().unique().tolist()

            # Convert bounds to WGS84
            tile_box = gpd.GeoSeries([box(tile_min_x, tile_min_y, tile_max_x, tile_max_y)], crs="EPSG:32723")
            tile_box_wgs84 = tile_box.to_crs(epsg=4326)
            bounds_wgs84 = tile_box_wgs84.total_bounds

            tiles.append({
                "id": f"tile_{i}_{j}",
                "grid_x": i,
                "grid_y": j,
                "bounds_wgs84": [float(b) for b in bounds_wgs84],
                "buildings": len(tile_buildings),
                "roads": len(tile_roads),
                "water": len(tile_water),
                "amenities": len(tile_amenity),
                "building_types": building_types[:5],  # limit
                "amenity_types": amenity_types[:5],
                "priority": len(tile_buildings) * 2 + len(tile_roads) + len(tile_amenity) * 3
            })

    # Sort by priority
    tiles_sorted = sorted(tiles, key=lambda t: t["priority"], reverse=True)

    # Stats
    tiles_with_content = [t for t in tiles if t["buildings"] > 0 or t["roads"] > 0]
    print(f"\nTiles with content: {len(tiles_with_content)} ({len(tiles_with_content)/len(tiles)*100:.1f}%)")

    # Save
    output_dir = DATA_DIR.parent / "grid"
    output_dir.mkdir(exist_ok=True)

    grid_config = {
        "center_wgs84": [float(center_wgs84.iloc[0].x), float(center_wgs84.iloc[0].y)],
        "bounds_wgs84": [float(b) for b in bounds],
        "tile_size_m": tile_size_m,
        "grid_size": [n_tiles_x, n_tiles_y],
        "total_tiles": len(tiles),
        "tiles_with_content": len(tiles_with_content),
        "total_buildings_in_area": total_core_buildings,
        "crs_utm": "EPSG:32723"
    }

    with open(output_dir / "grid_config.json", "w") as f:
        json.dump(grid_config, f, indent=2)

    with open(output_dir / "tiles.json", "w") as f:
        json.dump(tiles, f, indent=2)

    with open(output_dir / "tiles_by_priority.json", "w") as f:
        json.dump(tiles_sorted[:100], f, indent=2)  # Top 100

    print(f"Saved to: {output_dir}")

    # Show top priority tiles
    print("\nTop 10 priority tiles:")
    for t in tiles_sorted[:10]:
        print(f"  {t['id']}: {t['buildings']}b {t['roads']}r {t['amenities']}a = {t['priority']} priority")

    print("\n✓ Urban core analysis complete!")

if __name__ == "__main__":
    main()
