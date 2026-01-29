#!/usr/bin/env python3
"""
Analyze OSM data to find urban center and create tile grid
"""

import geopandas as gpd
import json
from pathlib import Path
import numpy as np

DATA_DIR = Path(__file__).parent.parent.parent / "data" / "inputs" / "osm"

def main():
    print("=== Analyzing Urban Center ===\n")

    # Load buildings
    buildings = gpd.read_file(DATA_DIR / "buildings.geojson")
    print(f"Buildings: {len(buildings)}")

    # Convert to UTM for accurate measurements
    buildings_utm = buildings.to_crs(epsg=32723)  # UTM 23S

    # Get centroids
    centroids = buildings_utm.geometry.centroid

    # Calculate density center (mean of all centroids)
    center_x = centroids.x.mean()
    center_y = centroids.y.mean()

    # Convert back to WGS84
    from shapely.geometry import Point
    center_point = gpd.GeoSeries([Point(center_x, center_y)], crs="EPSG:32723")
    center_wgs84 = center_point.to_crs(epsg=4326)
    center_lon = center_wgs84.iloc[0].x
    center_lat = center_wgs84.iloc[0].y

    print(f"\nUrban center: {center_lat:.6f}, {center_lon:.6f}")

    # Calculate building density per km²
    # Create 500m grid cells around center
    cell_size = 500  # meters

    # Define MVP area: 4km x 4km around center (16 km²)
    mvp_half_size = 2000  # 2km each direction

    min_x = center_x - mvp_half_size
    max_x = center_x + mvp_half_size
    min_y = center_y - mvp_half_size
    max_y = center_y + mvp_half_size

    # Count buildings in MVP area
    mvp_buildings = buildings_utm.cx[min_x:max_x, min_y:max_y]
    print(f"Buildings in MVP area (4x4 km): {len(mvp_buildings)}")

    # Create tile grid (512px = ~100m at our scale)
    # For 4km x 4km area, that's 40x40 = 1600 tiles
    # Let's use 200m per tile = 20x20 = 400 tiles
    tile_size_m = 200
    n_tiles_x = int((max_x - min_x) / tile_size_m)
    n_tiles_y = int((max_y - min_y) / tile_size_m)

    print(f"\nTile grid: {n_tiles_x} x {n_tiles_y} = {n_tiles_x * n_tiles_y} tiles")
    print(f"Tile size: {tile_size_m}m = 512px")

    # Generate tile metadata
    tiles = []
    for i in range(n_tiles_x):
        for j in range(n_tiles_y):
            tile_min_x = min_x + i * tile_size_m
            tile_max_x = tile_min_x + tile_size_m
            tile_min_y = min_y + j * tile_size_m
            tile_max_y = tile_min_y + tile_size_m

            # Count features in tile
            tile_buildings = buildings_utm.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]

            # Convert bounds to WGS84
            from shapely.geometry import box
            tile_box = gpd.GeoSeries([box(tile_min_x, tile_min_y, tile_max_x, tile_max_y)], crs="EPSG:32723")
            tile_box_wgs84 = tile_box.to_crs(epsg=4326)
            bounds_wgs84 = tile_box_wgs84.total_bounds

            tiles.append({
                "id": f"tile_{i}_{j}",
                "grid_x": i,
                "grid_y": j,
                "bounds_wgs84": list(bounds_wgs84),
                "buildings_count": len(tile_buildings),
                "has_content": len(tile_buildings) > 0
            })

    # Statistics
    tiles_with_content = sum(1 for t in tiles if t["has_content"])
    avg_buildings = np.mean([t["buildings_count"] for t in tiles if t["has_content"]])

    print(f"\nTiles with buildings: {tiles_with_content} ({tiles_with_content/len(tiles)*100:.1f}%)")
    print(f"Avg buildings/tile: {avg_buildings:.1f}")

    # Save grid config
    grid_config = {
        "center_wgs84": [center_lon, center_lat],
        "center_utm": [center_x, center_y],
        "mvp_bounds_utm": [min_x, min_y, max_x, max_y],
        "tile_size_m": tile_size_m,
        "grid_size": [n_tiles_x, n_tiles_y],
        "total_tiles": len(tiles),
        "tiles_with_content": tiles_with_content,
        "crs_utm": "EPSG:32723"
    }

    output_dir = DATA_DIR.parent / "grid"
    output_dir.mkdir(exist_ok=True)

    with open(output_dir / "grid_config.json", "w") as f:
        json.dump(grid_config, f, indent=2)

    with open(output_dir / "tiles.json", "w") as f:
        json.dump(tiles, f, indent=2)

    print(f"\nSaved to: {output_dir}")

    # Show top 10 densest tiles
    print("\nTop 10 densest tiles:")
    sorted_tiles = sorted(tiles, key=lambda t: t["buildings_count"], reverse=True)[:10]
    for t in sorted_tiles:
        print(f"  {t['id']}: {t['buildings_count']} buildings")

    print("\n✓ Analysis complete!")

if __name__ == "__main__":
    main()
