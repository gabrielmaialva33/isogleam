#!/usr/bin/env python3
"""
Create tile grid centered on commercial center of Capão Bonito
"""

import geopandas as gpd
import json
from pathlib import Path
import numpy as np
from shapely.geometry import Point, box

DATA_DIR = Path(__file__).parent.parent.parent / "data" / "inputs" / "osm"
OUTPUT_DIR = Path(__file__).parent.parent.parent / "data" / "inputs" / "grid"
OUTPUT_DIR.mkdir(exist_ok=True)

# Commercial center (from amenity analysis)
CENTER_LAT = -23.998498
CENTER_LON = -48.339841

# MVP area: 2km x 2km
AREA_SIZE_KM = 2.0
TILE_SIZE_M = 100  # 100m = 512px

def main():
    print("=== Creating Urban Grid ===\n")
    print(f"Center: {CENTER_LAT}, {CENTER_LON}")
    print(f"Area: {AREA_SIZE_KM}km x {AREA_SIZE_KM}km")
    print(f"Tile: {TILE_SIZE_M}m → 512px")

    # Create center point and convert to UTM
    center = gpd.GeoSeries([Point(CENTER_LON, CENTER_LAT)], crs="EPSG:4326")
    center_utm = center.to_crs(epsg=32723)
    cx, cy = center_utm.iloc[0].x, center_utm.iloc[0].y

    # Calculate grid bounds
    half_size = AREA_SIZE_KM * 1000 / 2
    min_x, max_x = cx - half_size, cx + half_size
    min_y, max_y = cy - half_size, cy + half_size

    n_tiles = int(AREA_SIZE_KM * 1000 / TILE_SIZE_M)
    print(f"\nGrid: {n_tiles} x {n_tiles} = {n_tiles**2} tiles")

    # Load all layers
    print("\nLoading OSM layers...")
    buildings = gpd.read_file(DATA_DIR / "buildings.geojson").to_crs(epsg=32723)
    roads = gpd.read_file(DATA_DIR / "roads.geojson").to_crs(epsg=32723)
    water = gpd.read_file(DATA_DIR / "water.geojson").to_crs(epsg=32723)
    amenity = gpd.read_file(DATA_DIR / "amenity.geojson").to_crs(epsg=32723)
    landuse = gpd.read_file(DATA_DIR / "landuse.geojson").to_crs(epsg=32723)
    leisure = gpd.read_file(DATA_DIR / "leisure.geojson").to_crs(epsg=32723)

    # Filter to area
    buildings_area = buildings.cx[min_x:max_x, min_y:max_y]
    roads_area = roads.cx[min_x:max_x, min_y:max_y]
    water_area = water.cx[min_x:max_x, min_y:max_y]
    amenity_area = amenity.cx[min_x:max_x, min_y:max_y]
    landuse_area = landuse.cx[min_x:max_x, min_y:max_y]
    leisure_area = leisure.cx[min_x:max_x, min_y:max_y]

    print(f"  Buildings: {len(buildings_area)}")
    print(f"  Roads: {len(roads_area)}")
    print(f"  Water: {len(water_area)}")
    print(f"  Amenities: {len(amenity_area)}")
    print(f"  Landuse: {len(landuse_area)}")
    print(f"  Leisure: {len(leisure_area)}")

    # Generate tiles
    print("\nGenerating tiles...")
    tiles = []

    for i in range(n_tiles):
        for j in range(n_tiles):
            tile_min_x = min_x + i * TILE_SIZE_M
            tile_max_x = tile_min_x + TILE_SIZE_M
            tile_min_y = min_y + j * TILE_SIZE_M
            tile_max_y = tile_min_y + TILE_SIZE_M

            # Get features in tile
            tile_buildings = buildings_area.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_roads = roads_area.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_water = water_area.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_amenity = amenity_area.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_landuse = landuse_area.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_leisure = leisure_area.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]

            # Extract types
            building_types = []
            if len(tile_buildings) > 0 and 'building' in tile_buildings.columns:
                building_types = [str(x) for x in tile_buildings['building'].dropna().unique()[:5]]

            road_types = []
            if len(tile_roads) > 0 and 'highway' in tile_roads.columns:
                road_types = [str(x) for x in tile_roads['highway'].dropna().unique()[:5]]

            amenity_types = []
            amenity_names = []
            if len(tile_amenity) > 0:
                if 'amenity' in tile_amenity.columns:
                    amenity_types = [str(x) for x in tile_amenity['amenity'].dropna().unique()[:5]]
                if 'name' in tile_amenity.columns:
                    amenity_names = [str(x) for x in tile_amenity['name'].dropna().unique()[:3]]

            landuse_types = []
            if len(tile_landuse) > 0 and 'landuse' in tile_landuse.columns:
                landuse_types = [str(x) for x in tile_landuse['landuse'].dropna().unique()[:3]]

            # Convert to WGS84
            tile_box = gpd.GeoSeries([box(tile_min_x, tile_min_y, tile_max_x, tile_max_y)], crs="EPSG:32723")
            bounds_wgs84 = tile_box.to_crs(epsg=4326).total_bounds

            # Calculate priority
            priority = (
                len(tile_buildings) * 3 +
                len(tile_roads) +
                len(tile_amenity) * 5 +
                len(tile_water) * 2 +
                len(tile_leisure) * 2
            )

            # Generate prompt keywords
            keywords = []
            if 'residential' in building_types or 'house' in building_types:
                keywords.append("residential houses")
            if 'commercial' in building_types or 'retail' in building_types:
                keywords.append("shops")
            if 'school' in amenity_types:
                keywords.append("school building")
            if 'hospital' in amenity_types or 'clinic' in amenity_types:
                keywords.append("hospital")
            if 'fuel' in amenity_types:
                keywords.append("gas station")
            if 'restaurant' in amenity_types or 'fast_food' in amenity_types:
                keywords.append("restaurant")
            if 'place_of_worship' in amenity_types:
                keywords.append("church")
            if 'bank' in amenity_types:
                keywords.append("bank building")
            if 'primary' in road_types or 'secondary' in road_types:
                keywords.append("main road")
            if 'residential' in road_types:
                keywords.append("residential street")
            if len(tile_water) > 0:
                keywords.append("water")
            if len(tile_leisure) > 0:
                keywords.append("park")
            if 'grass' in landuse_types:
                keywords.append("grass field")
            if 'farmland' in landuse_types:
                keywords.append("farmland")

            if not keywords and len(tile_buildings) > 0:
                keywords.append("buildings")
            if not keywords and len(tile_roads) > 0:
                keywords.append("road")
            if not keywords:
                keywords.append("empty lot")

            tiles.append({
                "id": f"tile_{i}_{j}",
                "grid_x": i,
                "grid_y": j,
                "bounds_wgs84": [float(b) for b in bounds_wgs84],
                "buildings": len(tile_buildings),
                "roads": len(tile_roads),
                "water": len(tile_water),
                "amenities": len(tile_amenity),
                "leisure": len(tile_leisure),
                "building_types": building_types,
                "road_types": road_types,
                "amenity_types": amenity_types,
                "amenity_names": amenity_names,
                "landuse_types": landuse_types,
                "keywords": keywords,
                "priority": priority
            })

    # Stats
    tiles_with_content = [t for t in tiles if t["priority"] > 0]
    tiles_by_priority = sorted(tiles, key=lambda t: t["priority"], reverse=True)

    print(f"\nTiles with content: {len(tiles_with_content)} ({len(tiles_with_content)/len(tiles)*100:.1f}%)")

    # Save
    grid_config = {
        "center_wgs84": [CENTER_LON, CENTER_LAT],
        "area_km": AREA_SIZE_KM,
        "tile_size_m": TILE_SIZE_M,
        "grid_size": n_tiles,
        "total_tiles": len(tiles),
        "tiles_with_content": len(tiles_with_content),
        "bounds_utm": [min_x, min_y, max_x, max_y],
        "crs_utm": "EPSG:32723"
    }

    with open(OUTPUT_DIR / "grid_config.json", "w") as f:
        json.dump(grid_config, f, indent=2)

    with open(OUTPUT_DIR / "tiles.json", "w") as f:
        json.dump(tiles, f, indent=2)

    with open(OUTPUT_DIR / "tiles_by_priority.json", "w") as f:
        json.dump(tiles_by_priority[:100], f, indent=2)

    print(f"\nSaved to: {OUTPUT_DIR}")

    # Show top tiles
    print("\nTop 15 priority tiles:")
    for t in tiles_by_priority[:15]:
        kw = ", ".join(t['keywords'][:3])
        names = ", ".join(t['amenity_names'][:2]) if t['amenity_names'] else ""
        extra = f" ({names})" if names else ""
        print(f"  {t['id']}: {t['priority']}p - {kw}{extra}")

    print("\n✓ Grid created!")

if __name__ == "__main__":
    main()
