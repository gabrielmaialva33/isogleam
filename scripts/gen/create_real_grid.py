#!/usr/bin/env python3
"""
Cria grid de tiles focado no centro urbano REAL de Capão Bonito
"""

import geopandas as gpd
import json
import numpy as np
from pathlib import Path
from shapely.geometry import box
from pyproj import Transformer

# Centro urbano real de Capão Bonito (verificado)
URBAN_CENTER = {
    "lon": -48.3470,
    "lat": -24.0056,
    "name": "Centro de Capão Bonito"
}

# Config do grid
GRID_RADIUS_M = 2500  # 2.5km do centro = 5x5km área
TILE_SIZE_M = 100     # 100m = 512px

DATA_DIR = Path(__file__).parent.parent.parent / "data"
OSM_DIR = DATA_DIR / "osm"
GRID_DIR = DATA_DIR / "grid"

def main():
    print("=" * 60)
    print("IsoGleam Grid Generator - Capão Bonito Centro")
    print("=" * 60)

    # Converter centro para UTM
    transformer_to_utm = Transformer.from_crs("EPSG:4326", "EPSG:32723", always_xy=True)
    transformer_to_wgs = Transformer.from_crs("EPSG:32723", "EPSG:4326", always_xy=True)

    center_x, center_y = transformer_to_utm.transform(URBAN_CENTER["lon"], URBAN_CENTER["lat"])
    print(f"\nCentro: {URBAN_CENTER['name']}")
    print(f"  WGS84: {URBAN_CENTER['lat']:.6f}, {URBAN_CENTER['lon']:.6f}")
    print(f"  UTM: {center_x:.0f}, {center_y:.0f}")

    # Bounds do grid
    min_x = center_x - GRID_RADIUS_M
    max_x = center_x + GRID_RADIUS_M
    min_y = center_y - GRID_RADIUS_M
    max_y = center_y + GRID_RADIUS_M

    grid_size_m = GRID_RADIUS_M * 2
    n_tiles = int(grid_size_m / TILE_SIZE_M)

    print(f"\nGrid: {grid_size_m/1000:.1f}km x {grid_size_m/1000:.1f}km")
    print(f"Tiles: {n_tiles} x {n_tiles} = {n_tiles * n_tiles} tiles")
    print(f"Tile size: {TILE_SIZE_M}m → 512px")

    # Carregar dados OSM
    print("\nCarregando dados OSM...")
    buildings = gpd.read_file(OSM_DIR / "buildings.geojson").to_crs(epsg=32723)
    roads = gpd.read_file(OSM_DIR / "roads.geojson").to_crs(epsg=32723)
    water = gpd.read_file(OSM_DIR / "water.geojson").to_crs(epsg=32723)
    amenity = gpd.read_file(OSM_DIR / "amenity.geojson").to_crs(epsg=32723)
    landuse = gpd.read_file(OSM_DIR / "landuse.geojson").to_crs(epsg=32723)

    print(f"  Buildings: {len(buildings)}")
    print(f"  Roads: {len(roads)}")
    print(f"  Water: {len(water)}")
    print(f"  Amenities: {len(amenity)}")
    print(f"  Landuse: {len(landuse)}")

    # Gerar tiles
    print("\nGerando tiles...")
    tiles = []
    stats = {"buildings": 0, "roads": 0, "with_content": 0}

    for i in range(n_tiles):
        for j in range(n_tiles):
            tile_min_x = min_x + i * TILE_SIZE_M
            tile_max_x = tile_min_x + TILE_SIZE_M
            tile_min_y = min_y + j * TILE_SIZE_M
            tile_max_y = tile_min_y + TILE_SIZE_M

            # Clip features
            tile_buildings = buildings.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_roads = roads.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_water = water.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_amenity = amenity.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]
            tile_landuse = landuse.cx[tile_min_x:tile_max_x, tile_min_y:tile_max_y]

            # Building types
            building_types = []
            if len(tile_buildings) > 0 and 'building' in tile_buildings.columns:
                building_types = tile_buildings['building'].dropna().unique().tolist()[:5]

            # Amenity types
            amenity_types = []
            if len(tile_amenity) > 0 and 'amenity' in tile_amenity.columns:
                amenity_types = tile_amenity['amenity'].dropna().unique().tolist()[:5]

            # Road types
            road_types = []
            if len(tile_roads) > 0 and 'highway' in tile_roads.columns:
                road_types = tile_roads['highway'].dropna().unique().tolist()[:5]

            # Landuse types
            landuse_types = []
            if len(tile_landuse) > 0 and 'landuse' in tile_landuse.columns:
                landuse_types = tile_landuse['landuse'].dropna().unique().tolist()[:5]

            # Bounds em WGS84
            sw_lon, sw_lat = transformer_to_wgs.transform(tile_min_x, tile_min_y)
            ne_lon, ne_lat = transformer_to_wgs.transform(tile_max_x, tile_max_y)

            # Priority score
            priority = (
                len(tile_buildings) * 3 +
                len(tile_roads) * 1 +
                len(tile_amenity) * 5 +
                len(tile_water) * 2
            )

            # Classificar tile
            tile_class = classify_tile(
                len(tile_buildings),
                len(tile_roads),
                len(tile_amenity),
                building_types,
                landuse_types
            )

            tile = {
                "id": f"tile_{i:02d}_{j:02d}",
                "grid_x": i,
                "grid_y": j,
                "bounds_wgs84": [sw_lon, sw_lat, ne_lon, ne_lat],
                "center_wgs84": [(sw_lon + ne_lon) / 2, (sw_lat + ne_lat) / 2],
                "buildings": len(tile_buildings),
                "roads": len(tile_roads),
                "water": len(tile_water),
                "amenities": len(tile_amenity),
                "landuse": len(tile_landuse),
                "building_types": building_types,
                "road_types": road_types,
                "amenity_types": amenity_types,
                "landuse_types": landuse_types,
                "priority": priority,
                "classification": tile_class
            }
            tiles.append(tile)

            # Stats
            stats["buildings"] += len(tile_buildings)
            stats["roads"] += len(tile_roads)
            if priority > 0:
                stats["with_content"] += 1

    # Sort by priority
    tiles_sorted = sorted(tiles, key=lambda t: t["priority"], reverse=True)

    # Grid config
    sw_lon, sw_lat = transformer_to_wgs.transform(min_x, min_y)
    ne_lon, ne_lat = transformer_to_wgs.transform(max_x, max_y)

    grid_config = {
        "name": "capao_bonito_centro",
        "center_wgs84": [URBAN_CENTER["lon"], URBAN_CENTER["lat"]],
        "bounds_wgs84": [sw_lon, sw_lat, ne_lon, ne_lat],
        "tile_size_m": TILE_SIZE_M,
        "tile_size_px": 512,
        "grid_size": [n_tiles, n_tiles],
        "total_tiles": len(tiles),
        "tiles_with_content": stats["with_content"],
        "total_buildings": stats["buildings"],
        "total_roads": stats["roads"],
        "crs_utm": "EPSG:32723"
    }

    # Salvar
    GRID_DIR.mkdir(exist_ok=True)

    with open(GRID_DIR / "grid_config.json", "w") as f:
        json.dump(grid_config, f, indent=2)

    with open(GRID_DIR / "tiles.json", "w") as f:
        json.dump(tiles, f, indent=2)

    with open(GRID_DIR / "tiles_by_priority.json", "w") as f:
        json.dump(tiles_sorted, f, indent=2)

    # Resumo
    print("\n" + "=" * 60)
    print("RESUMO")
    print("=" * 60)
    print(f"Total tiles: {len(tiles)}")
    print(f"Com conteúdo: {stats['with_content']} ({stats['with_content']/len(tiles)*100:.1f}%)")
    print(f"Total prédios: {stats['buildings']}")
    print(f"Total ruas: {stats['roads']}")

    # Top tiles
    print("\nTop 10 tiles por prioridade:")
    for t in tiles_sorted[:10]:
        print(f"  {t['id']}: {t['buildings']}b {t['roads']}r {t['amenities']}a [{t['classification']}]")

    # Por classificação
    from collections import Counter
    class_counts = Counter(t["classification"] for t in tiles)
    print("\nPor classificação:")
    for cls, count in class_counts.most_common():
        print(f"  {cls}: {count} tiles")

    print(f"\nSalvo em: {GRID_DIR}")
    print("\n✓ Grid gerado!")

def classify_tile(buildings, roads, amenities, building_types, landuse_types):
    """Classifica o tile baseado no conteúdo"""
    if amenities > 0:
        return "urban_center"
    if buildings > 5 and roads > 2:
        return "residential_dense"
    if buildings > 0 and roads > 0:
        return "residential"
    if roads > 0:
        return "road_only"
    if any(lt in ["forest", "grass", "meadow"] for lt in landuse_types):
        return "green"
    if buildings > 0:
        return "rural"
    return "empty"

if __name__ == "__main__":
    main()
