#!/usr/bin/env python3
"""
Fetch OSM data for Capão Bonito, SP
Downloads buildings, roads, water, landuse as GeoJSON
"""

import osmnx as ox
import geopandas as gpd
import json
from pathlib import Path
import sys

# Config
PLACE = "Capão Bonito, São Paulo, Brazil"
OUTPUT_DIR = Path(__file__).parent.parent.parent / "data" / "inputs" / "osm"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# OSM tags to fetch
LAYERS = {
    "buildings": {"building": True},
    "roads": {"highway": True},
    "water": {"natural": "water", "waterway": True},
    "landuse": {"landuse": True},
    "amenity": {"amenity": True},  # schools, hospitals, etc
    "leisure": {"leisure": True},  # parks, playgrounds
}

def fetch_layer(place: str, tags: dict, name: str) -> gpd.GeoDataFrame | None:
    """Fetch a single OSM layer"""
    print(f"  Fetching {name}...", end=" ", flush=True)
    try:
        gdf = ox.features_from_place(place, tags=tags)
        print(f"✓ {len(gdf)} features")
        return gdf
    except Exception as e:
        print(f"✗ {e}")
        return None

def save_geojson(gdf: gpd.GeoDataFrame, path: Path):
    """Save GeoDataFrame as GeoJSON, handling complex types"""
    # Convert to WGS84
    gdf = gdf.to_crs(epsg=4326)

    # Drop columns that can't be serialized
    cols_to_drop = []
    for col in gdf.columns:
        if col == 'geometry':
            continue
        try:
            # Test if column is serializable
            gdf[col].iloc[0] if len(gdf) > 0 else None
            if gdf[col].dtype == 'object':
                # Check for lists/dicts that JSON can't handle directly
                sample = gdf[col].dropna().iloc[0] if len(gdf[col].dropna()) > 0 else None
                if isinstance(sample, (list, dict)):
                    gdf[col] = gdf[col].apply(lambda x: json.dumps(x) if isinstance(x, (list, dict)) else x)
        except:
            cols_to_drop.append(col)

    if cols_to_drop:
        gdf = gdf.drop(columns=cols_to_drop)

    gdf.to_file(path, driver="GeoJSON")

def main():
    print(f"=== OSM Data Fetch: {PLACE} ===\n")

    # Get place boundary first
    print("Getting city boundary...", end=" ", flush=True)
    try:
        boundary = ox.geocode_to_gdf(PLACE)
        bounds = boundary.total_bounds  # [minx, miny, maxx, maxy]
        print(f"✓ Bounds: {bounds[0]:.4f},{bounds[1]:.4f} → {bounds[2]:.4f},{bounds[3]:.4f}")

        # Save boundary
        boundary.to_file(OUTPUT_DIR / "boundary.geojson", driver="GeoJSON")

        # Calculate approximate area
        area_km2 = boundary.to_crs(epsg=32723).area.iloc[0] / 1e6  # UTM zone 23S
        print(f"   Area: ~{area_km2:.0f} km²")
    except Exception as e:
        print(f"✗ {e}")
        sys.exit(1)

    print()

    # Fetch each layer
    results = {}
    for name, tags in LAYERS.items():
        gdf = fetch_layer(PLACE, tags, name)
        if gdf is not None and len(gdf) > 0:
            output_path = OUTPUT_DIR / f"{name}.geojson"
            save_geojson(gdf, output_path)
            results[name] = len(gdf)

    print("\n=== Summary ===")
    total = 0
    for name, count in results.items():
        print(f"  {name}: {count} features")
        total += count
    print(f"  TOTAL: {total} features")
    print(f"\nSaved to: {OUTPUT_DIR}")

    # Save metadata
    meta = {
        "place": PLACE,
        "bounds": list(bounds),
        "area_km2": area_km2,
        "layers": results,
        "total_features": total
    }
    with open(OUTPUT_DIR / "metadata.json", "w") as f:
        json.dump(meta, f, indent=2)

    print("\n✓ Done!")

if __name__ == "__main__":
    main()
