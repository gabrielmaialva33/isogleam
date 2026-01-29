#!/usr/bin/env python3
"""
IsoGleam Whitebox Generator (OSM Edition)
Converts OSM GeoJSON data into a 2.5D Whitebox guide for AI stylization.
"""

import json
import numpy as np
import matplotlib.pyplot as plt
import geopandas as gpd
from pathlib import Path
from shapely.geometry import shape, Polygon, MultiPolygon
from matplotlib.patches import Polygon as MPolygon

# Config
PROJECT_ROOT = Path(__file__).parent.parent.parent
OSM_DIR = PROJECT_ROOT / "data" / "inputs" / "osm"
OUTPUT_DIR = PROJECT_ROOT / "data" / "outputs" / "whitebox"
OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

# Colors for AI understanding (Fake Lighting)
COLORS = {
    "roof": "#FFFFFF",      # Brightest
    "wall_west": "#999999", # Medium
    "wall_east": "#666666", # Darker
    "ground": "#111111",    # Near black
    "road": "#222222",
}

# Isometric transformation matrix (True Isometric: 30 degrees)
# x' = (x - y) * cos(30)
# y' = (x + y) * sin(30) - z
COS30 = np.cos(np.radians(30))
SIN30 = np.sin(np.radians(30))

def to_isometric(x, y, z=0):
    """Converts 2D coords + height to isometric screen space"""
    # Centering logic would go here based on bounds
    iso_x = (x - y) * COS30
    iso_y = (x + y) * SIN30 + z
    return iso_x, iso_y

def generate_whitebox_tile(bbox, output_name):
    """
    bbox: [min_lat, min_lon, max_lat, max_lon]
    """
    fig, ax = plt.subplots(figsize=(10, 10), dpi=100)
    ax.set_aspect('equal')
    ax.axis('off')
    fig.patch.set_facecolor('black')
    
    # 1. Load Buildings
    buildings_path = OSM_DIR / "buildings.geojson"
    if not buildings_path.exists():
        print("Buildings data not found!")
        return
        
    gdf = gpd.read_file(buildings_path)
    # Filter by bbox if needed
    
    # 2. Render Loop
    for idx, row in gdf.iterrows():
        geom = row.geometry
        height = float(row.get('height', 10)) * 2 # Scale height for visual impact
        
        if isinstance(geom, Polygon):
            coords = list(geom.exterior.coords)
            
            # Ground footprint
            iso_footprint = [to_isometric(c[0], c[1], 0) for c in coords]
            # Roof
            iso_roof = [to_isometric(c[0], c[1], height) for c in coords]
            
            # Draw Walls (simplified: connect footprint to roof)
            for i in range(len(coords) - 1):
                p1 = coords[i]
                p2 = coords[i+1]
                
                wall_coords = [
                    to_isometric(p1[0], p1[1], 0),
                    to_isometric(p2[0], p2[1], 0),
                    to_isometric(p2[0], p2[1], height),
                    to_isometric(p1[0], p1[1], height)
                ]
                
                # Simple "shading" based on wall orientation
                color = COLORS["wall_west"] if (p2[0] - p1[0]) > 0 else COLORS["wall_east"]
                ax.add_patch(MPolygon(wall_coords, facecolor=color, edgecolor='none', zorder=height))
            
            # Draw Roof
            ax.add_patch(MPolygon(iso_roof, facecolor=COLORS["roof"], edgecolor='none', zorder=height+1))

    plt.savefig(OUTPUT_DIR / f"{output_name}.png", bbox_inches='tight', pad_inches=0)
    plt.close()

if __name__ == "__main__":
    print("🚀 Starting Operation Whitebox Sovereign...")
    # Prototype: Generate first tile from buildings
    generate_whitebox_tile(None, "prototype_nyc_style")
    print(f"✅ Whitebox saved to {OUTPUT_DIR}")
