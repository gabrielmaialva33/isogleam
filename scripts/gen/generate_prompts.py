#!/usr/bin/env python3
"""
Generate AI prompts from OSM tile data for isometric pixel art generation
"""

import json
from pathlib import Path

DATA_DIR = Path(__file__).parent.parent.parent / "data" / "inputs" / "grid"

# Mapping of keywords to detailed descriptions
KEYWORD_PROMPTS = {
    "residential houses": "residential houses with red tile roofs, small front yards",
    "shops": "commercial shops with storefronts, awnings, display windows",
    "school building": "school building with playground, flagpole, yellow school bus",
    "hospital": "hospital building with red cross, ambulance bay, emergency entrance",
    "gas station": "gas station with fuel pumps, convenience store, parked cars",
    "restaurant": "restaurant building with outdoor seating, neon signs",
    "church": "white church with bell tower, stained glass windows, small garden",
    "bank building": "bank building with classical columns, ATM, security cameras",
    "main road": "main asphalt road with lane markings, traffic lights",
    "residential street": "quiet residential street with sidewalks, trees",
    "water": "pond or small river with blue water, reeds",
    "park": "green park with benches, walking paths, trees, flower beds",
    "grass field": "open grass field, green lawn",
    "farmland": "farmland with crops, fences, barn",
    "buildings": "generic city buildings, mixed use",
    "road": "paved road with sidewalks",
    "empty lot": "empty lot with grass patches, dirt",
}

# Base prompt template
BASE_PROMPT = "isometric pixel art city tile, SimCity 2000 style, 16-bit graphics, dithering, limited color palette, bird's eye view"
NEGATIVE = "blurry, smooth gradients, realistic, antialiased, modern 3D, high resolution, photo"

def generate_prompt(tile: dict) -> str:
    """Generate AI prompt for a tile based on its OSM data"""

    # Use actual tile fields
    building_types = tile.get("building_types", [])
    road_types = tile.get("road_types", [])
    amenity_types = tile.get("amenity_types", [])
    landuse_types = tile.get("landuse_types", [])
    classification = tile.get("classification", "empty")

    # Build keywords from actual data
    keywords = []

    # From building types
    for bt in building_types[:2]:
        if bt in ["yes", "residential"]:
            keywords.append("residential houses")
        elif bt in ["commercial", "retail"]:
            keywords.append("shops")
        elif bt == "industrial":
            keywords.append("industrial warehouse")
        elif bt == "church":
            keywords.append("church")

    # From amenities
    for at in amenity_types[:3]:
        if at in ["hospital", "clinic"]:
            keywords.append("hospital")
        elif at in ["school", "university", "college"]:
            keywords.append("school building")
        elif at in ["fuel", "gas_station"]:
            keywords.append("gas station")
        elif at in ["restaurant", "fast_food", "cafe"]:
            keywords.append("restaurant")
        elif at in ["bank"]:
            keywords.append("bank building")
        elif at in ["pharmacy"]:
            keywords.append("pharmacy")
        elif at == "townhall":
            keywords.append("city hall")
        elif at in ["parking"]:
            keywords.append("parking lot")
        else:
            keywords.append(at.replace("_", " "))

    # From road types
    if any(rt in ["primary", "secondary", "tertiary"] for rt in road_types):
        keywords.append("main road")
    elif any(rt in ["residential", "service"] for rt in road_types):
        keywords.append("residential street")

    # From landuse
    for lt in landuse_types[:2]:
        if lt in ["grass", "meadow"]:
            keywords.append("grass field")
        elif lt in ["forest"]:
            keywords.append("trees and forest")
        elif lt == "farmland":
            keywords.append("farmland")

    # From classification if no content
    if not keywords:
        if classification == "green":
            keywords.append("park")
        elif classification == "road_only":
            keywords.append("road")
        else:
            keywords.append("empty lot")

    amenity_names = amenity_types  # Reuse for compatibility

    # Build description from keywords
    descriptions = []
    for kw in keywords[:4]:  # Max 4 keywords
        if kw in KEYWORD_PROMPTS:
            descriptions.append(KEYWORD_PROMPTS[kw])
        else:
            descriptions.append(kw)

    # Add specific location names if interesting
    for name in amenity_names[:2]:
        if name and name != "None":
            # Extract type from name
            name_lower = name.lower()
            if "hospital" in name_lower or "santa casa" in name_lower:
                descriptions.append("large hospital complex")
            elif "escola" in name_lower or "school" in name_lower:
                descriptions.append("school with students")
            elif "banco" in name_lower or "bank" in name_lower:
                descriptions.append("bank with security")
            elif "posto" in name_lower or "fuel" in name_lower:
                descriptions.append("busy gas station")
            elif "farmácia" in name_lower or "drogaria" in name_lower:
                descriptions.append("pharmacy with green cross")
            elif "prefeitura" in name_lower:
                descriptions.append("city hall government building")
            elif "terminal" in name_lower:
                descriptions.append("bus terminal with buses")

    # Remove duplicates while preserving order
    seen = set()
    unique_desc = []
    for d in descriptions:
        if d not in seen:
            seen.add(d)
            unique_desc.append(d)

    # Combine
    content = ", ".join(unique_desc[:5]) if unique_desc else "empty urban lot"

    return f"{BASE_PROMPT}, {content}"

def main():
    print("=== Generating AI Prompts ===\n")

    # Load tiles
    with open(DATA_DIR / "tiles_by_priority.json") as f:
        tiles = json.load(f)

    print(f"Loaded {len(tiles)} tiles")

    # Generate prompts for top tiles
    prompts = []
    for tile in tiles[:50]:  # Top 50
        prompt = generate_prompt(tile)
        prompts.append({
            "id": tile["id"],
            "prompt": prompt,
            "negative": NEGATIVE,
            "priority": tile["priority"],
            "classification": tile.get("classification", "unknown"),
            "buildings": tile.get("buildings", 0),
            "amenities": tile.get("amenities", 0),
            "bounds_wgs84": tile["bounds_wgs84"],
            "center_wgs84": tile.get("center_wgs84", [])
        })

    # Save
    output_path = DATA_DIR / "prompts.json"
    with open(output_path, "w") as f:
        json.dump(prompts, f, indent=2)

    print(f"\nSaved {len(prompts)} prompts to: {output_path}")

    # Preview
    print("\nSample prompts:")
    for p in prompts[:10]:
        print(f"\n{p['id']} (priority {p['priority']}):")
        print(f"  {p['prompt'][:100]}...")

    print("\n✓ Prompts generated!")

if __name__ == "__main__":
    main()
