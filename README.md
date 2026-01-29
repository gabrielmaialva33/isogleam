# IsoGleam 💎

**IsoGleam** is a Neural-Symbolic pipeline for generating large-scale isometric pixel art cities (SimCity 2000 style) using real-world GIS data.

It combines the precision of symbolic geometry (OpenStreetMap Whitebox) with the creativity of Generative AI (Qwen/Stable Diffusion), orchestrated by a high-performance Gleam core.

## 🏗 Architecture: "Whitebox Sovereign"

The project follows a strict "Geometry-First" philosophy:

1.  **Truth (Whitebox):** Python scripts convert OSM GeoJSON into exact 2.5D isometric projections (Whitebox). No hallucinations allowed for geometry.
2.  **Style (AI):** Generative models (Qwen-Image-Edit, SDXL) apply textures to the whitebox guide using In-Context Learning.
3.  **Core (Gleam):** A pure Gleam engine manages the grid state, coordinates workers, and stitching logic.

## 📂 Project Structure

```
isogleam/
├── src/                    # Core Engine (Gleam)
│   ├── isogleam.gleam      # Main entry point & Orchestrator
│   ├── isogleam/
│   │   ├── core/           # Grid, Tile, Config logic
│   │   ├── generation/     # Infill algorithms (Spiral, etc.)
│   │   ├── ai/             # Prompt engineering templates
│   │   └── tensor/         # Tensor processing for AI bridge
│   └── isogleam_image_ffi.erl # FFI bindings
│
├── scripts/                # Worker Scripts (Python)
│   ├── gen/                # Generators (Whitebox, AI Style Transfer)
│   ├── etl/                # Data Fetchers (OSM, Satellite)
│   └── analysis/           # Spatial analysis tools
│
├── data/                   # Data Lake (Git-ignored)
│   ├── inputs/             # Raw OSM, Satellite, Grid configs
│   └── outputs/            # Generated Tiles, Whiteboxes, Final Renders
│
├── test/                   # Tests
│   └── assets/             # Reference images for regression testing
│
└── viewer/                 # HTML5/Leaflet Tile Viewer
```

## 🚀 Getting Started

### Prerequisites

- **Gleam & Erlang/OTP:** For the core engine.
- **Python 3.10+:** For the generation workers.
- **uv:** Recommended for Python dependency management.

### Setup

1.  **Install Gleam dependencies:**
    ```sh
    gleam deps download
    ```

2.  **Setup Python environment:**
    ```sh
    cd scripts
    uv sync  # or pip install -r requirements.txt
    ```

### Usage

1.  **Fetch Data (Capão Bonito example):**
    ```sh
    python scripts/etl/fetch_osm.py
    ```

2.  **Generate Whitebox Guide:**
    ```sh
    python scripts/gen/generate_whitebox_osm.py
    ```

3.  **Run the Engine:**
    ```sh
    gleam run
    ```

## 🧪 Development

```sh
gleam test  # Run unit tests
```

## 📜 License

Apache 2.0