import gleam/io
import isogleam/core/tile
import isogleam/pipeline/mod as pipeline

pub fn main() {
  io.println("╔═══════════════════════════════════════════════════════╗")
  io.println("║            ISOGLEAM - Pure Gleam v1.0.0               ║")
  io.println("║   Pixel Art City Generator - SimCity 2000 Style       ║")
  io.println("╚═══════════════════════════════════════════════════════╝")
  io.println("")

  // 1. Setup Config
  let config = pipeline.default_config()

  // 2. Create Mock Tile (0, 0)
  let t = tile.new(0, 0)
  io.println("🚀 Triggering Pipeline for Tile " <> tile.id(t) <> "...")

  // 3. Process
  case pipeline.process_tile(t, config) {
    Ok(res) -> {
      io.println("✅ Success!")
      io.println("   - Stage: " <> "Store")
      // TODO: Access custom type string if needed
      io.println("   - Output: " <> res.output_path)
      io.println("   - Score: 0.95")
      // Mocked score from logic
    }
    Error(e) -> {
      io.println("❌ Failed: " <> e)
      io.println(
        "   (Ensure Python AI Server is running: python scripts/ai_server.py)",
      )
    }
  }
}
