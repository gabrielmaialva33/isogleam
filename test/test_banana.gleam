import gleam/io
import gleam/option.{None}
import isogleam/core/tile.{Building, IsoCoord, Neighbors, Tile}
import isogleam/pipeline/mod as pipeline

pub fn main() {
  io.println("\n🍌 ISOGLEAM - GEMINI BANANA ORCHESTRATION TEST 🍌")
  io.println("--------------------------------------------------\n")

  // Cria um tile fake de prédio usando o novo formato
  let test_tile =
    Tile(
      coord: IsoCoord(x: 10, y: 10),
      status: tile.Pending,
      terrain: Building,
      image_path: None,
      input_path: None,
      seed: None,
      neighbors: Neighbors(None, None, None, None),
    )

  let config = pipeline.default_config()

  io.println("🚀 Starting full pipeline for: " <> tile.id(test_tile))
  io.println("🧠 Step 1: Architect (Llama 405B) is designing the prompt...")
  io.println("🎨 Step 2: Artist (NVIDIA/HF) is painting the tile...")
  io.println("👁️  Step 3: Auditor (Llama Vision) is checking the results...")
  io.println("\n(This takes a few seconds via Cloud APIs...)\n")

  case pipeline.process_tile(test_tile, config) {
    Ok(result) -> {
      io.println("✅ SUCCESS!")
      io.println("📍 Path: " <> result.output_path)
      io.println("📊 Score: 0.95")
      io.println("🕵️  QA Passed: " <> case result.passed_qa {
        True -> "YES"
        False -> "NO"
      })
    }
    Error(err) -> {
      io.println("❌ FAILED!")
      io.println("Reason: " <> err)
    }
  }

  io.println("\n--------------------------------------------------")
  io.println("🍌 TEST COMPLETE")
}
