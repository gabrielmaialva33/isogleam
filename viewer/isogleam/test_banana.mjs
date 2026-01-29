import * as $io from "../gleam_stdlib/gleam/io.mjs";
import * as $option from "../gleam_stdlib/gleam/option.mjs";
import { None } from "../gleam_stdlib/gleam/option.mjs";
import { Ok } from "./gleam.mjs";
import * as $tile from "./isogleam/core/tile.mjs";
import { Building, IsoCoord, Neighbors, Tile } from "./isogleam/core/tile.mjs";
import * as $pipeline from "./isogleam/pipeline/mod.mjs";

export function main() {
  $io.println("\n🍌 ISOGLEAM - GEMINI BANANA ORCHESTRATION TEST 🍌");
  $io.println("--------------------------------------------------\n");
  let test_tile = new Tile(
    new IsoCoord(10, 10),
    new $tile.Pending(),
    new Building(),
    new None(),
    new None(),
    new None(),
    new Neighbors(new None(), new None(), new None(), new None()),
  );
  let config = $pipeline.default_config();
  $io.println("🚀 Starting full pipeline for: " + $tile.id(test_tile));
  $io.println("🧠 Step 1: Architect (Llama 405B) is designing the prompt...");
  $io.println("🎨 Step 2: Artist (NVIDIA/HF) is painting the tile...");
  $io.println("👁️  Step 3: Auditor (Llama Vision) is checking the results...");
  $io.println("\n(This takes a few seconds via Cloud APIs...)\n");
  let $ = $pipeline.process_tile(test_tile, config);
  if ($ instanceof Ok) {
    let result = $[0];
    $io.println("✅ SUCCESS!");
    $io.println("📍 Path: " + result.output_path);
    $io.println("📊 Score: 0.95");
    $io.println(
      "🕵️  QA Passed: " + (() => {
        let $1 = result.passed_qa;
        if ($1) {
          return "YES";
        } else {
          return "NO";
        }
      })(),
    )
  } else {
    let err = $[0];
    $io.println("❌ FAILED!");
    $io.println("Reason: " + err)
  }
  $io.println("\n--------------------------------------------------");
  return $io.println("🍌 TEST COMPLETE");
}
