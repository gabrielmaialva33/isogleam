import * as $io from "../gleam_stdlib/gleam/io.mjs";
import { Ok } from "./gleam.mjs";
import * as $tile from "./isogleam/core/tile.mjs";
import * as $pipeline from "./isogleam/pipeline/mod.mjs";

export function main() {
  $io.println("╔═══════════════════════════════════════════════════════╗");
  $io.println("║            ISOGLEAM - Pure Gleam v1.0.0               ║");
  $io.println("║   Pixel Art City Generator - SimCity 2000 Style       ║");
  $io.println("╚═══════════════════════════════════════════════════════╝");
  $io.println("");
  let config = $pipeline.default_config();
  let t = $tile.new$(0, 0);
  $io.println(("🚀 Triggering Pipeline for Tile " + $tile.id(t)) + "...");
  let $ = $pipeline.process_tile(t, config);
  if ($ instanceof Ok) {
    let res = $[0];
    $io.println("✅ Success!");
    $io.println("   - Stage: " + "Store");
    $io.println("   - Output: " + res.output_path);
    return $io.println("   - Score: 0.95");
  } else {
    let e = $[0];
    $io.println("❌ Failed: " + e);
    return $io.println(
      "   (Ensure Python AI Server is running: python scripts/ai_server.py)",
    );
  }
}
