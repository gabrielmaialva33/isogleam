/// IsoGleam Pipeline - Main Generation Pipeline
/// Orchestrates the full tile generation workflow

import gleam/list
import isogleam/tile.{type Tile}
import isogleam/qa/checker.{type QAConfig}

/// Pipeline stage
pub type Stage {
  Fetch      // Get geo data
  Render     // 3D render to iso view
  Generate   // AI pixel art generation
  QA         // Quality assurance check
  Infill     // Seamless border fix
  Store      // Save to grid
}

/// Pipeline result
pub type PipelineResult {
  PipelineResult(
    tile: Tile,
    stage: Stage,
    passed_qa: Bool,
    score: Float,
    retries: Int,
  )
}

/// Pipeline config
pub type PipelineConfig {
  PipelineConfig(
    max_retries: Int,
    qa_config: QAConfig,
    parallel_tiles: Int,
    save_intermediates: Bool,
  )
}

/// Default pipeline config
pub fn default_config() -> PipelineConfig {
  PipelineConfig(
    max_retries: 3,
    qa_config: checker.default_config(),
    parallel_tiles: 4,
    save_intermediates: False,
  )
}

/// Process a single tile through pipeline
pub fn process_tile(
  tile: Tile,
  config: PipelineConfig,
) -> Result(PipelineResult, String) {
  do_process(tile, config, 0)
}

fn do_process(
  tile: Tile,
  config: PipelineConfig,
  retry: Int,
) -> Result(PipelineResult, String) {
  case retry >= config.max_retries {
    True -> Error("Max retries exceeded for tile")
    False -> {
      // Simulate pipeline stages (actual implementation would call AI/render)
      // For now, return success
      Ok(PipelineResult(
        tile: tile,
        stage: Store,
        passed_qa: True,
        score: 1.0,
        retries: retry,
      ))
    }
  }
}

/// Process multiple tiles in batch
pub fn process_batch(
  tiles: List(Tile),
  config: PipelineConfig,
) -> List(Result(PipelineResult, String)) {
  list.map(tiles, fn(t) { process_tile(t, config) })
}

/// Get pipeline stats
pub type PipelineStats {
  PipelineStats(
    total: Int,
    completed: Int,
    failed: Int,
    avg_score: Float,
    avg_retries: Float,
  )
}

pub fn compute_stats(results: List(Result(PipelineResult, String))) -> PipelineStats {
  let total = list.length(results)

  let #(completed, failed, score_sum, retry_sum) =
    list.fold(results, #(0, 0, 0.0, 0), fn(acc, res) {
      let #(c, f, s, r) = acc
      case res {
        Ok(pr) -> #(c + 1, f, s +. pr.score, r + pr.retries)
        Error(_) -> #(c, f + 1, s, r)
      }
    })

  let avg_score = case completed {
    0 -> 0.0
    n -> score_sum /. int_to_float(n)
  }

  let avg_retries = case completed {
    0 -> 0.0
    n -> int_to_float(retry_sum) /. int_to_float(n)
  }

  PipelineStats(
    total: total,
    completed: completed,
    failed: failed,
    avg_score: avg_score,
    avg_retries: avg_retries,
  )
}

fn int_to_float(n: Int) -> Float {
  case n {
    0 -> 0.0
    _ -> {
      // Simple conversion
      let assert Ok(f) = gleam_stdlib_float_parse(n)
      f
    }
  }
}

@external(erlang, "erlang", "float")
fn gleam_stdlib_float_parse(n: Int) -> Result(Float, Nil)
