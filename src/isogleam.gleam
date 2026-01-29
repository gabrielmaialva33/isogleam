/// Isometric Gleam - Gerador de mapas pixel art isométricos
/// Inspirado no VIVA e no Isometric NYC
///
/// Arquitetura:
/// - Pure Gleam para lógica de coordenação
/// - FFI para Python (geração IA) e Elixir (GPU)
/// - Pipeline: OSM → Render → IA → Tiles → Viewer
import gleam/float
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import isogleam/core/config.{type Config}
import isogleam/core/grid.{type Grid}
import isogleam/core/tile
import isogleam/generation/infill

/// Estado do gerador
pub type GeneratorState {
  GeneratorState(
    config: Config,
    grid: Grid,
    current_tile: option.Option(tile.IsoCoord),
    errors: List(String),
    started_at: Int,
  )
}

/// Cria novo gerador
pub fn new(config: Config) -> GeneratorState {
  let grid = grid.new(config.grid_width, config.grid_height, config.tile_size)

  GeneratorState(
    config: config,
    grid: grid,
    current_tile: None,
    errors: [],
    started_at: 0,
  )
}

/// Inicializa com configuração padrão
pub fn init() -> GeneratorState {
  new(config.default())
}

/// Próximo tile a gerar
pub fn next_tile(state: GeneratorState) -> option.Option(tile.IsoCoord) {
  infill.next_to_generate(state.grid)
}

/// Marca tile como gerado
pub fn mark_generated(
  state: GeneratorState,
  coord: tile.IsoCoord,
  image_path: String,
) -> GeneratorState {
  case grid.get_tile(state.grid, coord) {
    None -> state
    Some(t) -> {
      let updated_tile = tile.set_image(t, image_path)
      let updated_grid = grid.update_tile(state.grid, updated_tile)
      GeneratorState(..state, grid: updated_grid)
    }
  }
}

/// Marca tile como falhou
pub fn mark_failed(
  state: GeneratorState,
  coord: tile.IsoCoord,
  reason: String,
) -> GeneratorState {
  case grid.get_tile(state.grid, coord) {
    None -> state
    Some(t) -> {
      let updated_tile = tile.set_status(t, tile.Failed(reason))
      let updated_grid = grid.update_tile(state.grid, updated_tile)
      let errors = [reason, ..state.errors]
      GeneratorState(..state, grid: updated_grid, errors: errors)
    }
  }
}

/// Progresso atual
pub fn progress(state: GeneratorState) -> Float {
  grid.progress(state.grid)
}

/// Verifica se completo
pub fn is_complete(state: GeneratorState) -> Bool {
  grid.is_complete(state.grid)
}

/// Estatísticas do estado
pub fn stats(state: GeneratorState) -> String {
  let total = grid.total_tiles(state.grid)
  let generated = state.grid.generated_count
  let pending = state.grid.pending_count
  let pct = progress(state) *. 100.0

  "IsoGleam Stats:\n"
  <> "  Grid: "
  <> int.to_string(state.config.grid_width)
  <> "x"
  <> int.to_string(state.config.grid_height)
  <> "\n"
  <> "  Total tiles: "
  <> int.to_string(total)
  <> "\n"
  <> "  Generated: "
  <> int.to_string(generated)
  <> "\n"
  <> "  Pending: "
  <> int.to_string(pending)
  <> "\n"
  <> "  Progress: "
  <> float.to_string(pct)
  <> "%\n"
  <> "  Errors: "
  <> int.to_string(list.length(state.errors))
}

import gleam/list

/// Entry point
pub fn main() -> Nil {
  io.println("╔═══════════════════════════════════════════════════════╗")
  io.println("║            ISOGLEAM - Pure Gleam v1.0.0               ║")
  io.println("║   Pixel Art City Generator - SimCity 2000 Style       ║")
  io.println("╚═══════════════════════════════════════════════════════╝")
  io.println("")

  // Inicializa com config de teste
  let state = new(config.test_config())

  io.println(stats(state))
  io.println("")

  // Mostra ordem de geração
  io.println("Generation order (spiral from corner):")
  let order = infill.generation_order(state.grid, infill.SpiralFromCorner)
  list.take(order, 10)
  |> list.each(fn(c) {
    io.println(
      "  -> (" <> int.to_string(c.x) <> ", " <> int.to_string(c.y) <> ")",
    )
  })

  io.println("")
  io.println("Ready to generate! Use FFI to call Python/HuggingFace API.")
}
