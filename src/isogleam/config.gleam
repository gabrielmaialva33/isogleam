/// Config - Configuração do projeto Isometric Gleam
/// Inspirado na filosofia do VIVA: configuração clara e imutável

import gleam/option.{type Option, None, Some}

/// Configuração principal
pub type Config {
  Config(
    // Grid
    grid_width: Int,
    grid_height: Int,
    tile_size: Int,

    // Geração
    model_id: String,
    prompt_template: String,
    negative_prompt: String,
    num_steps: Int,
    guidance_scale: Float,
    mask_percentage: Float,

    // Paths
    output_dir: String,
    input_dir: String,
    dataset_path: String,

    // API
    api_endpoint: Option(String),
    hf_space: Option(String),

    // Performance
    batch_size: Int,
    max_concurrent: Int,
  )
}

/// Configuração padrão para Capão Bonito (centro expandido)
pub fn default() -> Config {
  Config(
    // Grid ~10km² = 20x25 tiles de 512px (~500m cada)
    grid_width: 20,
    grid_height: 25,
    tile_size: 512,

    // Modelo Qwen-Image-Edit-2511
    model_id: "Qwen/Qwen-Image-Edit-2511",
    prompt_template: "Convert to isometric pixel art, SimCity 2000 style, 16-bit graphics, dithering, limited color palette, bird's eye view",
    negative_prompt: "blurry, smooth gradients, realistic, antialiased, modern, high resolution, 3d render",
    num_steps: 28,
    guidance_scale: 3.5,
    mask_percentage: 0.25,

    // Paths
    output_dir: "tiles/output",
    input_dir: "tiles/input",
    dataset_path: "dataset/training_data.csv",

    // API
    api_endpoint: None,
    hf_space: Some("mrootx/gleam-city"),

    // Performance
    batch_size: 1,
    max_concurrent: 4,
  )
}

/// Configuração para teste (grid pequeno)
pub fn test_config() -> Config {
  Config(
    ..default(),
    grid_width: 4,
    grid_height: 4,
    num_steps: 10,
  )
}

/// Configuração para produção (grid grande, mais steps)
pub fn production() -> Config {
  Config(
    ..default(),
    num_steps: 40,
    guidance_scale: 4.0,
    max_concurrent: 8,
  )
}

/// Configuração para fine-tuning
pub fn finetune_config() -> Config {
  Config(
    ..default(),
    num_steps: 50,
    batch_size: 4,
  )
}

/// Atualiza model_id
pub fn with_model(config: Config, model: String) -> Config {
  Config(..config, model_id: model)
}

/// Atualiza grid size
pub fn with_grid_size(config: Config, w: Int, h: Int) -> Config {
  Config(..config, grid_width: w, grid_height: h)
}

/// Atualiza prompt
pub fn with_prompt(config: Config, prompt: String) -> Config {
  Config(..config, prompt_template: prompt)
}

/// Atualiza output dir
pub fn with_output_dir(config: Config, dir: String) -> Config {
  Config(..config, output_dir: dir)
}

/// Atualiza HF Space
pub fn with_hf_space(config: Config, space: String) -> Config {
  Config(..config, hf_space: Some(space))
}

/// Total de tiles
pub fn total_tiles(config: Config) -> Int {
  config.grid_width * config.grid_height
}

/// Tamanho estimado do mapa final (pixels)
pub fn final_size(config: Config) -> #(Int, Int) {
  #(config.grid_width * config.tile_size, config.grid_height * config.tile_size)
}

/// Tempo estimado de geração (segundos, assumindo 15s/tile)
pub fn estimated_time_seconds(config: Config) -> Int {
  let tiles = total_tiles(config)
  let time_per_tile = 15  // segundos
  let parallel_factor = config.max_concurrent
  { tiles * time_per_tile } / parallel_factor
}
