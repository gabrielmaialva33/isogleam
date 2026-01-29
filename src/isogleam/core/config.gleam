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
    // API & Cloud
    generation_mode: GenerationMode,
    nvidia_api_key: Option(String),
    hf_token: Option(String),
    api_endpoint: Option(String),
    hf_space: Option(String),
    // Performance
    batch_size: Int,
    max_concurrent: Int,
  )
}

pub type GenerationMode {
  Local
  // RTX 4090 (ai_server.py)
  Nvidia
  // NVIDIA NIM API (Cloud)
  HuggingFace
  // HF Inference API (Cloud)
}

/// Configuração padrão para Capão Bonito (centro expandido)
pub fn default() -> Config {
  Config(
    // Grid ~10km² = 20x25 tiles de 512px (~500m cada)
    grid_width: 20,
    grid_height: 25,
    tile_size: 512,
    // Modelo
    model_id: "stabilityai/stable-diffusion-3.5-large",
    // NVIDIA Default
    prompt_template: "isometric pixel art, simcity 2000 style, 16-bit graphics, orthographic view",
    negative_prompt: "blurry, smooth gradients, realistic, antialiased, 3d render, perspective, vanishing point",
    num_steps: 28,
    guidance_scale: 5.0,
    mask_percentage: 0.25,
    // Paths
    output_dir: "tiles/output",
    input_dir: "tiles/input",
    dataset_path: "dataset/training_data.csv",
    // API & Cloud
    generation_mode: Nvidia,
    // Cloud First
    nvidia_api_key: None,
    // Set via ISOGLEAM_NVIDIA_KEY
    hf_token: None,
    // Set via ISOGLEAM_HF_TOKEN
    api_endpoint: None,
    hf_space: Some("mrootx/gleam-city"),
    // Performance
    batch_size: 1,
    max_concurrent: 4,
  )
}

/// Carrega configuração do ambiente
pub fn from_env() -> Config {
  let base = default()

  // Detecta chaves para habilitar modo Cloud automaticamente
  let nvidia_key = do_get_env("ISOGLEAM_NVIDIA_KEY")
  let hf_token = do_get_env("ISOGLEAM_HF_TOKEN")

  let mode = case nvidia_key, hf_token {
    Ok(_), _ -> Nvidia
    _, Ok(_) -> HuggingFace
    _, _ -> Local
  }

  Config(
    ..base,
    generation_mode: mode,
    nvidia_api_key: option.from_result(nvidia_key),
    hf_token: option.from_result(hf_token),
  )
}

@external(erlang, "env_ffi", "get_env")
@external(javascript, "./env_ffi.mjs", "get_env")
fn do_get_env(key: String) -> Result(String, Nil)

/// Configuração para teste (grid pequeno)
pub fn test_config() -> Config {
  Config(..default(), grid_width: 4, grid_height: 4, num_steps: 10)
}

/// Configuração para produção (grid grande, mais steps)
pub fn production() -> Config {
  Config(..default(), num_steps: 40, guidance_scale: 4.0, max_concurrent: 8)
}

/// Configuração para fine-tuning
pub fn finetune_config() -> Config {
  Config(..default(), num_steps: 50, batch_size: 4)
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
  let time_per_tile = 15
  // segundos
  let parallel_factor = config.max_concurrent
  { tiles * time_per_tile } / parallel_factor
}
