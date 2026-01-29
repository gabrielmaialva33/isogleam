import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import { Ok, CustomType as $CustomType, divideInt } from "../../gleam.mjs";
import { get_env as do_get_env } from "./env_ffi.mjs";

export class Config extends $CustomType {
  constructor(grid_width, grid_height, tile_size, model_id, prompt_template, negative_prompt, num_steps, guidance_scale, mask_percentage, output_dir, input_dir, dataset_path, generation_mode, nvidia_api_key, hf_token, api_endpoint, hf_space, batch_size, max_concurrent) {
    super();
    this.grid_width = grid_width;
    this.grid_height = grid_height;
    this.tile_size = tile_size;
    this.model_id = model_id;
    this.prompt_template = prompt_template;
    this.negative_prompt = negative_prompt;
    this.num_steps = num_steps;
    this.guidance_scale = guidance_scale;
    this.mask_percentage = mask_percentage;
    this.output_dir = output_dir;
    this.input_dir = input_dir;
    this.dataset_path = dataset_path;
    this.generation_mode = generation_mode;
    this.nvidia_api_key = nvidia_api_key;
    this.hf_token = hf_token;
    this.api_endpoint = api_endpoint;
    this.hf_space = hf_space;
    this.batch_size = batch_size;
    this.max_concurrent = max_concurrent;
  }
}
export const Config$Config = (grid_width, grid_height, tile_size, model_id, prompt_template, negative_prompt, num_steps, guidance_scale, mask_percentage, output_dir, input_dir, dataset_path, generation_mode, nvidia_api_key, hf_token, api_endpoint, hf_space, batch_size, max_concurrent) =>
  new Config(grid_width,
  grid_height,
  tile_size,
  model_id,
  prompt_template,
  negative_prompt,
  num_steps,
  guidance_scale,
  mask_percentage,
  output_dir,
  input_dir,
  dataset_path,
  generation_mode,
  nvidia_api_key,
  hf_token,
  api_endpoint,
  hf_space,
  batch_size,
  max_concurrent);
export const Config$isConfig = (value) => value instanceof Config;
export const Config$Config$grid_width = (value) => value.grid_width;
export const Config$Config$0 = (value) => value.grid_width;
export const Config$Config$grid_height = (value) => value.grid_height;
export const Config$Config$1 = (value) => value.grid_height;
export const Config$Config$tile_size = (value) => value.tile_size;
export const Config$Config$2 = (value) => value.tile_size;
export const Config$Config$model_id = (value) => value.model_id;
export const Config$Config$3 = (value) => value.model_id;
export const Config$Config$prompt_template = (value) => value.prompt_template;
export const Config$Config$4 = (value) => value.prompt_template;
export const Config$Config$negative_prompt = (value) => value.negative_prompt;
export const Config$Config$5 = (value) => value.negative_prompt;
export const Config$Config$num_steps = (value) => value.num_steps;
export const Config$Config$6 = (value) => value.num_steps;
export const Config$Config$guidance_scale = (value) => value.guidance_scale;
export const Config$Config$7 = (value) => value.guidance_scale;
export const Config$Config$mask_percentage = (value) => value.mask_percentage;
export const Config$Config$8 = (value) => value.mask_percentage;
export const Config$Config$output_dir = (value) => value.output_dir;
export const Config$Config$9 = (value) => value.output_dir;
export const Config$Config$input_dir = (value) => value.input_dir;
export const Config$Config$10 = (value) => value.input_dir;
export const Config$Config$dataset_path = (value) => value.dataset_path;
export const Config$Config$11 = (value) => value.dataset_path;
export const Config$Config$generation_mode = (value) => value.generation_mode;
export const Config$Config$12 = (value) => value.generation_mode;
export const Config$Config$nvidia_api_key = (value) => value.nvidia_api_key;
export const Config$Config$13 = (value) => value.nvidia_api_key;
export const Config$Config$hf_token = (value) => value.hf_token;
export const Config$Config$14 = (value) => value.hf_token;
export const Config$Config$api_endpoint = (value) => value.api_endpoint;
export const Config$Config$15 = (value) => value.api_endpoint;
export const Config$Config$hf_space = (value) => value.hf_space;
export const Config$Config$16 = (value) => value.hf_space;
export const Config$Config$batch_size = (value) => value.batch_size;
export const Config$Config$17 = (value) => value.batch_size;
export const Config$Config$max_concurrent = (value) => value.max_concurrent;
export const Config$Config$18 = (value) => value.max_concurrent;

export class Local extends $CustomType {}
export const GenerationMode$Local = () => new Local();
export const GenerationMode$isLocal = (value) => value instanceof Local;

export class Nvidia extends $CustomType {}
export const GenerationMode$Nvidia = () => new Nvidia();
export const GenerationMode$isNvidia = (value) => value instanceof Nvidia;

export class HuggingFace extends $CustomType {}
export const GenerationMode$HuggingFace = () => new HuggingFace();
export const GenerationMode$isHuggingFace = (value) =>
  value instanceof HuggingFace;

/**
 * Configuração padrão para Capão Bonito (centro expandido)
 */
export function default$() {
  return new Config(
    20,
    25,
    512,
    "stabilityai/stable-diffusion-3.5-large",
    "isometric pixel art, simcity 2000 style, 16-bit graphics, orthographic view",
    "blurry, smooth gradients, realistic, antialiased, 3d render, perspective, vanishing point",
    28,
    5.0,
    0.25,
    "tiles/output",
    "tiles/input",
    "dataset/training_data.csv",
    new Nvidia(),
    new None(),
    new None(),
    new None(),
    new Some("mrootx/gleam-city"),
    1,
    4,
  );
}

/**
 * Carrega configuração do ambiente
 */
export function from_env() {
  let base = default$();
  let nvidia_key = do_get_env("ISOGLEAM_NVIDIA_KEY");
  let hf_token = do_get_env("ISOGLEAM_HF_TOKEN");
  let _block;
  if (nvidia_key instanceof Ok) {
    _block = new Nvidia();
  } else if (hf_token instanceof Ok) {
    _block = new HuggingFace();
  } else {
    _block = new Local();
  }
  let mode = _block;
  return new Config(
    base.grid_width,
    base.grid_height,
    base.tile_size,
    base.model_id,
    base.prompt_template,
    base.negative_prompt,
    base.num_steps,
    base.guidance_scale,
    base.mask_percentage,
    base.output_dir,
    base.input_dir,
    base.dataset_path,
    mode,
    $option.from_result(nvidia_key),
    $option.from_result(hf_token),
    base.api_endpoint,
    base.hf_space,
    base.batch_size,
    base.max_concurrent,
  );
}

/**
 * Configuração para teste (grid pequeno)
 */
export function test_config() {
  let _record = default$();
  return new Config(
    4,
    4,
    _record.tile_size,
    _record.model_id,
    _record.prompt_template,
    _record.negative_prompt,
    10,
    _record.guidance_scale,
    _record.mask_percentage,
    _record.output_dir,
    _record.input_dir,
    _record.dataset_path,
    _record.generation_mode,
    _record.nvidia_api_key,
    _record.hf_token,
    _record.api_endpoint,
    _record.hf_space,
    _record.batch_size,
    _record.max_concurrent,
  );
}

/**
 * Configuração para produção (grid grande, mais steps)
 */
export function production() {
  let _record = default$();
  return new Config(
    _record.grid_width,
    _record.grid_height,
    _record.tile_size,
    _record.model_id,
    _record.prompt_template,
    _record.negative_prompt,
    40,
    4.0,
    _record.mask_percentage,
    _record.output_dir,
    _record.input_dir,
    _record.dataset_path,
    _record.generation_mode,
    _record.nvidia_api_key,
    _record.hf_token,
    _record.api_endpoint,
    _record.hf_space,
    _record.batch_size,
    8,
  );
}

/**
 * Configuração para fine-tuning
 */
export function finetune_config() {
  let _record = default$();
  return new Config(
    _record.grid_width,
    _record.grid_height,
    _record.tile_size,
    _record.model_id,
    _record.prompt_template,
    _record.negative_prompt,
    50,
    _record.guidance_scale,
    _record.mask_percentage,
    _record.output_dir,
    _record.input_dir,
    _record.dataset_path,
    _record.generation_mode,
    _record.nvidia_api_key,
    _record.hf_token,
    _record.api_endpoint,
    _record.hf_space,
    4,
    _record.max_concurrent,
  );
}

/**
 * Atualiza model_id
 */
export function with_model(config, model) {
  return new Config(
    config.grid_width,
    config.grid_height,
    config.tile_size,
    model,
    config.prompt_template,
    config.negative_prompt,
    config.num_steps,
    config.guidance_scale,
    config.mask_percentage,
    config.output_dir,
    config.input_dir,
    config.dataset_path,
    config.generation_mode,
    config.nvidia_api_key,
    config.hf_token,
    config.api_endpoint,
    config.hf_space,
    config.batch_size,
    config.max_concurrent,
  );
}

/**
 * Atualiza grid size
 */
export function with_grid_size(config, w, h) {
  return new Config(
    w,
    h,
    config.tile_size,
    config.model_id,
    config.prompt_template,
    config.negative_prompt,
    config.num_steps,
    config.guidance_scale,
    config.mask_percentage,
    config.output_dir,
    config.input_dir,
    config.dataset_path,
    config.generation_mode,
    config.nvidia_api_key,
    config.hf_token,
    config.api_endpoint,
    config.hf_space,
    config.batch_size,
    config.max_concurrent,
  );
}

/**
 * Atualiza prompt
 */
export function with_prompt(config, prompt) {
  return new Config(
    config.grid_width,
    config.grid_height,
    config.tile_size,
    config.model_id,
    prompt,
    config.negative_prompt,
    config.num_steps,
    config.guidance_scale,
    config.mask_percentage,
    config.output_dir,
    config.input_dir,
    config.dataset_path,
    config.generation_mode,
    config.nvidia_api_key,
    config.hf_token,
    config.api_endpoint,
    config.hf_space,
    config.batch_size,
    config.max_concurrent,
  );
}

/**
 * Atualiza output dir
 */
export function with_output_dir(config, dir) {
  return new Config(
    config.grid_width,
    config.grid_height,
    config.tile_size,
    config.model_id,
    config.prompt_template,
    config.negative_prompt,
    config.num_steps,
    config.guidance_scale,
    config.mask_percentage,
    dir,
    config.input_dir,
    config.dataset_path,
    config.generation_mode,
    config.nvidia_api_key,
    config.hf_token,
    config.api_endpoint,
    config.hf_space,
    config.batch_size,
    config.max_concurrent,
  );
}

/**
 * Atualiza HF Space
 */
export function with_hf_space(config, space) {
  return new Config(
    config.grid_width,
    config.grid_height,
    config.tile_size,
    config.model_id,
    config.prompt_template,
    config.negative_prompt,
    config.num_steps,
    config.guidance_scale,
    config.mask_percentage,
    config.output_dir,
    config.input_dir,
    config.dataset_path,
    config.generation_mode,
    config.nvidia_api_key,
    config.hf_token,
    config.api_endpoint,
    new Some(space),
    config.batch_size,
    config.max_concurrent,
  );
}

/**
 * Total de tiles
 */
export function total_tiles(config) {
  return config.grid_width * config.grid_height;
}

/**
 * Tamanho estimado do mapa final (pixels)
 */
export function final_size(config) {
  return [
    config.grid_width * config.tile_size,
    config.grid_height * config.tile_size,
  ];
}

/**
 * Tempo estimado de geração (segundos, assumindo 15s/tile)
 */
export function estimated_time_seconds(config) {
  let tiles = total_tiles(config);
  let time_per_tile = 15;
  let parallel_factor = config.max_concurrent;
  return divideInt(tiles * time_per_tile, parallel_factor);
}
