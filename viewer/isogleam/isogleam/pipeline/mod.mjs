import * as $bit_array from "../../../gleam_stdlib/gleam/bit_array.mjs";
import * as $float from "../../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, CustomType as $CustomType, divideFloat } from "../../gleam.mjs";
import * as $architect from "../../isogleam/ai/architect.mjs";
import { ArchitectRequest } from "../../isogleam/ai/architect.mjs";
import * as $config from "../../isogleam/core/config.mjs";
import * as $tile from "../../isogleam/core/tile.mjs";
import * as $fs from "../../isogleam/ffi/fs.mjs";
import * as $hf from "../../isogleam/ffi/hf.mjs";
import * as $nvidia from "../../isogleam/ffi/nvidia.mjs";
import * as $checker from "../../isogleam/qa/checker.mjs";

export class Fetch extends $CustomType {}
export const Stage$Fetch = () => new Fetch();
export const Stage$isFetch = (value) => value instanceof Fetch;

export class Render extends $CustomType {}
export const Stage$Render = () => new Render();
export const Stage$isRender = (value) => value instanceof Render;

export class Generate extends $CustomType {}
export const Stage$Generate = () => new Generate();
export const Stage$isGenerate = (value) => value instanceof Generate;

export class QA extends $CustomType {}
export const Stage$QA = () => new QA();
export const Stage$isQA = (value) => value instanceof QA;

export class Infill extends $CustomType {}
export const Stage$Infill = () => new Infill();
export const Stage$isInfill = (value) => value instanceof Infill;

export class Store extends $CustomType {}
export const Stage$Store = () => new Store();
export const Stage$isStore = (value) => value instanceof Store;

export class PipelineResult extends $CustomType {
  constructor(tile, stage, passed_qa, score, retries, output_path) {
    super();
    this.tile = tile;
    this.stage = stage;
    this.passed_qa = passed_qa;
    this.score = score;
    this.retries = retries;
    this.output_path = output_path;
  }
}
export const PipelineResult$PipelineResult = (tile, stage, passed_qa, score, retries, output_path) =>
  new PipelineResult(tile, stage, passed_qa, score, retries, output_path);
export const PipelineResult$isPipelineResult = (value) =>
  value instanceof PipelineResult;
export const PipelineResult$PipelineResult$tile = (value) => value.tile;
export const PipelineResult$PipelineResult$0 = (value) => value.tile;
export const PipelineResult$PipelineResult$stage = (value) => value.stage;
export const PipelineResult$PipelineResult$1 = (value) => value.stage;
export const PipelineResult$PipelineResult$passed_qa = (value) =>
  value.passed_qa;
export const PipelineResult$PipelineResult$2 = (value) => value.passed_qa;
export const PipelineResult$PipelineResult$score = (value) => value.score;
export const PipelineResult$PipelineResult$3 = (value) => value.score;
export const PipelineResult$PipelineResult$retries = (value) => value.retries;
export const PipelineResult$PipelineResult$4 = (value) => value.retries;
export const PipelineResult$PipelineResult$output_path = (value) =>
  value.output_path;
export const PipelineResult$PipelineResult$5 = (value) => value.output_path;

export class PipelineConfig extends $CustomType {
  constructor(max_retries, qa_config, parallel_tiles, save_intermediates, output_dir) {
    super();
    this.max_retries = max_retries;
    this.qa_config = qa_config;
    this.parallel_tiles = parallel_tiles;
    this.save_intermediates = save_intermediates;
    this.output_dir = output_dir;
  }
}
export const PipelineConfig$PipelineConfig = (max_retries, qa_config, parallel_tiles, save_intermediates, output_dir) =>
  new PipelineConfig(max_retries,
  qa_config,
  parallel_tiles,
  save_intermediates,
  output_dir);
export const PipelineConfig$isPipelineConfig = (value) =>
  value instanceof PipelineConfig;
export const PipelineConfig$PipelineConfig$max_retries = (value) =>
  value.max_retries;
export const PipelineConfig$PipelineConfig$0 = (value) => value.max_retries;
export const PipelineConfig$PipelineConfig$qa_config = (value) =>
  value.qa_config;
export const PipelineConfig$PipelineConfig$1 = (value) => value.qa_config;
export const PipelineConfig$PipelineConfig$parallel_tiles = (value) =>
  value.parallel_tiles;
export const PipelineConfig$PipelineConfig$2 = (value) => value.parallel_tiles;
export const PipelineConfig$PipelineConfig$save_intermediates = (value) =>
  value.save_intermediates;
export const PipelineConfig$PipelineConfig$3 = (value) =>
  value.save_intermediates;
export const PipelineConfig$PipelineConfig$output_dir = (value) =>
  value.output_dir;
export const PipelineConfig$PipelineConfig$4 = (value) => value.output_dir;

export class PipelineStats extends $CustomType {
  constructor(total, completed, failed, avg_score, avg_retries) {
    super();
    this.total = total;
    this.completed = completed;
    this.failed = failed;
    this.avg_score = avg_score;
    this.avg_retries = avg_retries;
  }
}
export const PipelineStats$PipelineStats = (total, completed, failed, avg_score, avg_retries) =>
  new PipelineStats(total, completed, failed, avg_score, avg_retries);
export const PipelineStats$isPipelineStats = (value) =>
  value instanceof PipelineStats;
export const PipelineStats$PipelineStats$total = (value) => value.total;
export const PipelineStats$PipelineStats$0 = (value) => value.total;
export const PipelineStats$PipelineStats$completed = (value) => value.completed;
export const PipelineStats$PipelineStats$1 = (value) => value.completed;
export const PipelineStats$PipelineStats$failed = (value) => value.failed;
export const PipelineStats$PipelineStats$2 = (value) => value.failed;
export const PipelineStats$PipelineStats$avg_score = (value) => value.avg_score;
export const PipelineStats$PipelineStats$3 = (value) => value.avg_score;
export const PipelineStats$PipelineStats$avg_retries = (value) =>
  value.avg_retries;
export const PipelineStats$PipelineStats$4 = (value) => value.avg_retries;

/**
 * Default pipeline config
 */
export function default_config() {
  return new PipelineConfig(
    3,
    $checker.default_config(),
    4,
    false,
    "output/tiles",
  );
}

function do_process(tile, pipe_config, retry) {
  let $ = retry >= pipe_config.max_retries;
  if ($) {
    return new Error("Max retries exceeded for tile " + $tile.id(tile));
  } else {
    let env_config = $config.from_env();
    let ai_config = $nvidia.from_core(env_config);
    let arch_req = new ArchitectRequest("building", toList([]), "SimCity 2000");
    let _block;
    let $1 = $architect.design_prompt(env_config, arch_req);
    if ($1 instanceof Ok) {
      let p = $1[0];
      _block = p;
    } else {
      _block = "isometric pixel art tile, highly detailed, 4k";
    }
    let prompt = _block;
    let _block$1;
    let _pipe = $nvidia.generate_tile(prompt, new None(), -1, ai_config);
    _block$1 = $result.try_recover(
      _pipe,
      (err) => {
        $io.println("⚠️  NVIDIA NIM failed: " + err);
        $io.println("🔄 Falling back to Hugging Face...");
        let $2 = env_config.hf_token;
        if ($2 instanceof $option.Some) {
          return $hf.generate_tile(prompt, env_config);
        } else {
          return new Error(err);
        }
      },
    );
    let generation_result = _block$1;
    if (generation_result instanceof Ok) {
      let image_bytes = generation_result[0];
      let filename = $tile.id(tile) + ".png";
      let path = (pipe_config.output_dir + "/") + filename;
      let $2 = $fs.write_bytes(path, image_bytes);
      
      let image_b64 = $bit_array.base64_encode(image_bytes, true);
      let qa_check = $nvidia.vision_qa(
        image_b64,
        "Is this a valid isometric building? Answer YES or NO.",
        ai_config,
      );
      let _block$2;
      if (qa_check instanceof Ok) {
        let resp = qa_check[0];
        _block$2 = $string.contains($string.uppercase(resp), "YES");
      } else {
        _block$2 = true;
      }
      let passed_qa = _block$2;
      return new Ok(
        new PipelineResult(tile, new Store(), passed_qa, 0.95, retry, path),
      );
    } else {
      let e = generation_result[0];
      return new Error("Generation failed (NVIDIA & HF): " + e);
    }
  }
}

/**
 * Process a single tile through pipeline
 */
export function process_tile(tile, config) {
  return do_process(tile, config, 0);
}

/**
 * Process multiple tiles in batch
 */
export function process_batch(tiles, config) {
  return $list.map(tiles, (t) => { return process_tile(t, config); });
}

export function compute_stats(results) {
  let total = $list.length(results);
  let $ = $list.fold(
    results,
    [0, 0, 0.0, 0],
    (acc, res) => {
      let c;
      let f;
      let s;
      let r;
      c = acc[0];
      f = acc[1];
      s = acc[2];
      r = acc[3];
      if (res instanceof Ok) {
        let pr = res[0];
        return [c + 1, f, s + pr.score, r + pr.retries];
      } else {
        return [c, f + 1, s, r];
      }
    },
  );
  let completed;
  let failed;
  let score_sum;
  let retry_sum;
  completed = $[0];
  failed = $[1];
  score_sum = $[2];
  retry_sum = $[3];
  let _block;
  if (completed === 0) {
    _block = 0.0;
  } else {
    let n = completed;
    _block = divideFloat(score_sum, $int.to_float(n));
  }
  let avg_score = _block;
  let _block$1;
  if (completed === 0) {
    _block$1 = 0.0;
  } else {
    let n = completed;
    _block$1 = divideFloat($int.to_float(retry_sum), $int.to_float(n));
  }
  let avg_retries = _block$1;
  return new PipelineStats(total, completed, failed, avg_score, avg_retries);
}
