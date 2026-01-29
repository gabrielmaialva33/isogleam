import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $autograd from "../viva_tensor/autograd.mjs";
import { Traced } from "../viva_tensor/autograd.mjs";
import * as $nn from "../viva_tensor/nn.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

export class TrainingState extends $CustomType {
  constructor(tape, layer1, layer2) {
    super();
    this.tape = tape;
    this.layer1 = layer1;
    this.layer2 = layer2;
  }
}
export const TrainingState$TrainingState = (tape, layer1, layer2) =>
  new TrainingState(tape, layer1, layer2);
export const TrainingState$isTrainingState = (value) =>
  value instanceof TrainingState;
export const TrainingState$TrainingState$tape = (value) => value.tape;
export const TrainingState$TrainingState$0 = (value) => value.tape;
export const TrainingState$TrainingState$layer1 = (value) => value.layer1;
export const TrainingState$TrainingState$1 = (value) => value.layer1;
export const TrainingState$TrainingState$layer2 = (value) => value.layer2;
export const TrainingState$TrainingState$2 = (value) => value.layer2;

const learning_rate = 0.01;

const epochs = 500;

const input_features = 1;

const hidden_features = 4;

const output_features = 1;
