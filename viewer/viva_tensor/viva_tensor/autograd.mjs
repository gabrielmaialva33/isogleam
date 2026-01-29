import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

export class Tape extends $CustomType {
  constructor(next_id, operations) {
    super();
    this.next_id = next_id;
    this.operations = operations;
  }
}
export const Tape$Tape = (next_id, operations) => new Tape(next_id, operations);
export const Tape$isTape = (value) => value instanceof Tape;
export const Tape$Tape$next_id = (value) => value.next_id;
export const Tape$Tape$0 = (value) => value.next_id;
export const Tape$Tape$operations = (value) => value.operations;
export const Tape$Tape$1 = (value) => value.operations;

export class Variable extends $CustomType {
  constructor(id, data) {
    super();
    this.id = id;
    this.data = data;
  }
}
export const Variable$Variable = (id, data) => new Variable(id, data);
export const Variable$isVariable = (value) => value instanceof Variable;
export const Variable$Variable$id = (value) => value.id;
export const Variable$Variable$0 = (value) => value.id;
export const Variable$Variable$data = (value) => value.data;
export const Variable$Variable$1 = (value) => value.data;

export class Traced extends $CustomType {
  constructor(value, tape) {
    super();
    this.value = value;
    this.tape = tape;
  }
}
export const Traced$Traced = (value, tape) => new Traced(value, tape);
export const Traced$isTraced = (value) => value instanceof Traced;
export const Traced$Traced$value = (value) => value.value;
export const Traced$Traced$0 = (value) => value.value;
export const Traced$Traced$tape = (value) => value.tape;
export const Traced$Traced$1 = (value) => value.tape;

/**
 * Creates a new empty tape
 */
export function new_tape() {
  return new Tape(0, $dict.new$());
}

/**
 * Registers a new variable (leaf node) in the graph
 */
export function new_variable(tape, data) {
  let id = tape.next_id;
  let var$ = new Variable(id, data);
  let new_tape$1 = new Tape(id + 1, tape.operations);
  return new Traced(var$, new_tape$1);
}

/**
 * Operation sequencing (Monadic Pipe)
 * Allows chaining layers: x |> sequence(layer1) |> sequence(layer2)
 */
export function sequence(input, layer_fn) {
  return $result.try$(
    input,
    (_use0) => {
      let var$;
      let tape;
      var$ = _use0.value;
      tape = _use0.tape;
      return layer_fn(tape, var$);
    },
  );
}

function string_shape(shape) {
  return ("[" + $string.join($list.map(shape, $int.to_string), ", ")) + "]";
}
