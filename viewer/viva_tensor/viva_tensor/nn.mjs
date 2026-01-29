import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";
import * as $autograd from "../viva_tensor/autograd.mjs";
import { Traced } from "../viva_tensor/autograd.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

export class Linear extends $CustomType {
  constructor(w, b) {
    super();
    this.w = w;
    this.b = b;
  }
}
export const Linear$Linear = (w, b) => new Linear(w, b);
export const Linear$isLinear = (value) => value instanceof Linear;
export const Linear$Linear$w = (value) => value.w;
export const Linear$Linear$0 = (value) => value.w;
export const Linear$Linear$b = (value) => value.b;
export const Linear$Linear$1 = (value) => value.b;
