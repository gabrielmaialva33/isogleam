import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import { CustomType as $CustomType, divideFloat } from "../gleam.mjs";
import * as $tensor from "../viva_tensor/tensor.mjs";

export class Scale extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const TensorOp$Scale = ($0) => new Scale($0);
export const TensorOp$isScale = (value) => value instanceof Scale;
export const TensorOp$Scale$0 = (value) => value[0];

export class Normalize extends $CustomType {}
export const TensorOp$Normalize = () => new Normalize();
export const TensorOp$isNormalize = (value) => value instanceof Normalize;

export class SearchResult extends $CustomType {
  constructor(index, similarity) {
    super();
    this.index = index;
    this.similarity = similarity;
  }
}
export const SearchResult$SearchResult = (index, similarity) =>
  new SearchResult(index, similarity);
export const SearchResult$isSearchResult = (value) =>
  value instanceof SearchResult;
export const SearchResult$SearchResult$index = (value) => value.index;
export const SearchResult$SearchResult$0 = (value) => value.index;
export const SearchResult$SearchResult$similarity = (value) => value.similarity;
export const SearchResult$SearchResult$1 = (value) => value.similarity;

function safe_div(a, b) {
  let $ = b > 0;
  if ($) {
    return divideFloat($int.to_float(a), $int.to_float(b));
  } else {
    return 1.0;
  }
}

function float_to_string(f) {
  let rounded = $int.to_float($float.round(f * 100.0)) / 100.0;
  return $float.to_string(rounded);
}
