import * as $gleeunit from "../gleeunit/gleeunit.mjs";
import * as $should from "../gleeunit/gleeunit/should.mjs";
import { Ok, makeError } from "./gleam.mjs";
import * as $qa from "./isogleam/qa.mjs";
import * as $pixel from "./isogleam/qa/pixel.mjs";

const FILEPATH = "test/qa_test.gleam";

const base_path = "/home/mrootx/isogleam/isogleam/test_assets/";

export function main() {
  return $gleeunit.main();
}

export function valid_tile_test() {
  let result = $qa.check_file(base_path + "test_tile.png");
  if (result instanceof Ok) {
    let qa_result = result[0];
    let formatted = $qa.format(qa_result);
    let $ = qa_result.passed;
    if ($) {
      return undefined;
    } else {
      throw makeError(
        "panic",
        FILEPATH,
        "qa_test",
        22,
        "valid_tile_test",
        formatted,
        {}
      )
    }
  } else {
    let e = result[0];
    let _block;
    if (e instanceof $pixel.FileNotFound) {
      _block = "FileNotFound";
    } else if (e instanceof $pixel.InvalidFormat) {
      _block = "InvalidFormat";
    } else {
      let s = e[0];
      _block = "DecodeFailed: " + s;
    }
    let msg = _block;
    throw makeError(
      "panic",
      FILEPATH,
      "qa_test",
      32,
      "valid_tile_test",
      msg,
      {}
    )
  }
}

export function wrong_dimensions_test() {
  let $ = $qa.check_file(base_path + "wrong_size.png");
  if ($ instanceof Ok) {
    let qa_result = $[0];
    let _pipe = qa_result.passed;
    return $should.be_false(_pipe);
  } else {
    return undefined;
  }
}

export function bad_palette_test() {
  let $ = $qa.check_file(base_path + "bad_palette.png");
  if ($ instanceof Ok) {
    let qa_result = $[0];
    return $should.be_true(qa_result.score < 1.0);
  } else {
    return undefined;
  }
}

export function quick_check_test() {
  let passed = $qa.quick_check(base_path + "test_tile.png");
  let _pipe = passed;
  return $should.be_true(_pipe);
}
