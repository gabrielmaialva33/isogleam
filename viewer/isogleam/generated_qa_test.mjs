import * as $should from "../gleeunit/gleeunit/should.mjs";
import { Ok, Empty as $Empty, makeError } from "./gleam.mjs";
import * as $palette from "./isogleam/qa/palette.mjs";
import * as $pixel from "./isogleam/qa/pixel.mjs";

const FILEPATH = "test/generated_qa_test.gleam";

export function generated_tile_colors_test() {
  let path = "test_images/generated_tile_2.png";
  let $ = $pixel.read_png(path);
  let img;
  if ($ instanceof Ok) {
    img = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "generated_qa_test",
      23,
      "generated_tile_colors_test",
      "Pattern match failed, no pattern matched the value.",
      { value: $, start: 557, end: 598, pattern_start: 568, pattern_end: 575 }
    )
  }
  let analysis = $palette.analyze(img.pixels, $palette.isogleam_palette(), 30.0);
  $should.be_true(analysis.unique_colors > 0);
  let $1 = analysis.unique_colors;
  
  return $1;
}

function do_list_length(loop$l, loop$acc) {
  while (true) {
    let l = loop$l;
    let acc = loop$acc;
    if (l instanceof $Empty) {
      return acc;
    } else {
      let rest = l.tail;
      loop$l = rest;
      loop$acc = acc + 1;
    }
  }
}

function list_length(l) {
  return do_list_length(l, 0);
}

export function generated_tile_load_test() {
  let path = "test_images/generated_tile_1.png";
  let $ = $pixel.read_png(path);
  let img;
  if ($ instanceof Ok) {
    img = $[0];
  } else {
    throw makeError(
      "let_assert",
      FILEPATH,
      "generated_qa_test",
      9,
      "generated_tile_load_test",
      "Pattern match failed, no pattern matched the value.",
      { value: $, start: 207, end: 248, pattern_start: 218, pattern_end: 225 }
    )
  }
  $should.equal(img.width, 512);
  $should.equal(img.height, 512);
  let pixel_count = list_length(img.pixels);
  return $should.equal(pixel_count, 512 * 512);
}
