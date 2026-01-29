/// Test QA on generated tiles (simplified)
import gleeunit/should
import isogleam/qa/pixel
import isogleam/qa/palette

pub fn generated_tile_load_test() {
  let path = "test_images/generated_tile_1.png"

  let assert Ok(img) = pixel.read_png(path)

  // Check dimensions (should be 512x512)
  should.equal(img.width, 512)
  should.equal(img.height, 512)

  // Check we have pixels
  let pixel_count = list_length(img.pixels)
  should.equal(pixel_count, 512 * 512)
}

pub fn generated_tile_colors_test() {
  let path = "test_images/generated_tile_2.png"

  let assert Ok(img) = pixel.read_png(path)

  // Count unique colors
  let analysis = palette.analyze(img.pixels, palette.isogleam_palette(), 30.0)

  // Should have some colors
  should.be_true(analysis.unique_colors > 0)

  // Print info
  let _ = analysis.unique_colors
}

fn list_length(l: List(a)) -> Int {
  do_list_length(l, 0)
}

fn do_list_length(l: List(a), acc: Int) -> Int {
  case l {
    [] -> acc
    [_, ..rest] -> do_list_length(rest, acc + 1)
  }
}
