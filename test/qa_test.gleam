import gleeunit
import gleeunit/should
import isogleam/qa
import isogleam/qa/pixel

const base_path = "/home/mrootx/isogleam/isogleam/test_assets/"

pub fn main() {
  gleeunit.main()
}

// Test: Valid tile should pass
pub fn valid_tile_test() {
  let result = qa.check_file(base_path <> "test_tile.png")

  case result {
    Ok(qa_result) -> {
      // Debug: show full result
      let formatted = qa.format(qa_result)
      case qa_result.passed {
        True -> Nil
        False -> panic as formatted
      }
    }
    Error(e) -> {
      // Print error for debugging
      let msg = case e {
        pixel.FileNotFound -> "FileNotFound"
        pixel.InvalidFormat -> "InvalidFormat"
        pixel.DecodeFailed(s) -> "DecodeFailed: " <> s
      }
      panic as msg
    }
  }
}

// Test: Wrong dimensions should fail
pub fn wrong_dimensions_test() {
  case qa.check_file(base_path <> "wrong_size.png") {
    Ok(qa_result) -> {
      // Should fail dimensions check
      qa_result.passed |> should.be_false()
    }
    Error(_) -> Nil  // File error is also acceptable
  }
}

// Test: Bad palette should have lower score
pub fn bad_palette_test() {
  case qa.check_file(base_path <> "bad_palette.png") {
    Ok(qa_result) -> {
      // Score should be lower due to non-palette colors
      should.be_true(qa_result.score <. 1.0)
    }
    Error(_) -> Nil
  }
}

// Test: Quick check helper
pub fn quick_check_test() {
  let passed = qa.quick_check(base_path <> "test_tile.png")
  passed |> should.be_true()
}
