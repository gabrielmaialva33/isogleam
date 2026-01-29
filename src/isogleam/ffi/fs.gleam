import gleam/bit_array
import gleam/result

@external(erlang, "fs_ffi", "write_file")
@external(javascript, "./fs_ffi.mjs", "write_file")
fn do_write_file(path: String, data: BitArray) -> Result(Nil, String)

pub fn write_bytes(path: String, data: BitArray) -> Result(Nil, String) {
  case do_write_file(path, data) {
    Ok(_) -> Ok(Nil)
    Error(e) -> Error("Failed to write file: " <> path <> " (" <> e <> ")")
  }
}

pub fn write_base64_image(path: String, b64_data: String) -> Result(Nil, String) {
  use bytes <- result.try(
    bit_array.base64_decode(b64_data)
    |> result.replace_error("Failed to decode Base64"),
  )
  write_bytes(path, bytes)
}
