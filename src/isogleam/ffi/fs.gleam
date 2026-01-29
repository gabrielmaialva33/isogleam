/// File System FFI
/// Helper for writing files and decoding base64
@external(erlang, "file", "write_file")
fn erl_write_file(path: String, data: BitArray) -> Result(Nil, term)

@external(erlang, "base64", "decode")
fn erl_base64_decode(data: String) -> BitArray

pub fn write_bytes(path: String, data: BitArray) -> Result(Nil, String) {
  case erl_write_file(path, data) {
    Ok(_) -> Ok(Nil)
    Error(_) -> Error("Failed to write file: " <> path)
  }
}

pub fn write_base64_image(path: String, b64_data: String) -> Result(Nil, String) {
  let bytes = erl_base64_decode(b64_data)
  write_bytes(path, bytes)
}
