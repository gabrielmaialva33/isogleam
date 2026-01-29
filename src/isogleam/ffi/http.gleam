/// IsoGleam FFI - HTTP Client
/// Simple HTTP client for API calls to Qwen, NVIDIA NIMs, etc.
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// HTTP Response
pub type Response {
  Response(status: Int, headers: List(#(String, String)), body: BitArray)
}

/// HTTP Error
pub type HttpError {
  ConnectionError(String)
  TimeoutError
  InvalidResponse(String)
}

/// Make a GET request
pub fn get(url: String) -> Result(Response, HttpError) {
  get_with_headers(url, [])
}

/// Make a GET request with headers
pub fn get_with_headers(
  url: String,
  headers: List(#(String, String)),
) -> Result(Response, HttpError) {
  case do_get(url, headers) {
    Ok(#(status, resp_headers, body)) ->
      Ok(Response(status, resp_headers, body))
    Error(reason) -> Error(ConnectionError(reason))
  }
}

/// Make a POST request with JSON body
pub fn post_json(url: String, body: String) -> Result(Response, HttpError) {
  post_json_with_headers(url, body, [])
}

/// Make a POST request with JSON body and headers
pub fn post_json_with_headers(
  url: String,
  body: String,
  headers: List(#(String, String)),
) -> Result(Response, HttpError) {
  case do_post_json(url, body, headers) {
    Ok(#(status, resp_headers, resp_body)) ->
      Ok(Response(status, resp_headers, resp_body))
    Error(reason) -> Error(ConnectionError(reason))
  }
}

/// Check if a URL is reachable
pub fn health_check(url: String) -> Bool {
  case get(url) {
    Ok(Response(status, _, _)) -> status >= 200 && status < 400
    Error(_) -> False
  }
}

/// Build a URL with query parameters
pub fn build_url(
  base: String,
  path: String,
  params: List(#(String, String)),
) -> String {
  let url = base <> path
  case params {
    [] -> url
    _ -> {
      let query =
        list.map(params, fn(p) {
          let #(k, v) = p
          k <> "=" <> url_encode(v)
        })
        |> string.join("&")
      url <> "?" <> query
    }
  }
}

/// Simple URL encoding (basic implementation)
fn url_encode(s: String) -> String {
  string.to_graphemes(s)
  |> list.map(fn(c) {
    case c {
      " " -> "%20"
      "!" -> "%21"
      "#" -> "%23"
      "$" -> "%24"
      "&" -> "%26"
      "'" -> "%27"
      "(" -> "%28"
      ")" -> "%29"
      "*" -> "%2A"
      "+" -> "%2B"
      "," -> "%2C"
      "/" -> "%2F"
      ":" -> "%3A"
      ";" -> "%3B"
      "=" -> "%3D"
      "?" -> "%3F"
      "@" -> "%40"
      "[" -> "%5B"
      "]" -> "%5D"
      _ -> c
    }
  })
  |> string.concat
}

/// Get a header value from response
pub fn get_header(response: Response, name: String) -> Option(String) {
  let lower_name = string.lowercase(name)
  list.find_map(response.headers, fn(h) {
    let #(k, v) = h
    case string.lowercase(k) == lower_name {
      True -> Ok(v)
      False -> Error(Nil)
    }
  })
  |> option.from_result
}

/// Check if response is successful (2xx)
pub fn is_success(response: Response) -> Bool {
  response.status >= 200 && response.status < 300
}

/// Check if response is JSON
pub fn is_json(response: Response) -> Bool {
  case get_header(response, "content-type") {
    Some(ct) -> string.contains(ct, "application/json")
    None -> False
  }
}

// Erlang FFI
// Erlang FFI
@external(erlang, "http_ffi", "get")
@external(javascript, "./http_js.mjs", "do_get")
fn do_get(
  url: String,
  headers: List(#(String, String)),
) -> Result(#(Int, List(#(String, String)), BitArray), String)

@external(erlang, "http_ffi", "post_json")
@external(javascript, "../ffi/http_js.mjs", "do_post_json")
fn do_post_json(
  url: String,
  body: String,
  headers: List(#(String, String)),
) -> Result(#(Int, List(#(String, String)), BitArray), String)
