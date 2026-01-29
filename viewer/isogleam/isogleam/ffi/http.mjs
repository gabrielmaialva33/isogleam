import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, Empty as $Empty, CustomType as $CustomType } from "../../gleam.mjs";
import { do_post_json } from "../ffi/http_js.mjs";
import { do_get } from "./http_js.mjs";

export class Response extends $CustomType {
  constructor(status, headers, body) {
    super();
    this.status = status;
    this.headers = headers;
    this.body = body;
  }
}
export const Response$Response = (status, headers, body) =>
  new Response(status, headers, body);
export const Response$isResponse = (value) => value instanceof Response;
export const Response$Response$status = (value) => value.status;
export const Response$Response$0 = (value) => value.status;
export const Response$Response$headers = (value) => value.headers;
export const Response$Response$1 = (value) => value.headers;
export const Response$Response$body = (value) => value.body;
export const Response$Response$2 = (value) => value.body;

export class ConnectionError extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const HttpError$ConnectionError = ($0) => new ConnectionError($0);
export const HttpError$isConnectionError = (value) =>
  value instanceof ConnectionError;
export const HttpError$ConnectionError$0 = (value) => value[0];

export class TimeoutError extends $CustomType {}
export const HttpError$TimeoutError = () => new TimeoutError();
export const HttpError$isTimeoutError = (value) =>
  value instanceof TimeoutError;

export class InvalidResponse extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const HttpError$InvalidResponse = ($0) => new InvalidResponse($0);
export const HttpError$isInvalidResponse = (value) =>
  value instanceof InvalidResponse;
export const HttpError$InvalidResponse$0 = (value) => value[0];

/**
 * Simple URL encoding (basic implementation)
 * 
 * @ignore
 */
function url_encode(s) {
  let _pipe = $string.to_graphemes(s);
  let _pipe$1 = $list.map(
    _pipe,
    (c) => {
      if (c === " ") {
        return "%20";
      } else if (c === "!") {
        return "%21";
      } else if (c === "#") {
        return "%23";
      } else if (c === "$") {
        return "%24";
      } else if (c === "&") {
        return "%26";
      } else if (c === "'") {
        return "%27";
      } else if (c === "(") {
        return "%28";
      } else if (c === ")") {
        return "%29";
      } else if (c === "*") {
        return "%2A";
      } else if (c === "+") {
        return "%2B";
      } else if (c === ",") {
        return "%2C";
      } else if (c === "/") {
        return "%2F";
      } else if (c === ":") {
        return "%3A";
      } else if (c === ";") {
        return "%3B";
      } else if (c === "=") {
        return "%3D";
      } else if (c === "?") {
        return "%3F";
      } else if (c === "@") {
        return "%40";
      } else if (c === "[") {
        return "%5B";
      } else if (c === "]") {
        return "%5D";
      } else {
        return c;
      }
    },
  );
  return $string.concat(_pipe$1);
}

/**
 * Build a URL with query parameters
 */
export function build_url(base, path, params) {
  let url = base + path;
  if (params instanceof $Empty) {
    return url;
  } else {
    let _block;
    let _pipe = $list.map(
      params,
      (p) => {
        let k;
        let v;
        k = p[0];
        v = p[1];
        return (k + "=") + url_encode(v);
      },
    );
    _block = $string.join(_pipe, "&");
    let query = _block;
    return (url + "?") + query;
  }
}

/**
 * Get a header value from response
 */
export function get_header(response, name) {
  let lower_name = $string.lowercase(name);
  let _pipe = $list.find_map(
    response.headers,
    (h) => {
      let k;
      let v;
      k = h[0];
      v = h[1];
      let $ = $string.lowercase(k) === lower_name;
      if ($) {
        return new Ok(v);
      } else {
        return new Error(undefined);
      }
    },
  );
  return $option.from_result(_pipe);
}

/**
 * Check if response is successful (2xx)
 */
export function is_success(response) {
  return (response.status >= 200) && (response.status < 300);
}

/**
 * Check if response is JSON
 */
export function is_json(response) {
  let $ = get_header(response, "content-type");
  if ($ instanceof Some) {
    let ct = $[0];
    return $string.contains(ct, "application/json");
  } else {
    return false;
  }
}

/**
 * Make a GET request with headers
 */
export function get_with_headers(url, headers) {
  let $ = do_get(url, headers);
  if ($ instanceof Ok) {
    let status = $[0][0];
    let resp_headers = $[0][1];
    let body = $[0][2];
    return new Ok(new Response(status, resp_headers, body));
  } else {
    let reason = $[0];
    return new Error(new ConnectionError(reason));
  }
}

/**
 * Make a GET request
 */
export function get(url) {
  return get_with_headers(url, toList([]));
}

/**
 * Check if a URL is reachable
 */
export function health_check(url) {
  let $ = get(url);
  if ($ instanceof Ok) {
    let status = $[0].status;
    return (status >= 200) && (status < 400);
  } else {
    return false;
  }
}

/**
 * Make a POST request with JSON body and headers
 */
export function post_json_with_headers(url, body, headers) {
  let $ = do_post_json(url, body, headers);
  if ($ instanceof Ok) {
    let status = $[0][0];
    let resp_headers = $[0][1];
    let resp_body = $[0][2];
    return new Ok(new Response(status, resp_headers, resp_body));
  } else {
    let reason = $[0];
    return new Error(new ConnectionError(reason));
  }
}

/**
 * Make a POST request with JSON body
 */
export function post_json(url, body) {
  return post_json_with_headers(url, body, toList([]));
}
