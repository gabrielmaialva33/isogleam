export function get_env(key) {
  // Node.js
  if (typeof process !== "undefined" && process.env && process.env[key]) {
    return { _tag: "Ok", _0: process.env[key] };
  }

  // Vite / Browser
  // Vite prefixes env vars with VITE_ by default for security
  try {
    if (typeof import.meta !== "undefined" && import.meta.env) {
      const val = import.meta.env[key] || import.meta.env["VITE_" + key];
      if (val) return { _tag: "Ok", _0: val };
    }
  } catch (e) {
    // Ignore error
  }

  return { _tag: "Error", _0: undefined };
}
