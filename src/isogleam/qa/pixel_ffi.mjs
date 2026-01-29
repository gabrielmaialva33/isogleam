// FFI for pixel.gleam - PNG reading and decoding in JavaScript
// Works in both Node.js and browser environments

import { decode as pngDecode } from "https://esm.sh/upng-js@2.1.0";

/**
 * Read file from filesystem (Node.js) or fetch (browser)
 * @param {string} path - File path or URL
 * @returns {{ _tag: "Ok", _0: Uint8Array } | { _tag: "Error", _0: string }}
 */
export async function read_file(path) {
  try {
    // Check if we're in Node.js or browser
    if (typeof process !== "undefined" && process.versions?.node) {
      // Node.js environment
      const fs = await import("fs");
      const buffer = fs.readFileSync(path);
      return { _tag: "Ok", _0: new Uint8Array(buffer) };
    } else {
      // Browser environment - use fetch
      const response = await fetch(path);
      if (!response.ok) {
        return { _tag: "Error", _0: `HTTP ${response.status}: ${response.statusText}` };
      }
      const arrayBuffer = await response.arrayBuffer();
      return { _tag: "Ok", _0: new Uint8Array(arrayBuffer) };
    }
  } catch (error) {
    return { _tag: "Error", _0: error.message || "Failed to read file" };
  }
}

/**
 * Decode PNG bytes to image data
 * @param {Uint8Array} bytes - PNG file bytes
 * @returns {{ _tag: "Ok", _0: [number, number, number, Uint8Array] } | { _tag: "Error", _0: string }}
 */
export function png_decode(bytes) {
  try {
    // UPNG.js decode
    const png = pngDecode(bytes.buffer || bytes);

    if (!png || !png.width || !png.height) {
      return { _tag: "Error", _0: "Invalid PNG data" };
    }

    // Get image dimensions
    const width = png.width;
    const height = png.height;

    // Determine color type
    // UPNG doesn't expose this directly, but we can infer from depth/ctype
    // For simplicity, we'll convert everything to RGBA and report type 6
    const colorType = png.ctype || 6; // 0=Gray, 2=RGB, 4=GrayA, 6=RGBA

    // Convert to RGBA (UPNG.toRGBA8 always returns RGBA)
    const rgba = new Uint8Array(pngDecode.toRGBA8(png)[0]);

    // Return tuple: (width, height, colorType, pixelBytes)
    return {
      _tag: "Ok",
      _0: [width, height, colorType, rgba]
    };
  } catch (error) {
    return {
      _tag: "Error",
      _0: error.message || "PNG decode failed"
    };
  }
}

/**
 * Synchronous version of read_file for environments that support it
 * Falls back to throwing error if async is required
 */
export function read_file_sync(path) {
  if (typeof process !== "undefined" && process.versions?.node) {
    // Node.js synchronous read
    try {
      // Dynamic import won't work here, so we can't use it
      // This function should only be called in Node.js context
      const fs = require("fs");
      const buffer = fs.readFileSync(path);
      return { _tag: "Ok", _0: new Uint8Array(buffer) };
    } catch (error) {
      return { _tag: "Error", _0: error.message || "Failed to read file" };
    }
  } else {
    // Browser - cannot do sync file read
    return {
      _tag: "Error",
      _0: "Synchronous file reading not supported in browser. Use async version."
    };
  }
}
