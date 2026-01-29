export function write_file(path, data) {
  // Node.js environment check
  if (typeof process !== "undefined" && typeof require !== "undefined") {
    try {
      const fs = require("node:fs");
      // data is Uint8Array in JS FFI for BitArray
      fs.writeFileSync(path, data);
      return { _tag: "Ok", _0: undefined };
    } catch (e) {
      return { _tag: "Error", _0: e.toString() };
    }
  }

  return { _tag: "Error", _0: "File system write not supported in this environment" };
}
