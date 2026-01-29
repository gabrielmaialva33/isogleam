import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  bitArraySlice,
} from "../../gleam.mjs";
import * as $color from "../../isogleam/qa/color.mjs";
import { RGB } from "../../isogleam/qa/color.mjs";
import { read_file_sync as read_file, png_decode } from "../qa/pixel_ffi.mjs";

export class ImageSize extends $CustomType {
  constructor(width, height) {
    super();
    this.width = width;
    this.height = height;
  }
}
export const ImageSize$ImageSize = (width, height) =>
  new ImageSize(width, height);
export const ImageSize$isImageSize = (value) => value instanceof ImageSize;
export const ImageSize$ImageSize$width = (value) => value.width;
export const ImageSize$ImageSize$0 = (value) => value.width;
export const ImageSize$ImageSize$height = (value) => value.height;
export const ImageSize$ImageSize$1 = (value) => value.height;

export class ImageData extends $CustomType {
  constructor(width, height, pixels, raw_bytes) {
    super();
    this.width = width;
    this.height = height;
    this.pixels = pixels;
    this.raw_bytes = raw_bytes;
  }
}
export const ImageData$ImageData = (width, height, pixels, raw_bytes) =>
  new ImageData(width, height, pixels, raw_bytes);
export const ImageData$isImageData = (value) => value instanceof ImageData;
export const ImageData$ImageData$width = (value) => value.width;
export const ImageData$ImageData$0 = (value) => value.width;
export const ImageData$ImageData$height = (value) => value.height;
export const ImageData$ImageData$1 = (value) => value.height;
export const ImageData$ImageData$pixels = (value) => value.pixels;
export const ImageData$ImageData$2 = (value) => value.pixels;
export const ImageData$ImageData$raw_bytes = (value) => value.raw_bytes;
export const ImageData$ImageData$3 = (value) => value.raw_bytes;

export class FileNotFound extends $CustomType {}
export const ImageError$FileNotFound = () => new FileNotFound();
export const ImageError$isFileNotFound = (value) =>
  value instanceof FileNotFound;

export class InvalidFormat extends $CustomType {}
export const ImageError$InvalidFormat = () => new InvalidFormat();
export const ImageError$isInvalidFormat = (value) =>
  value instanceof InvalidFormat;

export class DecodeFailed extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const ImageError$DecodeFailed = ($0) => new DecodeFailed($0);
export const ImageError$isDecodeFailed = (value) =>
  value instanceof DecodeFailed;
export const ImageError$DecodeFailed$0 = (value) => value[0];

export class North extends $CustomType {}
export const BorderSide$North = () => new North();
export const BorderSide$isNorth = (value) => value instanceof North;

export class South extends $CustomType {}
export const BorderSide$South = () => new South();
export const BorderSide$isSouth = (value) => value instanceof South;

export class East extends $CustomType {}
export const BorderSide$East = () => new East();
export const BorderSide$isEast = (value) => value instanceof East;

export class West extends $CustomType {}
export const BorderSide$West = () => new West();
export const BorderSide$isWest = (value) => value instanceof West;

/**
 * Get image dimensions
 */
export function size(img) {
  return new ImageSize(img.width, img.height);
}

/**
 * Check if image has correct dimensions
 */
export function check_size(img, expected_width, expected_height) {
  return (img.width === expected_width) && (img.height === expected_height);
}

function do_bytes_to_rgb_gray(loop$bytes, loop$acc) {
  while (true) {
    let bytes = loop$bytes;
    let acc = loop$acc;
    if (bytes.bitSize >= 8) {
      let g = bytes.byteAt(0);
      let rest = bitArraySlice(bytes, 8);
      loop$bytes = rest;
      loop$acc = listPrepend(new RGB(g, g, g), acc);
    } else {
      return acc;
    }
  }
}

function do_bytes_to_rgb_graya(loop$bytes, loop$acc) {
  while (true) {
    let bytes = loop$bytes;
    let acc = loop$acc;
    if (bytes.bitSize >= 8 && bytes.bitSize >= 16) {
      let g = bytes.byteAt(0);
      let rest = bitArraySlice(bytes, 16);
      loop$bytes = rest;
      loop$acc = listPrepend(new RGB(g, g, g), acc);
    } else {
      return acc;
    }
  }
}

function do_bytes_to_rgb_rgb(loop$bytes, loop$acc) {
  while (true) {
    let bytes = loop$bytes;
    let acc = loop$acc;
    if (bytes.bitSize >= 8 && bytes.bitSize >= 16 && bytes.bitSize >= 24) {
      let r = bytes.byteAt(0);
      let g = bytes.byteAt(1);
      let b = bytes.byteAt(2);
      let rest = bitArraySlice(bytes, 24);
      loop$bytes = rest;
      loop$acc = listPrepend(new RGB(r, g, b), acc);
    } else {
      return acc;
    }
  }
}

function do_bytes_to_rgb_rgba(loop$bytes, loop$acc) {
  while (true) {
    let bytes = loop$bytes;
    let acc = loop$acc;
    if (
      bytes.bitSize >= 8 &&
      bytes.bitSize >= 16 &&
      bytes.bitSize >= 24 &&
      bytes.bitSize >= 32
    ) {
      let r = bytes.byteAt(0);
      let g = bytes.byteAt(1);
      let b = bytes.byteAt(2);
      let rest = bitArraySlice(bytes, 32);
      loop$bytes = rest;
      loop$acc = listPrepend(new RGB(r, g, b), acc);
    } else {
      return acc;
    }
  }
}

/**
 * Convert raw bytes to RGB list based on bytes per pixel
 * 
 * @ignore
 */
function bytes_to_rgb_list(bytes, bpp) {
  let _block;
  if (bpp === 1) {
    _block = do_bytes_to_rgb_gray(bytes, toList([]));
  } else if (bpp === 2) {
    _block = do_bytes_to_rgb_graya(bytes, toList([]));
  } else if (bpp === 3) {
    _block = do_bytes_to_rgb_rgb(bytes, toList([]));
  } else if (bpp === 4) {
    _block = do_bytes_to_rgb_rgba(bytes, toList([]));
  } else {
    _block = do_bytes_to_rgb_rgb(bytes, toList([]));
  }
  let _pipe = _block;
  return $list.reverse(_pipe);
}

function do_list_at(loop$l, loop$index) {
  while (true) {
    let l = loop$l;
    let index = loop$index;
    if (l instanceof $Empty) {
      return new Error(undefined);
    } else {
      let first = l.head;
      let rest = l.tail;
      let $ = index === 0;
      if ($) {
        return new Ok(first);
      } else {
        loop$l = rest;
        loop$index = index - 1;
      }
    }
  }
}

function list_at(l, index) {
  let $ = index < 0;
  if ($) {
    return new Error(undefined);
  } else {
    return do_list_at(l, index);
  }
}

/**
 * Get pixel at coordinates
 */
export function get_pixel(img, x, y) {
  let $ = (((x >= 0) && (x < img.width)) && (y >= 0)) && (y < img.height);
  if ($) {
    let index = y * img.width + x;
    return list_at(img.pixels, index);
  } else {
    return new Error(undefined);
  }
}

function get_north_border(img, thickness) {
  return $list.flat_map(
    $list.range(0, thickness - 1),
    (y) => {
      return $list.filter_map(
        $list.range(0, img.width - 1),
        (x) => { return get_pixel(img, x, y); },
      );
    },
  );
}

function get_south_border(img, thickness) {
  let start_y = img.height - thickness;
  return $list.flat_map(
    $list.range(start_y, img.height - 1),
    (y) => {
      return $list.filter_map(
        $list.range(0, img.width - 1),
        (x) => { return get_pixel(img, x, y); },
      );
    },
  );
}

function get_east_border(img, thickness) {
  let start_x = img.width - thickness;
  return $list.flat_map(
    $list.range(0, img.height - 1),
    (y) => {
      return $list.filter_map(
        $list.range(start_x, img.width - 1),
        (x) => { return get_pixel(img, x, y); },
      );
    },
  );
}

function get_west_border(img, thickness) {
  return $list.flat_map(
    $list.range(0, img.height - 1),
    (y) => {
      return $list.filter_map(
        $list.range(0, thickness - 1),
        (x) => { return get_pixel(img, x, y); },
      );
    },
  );
}

/**
 * Extract border pixels (for seamless checking)
 */
export function get_border(img, side, thickness) {
  if (side instanceof North) {
    return get_north_border(img, thickness);
  } else if (side instanceof South) {
    return get_south_border(img, thickness);
  } else if (side instanceof East) {
    return get_east_border(img, thickness);
  } else {
    return get_west_border(img, thickness);
  }
}

/**
 * Decode PNG bytes to image data
 */
export function decode_png(bytes) {
  let $ = png_decode(bytes);
  if ($ instanceof Ok) {
    let width = $[0][0];
    let height = $[0][1];
    let color_type = $[0][2];
    let pixel_bytes = $[0][3];
    let _block;
    if (color_type === 0) {
      _block = 1;
    } else if (color_type === 2) {
      _block = 3;
    } else if (color_type === 4) {
      _block = 2;
    } else if (color_type === 6) {
      _block = 4;
    } else {
      _block = 3;
    }
    let bpp = _block;
    let pixels = bytes_to_rgb_list(pixel_bytes, bpp);
    return new Ok(new ImageData(width, height, pixels, pixel_bytes));
  } else {
    let reason = $[0];
    return new Error(new DecodeFailed(reason));
  }
}

/**
 * Read PNG file and extract pixels
 */
export function read_png(path) {
  let $ = read_file(path);
  if ($ instanceof Ok) {
    let bytes = $[0];
    return decode_png(bytes);
  } else {
    return new Error(new FileNotFound());
  }
}
