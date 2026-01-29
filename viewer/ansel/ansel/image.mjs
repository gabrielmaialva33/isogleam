import * as $bit_array from "../../gleam_stdlib/gleam/bit_array.mjs";
import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import * as $snag from "../../snag/snag.mjs";
import * as $ansel from "../ansel.mjs";
import * as $bounding_box from "../ansel/bounding_box.mjs";
import * as $color from "../ansel/color.mjs";
import { CustomType as $CustomType } from "../gleam.mjs";

export class JPEG extends $CustomType {
  constructor(quality, keep_metadata) {
    super();
    this.quality = quality;
    this.keep_metadata = keep_metadata;
  }
}
export const ImageFormat$JPEG = (quality, keep_metadata) =>
  new JPEG(quality, keep_metadata);
export const ImageFormat$isJPEG = (value) => value instanceof JPEG;
export const ImageFormat$JPEG$quality = (value) => value.quality;
export const ImageFormat$JPEG$0 = (value) => value.quality;
export const ImageFormat$JPEG$keep_metadata = (value) => value.keep_metadata;
export const ImageFormat$JPEG$1 = (value) => value.keep_metadata;

export class JPEG2000 extends $CustomType {
  constructor(quality, keep_metadata) {
    super();
    this.quality = quality;
    this.keep_metadata = keep_metadata;
  }
}
export const ImageFormat$JPEG2000 = (quality, keep_metadata) =>
  new JPEG2000(quality, keep_metadata);
export const ImageFormat$isJPEG2000 = (value) => value instanceof JPEG2000;
export const ImageFormat$JPEG2000$quality = (value) => value.quality;
export const ImageFormat$JPEG2000$0 = (value) => value.quality;
export const ImageFormat$JPEG2000$keep_metadata = (value) =>
  value.keep_metadata;
export const ImageFormat$JPEG2000$1 = (value) => value.keep_metadata;

export class JPEGXL extends $CustomType {
  constructor(quality, keep_metadata) {
    super();
    this.quality = quality;
    this.keep_metadata = keep_metadata;
  }
}
export const ImageFormat$JPEGXL = (quality, keep_metadata) =>
  new JPEGXL(quality, keep_metadata);
export const ImageFormat$isJPEGXL = (value) => value instanceof JPEGXL;
export const ImageFormat$JPEGXL$quality = (value) => value.quality;
export const ImageFormat$JPEGXL$0 = (value) => value.quality;
export const ImageFormat$JPEGXL$keep_metadata = (value) => value.keep_metadata;
export const ImageFormat$JPEGXL$1 = (value) => value.keep_metadata;

export class PNG extends $CustomType {}
export const ImageFormat$PNG = () => new PNG();
export const ImageFormat$isPNG = (value) => value instanceof PNG;

export class WebP extends $CustomType {
  constructor(quality, keep_metadata) {
    super();
    this.quality = quality;
    this.keep_metadata = keep_metadata;
  }
}
export const ImageFormat$WebP = (quality, keep_metadata) =>
  new WebP(quality, keep_metadata);
export const ImageFormat$isWebP = (value) => value instanceof WebP;
export const ImageFormat$WebP$quality = (value) => value.quality;
export const ImageFormat$WebP$0 = (value) => value.quality;
export const ImageFormat$WebP$keep_metadata = (value) => value.keep_metadata;
export const ImageFormat$WebP$1 = (value) => value.keep_metadata;

export class AVIF extends $CustomType {
  constructor(quality, keep_metadata) {
    super();
    this.quality = quality;
    this.keep_metadata = keep_metadata;
  }
}
export const ImageFormat$AVIF = (quality, keep_metadata) =>
  new AVIF(quality, keep_metadata);
export const ImageFormat$isAVIF = (value) => value instanceof AVIF;
export const ImageFormat$AVIF$quality = (value) => value.quality;
export const ImageFormat$AVIF$0 = (value) => value.quality;
export const ImageFormat$AVIF$keep_metadata = (value) => value.keep_metadata;
export const ImageFormat$AVIF$1 = (value) => value.keep_metadata;

export class TIFF extends $CustomType {
  constructor(quality, keep_metadata) {
    super();
    this.quality = quality;
    this.keep_metadata = keep_metadata;
  }
}
export const ImageFormat$TIFF = (quality, keep_metadata) =>
  new TIFF(quality, keep_metadata);
export const ImageFormat$isTIFF = (value) => value instanceof TIFF;
export const ImageFormat$TIFF$quality = (value) => value.quality;
export const ImageFormat$TIFF$0 = (value) => value.quality;
export const ImageFormat$TIFF$keep_metadata = (value) => value.keep_metadata;
export const ImageFormat$TIFF$1 = (value) => value.keep_metadata;

export class HEIC extends $CustomType {
  constructor(quality, keep_metadata) {
    super();
    this.quality = quality;
    this.keep_metadata = keep_metadata;
  }
}
export const ImageFormat$HEIC = (quality, keep_metadata) =>
  new HEIC(quality, keep_metadata);
export const ImageFormat$isHEIC = (value) => value instanceof HEIC;
export const ImageFormat$HEIC$quality = (value) => value.quality;
export const ImageFormat$HEIC$0 = (value) => value.quality;
export const ImageFormat$HEIC$keep_metadata = (value) => value.keep_metadata;
export const ImageFormat$HEIC$1 = (value) => value.keep_metadata;

export class FITS extends $CustomType {}
export const ImageFormat$FITS = () => new FITS();
export const ImageFormat$isFITS = (value) => value instanceof FITS;

export class Matlab extends $CustomType {}
export const ImageFormat$Matlab = () => new Matlab();
export const ImageFormat$isMatlab = (value) => value instanceof Matlab;

export class PDF extends $CustomType {}
export const ImageFormat$PDF = () => new PDF();
export const ImageFormat$isPDF = (value) => value instanceof PDF;

export class SVG extends $CustomType {}
export const ImageFormat$SVG = () => new SVG();
export const ImageFormat$isSVG = (value) => value instanceof SVG;

export class HDR extends $CustomType {}
export const ImageFormat$HDR = () => new HDR();
export const ImageFormat$isHDR = (value) => value instanceof HDR;

export class PPM extends $CustomType {}
export const ImageFormat$PPM = () => new PPM();
export const ImageFormat$isPPM = (value) => value instanceof PPM;

export class CSV extends $CustomType {}
export const ImageFormat$CSV = () => new CSV();
export const ImageFormat$isCSV = (value) => value instanceof CSV;

export class GIF extends $CustomType {}
export const ImageFormat$GIF = () => new GIF();
export const ImageFormat$isGIF = (value) => value instanceof GIF;

export class Analyze extends $CustomType {}
export const ImageFormat$Analyze = () => new Analyze();
export const ImageFormat$isAnalyze = (value) => value instanceof Analyze;

export class NIfTI extends $CustomType {}
export const ImageFormat$NIfTI = () => new NIfTI();
export const ImageFormat$isNIfTI = (value) => value instanceof NIfTI;

export class DeepZoom extends $CustomType {}
export const ImageFormat$DeepZoom = () => new DeepZoom();
export const ImageFormat$isDeepZoom = (value) => value instanceof DeepZoom;

export class Custom extends $CustomType {
  constructor(extension, format) {
    super();
    this.extension = extension;
    this.format = format;
  }
}
export const ImageFormat$Custom = (extension, format) =>
  new Custom(extension, format);
export const ImageFormat$isCustom = (value) => value instanceof Custom;
export const ImageFormat$Custom$extension = (value) => value.extension;
export const ImageFormat$Custom$0 = (value) => value.extension;
export const ImageFormat$Custom$format = (value) => value.format;
export const ImageFormat$Custom$1 = (value) => value.format;

class FormatComponents extends $CustomType {
  constructor(extension, options) {
    super();
    this.extension = extension;
    this.options = options;
  }
}

function format_common_options(quality, keep_metadata) {
  return ((("[Q=" + $int.to_string(quality)) + ",strip=") + (() => {
    let _pipe = $bool.to_string(!keep_metadata);
    return $string.lowercase(_pipe);
  })()) + "]";
}

function image_format_to_string(format) {
  if (format instanceof JPEG) {
    let quality = format.quality;
    let keep_metadata = format.keep_metadata;
    return new FormatComponents(
      ".jpeg",
      format_common_options(quality, keep_metadata),
    );
  } else if (format instanceof JPEG2000) {
    let quality = format.quality;
    let keep_metadata = format.keep_metadata;
    return new FormatComponents(
      ".jp2",
      format_common_options(quality, keep_metadata),
    );
  } else if (format instanceof JPEGXL) {
    let quality = format.quality;
    let keep_metadata = format.keep_metadata;
    return new FormatComponents(
      ".jxl",
      format_common_options(quality, keep_metadata),
    );
  } else if (format instanceof PNG) {
    return new FormatComponents(".png", "");
  } else if (format instanceof WebP) {
    let quality = format.quality;
    let keep_metadata = format.keep_metadata;
    return new FormatComponents(
      ".webp",
      format_common_options(quality, keep_metadata),
    );
  } else if (format instanceof AVIF) {
    let quality = format.quality;
    let keep_metadata = format.keep_metadata;
    return new FormatComponents(
      ".avif",
      format_common_options(quality, keep_metadata),
    );
  } else if (format instanceof TIFF) {
    let quality = format.quality;
    let keep_metadata = format.keep_metadata;
    return new FormatComponents(
      ".tiff",
      format_common_options(quality, keep_metadata),
    );
  } else if (format instanceof HEIC) {
    let quality = format.quality;
    let keep_metadata = format.keep_metadata;
    return new FormatComponents(
      ".heic",
      format_common_options(quality, keep_metadata),
    );
  } else if (format instanceof FITS) {
    return new FormatComponents(".fits", "");
  } else if (format instanceof Matlab) {
    return new FormatComponents(".mat", "");
  } else if (format instanceof PDF) {
    return new FormatComponents(".pdf", "");
  } else if (format instanceof SVG) {
    return new FormatComponents(".svg", "");
  } else if (format instanceof HDR) {
    return new FormatComponents(".hdr", "");
  } else if (format instanceof PPM) {
    return new FormatComponents(".ppm", "");
  } else if (format instanceof CSV) {
    return new FormatComponents(".csv", "");
  } else if (format instanceof GIF) {
    return new FormatComponents(".gif", "");
  } else if (format instanceof Analyze) {
    return new FormatComponents(".analyze", "");
  } else if (format instanceof NIfTI) {
    return new FormatComponents(".nii", "");
  } else if (format instanceof DeepZoom) {
    return new FormatComponents(".dzi", "");
  } else {
    let extension = format.extension;
    let format$1 = format.format;
    return new FormatComponents(extension, ("[" + format$1) + "]");
  }
}
