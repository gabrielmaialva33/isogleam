import { Empty as $Empty, CustomType as $CustomType } from "../gleam.mjs";

export class Batch extends $CustomType {}
export const Axis$Batch = () => new Batch();
export const Axis$isBatch = (value) => value instanceof Batch;

export class Seq extends $CustomType {}
export const Axis$Seq = () => new Seq();
export const Axis$isSeq = (value) => value instanceof Seq;

export class Feature extends $CustomType {}
export const Axis$Feature = () => new Feature();
export const Axis$isFeature = (value) => value instanceof Feature;

export class Height extends $CustomType {}
export const Axis$Height = () => new Height();
export const Axis$isHeight = (value) => value instanceof Height;

export class Width extends $CustomType {}
export const Axis$Width = () => new Width();
export const Axis$isWidth = (value) => value instanceof Width;

export class Channel extends $CustomType {}
export const Axis$Channel = () => new Channel();
export const Axis$isChannel = (value) => value instanceof Channel;

export class Input extends $CustomType {}
export const Axis$Input = () => new Input();
export const Axis$isInput = (value) => value instanceof Input;

export class Output extends $CustomType {}
export const Axis$Output = () => new Output();
export const Axis$isOutput = (value) => value instanceof Output;

export class Head extends $CustomType {}
export const Axis$Head = () => new Head();
export const Axis$isHead = (value) => value instanceof Head;

export class Embed extends $CustomType {}
export const Axis$Embed = () => new Embed();
export const Axis$isEmbed = (value) => value instanceof Embed;

/**
 * Custom named axis
 */
export class Named extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Axis$Named = ($0) => new Named($0);
export const Axis$isNamed = (value) => value instanceof Named;
export const Axis$Named$0 = (value) => value[0];

export class Anon extends $CustomType {}
export const Axis$Anon = () => new Anon();
export const Axis$isAnon = (value) => value instanceof Anon;

export class AxisSpec extends $CustomType {
  constructor(name, size) {
    super();
    this.name = name;
    this.size = size;
  }
}
export const AxisSpec$AxisSpec = (name, size) => new AxisSpec(name, size);
export const AxisSpec$isAxisSpec = (value) => value instanceof AxisSpec;
export const AxisSpec$AxisSpec$name = (value) => value.name;
export const AxisSpec$AxisSpec$0 = (value) => value.name;
export const AxisSpec$AxisSpec$size = (value) => value.size;
export const AxisSpec$AxisSpec$1 = (value) => value.size;

/**
 * Create axis spec
 */
export function axis(name, size) {
  return new AxisSpec(name, size);
}

/**
 * Batch dimension
 */
export function batch(size) {
  return new AxisSpec(new Batch(), size);
}

/**
 * Sequence dimension
 */
export function seq(size) {
  return new AxisSpec(new Seq(), size);
}

/**
 * Feature dimension
 */
export function feature(size) {
  return new AxisSpec(new Feature(), size);
}

/**
 * Height dimension
 */
export function height(size) {
  return new AxisSpec(new Height(), size);
}

/**
 * Width dimension
 */
export function width(size) {
  return new AxisSpec(new Width(), size);
}

/**
 * Channel dimension
 */
export function channel(size) {
  return new AxisSpec(new Channel(), size);
}

/**
 * Input dimension
 */
export function input(size) {
  return new AxisSpec(new Input(), size);
}

/**
 * Output dimension
 */
export function output(size) {
  return new AxisSpec(new Output(), size);
}

/**
 * Head dimension
 */
export function head(size) {
  return new AxisSpec(new Head(), size);
}

/**
 * Embed dimension
 */
export function embed(size) {
  return new AxisSpec(new Embed(), size);
}

/**
 * Custom named dimension
 */
export function named(name, size) {
  return new AxisSpec(new Named(name), size);
}

/**
 * Check if two axes are equal
 */
export function equals(a, b) {
  if (a instanceof Batch) {
    if (b instanceof Batch) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Seq) {
    if (b instanceof Seq) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Feature) {
    if (b instanceof Feature) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Height) {
    if (b instanceof Height) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Width) {
    if (b instanceof Width) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Channel) {
    if (b instanceof Channel) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Input) {
    if (b instanceof Input) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Output) {
    if (b instanceof Output) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Head) {
    if (b instanceof Head) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Embed) {
    if (b instanceof Embed) {
      return true;
    } else {
      return false;
    }
  } else if (a instanceof Named) {
    if (b instanceof Named) {
      let s1 = a[0];
      let s2 = b[0];
      return s1 === s2;
    } else {
      return false;
    }
  } else if (b instanceof Anon) {
    return true;
  } else {
    return false;
  }
}

/**
 * Check if two axis spec lists are equal
 */
export function specs_equal(a, b) {
  if (a instanceof $Empty) {
    if (b instanceof $Empty) {
      return true;
    } else {
      return false;
    }
  } else if (b instanceof $Empty) {
    return false;
  } else {
    let a1 = a.head;
    let a_rest = a.tail;
    let b1 = b.head;
    let b_rest = b.tail;
    return (equals(a1.name, b1.name) && (a1.size === b1.size)) && specs_equal(
      a_rest,
      b_rest,
    );
  }
}

/**
 * Get human-readable axis name
 */
export function to_string(a) {
  if (a instanceof Batch) {
    return "batch";
  } else if (a instanceof Seq) {
    return "seq";
  } else if (a instanceof Feature) {
    return "feature";
  } else if (a instanceof Height) {
    return "height";
  } else if (a instanceof Width) {
    return "width";
  } else if (a instanceof Channel) {
    return "channel";
  } else if (a instanceof Input) {
    return "input";
  } else if (a instanceof Output) {
    return "output";
  } else if (a instanceof Head) {
    return "head";
  } else if (a instanceof Embed) {
    return "embed";
  } else if (a instanceof Named) {
    let s = a[0];
    return s;
  } else {
    return "_";
  }
}
