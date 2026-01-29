import * as $bool from "../../gleam_stdlib/gleam/bool.mjs";
import * as $float from "../../gleam_stdlib/gleam/float.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $snag from "../../snag/snag.mjs";
import { Ok, Error, toList, CustomType as $CustomType } from "../gleam.mjs";

class LTWH extends $CustomType {
  constructor(left, top, width, height) {
    super();
    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;
  }
}

class LTRB extends $CustomType {
  constructor(left, top, right, bottom) {
    super();
    this.left = left;
    this.top = top;
    this.right = right;
    this.bottom = bottom;
  }
}

class X1Y1X2Y2 extends $CustomType {
  constructor(x1, y1, x2, y2) {
    super();
    this.x1 = x1;
    this.y1 = y1;
    this.x2 = x2;
    this.y2 = y2;
  }
}

/**
 * Creates a new bounding box from the given values in the (Left, Top), (Width, 
 * Height) rectangular coordinate format.
 */
export function ltwh(left, top, width, height) {
  let $ = (((width > 0) && (height > 0)) && (left >= 0)) && (top >= 0);
  if ($) {
    return new Ok(new LTWH(left, top, width, height));
  } else {
    return $snag.error("Impossible ltwh bounding box values passed");
  }
}

export function unchecked_ltwh(left, top, width, height) {
  return new LTWH(left, top, width, height);
}

/**
 * Creates a new bounding box from the given values in the (Left, Top), (Right, 
 * Bottom) rectangular coordinate format.
 */
export function ltrb(left, top, right, bottom) {
  let $ = (((left < right) && (top < bottom)) && (left >= 0)) && (top >= 0);
  if ($) {
    return new Ok(new LTRB(left, top, right, bottom));
  } else {
    return $snag.error("Impossible ltrb bounding box values passed");
  }
}

/**
 * Creates a new bounding box from the given values in the (x1, y1), (x2, y2) 
 * rectangular coordinate format.
 */
export function x1y1x2y2(x1, y1, x2, y2) {
  let $ = (((x1 < x2) && (y1 < y2)) && (x1 >= 0)) && (y1 >= 0);
  if ($) {
    return new Ok(new X1Y1X2Y2(x1, y1, x2, y2));
  } else {
    return $snag.error("Impossible x1y1x2y2 bounding box values passed");
  }
}

/**
 * Converts a bounding box to a tuple with the coordinate values left, top, 
 * width, height. Useful for working with with custom bounding box operations
 * and getting the width and height of a bounding box.
 * 
 * ## Example
 * ```gleam
 * let assert Ok(box) = bounding_box.x1y1x2y2(x1: 2, y1: 2, x2: 6, y2: 6) 
 * bounding_box.to_ltwh_tuple(box)
 * // -> #(2, 2, 4, 4)
 * ```
 * 
 * ```gleam
 * let assert Ok(box) = bounding_box.x1y1x2y2(x1: 4, y1: 4, x2: 10, y2: 10) 
 * let #(_, _, width, height) = bounding_box.to_ltwh_tuple(box)
 * // -> 6, 6
 * ```
 */
export function to_ltwh_tuple(bounding_box) {
  if (bounding_box instanceof LTWH) {
    let left = bounding_box.left;
    let top = bounding_box.top;
    let width = bounding_box.width;
    let height = bounding_box.height;
    return [left, top, width, height];
  } else if (bounding_box instanceof LTRB) {
    let left = bounding_box.left;
    let top = bounding_box.top;
    let right = bounding_box.right;
    let bottom = bounding_box.bottom;
    return [left, top, right - left, bottom - top];
  } else {
    let x1 = bounding_box.x1;
    let y1 = bounding_box.y1;
    let x2 = bounding_box.x2;
    let y2 = bounding_box.y2;
    return [x1, y1, x2 - x1, y2 - y1];
  }
}

/**
 * Converts a bounding box to a tuple with the coordinate values left, top, 
 * right, bottom. Useful for working with with custom bounding box operations.
 * 
 * ## Example
 * ```gleam
 * let assert Ok(box) = bounding_box.x1y1x2y2(x1: 2, y1: 2, x2: 6, y2: 6) 
 * bounding_box.to_ltrb_tuple(box)
 * // -> #(2, 2, 6, 6)
 * ```
 */
export function to_ltrb_tuple(bounding_box) {
  if (bounding_box instanceof LTWH) {
    let left = bounding_box.left;
    let top = bounding_box.top;
    let width = bounding_box.width;
    let height = bounding_box.height;
    return [left, top, left + width, top + height];
  } else if (bounding_box instanceof LTRB) {
    let left = bounding_box.left;
    let top = bounding_box.top;
    let right = bounding_box.right;
    let bottom = bounding_box.bottom;
    return [left, top, right, bottom];
  } else {
    let x1 = bounding_box.x1;
    let y1 = bounding_box.y1;
    let x2 = bounding_box.x2;
    let y2 = bounding_box.y2;
    return [x1, y1, x2, y2];
  }
}

/**
 * Converts a bounding box to a tuple with the coordinate values x1, y1, x2, 
 * y2. Useful for working with with custom bounding box operations.
 * 
 * ## Example
 * ```gleam
 * let assert Ok(box) = bounding_box.ltwh(2, 2, 4, 4) 
 * bounding_box.to_x1y1x2y2_tuple(box)
 * // -> #(2, 2, 6, 6)
 * ```
 */
export function to_x1y1x2y2_tuple(bounding_box) {
  if (bounding_box instanceof LTWH) {
    let left = bounding_box.left;
    let top = bounding_box.top;
    let width = bounding_box.width;
    let height = bounding_box.height;
    return [left, top, left + width, top + height];
  } else if (bounding_box instanceof LTRB) {
    let left = bounding_box.left;
    let top = bounding_box.top;
    let right = bounding_box.right;
    let bottom = bounding_box.bottom;
    return [left, top, right, bottom];
  } else {
    let x1 = bounding_box.x1;
    let y1 = bounding_box.y1;
    let x2 = bounding_box.x2;
    let y2 = bounding_box.y2;
    return [x1, y1, x2, y2];
  }
}

/**
 * Shrinks a bounding box by the given amount in all dimensions. If the amount 
 * is negative, the bounding box will not be modified. If the amount to shrink 
 * is greater than the size of the bounding box, an error will be returned.
 */
export function shrink(bounding_box, amount) {
  return $bool.guard(
    amount < 0,
    new Ok(bounding_box),
    () => {
      let $ = to_ltwh_tuple(bounding_box);
      let width;
      let height;
      width = $[2];
      height = $[3];
      return $bool.guard(
        (amount * 2 >= width) || (amount * 2 >= height),
        new Error(undefined),
        () => {
          let _block;
          if (bounding_box instanceof LTWH) {
            let left = bounding_box.left;
            let top = bounding_box.top;
            let width$1 = bounding_box.width;
            let height$1 = bounding_box.height;
            _block = new LTWH(
              left + amount,
              top + amount,
              $int.max(width$1 - amount * 2, 0),
              $int.max(height$1 - amount * 2, 0),
            );
          } else if (bounding_box instanceof LTRB) {
            let left = bounding_box.left;
            let top = bounding_box.top;
            let right = bounding_box.right;
            let bottom = bounding_box.bottom;
            _block = new LTRB(
              left + amount,
              top + amount,
              $int.max(right - amount, 0),
              $int.max(bottom - amount, 0),
            );
          } else {
            let x1 = bounding_box.x1;
            let y1 = bounding_box.y1;
            let x2 = bounding_box.x2;
            let y2 = bounding_box.y2;
            _block = new X1Y1X2Y2(
              x1 + amount,
              y1 + amount,
              $int.max(x2 - amount, 0),
              $int.max(y2 - amount, 0),
            );
          }
          let _pipe = _block;
          return new Ok(_pipe);
        },
      );
    },
  );
}

/**
 * Expands a bounding box by the given amount in all dimensions. If the amount 
 * is negative, the bounding box will not be modified.
 */
export function expand(bounding_box, amount) {
  return $bool.guard(
    amount < 0,
    bounding_box,
    () => {
      if (bounding_box instanceof LTWH) {
        let left = bounding_box.left;
        let top = bounding_box.top;
        let width = bounding_box.width;
        let height = bounding_box.height;
        return new LTWH(
          $int.max(left - amount, 0),
          $int.max(top - amount, 0),
          width + amount * 2,
          height + amount * 2,
        );
      } else if (bounding_box instanceof LTRB) {
        let left = bounding_box.left;
        let top = bounding_box.top;
        let right = bounding_box.right;
        let bottom = bounding_box.bottom;
        return new LTRB(
          $int.max(left - amount, 0),
          $int.max(top - amount, 0),
          right + amount,
          bottom + amount,
        );
      } else {
        let x1 = bounding_box.x1;
        let y1 = bounding_box.y1;
        let x2 = bounding_box.x2;
        let y2 = bounding_box.y2;
        return new X1Y1X2Y2(
          $int.max(x1 - amount, 0),
          $int.max(y1 - amount, 0),
          x2 + amount,
          y2 + amount,
        );
      }
    },
  );
}

/**
 * Resizes a bounding box by the given scale.
 */
export function scale(bounding_box, scale) {
  let $ = to_ltrb_tuple(bounding_box);
  let left;
  let top;
  let right;
  let bottom;
  left = $[0];
  top = $[1];
  right = $[2];
  bottom = $[3];
  return new LTRB(
    $float.round($int.to_float(left) * scale),
    $float.round($int.to_float(top) * scale),
    $float.round($int.to_float(right) * scale),
    $float.round($int.to_float(bottom) * scale),
  );
}

/**
 * Cuts a bounding box out of another bounding box, returning a list of 
 * bounding boxes that represent the area of the original that was not cut out.
 */
export function cut(to_cut, cutter) {
  let $ = to_ltwh_tuple(to_cut);
  let x1;
  let y1;
  let w1;
  let h1;
  x1 = $[0];
  y1 = $[1];
  w1 = $[2];
  h1 = $[3];
  let $1 = to_ltwh_tuple(cutter);
  let x2;
  let y2;
  let w2;
  let h2;
  x2 = $1[0];
  y2 = $1[1];
  w2 = $1[2];
  h2 = $1[3];
  let int_left = $int.max(x1, x2);
  let int_top = $int.max(y1, y2);
  let int_right = $int.min(x1 + w1, x2 + w2);
  let int_bottom = $int.min(y1 + h1, y2 + h2);
  return $bool.guard(
    (int_left >= int_right) || (int_top >= int_bottom),
    toList([to_cut]),
    () => {
      let cut_pieces = toList([
        (() => {
          let $2 = y1 < int_top;
          if ($2) {
            return new Some(new LTWH(x1, y1, w1, int_top - y1));
          } else {
            return new None();
          }
        })(),
        (() => {
          let $2 = x1 < int_left;
          if ($2) {
            return new Some(
              new LTWH(x1, int_top, int_left - x1, int_bottom - int_top),
            );
          } else {
            return new None();
          }
        })(),
        (() => {
          let $2 = int_right < (x1 + w1);
          if ($2) {
            return new Some(
              new LTWH(
                int_right,
                int_top,
                (x1 + w1) - int_right,
                int_bottom - int_top,
              ),
            );
          } else {
            return new None();
          }
        })(),
        (() => {
          let $2 = int_bottom < (y1 + h1);
          if ($2) {
            return new Some(
              new LTWH(x1, int_bottom, w1, (y1 + h1) - int_bottom),
            );
          } else {
            return new None();
          }
        })(),
      ]);
      return $option.values(cut_pieces);
    },
  );
}

/**
 * Returns the intersection of two bounding boxes. If they do not intersect,
 * `None` will be returned.
 */
export function intersection(box1, box2) {
  let $ = to_ltrb_tuple(box1);
  let l1;
  let t1;
  let r1;
  let b1;
  l1 = $[0];
  t1 = $[1];
  r1 = $[2];
  b1 = $[3];
  let $1 = to_ltrb_tuple(box2);
  let l2;
  let t2;
  let r2;
  let b2;
  l2 = $1[0];
  t2 = $1[1];
  r2 = $1[2];
  b2 = $1[3];
  return $bool.guard(
    (((l1 >= l2) && (t1 >= t2)) && (r1 <= r2)) && (b1 <= b2),
    new Some(box1),
    () => {
      return $bool.guard(
        (((l1 <= l2) && (t1 <= t2)) && (r1 >= r2)) && (b1 >= b2),
        new Some(box2),
        () => {
          let left = $int.max(l1, l2);
          let top = $int.max(t1, t2);
          let right = $int.min(r1, r2);
          let bottom = $int.min(b1, b2);
          return $bool.guard(
            (left >= right) || (top >= bottom),
            new None(),
            () => {
              let _pipe = new LTRB(left, top, right, bottom);
              return new Some(_pipe);
            },
          );
        },
      );
    },
  );
}

/**
 * Fits a bounding box into another bounding box, dropping any pixels outside 
 * the dimensions of the reference bounding box.
 */
export function fit(box, reference) {
  let $ = to_ltwh_tuple(reference);
  let width;
  let height;
  width = $[2];
  height = $[3];
  let $1 = to_ltrb_tuple(box);
  let left;
  let top;
  let right;
  let bottom;
  left = $1[0];
  top = $1[1];
  right = $1[2];
  bottom = $1[3];
  let $2 = left < width;
  let $3 = top < height;
  if ($2 && $3) {
    return new Some(
      new LTRB(left, top, $int.min(right, width), $int.min(bottom, height)),
    );
  } else {
    return new None();
  }
}

/**
 * Makes a bounding box relative to and fit inside another bounding box. 
 * Assuming both bounding boxes are on the same image, they are both relative
 * to 0,0 on that image. This adjusts the first bounding box so that the 
 * original coordinates are relative to the top left corner of the second 
 * bounding box instead, and then fits the adjusted bounding box into the 
 * reference bounding box.
 * 
 * This is useful when you have two bounding boxes on an image, where one
 * represents an extracted area of the original image and you want to do
 * an operation on that extracted area with the second bounding box, but the 
 * second bounding box was calculated with the coordinates of the original 
 * image.
 * 
 * ## Example
 * ```gleam
 * let assert Ok(box) = bounding_box.ltwh(left: 2, top: 2, width: 4, height: 4) 
 * let assert Ok(ref) = bounding_box.ltwh(left: 4, top: 4, width: 6, height: 6) 
 * bounding_box.make_relative(box, to: ref)
 * // -> Some(bounding_box.ltwh(left: 0, top: 0, width: 2, height: 2))
 * ```
 */
export function make_relative(box, reference) {
  let $ = to_ltrb_tuple(box);
  let left;
  let top;
  let right;
  let bottom;
  left = $[0];
  top = $[1];
  right = $[2];
  bottom = $[3];
  let $1 = to_ltwh_tuple(reference);
  let ref_left;
  let ref_top;
  ref_left = $1[0];
  ref_top = $1[1];
  let adj_box = new LTRB(
    $int.max(left - ref_left, 0),
    $int.max(top - ref_top, 0),
    $int.max(right - ref_left, 0),
    $int.max(bottom - ref_top, 0),
  );
  let $2 = adj_box.left;
  if ($2 === 0) {
    let $3 = adj_box.top;
    if ($3 === 0) {
      let $4 = adj_box.right;
      if ($4 === 0) {
        let $5 = adj_box.bottom;
        if ($5 === 0) {
          return new None();
        } else {
          return fit(adj_box, reference);
        }
      } else {
        return fit(adj_box, reference);
      }
    } else {
      return fit(adj_box, reference);
    }
  } else {
    return fit(adj_box, reference);
  }
}
