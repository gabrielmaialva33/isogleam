import * as $tensor from "./viva_tensor/tensor.mjs";

/**
 * Create tensor of zeros
 */
export function zeros(shape) {
  return $tensor.zeros(shape);
}

/**
 * Create tensor of ones
 */
export function ones(shape) {
  return $tensor.ones(shape);
}

/**
 * Create tensor filled with value
 */
export function fill(shape, value) {
  return $tensor.fill(shape, value);
}

/**
 * Create tensor from list (1D)
 */
export function from_list(data) {
  return $tensor.from_list(data);
}

/**
 * Create 2D tensor from list of lists
 */
export function from_list2d(rows) {
  return $tensor.from_list2d(rows);
}

/**
 * Create vector (1D tensor)
 */
export function vector(data) {
  return $tensor.vector(data);
}

/**
 * Create matrix (2D tensor)
 */
export function matrix(rows, cols, data) {
  return $tensor.matrix(rows, cols, data);
}

/**
 * Get shape
 */
export function shape(t) {
  return t.shape;
}

/**
 * Get total size
 */
export function size(t) {
  return $tensor.size(t);
}

/**
 * Get rank (number of dimensions)
 */
export function rank(t) {
  return $tensor.rank(t);
}

/**
 * Check if shapes can broadcast
 */
export function can_broadcast(a, b) {
  return $tensor.can_broadcast(a, b);
}

/**
 * Check if contiguous
 */
export function is_contiguous(t) {
  return $tensor.is_contiguous(t);
}

/**
 * Default conv2d config (3x3 kernel, stride 1, no padding)
 */
export function conv2d_config() {
  return $tensor.conv2d_config();
}

/**
 * Conv2d config with "same" padding (output same size as input)
 */
export function conv2d_same(kernel_h, kernel_w) {
  return $tensor.conv2d_same(kernel_h, kernel_w);
}
