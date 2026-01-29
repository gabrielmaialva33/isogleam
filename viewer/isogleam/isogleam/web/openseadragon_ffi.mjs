// OpenSeaDragon FFI for Gleam
// Provides native JavaScript bindings to OpenSeaDragon viewer

import OpenSeadragon from "https://esm.sh/openseadragon@5.0.0";

/**
 * Initialize OpenSeaDragon viewer
 * @param {string} elementId - DOM element ID to attach viewer
 * @param {object} config - Viewer configuration
 * @returns {object} - OpenSeaDragon viewer instance
 */
export function initViewer(elementId, config) {
  return OpenSeadragon({
    id: elementId,
    prefixUrl: config.prefixUrl || "//openseadragon.github.io/openseadragon/images/",

    // Navigation controls
    showNavigationControl: config.showNavigationControl ?? true,
    showZoomControl: config.showZoomControl ?? true,
    showHomeControl: config.showHomeControl ?? true,
    showFullPageControl: config.showFullPageControl ?? true,

    // Navigator (minimap)
    showNavigator: config.showNavigator ?? true,
    navigatorPosition: config.navigatorPosition || "TOP_RIGHT",

    // Performance
    blendTime: config.blendTime ?? 0.3,
    animationTime: config.animationTime ?? 1.2,
    springStiffness: config.springStiffness ?? 10.0,

    // Visual settings
    minZoomImageRatio: config.minZoomImageRatio ?? 0.8,
    maxZoomPixelRatio: config.maxZoomPixelRatio ?? 2.0,
    visibilityRatio: config.visibilityRatio ?? 1.0,

    // Gesture settings
    gestureSettingsMouse: {
      clickToZoom: config.clickToZoom ?? false,
      dblClickToZoom: config.dblClickToZoom ?? true,
    },

    // Tile source (will be set separately)
    tileSources: null,
  });
}

/**
 * Initialize OpenSeaDragon viewer asynchronously (waits for DOM element)
 * @param {string} elementId - DOM element ID
 * @param {object} config - Viewer configuration
 * @param {function} callback - Callback with viewer instance
 */
export function init(elementId, config, callback) {
  function check() {
    if (document.getElementById(elementId)) {
      callback(initViewer(elementId, config));
    } else {
      requestAnimationFrame(check);
    }
  }
  check();
}

/**
 * Load DZI tile source
 * @param {object} viewer - OpenSeaDragon viewer instance
 * @param {string} dziUrl - URL to DZI descriptor (.dzi file)
 */
export function loadDziTileSource(viewer, dziUrl) {
  viewer.open(dziUrl);
}

/**
 * Load custom tile source with getTileUrl function
 * @param {object} viewer - OpenSeaDragon viewer instance
 * @param {object} tileConfig - Tile configuration
 */
export function loadCustomTileSource(viewer, tileConfig) {
  viewer.open({
    height: tileConfig.height,
    width: tileConfig.width,
    tileSize: tileConfig.tileSize,
    minLevel: tileConfig.minLevel || 0,
    maxLevel: tileConfig.maxLevel,
    getTileUrl: function (level, x, y) {
      // Template: {z}/{x}/{y}.webp
      return tileConfig.urlTemplate
        .replace("{z}", level)
        .replace("{x}", x)
        .replace("{y}", y);
    }
  });
}

/**
 * Add event listener to viewer
 * @param {object} viewer - OpenSeaDragon viewer instance
 * @param {string} eventName - Event name (e.g., "open", "zoom", "pan")
 * @param {function} callback - Callback function from Gleam
 */
export function addEventListener(viewer, eventName, callback) {
  viewer.addHandler(eventName, function (event) {
    callback(event);
  });
}

/**
 * Get current viewport bounds
 * @param {object} viewer - OpenSeaDragon viewer instance
 * @returns {object} - Bounds { x, y, width, height }
 */
export function getViewportBounds(viewer) {
  const bounds = viewer.viewport.getBounds();
  return {
    x: bounds.x,
    y: bounds.y,
    width: bounds.width,
    height: bounds.height,
  };
}

/**
 * Get current zoom level
 * @param {object} viewer - OpenSeaDragon viewer instance
 * @returns {number} - Current zoom level
 */
export function getZoom(viewer) {
  return viewer.viewport.getZoom();
}

/**
 * Set zoom level
 * @param {object} viewer - OpenSeaDragon viewer instance
 * @param {number} zoomLevel - Target zoom level
 * @param {boolean} immediately - Whether to skip animation
 */
export function setZoom(viewer, zoomLevel, immediately) {
  viewer.viewport.zoomTo(zoomLevel, null, immediately);
}

/**
 * Pan to position
 * @param {object} viewer - OpenSeaDragon viewer instance
 * @param {number} x - Target x coordinate
 * @param {number} y - Target y coordinate
 * @param {boolean} immediately - Whether to skip animation
 */
export function panTo(viewer, x, y, immediately) {
  viewer.viewport.panTo(new OpenSeadragon.Point(x, y), immediately);
}

/**
 * Go home (fit to viewport)
 * @param {object} viewer - OpenSeaDragon viewer instance
 */
export function goHome(viewer) {
  viewer.viewport.goHome();
}

/**
 * Destroy viewer instance
 * @param {object} viewer - OpenSeaDragon viewer instance
 */
export function destroyViewer(viewer) {
  if (viewer) {
    viewer.destroy();
  }
}
