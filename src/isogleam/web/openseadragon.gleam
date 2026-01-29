/// OpenSeaDragon viewer bindings for Gleam
/// Pure Gleam interface to OpenSeaDragon tile viewer
import gleam/dynamic.{type Dynamic}
import gleam/json.{type DecodeError, type Json}

/// Opaque type representing an OpenSeaDragon viewer instance
pub opaque type Viewer {
  Viewer
}

/// Configuration for initializing the viewer
pub type ViewerConfig {
  ViewerConfig(
    prefix_url: String,
    show_navigation_control: Bool,
    show_zoom_control: Bool,
    show_home_control: Bool,
    show_full_page_control: Bool,
    show_navigator: Bool,
    navigator_position: NavigatorPosition,
    blend_time: Float,
    animation_time: Float,
    spring_stiffness: Float,
    min_zoom_image_ratio: Float,
    max_zoom_pixel_ratio: Float,
    visibility_ratio: Float,
    click_to_zoom: Bool,
    dbl_click_to_zoom: Bool,
  )
}

/// Navigator minimap position
pub type NavigatorPosition {
  TopLeft
  TopRight
  BottomLeft
  BottomRight
}

/// Tile source configuration for custom tile servers
pub type TileConfig {
  TileConfig(
    height: Int,
    width: Int,
    tile_size: Int,
    min_level: Int,
    max_level: Int,
    url_template: String,
  )
}

/// Viewport bounds
pub type Bounds {
  Bounds(x: Float, y: Float, width: Float, height: Float)
}

/// Create default viewer configuration
pub fn default_config() -> ViewerConfig {
  ViewerConfig(
    prefix_url: "//openseadragon.github.io/openseadragon/images/",
    show_navigation_control: True,
    show_zoom_control: True,
    show_home_control: True,
    show_full_page_control: True,
    show_navigator: True,
    navigator_position: TopRight,
    blend_time: 0.3,
    animation_time: 1.2,
    spring_stiffness: 10.0,
    min_zoom_image_ratio: 0.8,
    max_zoom_pixel_ratio: 2.0,
    visibility_ratio: 1.0,
    click_to_zoom: False,
    dbl_click_to_zoom: True,
  )
}

/// Initialize OpenSeaDragon viewer
@external(javascript, "../web/openseadragon_ffi.mjs", "initViewer")
pub fn init_viewer(element_id: String, config: ViewerConfig) -> Viewer {
  panic as "JavaScript only"
}

/// Initialize OpenSeaDragon viewer asynchronously (waits for DOM)
@external(javascript, "../web/openseadragon_ffi.mjs", "init")
pub fn init(
  element_id: String,
  config: ViewerConfig,
  callback: fn(Viewer) -> Nil,
) -> Nil {
  panic as "JavaScript only"
}

/// Load DZI (Deep Zoom Image) tile source
@external(javascript, "../web/openseadragon_ffi.mjs", "loadDziTileSource")
pub fn load_dzi(viewer: Viewer, dzi_url: String) -> Nil {
  panic as "JavaScript only"
}

/// Load custom tile source with template URL
@external(javascript, "../web/openseadragon_ffi.mjs", "loadCustomTileSource")
pub fn load_custom_tiles(viewer: Viewer, config: TileConfig) -> Nil {
  panic as "JavaScript only"
}

/// Add event listener to viewer
@external(javascript, "../web/openseadragon_ffi.mjs", "addEventListener")
pub fn add_event_listener(
  viewer: Viewer,
  event_name: String,
  callback: fn(Dynamic) -> Nil,
) -> Nil {
  panic as "JavaScript only"
}

/// Get current viewport bounds
@external(javascript, "../web/openseadragon_ffi.mjs", "getViewportBounds")
pub fn get_viewport_bounds(viewer: Viewer) -> Bounds {
  panic as "JavaScript only"
}

/// Get current zoom level
@external(javascript, "../web/openseadragon_ffi.mjs", "getZoom")
pub fn get_zoom(viewer: Viewer) -> Float {
  panic as "JavaScript only"
}

/// Set zoom level
@external(javascript, "../web/openseadragon_ffi.mjs", "setZoom")
pub fn set_zoom(viewer: Viewer, zoom_level: Float, immediately: Bool) -> Nil {
  panic as "JavaScript only"
}

/// Pan to position
@external(javascript, "../web/openseadragon_ffi.mjs", "panTo")
pub fn pan_to(viewer: Viewer, x: Float, y: Float, immediately: Bool) -> Nil {
  panic as "JavaScript only"
}

/// Go to home position (fit to viewport)
@external(javascript, "../web/openseadragon_ffi.mjs", "goHome")
pub fn go_home(viewer: Viewer) -> Nil {
  panic as "JavaScript only"
}

/// Destroy viewer instance (cleanup)
@external(javascript, "../web/openseadragon_ffi.mjs", "destroyViewer")
pub fn destroy(viewer: Viewer) -> Nil {
  panic as "JavaScript only"
}

// --- Helpers ---

/// Convert NavigatorPosition to string for JSON
fn navigator_position_to_string(pos: NavigatorPosition) -> String {
  case pos {
    TopLeft -> "TOP_LEFT"
    TopRight -> "TOP_RIGHT"
    BottomLeft -> "BOTTOM_LEFT"
    BottomRight -> "BOTTOM_RIGHT"
  }
}
