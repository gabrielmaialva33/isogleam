import * as $json from "../../../gleam_json/gleam/json.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import { CustomType as $CustomType } from "../../gleam.mjs";
import {
  initViewer as init_viewer,
  init,
  loadDziTileSource as load_dzi,
  loadCustomTileSource as load_custom_tiles,
  addEventListener as add_event_listener,
  getViewportBounds as get_viewport_bounds,
  getZoom as get_zoom,
  setZoom as set_zoom,
  panTo as pan_to,
  goHome as go_home,
  destroyViewer as destroy,
} from "../web/openseadragon_ffi.mjs";

export {
  add_event_listener,
  destroy,
  get_viewport_bounds,
  get_zoom,
  go_home,
  init,
  init_viewer,
  load_custom_tiles,
  load_dzi,
  pan_to,
  set_zoom,
};

class Viewer extends $CustomType {}

export class ViewerConfig extends $CustomType {
  constructor(prefix_url, show_navigation_control, show_zoom_control, show_home_control, show_full_page_control, show_navigator, navigator_position, blend_time, animation_time, spring_stiffness, min_zoom_image_ratio, max_zoom_pixel_ratio, visibility_ratio, click_to_zoom, dbl_click_to_zoom) {
    super();
    this.prefix_url = prefix_url;
    this.show_navigation_control = show_navigation_control;
    this.show_zoom_control = show_zoom_control;
    this.show_home_control = show_home_control;
    this.show_full_page_control = show_full_page_control;
    this.show_navigator = show_navigator;
    this.navigator_position = navigator_position;
    this.blend_time = blend_time;
    this.animation_time = animation_time;
    this.spring_stiffness = spring_stiffness;
    this.min_zoom_image_ratio = min_zoom_image_ratio;
    this.max_zoom_pixel_ratio = max_zoom_pixel_ratio;
    this.visibility_ratio = visibility_ratio;
    this.click_to_zoom = click_to_zoom;
    this.dbl_click_to_zoom = dbl_click_to_zoom;
  }
}
export const ViewerConfig$ViewerConfig = (prefix_url, show_navigation_control, show_zoom_control, show_home_control, show_full_page_control, show_navigator, navigator_position, blend_time, animation_time, spring_stiffness, min_zoom_image_ratio, max_zoom_pixel_ratio, visibility_ratio, click_to_zoom, dbl_click_to_zoom) =>
  new ViewerConfig(prefix_url,
  show_navigation_control,
  show_zoom_control,
  show_home_control,
  show_full_page_control,
  show_navigator,
  navigator_position,
  blend_time,
  animation_time,
  spring_stiffness,
  min_zoom_image_ratio,
  max_zoom_pixel_ratio,
  visibility_ratio,
  click_to_zoom,
  dbl_click_to_zoom);
export const ViewerConfig$isViewerConfig = (value) =>
  value instanceof ViewerConfig;
export const ViewerConfig$ViewerConfig$prefix_url = (value) => value.prefix_url;
export const ViewerConfig$ViewerConfig$0 = (value) => value.prefix_url;
export const ViewerConfig$ViewerConfig$show_navigation_control = (value) =>
  value.show_navigation_control;
export const ViewerConfig$ViewerConfig$1 = (value) =>
  value.show_navigation_control;
export const ViewerConfig$ViewerConfig$show_zoom_control = (value) =>
  value.show_zoom_control;
export const ViewerConfig$ViewerConfig$2 = (value) => value.show_zoom_control;
export const ViewerConfig$ViewerConfig$show_home_control = (value) =>
  value.show_home_control;
export const ViewerConfig$ViewerConfig$3 = (value) => value.show_home_control;
export const ViewerConfig$ViewerConfig$show_full_page_control = (value) =>
  value.show_full_page_control;
export const ViewerConfig$ViewerConfig$4 = (value) =>
  value.show_full_page_control;
export const ViewerConfig$ViewerConfig$show_navigator = (value) =>
  value.show_navigator;
export const ViewerConfig$ViewerConfig$5 = (value) => value.show_navigator;
export const ViewerConfig$ViewerConfig$navigator_position = (value) =>
  value.navigator_position;
export const ViewerConfig$ViewerConfig$6 = (value) => value.navigator_position;
export const ViewerConfig$ViewerConfig$blend_time = (value) => value.blend_time;
export const ViewerConfig$ViewerConfig$7 = (value) => value.blend_time;
export const ViewerConfig$ViewerConfig$animation_time = (value) =>
  value.animation_time;
export const ViewerConfig$ViewerConfig$8 = (value) => value.animation_time;
export const ViewerConfig$ViewerConfig$spring_stiffness = (value) =>
  value.spring_stiffness;
export const ViewerConfig$ViewerConfig$9 = (value) => value.spring_stiffness;
export const ViewerConfig$ViewerConfig$min_zoom_image_ratio = (value) =>
  value.min_zoom_image_ratio;
export const ViewerConfig$ViewerConfig$10 = (value) =>
  value.min_zoom_image_ratio;
export const ViewerConfig$ViewerConfig$max_zoom_pixel_ratio = (value) =>
  value.max_zoom_pixel_ratio;
export const ViewerConfig$ViewerConfig$11 = (value) =>
  value.max_zoom_pixel_ratio;
export const ViewerConfig$ViewerConfig$visibility_ratio = (value) =>
  value.visibility_ratio;
export const ViewerConfig$ViewerConfig$12 = (value) => value.visibility_ratio;
export const ViewerConfig$ViewerConfig$click_to_zoom = (value) =>
  value.click_to_zoom;
export const ViewerConfig$ViewerConfig$13 = (value) => value.click_to_zoom;
export const ViewerConfig$ViewerConfig$dbl_click_to_zoom = (value) =>
  value.dbl_click_to_zoom;
export const ViewerConfig$ViewerConfig$14 = (value) => value.dbl_click_to_zoom;

export class TopLeft extends $CustomType {}
export const NavigatorPosition$TopLeft = () => new TopLeft();
export const NavigatorPosition$isTopLeft = (value) => value instanceof TopLeft;

export class TopRight extends $CustomType {}
export const NavigatorPosition$TopRight = () => new TopRight();
export const NavigatorPosition$isTopRight = (value) =>
  value instanceof TopRight;

export class BottomLeft extends $CustomType {}
export const NavigatorPosition$BottomLeft = () => new BottomLeft();
export const NavigatorPosition$isBottomLeft = (value) =>
  value instanceof BottomLeft;

export class BottomRight extends $CustomType {}
export const NavigatorPosition$BottomRight = () => new BottomRight();
export const NavigatorPosition$isBottomRight = (value) =>
  value instanceof BottomRight;

export class TileConfig extends $CustomType {
  constructor(height, width, tile_size, min_level, max_level, url_template) {
    super();
    this.height = height;
    this.width = width;
    this.tile_size = tile_size;
    this.min_level = min_level;
    this.max_level = max_level;
    this.url_template = url_template;
  }
}
export const TileConfig$TileConfig = (height, width, tile_size, min_level, max_level, url_template) =>
  new TileConfig(height, width, tile_size, min_level, max_level, url_template);
export const TileConfig$isTileConfig = (value) => value instanceof TileConfig;
export const TileConfig$TileConfig$height = (value) => value.height;
export const TileConfig$TileConfig$0 = (value) => value.height;
export const TileConfig$TileConfig$width = (value) => value.width;
export const TileConfig$TileConfig$1 = (value) => value.width;
export const TileConfig$TileConfig$tile_size = (value) => value.tile_size;
export const TileConfig$TileConfig$2 = (value) => value.tile_size;
export const TileConfig$TileConfig$min_level = (value) => value.min_level;
export const TileConfig$TileConfig$3 = (value) => value.min_level;
export const TileConfig$TileConfig$max_level = (value) => value.max_level;
export const TileConfig$TileConfig$4 = (value) => value.max_level;
export const TileConfig$TileConfig$url_template = (value) => value.url_template;
export const TileConfig$TileConfig$5 = (value) => value.url_template;

export class Bounds extends $CustomType {
  constructor(x, y, width, height) {
    super();
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }
}
export const Bounds$Bounds = (x, y, width, height) =>
  new Bounds(x, y, width, height);
export const Bounds$isBounds = (value) => value instanceof Bounds;
export const Bounds$Bounds$x = (value) => value.x;
export const Bounds$Bounds$0 = (value) => value.x;
export const Bounds$Bounds$y = (value) => value.y;
export const Bounds$Bounds$1 = (value) => value.y;
export const Bounds$Bounds$width = (value) => value.width;
export const Bounds$Bounds$2 = (value) => value.width;
export const Bounds$Bounds$height = (value) => value.height;
export const Bounds$Bounds$3 = (value) => value.height;

/**
 * Create default viewer configuration
 */
export function default_config() {
  return new ViewerConfig(
    "//openseadragon.github.io/openseadragon/images/",
    true,
    true,
    true,
    true,
    true,
    new TopRight(),
    0.3,
    1.2,
    10.0,
    0.8,
    2.0,
    1.0,
    false,
    true,
  );
}
