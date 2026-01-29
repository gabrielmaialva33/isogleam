import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $io from "../../../gleam_stdlib/gleam/io.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import * as $lustre from "../../../lustre/lustre.mjs";
import * as $attribute from "../../../lustre/lustre/attribute.mjs";
import { class$, id } from "../../../lustre/lustre/attribute.mjs";
import * as $effect from "../../../lustre/lustre/effect.mjs";
import * as $element from "../../../lustre/lustre/element.mjs";
import * as $html from "../../../lustre/lustre/element/html.mjs";
import { Ok, toList, CustomType as $CustomType, makeError } from "../../gleam.mjs";
import * as $osd from "../../isogleam/web/openseadragon.mjs";

const FILEPATH = "src/isogleam/web/viewer_app.gleam";

export class Model extends $CustomType {
  constructor(viewer, tile_source, status) {
    super();
    this.viewer = viewer;
    this.tile_source = tile_source;
    this.status = status;
  }
}
export const Model$Model = (viewer, tile_source, status) =>
  new Model(viewer, tile_source, status);
export const Model$isModel = (value) => value instanceof Model;
export const Model$Model$viewer = (value) => value.viewer;
export const Model$Model$0 = (value) => value.viewer;
export const Model$Model$tile_source = (value) => value.tile_source;
export const Model$Model$1 = (value) => value.tile_source;
export const Model$Model$status = (value) => value.status;
export const Model$Model$2 = (value) => value.status;

export class DziSource extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}
export const TileSource$DziSource = (url) => new DziSource(url);
export const TileSource$isDziSource = (value) => value instanceof DziSource;
export const TileSource$DziSource$url = (value) => value.url;
export const TileSource$DziSource$0 = (value) => value.url;

export class CustomSource extends $CustomType {
  constructor(config) {
    super();
    this.config = config;
  }
}
export const TileSource$CustomSource = (config) => new CustomSource(config);
export const TileSource$isCustomSource = (value) =>
  value instanceof CustomSource;
export const TileSource$CustomSource$config = (value) => value.config;
export const TileSource$CustomSource$0 = (value) => value.config;

export class Loading extends $CustomType {}
export const Status$Loading = () => new Loading();
export const Status$isLoading = (value) => value instanceof Loading;

export class Ready extends $CustomType {}
export const Status$Ready = () => new Ready();
export const Status$isReady = (value) => value instanceof Ready;

export class Error extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Status$Error = ($0) => new Error($0);
export const Status$isError = (value) => value instanceof Error;
export const Status$Error$0 = (value) => value[0];

export class ViewerInitialized extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Msg$ViewerInitialized = ($0) => new ViewerInitialized($0);
export const Msg$isViewerInitialized = (value) =>
  value instanceof ViewerInitialized;
export const Msg$ViewerInitialized$0 = (value) => value[0];

export class TilesLoaded extends $CustomType {}
export const Msg$TilesLoaded = () => new TilesLoaded();
export const Msg$isTilesLoaded = (value) => value instanceof TilesLoaded;

export class ViewerError extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Msg$ViewerError = ($0) => new ViewerError($0);
export const Msg$isViewerError = (value) => value instanceof ViewerError;
export const Msg$ViewerError$0 = (value) => value[0];

export class ZoomIn extends $CustomType {}
export const Msg$ZoomIn = () => new ZoomIn();
export const Msg$isZoomIn = (value) => value instanceof ZoomIn;

export class ZoomOut extends $CustomType {}
export const Msg$ZoomOut = () => new ZoomOut();
export const Msg$isZoomOut = (value) => value instanceof ZoomOut;

export class GoHome extends $CustomType {}
export const Msg$GoHome = () => new GoHome();
export const Msg$isGoHome = (value) => value instanceof GoHome;

export class NoOp extends $CustomType {}
export const Msg$NoOp = () => new NoOp();
export const Msg$isNoOp = (value) => value instanceof NoOp;

function status_view(status) {
  if (status instanceof Loading) {
    return $html.div(
      toList([class$("status loading")]),
      toList([$html.text("⏳ Loading viewer...")]),
    );
  } else if (status instanceof Ready) {
    return $html.div(
      toList([class$("status ready")]),
      toList([$html.text("✅ Viewer ready • Drag to pan, scroll to zoom")]),
    );
  } else {
    let msg = status[0];
    return $html.div(
      toList([class$("status error")]),
      toList([$html.text("❌ Error: " + msg)]),
    );
  }
}

function styles() {
  return $element.element(
    "style",
    toList([]),
    toList([
      $html.text(
        "\n    * {\n      margin: 0;\n      padding: 0;\n      box-sizing: border-box;\n    }\n\n    body {\n      font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;\n      background: linear-gradient(135deg, #1e1e2e 0%, #2d2d44 100%);\n      color: #e0e0e0;\n      min-height: 100vh;\n    }\n\n    .app-container {\n      display: flex;\n      flex-direction: column;\n      height: 100vh;\n    }\n\n    .app-header {\n      background: rgba(30, 30, 46, 0.95);\n      backdrop-filter: blur(10px);\n      padding: 1.5rem 2rem;\n      border-bottom: 2px solid rgba(139, 92, 246, 0.3);\n      box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);\n    }\n\n    .app-header h1 {\n      font-size: 2rem;\n      font-weight: 700;\n      background: linear-gradient(135deg, #8b5cf6 0%, #ec4899 100%);\n      -webkit-background-clip: text;\n      -webkit-text-fill-color: transparent;\n      background-clip: text;\n      margin-bottom: 0.5rem;\n    }\n\n    .subtitle {\n      font-size: 0.95rem;\n      color: #a0a0b0;\n      font-weight: 400;\n    }\n\n    .viewer-container {\n      flex: 1;\n      position: relative;\n      background: #1a1a2e;\n      border: 1px solid rgba(139, 92, 246, 0.2);\n      box-shadow: inset 0 2px 10px rgba(0, 0, 0, 0.5);\n    }\n\n    .status {\n      padding: 1rem 2rem;\n      text-align: center;\n      font-size: 0.9rem;\n      font-weight: 500;\n      backdrop-filter: blur(10px);\n    }\n\n    .status.loading {\n      background: rgba(59, 130, 246, 0.15);\n      color: #60a5fa;\n      border-top: 2px solid rgba(59, 130, 246, 0.3);\n    }\n\n    .status.ready {\n      background: rgba(16, 185, 129, 0.15);\n      color: #34d399;\n      border-top: 2px solid rgba(16, 185, 129, 0.3);\n    }\n\n    .status.error {\n      background: rgba(239, 68, 68, 0.15);\n      color: #f87171;\n      border-top: 2px solid rgba(239, 68, 68, 0.3);\n    }\n\n    /* OpenSeaDragon overrides */\n    #openseadragon-viewer canvas {\n      background: #0f0f1e !important;\n    }\n  ",
      ),
    ]),
  );
}

export function view(model) {
  return $html.div(
    toList([class$("app-container")]),
    toList([
      $html.header(
        toList([class$("app-header")]),
        toList([
          $html.h1(toList([]), toList([$html.text("IsoGleam Viewer")])),
          $html.p(
            toList([class$("subtitle")]),
            toList([
              $html.text(
                "Isometric NYC-style viewer • Pure Gleam + OpenSeaDragon",
              ),
            ]),
          ),
        ]),
      ),
      $html.div(
        toList([id("openseadragon-viewer"), class$("viewer-container")]),
        toList([]),
      ),
      status_view(model.status),
      styles(),
    ]),
  );
}

/**
 * Effect to initialize the OpenSeaDragon viewer
 * 
 * @ignore
 */
function init_viewer_effect() {
  return $effect.from(
    (dispatch) => {
      let config = $osd.default_config();
      return $osd.init(
        "openseadragon-viewer",
        config,
        (viewer) => {
          $osd.add_event_listener(
            viewer,
            "open",
            (_) => { return dispatch(new TilesLoaded()); },
          );
          $osd.add_event_listener(
            viewer,
            "open-failed",
            (_) => { return dispatch(new ViewerError("Failed to load tiles")); },
          );
          return dispatch(new ViewerInitialized(viewer));
        },
      );
    },
  );
}

/**
 * Initialize the model
 */
export function init(_) {
  let model = new Model(new None(), new DziSource("/tiles.dzi"), new Loading());
  return [model, init_viewer_effect()];
}

/**
 * Effect to load tiles into the viewer
 * 
 * @ignore
 */
function load_tiles_effect(viewer, source) {
  return $effect.from(
    (_) => {
      if (source instanceof DziSource) {
        let url = source.url;
        return $osd.load_dzi(viewer, url);
      } else {
        let config = source.config;
        return $osd.load_custom_tiles(viewer, config);
      }
    },
  );
}

export function update(model, msg) {
  if (msg instanceof ViewerInitialized) {
    let viewer = msg[0];
    $io.println("✅ Viewer initialized!");
    return [
      new Model(new Some(viewer), model.tile_source, new Ready()),
      load_tiles_effect(viewer, model.tile_source),
    ];
  } else if (msg instanceof TilesLoaded) {
    $io.println("✅ Tiles loaded!");
    return [model, $effect.none()];
  } else if (msg instanceof ViewerError) {
    let error = msg[0];
    $io.println("❌ Viewer error: " + error);
    return [
      new Model(model.viewer, model.tile_source, new Error(error)),
      $effect.none(),
    ];
  } else if (msg instanceof ZoomIn) {
    let $ = model.viewer;
    if ($ instanceof Some) {
      let viewer = $[0];
      let current_zoom = $osd.get_zoom(viewer);
      $osd.set_zoom(viewer, current_zoom * 1.5, false);
      return [model, $effect.none()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof ZoomOut) {
    let $ = model.viewer;
    if ($ instanceof Some) {
      let viewer = $[0];
      let current_zoom = $osd.get_zoom(viewer);
      $osd.set_zoom(viewer, current_zoom / 1.5, false);
      return [model, $effect.none()];
    } else {
      return [model, $effect.none()];
    }
  } else if (msg instanceof GoHome) {
    let $ = model.viewer;
    if ($ instanceof Some) {
      let viewer = $[0];
      $osd.go_home(viewer);
      return [model, $effect.none()];
    } else {
      return [model, $effect.none()];
    }
  } else {
    return [model, $effect.none()];
  }
}

export function main() {
  let app = $lustre.application(init, update, view);
  let $ = $lustre.start(app, "#app", undefined);
  if (!($ instanceof Ok)) {
    throw makeError(
      "let_assert",
      FILEPATH,
      "isogleam/web/viewer_app",
      273,
      "main",
      "Pattern match failed, no pattern matched the value.",
      {
        value: $,
        start: 6371,
        end: 6420,
        pattern_start: 6382,
        pattern_end: 6387
      }
    )
  }
  $io.println("🚀 IsoGleam Viewer started!");
  return undefined;
}
