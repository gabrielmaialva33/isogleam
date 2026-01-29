/// Isometric viewer web app - Lustre + OpenSeaDragon
/// Pure Gleam frontend for viewing generated isometric tiles
import gleam/dynamic.{type Dynamic}
import gleam/io
import gleam/option.{type Option, None, Some}
import isogleam/web/openseadragon as osd
import lustre
import lustre/attribute.{class, id}
import lustre/effect.{type Effect}
import lustre/element
import lustre/element/html

// --- MODEL ---

pub type Model {
  Model(viewer: Option(osd.Viewer), tile_source: TileSource, status: Status)
}

pub type TileSource {
  DziSource(url: String)
  CustomSource(config: osd.TileConfig)
}

pub type Status {
  Loading
  Ready
  Error(String)
}

/// Initialize the model
pub fn init(_flags) -> #(Model, Effect(Msg)) {
  let model =
    Model(
      viewer: None,
      // Use CustomSource with scanned grid dimensions
      tile_source: CustomSource(osd.TileConfig(
        url_template: "tiles/tile_{x}_{y}.png",
        // Total Width: 15872, Total Height: 17920
        width: 15_872,
        height: 17_920,
        tile_size: 512,
        min_level: Some(0),
        max_level: Some(0),
        x_offset: 15,
        y_offset: 8,
      )),
      status: Loading,
    )

  #(model, init_viewer_effect())
}

// --- UPDATE ---

pub type Msg {
  ViewerInitialized(osd.Viewer)
  TilesLoaded
  ViewerError(String)
  ZoomIn
  ZoomOut
  GoHome
  NoOp
}

pub fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    ViewerInitialized(viewer) -> {
      io.println("✅ Viewer initialized!")
      #(
        Model(..model, viewer: Some(viewer), status: Ready),
        load_tiles_effect(viewer, model.tile_source),
      )
    }

    TilesLoaded -> {
      io.println("✅ Tiles loaded!")
      #(model, effect.none())
    }

    ViewerError(error) -> {
      io.println("❌ Viewer error: " <> error)
      #(Model(..model, status: Error(error)), effect.none())
    }

    ZoomIn -> {
      case model.viewer {
        Some(viewer) -> {
          let current_zoom = osd.get_zoom(viewer)
          osd.set_zoom(viewer, current_zoom *. 1.5, False)
          #(model, effect.none())
        }
        None -> #(model, effect.none())
      }
    }

    ZoomOut -> {
      case model.viewer {
        Some(viewer) -> {
          let current_zoom = osd.get_zoom(viewer)
          osd.set_zoom(viewer, current_zoom /. 1.5, False)
          #(model, effect.none())
        }
        None -> #(model, effect.none())
      }
    }

    GoHome -> {
      case model.viewer {
        Some(viewer) -> {
          osd.go_home(viewer)
          #(model, effect.none())
        }
        None -> #(model, effect.none())
      }
    }

    NoOp -> #(model, effect.none())
  }
}

// --- VIEW ---

pub fn view(model: Model) -> element.Element(Msg) {
  html.div([class("app-container")], [
    // Header
    html.header([class("app-header")], [
      html.h1([], [html.text("IsoGleam Viewer")]),
      html.p([class("subtitle")], [
        html.text("Isometric NYC-style viewer • Pure Gleam + OpenSeaDragon"),
      ]),
    ]),
    // Viewer container
    html.div([id("openseadragon-viewer"), class("viewer-container")], []),
    // Status bar
    status_view(model.status),
    // Styles
    styles(),
  ])
}

fn status_view(status: Status) -> element.Element(Msg) {
  case status {
    Loading ->
      html.div([class("status loading")], [html.text("⏳ Loading viewer...")])
    Ready ->
      html.div([class("status ready")], [
        html.text("✅ Viewer ready • Drag to pan, scroll to zoom"),
      ])
    Error(msg) ->
      html.div([class("status error")], [
        html.text("❌ Error: " <> msg),
      ])
  }
}

fn styles() -> element.Element(Msg) {
  element.element("style", [], [
    html.text(
      "
    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }

    body {
      font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      background: linear-gradient(135deg, #1e1e2e 0%, #2d2d44 100%);
      color: #e0e0e0;
      min-height: 100vh;
    }

    .app-container {
      display: flex;
      flex-direction: column;
      height: 100vh;
    }

    .app-header {
      background: rgba(30, 30, 46, 0.95);
      backdrop-filter: blur(10px);
      padding: 1.5rem 2rem;
      border-bottom: 2px solid rgba(139, 92, 246, 0.3);
      box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
    }

    .app-header h1 {
      font-size: 2rem;
      font-weight: 700;
      background: linear-gradient(135deg, #8b5cf6 0%, #ec4899 100%);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
      margin-bottom: 0.5rem;
    }

    .subtitle {
      font-size: 0.95rem;
      color: #a0a0b0;
      font-weight: 400;
    }

    .viewer-container {
      flex: 1;
      position: relative;
      background: #1a1a2e;
      border: 1px solid rgba(139, 92, 246, 0.2);
      box-shadow: inset 0 2px 10px rgba(0, 0, 0, 0.5);
    }

    .status {
      padding: 1rem 2rem;
      text-align: center;
      font-size: 0.9rem;
      font-weight: 500;
      backdrop-filter: blur(10px);
    }

    .status.loading {
      background: rgba(59, 130, 246, 0.15);
      color: #60a5fa;
      border-top: 2px solid rgba(59, 130, 246, 0.3);
    }

    .status.ready {
      background: rgba(16, 185, 129, 0.15);
      color: #34d399;
      border-top: 2px solid rgba(16, 185, 129, 0.3);
    }

    .status.error {
      background: rgba(239, 68, 68, 0.15);
      color: #f87171;
      border-top: 2px solid rgba(239, 68, 68, 0.3);
    }

    /* OpenSeaDragon overrides */
    #openseadragon-viewer canvas {
      background: #0f0f1e !important;
    }
  ",
    ),
  ])
}

// --- EFFECTS ---

/// Effect to initialize the OpenSeaDragon viewer
fn init_viewer_effect() -> Effect(Msg) {
  effect.from(fn(dispatch) {
    let config = osd.default_config()

    // Initialize asynchronously to wait for DOM element
    osd.init("openseadragon-viewer", config, fn(viewer) {
      // Add event listeners
      osd.add_event_listener(viewer, "open", fn(_event) {
        dispatch(TilesLoaded)
      })

      osd.add_event_listener(viewer, "open-failed", fn(_event) {
        dispatch(ViewerError("Failed to load tiles"))
      })

      dispatch(ViewerInitialized(viewer))
    })
  })
}

/// Effect to load tiles into the viewer
fn load_tiles_effect(viewer: osd.Viewer, source: TileSource) -> Effect(Msg) {
  effect.from(fn(_dispatch) {
    case source {
      DziSource(url) -> osd.load_dzi(viewer, url)
      CustomSource(config) -> osd.load_custom_tiles(viewer, config)
    }
  })
}

// --- MAIN ---

pub fn main() {
  let app = lustre.application(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  io.println("🚀 IsoGleam Viewer started!")
  Nil
}
