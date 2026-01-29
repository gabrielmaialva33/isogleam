#!/usr/bin/env bash
# Build script for IsoGleam web viewer

set -euo pipefail

echo "🔨 Building IsoGleam Viewer (JavaScript target)..."

# Build Gleam project for JavaScript
echo "📦 Compiling Gleam to JavaScript..."
gleam build --target javascript

# Output directory
BUILD_DIR="viewer"
GLEAM_BUILD="build/dev/javascript/isogleam"

# Create viewer directory if it doesn't exist
mkdir -p "$BUILD_DIR"

# Clean old artifacts
rm -rf "$BUILD_DIR/isogleam" "$BUILD_DIR/lustre" "$BUILD_DIR/gleam_*" "$BUILD_DIR/*.mjs"

# Copy all build artifacts to preserve structure
cp -r build/dev/javascript/* "$BUILD_DIR/" 2>/dev/null || true

# Explicitly ensure prelude fallback exists if needed (e.g. for legacy deps)
cp "$BUILD_DIR/gleam.mjs" "$BUILD_DIR/prelude.mjs" 2>/dev/null || true

echo "✅ Build complete!"
echo ""
echo "📂 Files in $BUILD_DIR:"
ls -lh "$BUILD_DIR"
echo ""
echo "🚀 To run the viewer:"
echo "   cd viewer && python3 -m http.server 8080"
echo "   Then open http://localhost:8080"
