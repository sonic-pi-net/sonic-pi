#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Sam Aaron
#
# Builds "Sonic Pi.vst3": the plugin, the web GUI it shows, and Sonic Pi's
# synthdefs, samples and random tables, which each instance loads its own.
#
#   scripts/build.sh [--test] [--install]
#
# --test     cargo's tests (offline renders), then test/host.mm against the
#            bundle: off-screen windows, rendered audio kept and measured
# --install  copies the bundle to ~/Library/Audio/Plug-Ins/VST3
#
# Needs the web's native runtime (app/web: scripts/build-runtime.sh --native)
# and Sonic Pi's SuperSonic checked out (app/external/supersonic).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WEB="${SONIC_PI_WEB_ROOT:-$HERE/../web}"
ETC="${SONIC_PI_ETC:-$HERE/../../etc}"
PAGE="$WEB/build/plugin-web"   # the page as the plugin carries it: SuperSonic's client in it, nothing from a CDN
# VST3's interface headers, at the tag Clockwork pins (plugins/vst3/CMakeLists.txt): the first under the MIT licence.
VST3_TAG="v3.8.1_build_84"
VST3="${VST3_PLUGINTERFACES_INCLUDE_DIR:-$HERE/target/vst3_pluginterfaces}"
TEST=0; INSTALL=0
for arg in "$@"; do
    case "$arg" in
        --test) TEST=1 ;;
        --install) INSTALL=1 ;;
        *) echo "unknown argument: $arg" >&2; exit 2 ;;
    esac
done
( cd "$WEB" && node scripts/build-web.mjs --out build/plugin-web --supersonic local > /dev/null )
[ -f "$PAGE/code.html" ] || { echo "no built page at $PAGE" >&2; exit 1; }
[ -d "$ETC/synthdefs/compiled" ] || { echo "no Sonic Pi etc/ at $ETC (set SONIC_PI_ETC)" >&2; exit 1; }

if [ ! -f "$VST3/pluginterfaces/base/funknown.h" ]; then
    mkdir -p "$VST3"
    git clone --quiet --depth 1 --branch "$VST3_TAG" https://github.com/steinbergmedia/vst3_pluginterfaces.git "$VST3/pluginterfaces"
fi

( cd "$HERE" && cargo build --release )

NAME="Sonic Pi"
BUNDLE="$HERE/dist/$NAME.vst3"
rm -rf "$BUNDLE"
mkdir -p "$BUNDLE/Contents/MacOS" "$BUNDLE/Contents/Resources/page"
cp "$HERE/target/release/libsonic_pi_plugin.dylib" "$BUNDLE/Contents/MacOS/$NAME"
rsync -a --exclude '*.map' "$PAGE/" "$BUNDLE/Contents/Resources/page/"
rsync -a --include '*.scsyndef' --exclude '*' "$ETC/synthdefs/compiled/" "$BUNDLE/Contents/Resources/synthdefs/"
rsync -a "$ETC/samples/" "$BUNDLE/Contents/Resources/samples/"
rsync -a "$ETC/buffers/" "$BUNDLE/Contents/Resources/buffers/"
cat > "$BUNDLE/Contents/Info.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleExecutable</key><string>$NAME</string>
  <key>CFBundleIdentifier</key><string>net.sonic-pi.plugin</string>
  <key>CFBundleName</key><string>$NAME</string>
  <key>CFBundlePackageType</key><string>BNDL</string>
  <key>CFBundleSignature</key><string>????</string>
  <key>CFBundleShortVersionString</key><string>0.1.0</string>
  <key>CFBundleVersion</key><string>0.1.0</string>
  <key>CFBundleSupportedPlatforms</key><array><string>MacOSX</string></array>
</dict>
</plist>
PLIST
printf 'BNDL????' > "$BUNDLE/Contents/PkgInfo"
codesign --force --deep --sign - "$BUNDLE"
R="$BUNDLE/Contents/Resources"
echo "built $BUNDLE: binary $(du -sh "$BUNDLE/Contents/MacOS" | cut -f1), page $(du -sh "$R/page" | cut -f1), synthdefs $(du -sh "$R/synthdefs" | cut -f1), samples $(du -sh "$R/samples" | cut -f1)"

if [ "$TEST" = 1 ]; then
    ( cd "$HERE" && cargo test --release -- --test-threads=1 )
    c++ -std=c++17 -fobjc-arc -O1 -I"$VST3" "$HERE/test/host.mm" -framework AppKit -framework WebKit -framework Carbon -o "$HERE/target/host"
    SONIC_PI_PLUGIN_LOG="$HERE/target/host.log" SONIC_PI_PLUGIN_PROBE=1 "$HERE/target/host" "$BUNDLE"
fi

if [ "$INSTALL" = 1 ]; then
    DEST="$HOME/Library/Audio/Plug-Ins/VST3"
    mkdir -p "$DEST"
    rm -rf "$DEST/$NAME.vst3"
    cp -R "$BUNDLE" "$DEST/"
    echo "installed $DEST/$NAME.vst3"
fi
