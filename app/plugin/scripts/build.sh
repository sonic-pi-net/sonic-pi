#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (c) 2026 Sam Aaron
#
# Builds "Sonic Pi.vst3": the plugin, the web GUI it shows, and Sonic Pi's
# synthdefs, samples and random tables, which each instance loads its own.
#
#   scripts/build.sh [--test] [--install] [--universal]
#
# --universal  the plugin for both of a Mac's architectures (arm64 and x86_64) in one bundle, as a release is.
#              Without it the bundle is for this machine's architecture only, which is quicker to build.
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
TEST=0; INSTALL=0; UNIVERSAL=0
for arg in "$@"; do
    case "$arg" in
        --test) TEST=1 ;;
        --install) INSTALL=1 ;;
        --universal) UNIVERSAL=1 ;;
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

LIB="$HERE/target/release/libsonic_pi_plugin.dylib"
if [ "$UNIVERSAL" = 1 ]; then
    # mruby once more, for the architecture this machine is not, and the two joined: its build cannot make both at
    # once (it runs the preprocessor alone, which takes one architecture)
    MRUBY="$WEB/build/mruby"
    case "$(uname -m)" in arm64) OTHER=x86_64 ;; *) OTHER=arm64 ;; esac
    ( cd "$WEB" && SP_MRUBY_ARCHS="$OTHER" SP_MRUBY_BUILD_DIR="$MRUBY-$OTHER" scripts/build-runtime.sh --native )
    rm -rf "$MRUBY-universal"
    mkdir -p "$MRUBY-universal/host/lib"
    cp -R "$MRUBY/host/include" "$MRUBY-universal/host/include"
    cp "$MRUBY/host/lib/libmruby.flags.mak" "$MRUBY-universal/host/lib/"
    lipo -create "$MRUBY/host/lib/libmruby.a" "$MRUBY-$OTHER/host/lib/libmruby.a" -output "$MRUBY-universal/host/lib/libmruby.a"
    export SONIC_PI_MRUBY="$MRUBY-universal/host"
    # the engine's C++ for both architectures at once, with the options .cargo/config.toml gives every build
    ARGS="$(sed -n 's/^CLOCKWORK_SYS_CMAKE_ARGS = "\(.*\)"$/\1/p' "$HERE/.cargo/config.toml")"
    [ -n "$ARGS" ] || { echo "no CLOCKWORK_SYS_CMAKE_ARGS in .cargo/config.toml" >&2; exit 1; }
    export CLOCKWORK_SYS_CMAKE_ARGS="$ARGS -DCMAKE_OSX_ARCHITECTURES=arm64;x86_64"
    # rustup's toolchain, which holds the standard library for both targets (a Rust from a package manager has it
    # for its own machine only)
    command -v rustup > /dev/null || { echo "a universal build needs rustup, with: rustup target add aarch64-apple-darwin x86_64-apple-darwin" >&2; exit 1; }
    # by their own paths: with a package manager's Rust first on PATH, `rustup run` still finds that one
    CARGO="$(rustup which cargo --toolchain stable)"
    RUSTC="$(rustup which rustc --toolchain stable)"
    for target in aarch64-apple-darwin x86_64-apple-darwin; do
        ( cd "$HERE" && RUSTC="$RUSTC" "$CARGO" build --release --target "$target" )
    done
    LIB="$HERE/target/universal/libsonic_pi_plugin.dylib"
    mkdir -p "$HERE/target/universal"
    lipo -create "$HERE/target/aarch64-apple-darwin/release/libsonic_pi_plugin.dylib" \
                 "$HERE/target/x86_64-apple-darwin/release/libsonic_pi_plugin.dylib" -output "$LIB"
else
    ( cd "$HERE" && cargo build --release )
fi

NAME="Sonic Pi"
VERSION="$(sed -n 's/^version = "\(.*\)"$/\1/p' "$HERE/Cargo.toml" | head -n 1)"   # the plugin's version is Cargo.toml's
[ -n "$VERSION" ] || { echo "no version in $HERE/Cargo.toml" >&2; exit 1; }
BUNDLE="$HERE/dist/$NAME.vst3"
rm -rf "$BUNDLE"
mkdir -p "$BUNDLE/Contents/MacOS" "$BUNDLE/Contents/Resources/page"
cp "$LIB" "$BUNDLE/Contents/MacOS/$NAME"
# The page, less what only the web's own engine and runtime use. In the plugin the sound is the plugin's and so is
# the runtime (web/runtime.js inPlugin), so the page never asks for the wasm engine, the wasm runtime, their random
# tables, or the engine's copies of the samples and synthdefs: 44 MB the bundle would otherwise carry twice.
# The other wasm beside the runtime stays: the Share button makes its links with it (sp_share).
rsync -a --exclude '*.map' \
    --exclude '/engine/samples' --exclude '/engine/synthdefs' --exclude '/engine/wasm/clockwork-scsynth*' \
    --exclude '/runtime/sp_runtime.wasm' --exclude '/buffers' \
    "$PAGE/" "$BUNDLE/Contents/Resources/page/"
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
  <key>CFBundleShortVersionString</key><string>$VERSION</string>
  <key>CFBundleVersion</key><string>$VERSION</string>
  <key>CFBundleSupportedPlatforms</key><array><string>MacOSX</string></array>
</dict>
</plist>
PLIST
printf 'BNDL????' > "$BUNDLE/Contents/PkgInfo"
codesign --force --deep --sign - "$BUNDLE"
R="$BUNDLE/Contents/Resources"
echo "built $BUNDLE ($(lipo -archs "$BUNDLE/Contents/MacOS/$NAME")): binary $(du -sh "$BUNDLE/Contents/MacOS" | cut -f1), page $(du -sh "$R/page" | cut -f1), synthdefs $(du -sh "$R/synthdefs" | cut -f1), samples $(du -sh "$R/samples" | cut -f1)"

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
