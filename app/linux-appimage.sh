#!/bin/bash
set -euo pipefail

# ──────────────────────────────────────────────────────────────────────────────
# Build a Sonic Pi AppImage from app/build/linux_dist (produced by
# linux-release.sh). Auto-downloads linuxdeploy + linuxdeploy-plugin-qt +
# appimagetool into a cache dir on first run.
#
# Usage:
#   ./linux-appimage.sh                  build AppImage from existing linux_dist
#   ./linux-appimage.sh --rebuild-dist   rerun linux-release.sh first
#   VERSION=4.6.0 ./linux-appimage.sh    override version (else parsed from
#                                         server/ruby/lib/sonicpi/runtime.rb)
# ──────────────────────────────────────────────────────────────────────────────

readonly SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
readonly DIST_DIR="${SCRIPT_DIR}/build/linux_dist"
readonly APPDIR="${SCRIPT_DIR}/build/Sonic_Pi.AppDir"
readonly TOOLS_DIR="${SCRIPT_DIR}/build/_appimage_tools"
readonly ICON_SRC="${SCRIPT_DIR}/gui/images/icon.png"
readonly ICON_SIZE=256
readonly ARCH="$(uname -m)"

# Pure-video codec libs aubio_onset transitively pulls in via libavcodec but
# Sonic Pi never decodes video — safe to omit, saves ~40MB.
readonly EXCLUDED_LIBS=(libx265 libaom libSvtAv1Enc libcodec2 librsvg-2)

# Saved at script entry so the EXIT trap can restore it.
readonly WORKING_DIR="$(pwd)"

usage() {
    sed -n '/^# ─*$/,/^# ─*$/p' "$0" | sed 's/^# \?//' | head -n -1 | tail -n +2
    exit "${1:-0}"
}

cleanup() {
    local rc=$?
    cd "${WORKING_DIR}"
    # Leave a half-built AppDir behind only in interactive runs; in CI/scripts
    # the next run will rm it anyway, but a clean failure is friendlier.
    if [ $rc -ne 0 ] && [ -d "$APPDIR" ] && [ -z "${KEEP_APPDIR_ON_FAIL:-}" ]; then
        echo "Cleaning up partial AppDir (set KEEP_APPDIR_ON_FAIL=1 to retain)..." >&2
        rm -rf "$APPDIR"
    fi
}
trap cleanup EXIT

parse_args() {
    REBUILD_DIST=false
    while [ $# -gt 0 ]; do
        case "$1" in
            --rebuild-dist) REBUILD_DIST=true ;;
            -h|--help)      usage 0 ;;
            *)              echo "Unknown arg: $1" >&2; usage 1 ;;
        esac
        shift
    done
}

resolve_version() {
    if [ -n "${VERSION:-}" ]; then return; fi
    # Source of truth is the active @version assignment in runtime.rb.
    # The line directly above is a commented-out dev version; the leading
    # `#` stops it matching ^\s*@version.
    local runtime_rb="${SCRIPT_DIR}/server/ruby/lib/sonicpi/runtime.rb"
    VERSION="$(grep -E '^[[:space:]]*@version[[:space:]]*=[[:space:]]*Version\.new' "$runtime_rb" \
        | head -n1 \
        | grep -oE '[0-9]+,[[:space:]]*[0-9]+,[[:space:]]*[0-9]+' \
        | tr -d ' ' | tr ',' '.')"
    if [ -z "$VERSION" ]; then
        echo "ERROR: could not parse @version from $runtime_rb — pass VERSION=x.y.z" >&2
        exit 1
    fi
}

# ── Tool fetching ────────────────────────────────────────────────────────────

# Download an AppImage tool to TOOLS_DIR if missing, and echo a runnable path.
# Falls back to --appimage-extract on systems without FUSE.
ensure_tool() {
    local name="$1" url="$2"
    local appimage="${TOOLS_DIR}/${name}"
    local extracted="${TOOLS_DIR}/${name%.AppImage}.extracted"

    if [ ! -x "$appimage" ]; then
        echo "    fetching ${name}..." >&2
        curl --fail --location --silent --show-error -o "$appimage" "$url"
        chmod +x "$appimage"
    fi

    if "$appimage" --version >/dev/null 2>&1; then
        echo "$appimage"
        return
    fi

    # FUSE unavailable — extract once and use the inner AppRun.
    if [ ! -d "$extracted" ]; then
        echo "    extracting ${name} (no FUSE)..." >&2
        (cd "${TOOLS_DIR}" && "$appimage" --appimage-extract >/dev/null)
        mv "${TOOLS_DIR}/squashfs-root" "$extracted"
    fi
    echo "${extracted}/AppRun"
}

ensure_tools() {
    mkdir -p "${TOOLS_DIR}"
    LINUXDEPLOY="$(ensure_tool \
        "linuxdeploy-${ARCH}.AppImage" \
        "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-${ARCH}.AppImage")"
    local qt_plugin
    qt_plugin="$(ensure_tool \
        "linuxdeploy-plugin-qt-${ARCH}.AppImage" \
        "https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases/download/continuous/linuxdeploy-plugin-qt-${ARCH}.AppImage")"
    APPIMAGETOOL="$(ensure_tool \
        "appimagetool-${ARCH}.AppImage" \
        "https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-${ARCH}.AppImage")"

    # linuxdeploy looks plugins up by short name on PATH (e.g. `qt` → `linuxdeploy-plugin-qt`).
    ln -sf "$qt_plugin" "${TOOLS_DIR}/linuxdeploy-plugin-qt"
    export PATH="${TOOLS_DIR}:${PATH}"
}

# ── AppDir staging ───────────────────────────────────────────────────────────

ensure_dist() {
    if [ "$REBUILD_DIST" = true ] || [ ! -d "$DIST_DIR" ]; then
        echo "[1/5] Staging linux_dist via linux-release.sh..."
        "${SCRIPT_DIR}/linux-release.sh"
    else
        echo "[1/5] Reusing existing linux_dist (pass --rebuild-dist to refresh)"
    fi
}

stage_appdir() {
    echo "[2/5] Staging AppDir..."
    rm -rf "$APPDIR"
    mkdir -p "$APPDIR/usr/bin" \
             "$APPDIR/usr/lib" \
             "$APPDIR/usr/translations" \
             "$APPDIR/usr/share/applications" \
             "$APPDIR/usr/share/icons/hicolor/${ICON_SIZE}x${ICON_SIZE}/apps" \
             "$APPDIR/usr/share/sonic-pi"
    cp -a "${DIST_DIR}/." "$APPDIR/usr/share/sonic-pi/"

    # linuxdeploy inspects executables it's told about; symlinking the GUI binary
    # into usr/bin gives it a stable target with a relative path.
    ln -sf "../share/sonic-pi/app/build/gui/sonic-pi" "$APPDIR/usr/bin/sonic-pi"
}

write_icon() {
    if ! command -v convert >/dev/null 2>&1; then
        echo "ERROR: ImageMagick 'convert' not found — install with:" >&2
        echo "  sudo apt install imagemagick" >&2
        exit 1
    fi
    local canonical="$APPDIR/usr/share/icons/hicolor/${ICON_SIZE}x${ICON_SIZE}/apps/sonic-pi.png"
    # Force exact size (the bang ignores aspect ratio); source icon is a near-square
    # 2644x2666 so the squash is invisible. linuxdeploy rejects non-square icons.
    convert "$ICON_SRC" -resize "${ICON_SIZE}x${ICON_SIZE}!" "$canonical"
    ln -sf "usr/share/icons/hicolor/${ICON_SIZE}x${ICON_SIZE}/apps/sonic-pi.png" "$APPDIR/sonic-pi.png"
    ln -sf "sonic-pi.png" "$APPDIR/.DirIcon"
}

write_desktop() {
    local canonical="$APPDIR/usr/share/applications/sonic-pi.desktop"
    cat > "$canonical" <<'EOF'
[Desktop Entry]
Type=Application
Name=Sonic Pi
GenericName=Live Coding Music Synth
Comment=The Live Coding Music Synth for Everyone (requires system Ruby 3.x)
Exec=sonic-pi %F
Icon=sonic-pi
Terminal=false
Categories=AudioVideo;Audio;Music;Education;
Keywords=music;livecoding;synth;programming;
StartupWMClass=sonic-pi
EOF
    ln -sf "usr/share/applications/sonic-pi.desktop" "$APPDIR/sonic-pi.desktop"
}

write_apprun() {
    cat > "$APPDIR/AppRun" <<'EOF'
#!/bin/bash
HERE="$(dirname "$(readlink -f "$0")")"

if ! command -v ruby >/dev/null 2>&1; then
    msg="Sonic Pi requires Ruby 3.x to run.

Please install via your distro:
  Debian/Ubuntu:  sudo apt install ruby
  Fedora:         sudo dnf install ruby
  Arch:           sudo pacman -S ruby"
    if command -v zenity >/dev/null 2>&1; then
        zenity --error --text="$msg" --width=400
    else
        echo "$msg" >&2
    fi
    exit 1
fi

export LD_LIBRARY_PATH="$HERE/usr/lib:${LD_LIBRARY_PATH:-}"
export QT_PLUGIN_PATH="$HERE/usr/plugins:${QT_PLUGIN_PATH:-}"
export QML2_IMPORT_PATH="$HERE/usr/qml:${QML2_IMPORT_PATH:-}"
exec "$HERE/usr/share/sonic-pi/app/build/gui/sonic-pi" "$@"
EOF
    chmod +x "$APPDIR/AppRun"
}

write_metadata() {
    echo "[3/5] Writing icon, .desktop, AppRun..."
    write_icon
    write_desktop
    write_apprun
}

# ── Bundling + packaging ─────────────────────────────────────────────────────

bundle_deps() {
    echo "[4/5] Bundling Qt + native deps via linuxdeploy..."
    local exclude_args=()
    for lib in "${EXCLUDED_LIBS[@]}"; do
        exclude_args+=(--exclude-library "${lib}*")
    done

    "$LINUXDEPLOY" \
        --appdir "$APPDIR" \
        --plugin qt \
        --executable "$APPDIR/usr/bin/sonic-pi" \
        --executable "$APPDIR/usr/share/sonic-pi/app/server/native/supersonic" \
        --executable "$APPDIR/usr/share/sonic-pi/app/server/native/aubio_onset" \
        "${exclude_args[@]}" \
        --desktop-file "$APPDIR/sonic-pi.desktop" \
        --icon-file "$APPDIR/sonic-pi.png"

    # --exclude-library isn't documented in linuxdeploy's --help; verify it
    # was honoured rather than silently re-bundling video codecs.
    local leaked=()
    for lib in "${EXCLUDED_LIBS[@]}"; do
        local hits
        hits=$(find "$APPDIR/usr/lib" -maxdepth 1 -name "${lib}*" 2>/dev/null)
        [ -n "$hits" ] && leaked+=("$lib")
    done
    if [ ${#leaked[@]} -gt 0 ]; then
        echo "ERROR: linuxdeploy bundled libs we asked to exclude: ${leaked[*]}" >&2
        echo "       linuxdeploy may have changed its --exclude-library semantics." >&2
        exit 1
    fi
}

package_appimage() {
    echo "[5/5] Packaging AppImage..."
    # appimagetool reads ARCH from the env to embed in the AppImage runtime
    # header. Use `env` so the assignment doesn't conflict with our readonly ARCH.
    env ARCH="$ARCH" "$APPIMAGETOOL" "$APPDIR" "$OUTPUT"
}

# ── Main ─────────────────────────────────────────────────────────────────────

main() {
    parse_args "$@"
    resolve_version
    OUTPUT="${SCRIPT_DIR}/build/Sonic-Pi-${VERSION}-${ARCH}.AppImage"

    cd "${SCRIPT_DIR}"

    cat <<EOF
=== Sonic Pi AppImage ===
  arch:    ${ARCH}
  version: ${VERSION}
  output:  ${OUTPUT}

EOF

    ensure_dist
    ensure_tools
    stage_appdir
    write_metadata
    bundle_deps
    package_appimage

    echo
    echo "=== AppImage built ==="
    ls -lh "$OUTPUT"
    echo
    echo "Run with:  $OUTPUT"
}

main "$@"
