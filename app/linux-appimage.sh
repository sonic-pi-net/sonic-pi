#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

# Build a Sonic Pi AppImage from app/build/linux_dist (produced by
# linux-release.sh). Auto-downloads linuxdeploy + linuxdeploy-plugin-qt +
# appimagetool into a cache dir on first run.
#
# Usage:
#   ./linux-appimage.sh                  build AppImage from existing linux_dist
#   ./linux-appimage.sh --rebuild-dist   rerun linux-release.sh first
#   VERSION=4.6.0 ./linux-appimage.sh    override version (else read from
#                                         the repo-root VERSION file)

DIST_DIR="${SCRIPT_DIR}/build/linux_dist"
APPDIR="${SCRIPT_DIR}/build/Sonic_Pi.AppDir"
TOOLS_DIR="${SCRIPT_DIR}/build/_appimage_tools"
ICON_SRC="${SCRIPT_DIR}/gui/images/icon.png"
ICON_SIZE=256
ARCH="$(uname -m)"

# Pure-video codec libs aubio_onset transitively pulls in via libavcodec but
# Sonic Pi never decodes video — safe to omit, saves ~40MB.
EXCLUDED_LIBS=(libx265 libaom libSvtAv1Enc libcodec2 librsvg-2)

usage() {
    cat <<EOF
Build a Sonic Pi AppImage from app/build/linux_dist (produced by linux-release.sh).

Usage:
  ./linux-appimage.sh                  build AppImage from existing linux_dist
  ./linux-appimage.sh --rebuild-dist   rerun linux-release.sh first
  VERSION=4.6.0 ./linux-appimage.sh    override version (else read from VERSION file)
EOF
    exit "${1:-0}"
}

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
    # Repo-root VERSION file is the single source of truth (also read by
    # runtime.rb at boot via Version.init_from_string).
    local version_file="${SCRIPT_DIR}/../VERSION"
    if [ ! -f "$version_file" ]; then
        echo "ERROR: VERSION file not found at $version_file — pass VERSION=x.y.z" >&2
        exit 1
    fi
    VERSION="$(tr -d '[:space:]' < "$version_file")"
    if [ -z "$VERSION" ]; then
        echo "ERROR: VERSION file empty — pass VERSION=x.y.z" >&2
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

    # The AppImage ships no compiled Ruby C extensions: target Rubies vary across
    # user systems so prebuilt .so files would mismatch ABI. Strip rb-native/ and
    # the rugged vendor tree so gitsave.rb's LoadError fallback engages cleanly.
    local ruby_root="$APPDIR/usr/share/sonic-pi/app/server/ruby"
    rm -rf "$ruby_root/rb-native" "$ruby_root/vendor/rugged-"*

    if [ -n "${BUNDLED_RUBY_DIR:-}" ]; then
        if [ ! -x "${BUNDLED_RUBY_DIR}/bin/ruby" ]; then
            echo "ERROR: BUNDLED_RUBY_DIR=${BUNDLED_RUBY_DIR} has no bin/ruby" >&2
            exit 1
        fi
        echo "    bundling Ruby from ${BUNDLED_RUBY_DIR}"
        mkdir -p "$APPDIR/usr/ruby"
        cp -a "${BUNDLED_RUBY_DIR}/." "$APPDIR/usr/ruby/"
    else
        echo "    BUNDLED_RUBY_DIR unset — AppImage will require system Ruby 3.x"
    fi

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

# Bundled Ruby (configure --enable-load-relative) finds its stdlib relative to
# the bin/ruby that's invoked. Falling back to the system ruby is the legacy
# path for AppImages built without BUNDLED_RUBY_DIR set.
if [ -x "$HERE/usr/ruby/bin/ruby" ]; then
    export PATH="$HERE/usr/ruby/bin:$PATH"
    export LD_LIBRARY_PATH="$HERE/usr/lib:$HERE/usr/ruby/lib:${LD_LIBRARY_PATH:-}"
elif command -v ruby >/dev/null 2>&1; then
    export LD_LIBRARY_PATH="$HERE/usr/lib:${LD_LIBRARY_PATH:-}"
else
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

    # Tell linuxdeploy about every native binary that runs at user time so its
    # transitive .so deps land in usr/lib. The Tau Erlang prod release brings
    # its own erts/ tree; beam.smp is the emulator and pulls in libcrypto etc.
    local extra_exes=()
    if [ -n "${BUNDLED_RUBY_DIR:-}" ] && [ -x "$APPDIR/usr/ruby/bin/ruby" ]; then
        extra_exes+=(--executable "$APPDIR/usr/ruby/bin/ruby")
    fi
    local beam_smp
    beam_smp=$(find "$APPDIR/usr/share/sonic-pi/app/server/beam/tau" \
                    -path '*/erts-*/bin/beam.smp' -type f 2>/dev/null | head -n1)
    if [ -n "$beam_smp" ]; then
        extra_exes+=(--executable "$beam_smp")
    fi

    "$LINUXDEPLOY" \
        --appdir "$APPDIR" \
        --plugin qt \
        --executable "$APPDIR/usr/bin/sonic-pi" \
        --executable "$APPDIR/usr/share/sonic-pi/app/server/native/supersonic" \
        --executable "$APPDIR/usr/share/sonic-pi/app/server/native/aubio_onset" \
        "${extra_exes[@]}" \
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
