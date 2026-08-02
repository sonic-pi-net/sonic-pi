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
#
# Environment:
#   SP_APPIMAGE_ARCH  override the detected arch (x86_64/aarch64/i686)
#   SP_MAX_GLIBC      fail the build if the glibc floor rises above this
#                     version — see check_glibc_floor()

DIST_DIR="${SCRIPT_DIR}/build/linux_dist"
APPDIR="${SCRIPT_DIR}/build/Sonic_Pi.AppDir"
TOOLS_DIR="${SCRIPT_DIR}/build/_appimage_tools"
ICON_SRC="${SCRIPT_DIR}/gui/images/icon.png"
ICON_SIZE=256
ARCH="${SP_APPIMAGE_ARCH:-$(uname -m)}"

# Canonicalise the ix86 family to i686. Which of i386/i486/i586/i686 `uname -m`
# reports depends on the process personality, so a 32-bit build can arrive here
# under any of them.
case "$ARCH" in
    i[3456]86) ARCH="i686" ;;
esac

# Three different arch spellings are needed per target:
#   ARCH_DISPLAY      output filename — matches MSI/DMG conventions (x64/arm64/x86)
#   LINUXDEPLOY_ARCH  linuxdeploy + linuxdeploy-plugin-qt release asset names
#   APPIMAGETOOL_ARCH appimagetool release asset name, and the ARCH env var it
#                     reads to pick the runtime embedded in the AppImage header
# They agree on x86_64/aarch64 but not on 32-bit x86, where linuxdeploy ships
# `-i386` assets and appimagetool ships `-i686` ones.
case "$ARCH" in
    x86_64)  ARCH_DISPLAY="x64"   ; LINUXDEPLOY_ARCH="x86_64"  ; APPIMAGETOOL_ARCH="x86_64" ;;
    aarch64) ARCH_DISPLAY="arm64" ; LINUXDEPLOY_ARCH="aarch64" ; APPIMAGETOOL_ARCH="aarch64" ;;
    i686)    ARCH_DISPLAY="x86"   ; LINUXDEPLOY_ARCH="i386"    ; APPIMAGETOOL_ARCH="i686"   ;;
    *)       ARCH_DISPLAY="$ARCH" ; LINUXDEPLOY_ARCH="$ARCH"   ; APPIMAGETOOL_ARCH="$ARCH"  ;;
esac

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
        "linuxdeploy-${LINUXDEPLOY_ARCH}.AppImage" \
        "https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-${LINUXDEPLOY_ARCH}.AppImage")"
    local qt_plugin
    qt_plugin="$(ensure_tool \
        "linuxdeploy-plugin-qt-${LINUXDEPLOY_ARCH}.AppImage" \
        "https://github.com/linuxdeploy/linuxdeploy-plugin-qt/releases/download/continuous/linuxdeploy-plugin-qt-${LINUXDEPLOY_ARCH}.AppImage")"
    APPIMAGETOOL="$(ensure_tool \
        "appimagetool-${APPIMAGETOOL_ARCH}.AppImage" \
        "https://github.com/AppImage/appimagetool/releases/download/continuous/appimagetool-${APPIMAGETOOL_ARCH}.AppImage")"

    # linuxdeploy searches PATH for `linuxdeploy-plugin-qt*` to load the
    # `--plugin qt` argument. Put the symlink to the AppRun in a clean
    # bin/ dir that ONLY contains it — if the dir also held the raw
    # `.AppImage` file (which it would if we just put TOOLS_DIR on PATH),
    # linuxdeploy's glob would match the .AppImage first and try to run
    # it as an AppImage — fatal in containers without FUSE.
    mkdir -p "${TOOLS_DIR}/bin"
    ln -sf "$qt_plugin" "${TOOLS_DIR}/bin/linuxdeploy-plugin-qt"
    export PATH="${TOOLS_DIR}/bin:${PATH}"
}

# ── AppDir staging ───────────────────────────────────────────────────────────

ensure_dist() {
    if [ "$REBUILD_DIST" = true ] || [ ! -d "$DIST_DIR" ]; then
        echo "[1/6] Staging linux_dist via linux-release.sh..."
        "${SCRIPT_DIR}/linux-release.sh"
    else
        echo "[1/6] Reusing existing linux_dist (pass --rebuild-dist to refresh)"
    fi
}

stage_appdir() {
    echo "[2/6] Staging AppDir..."
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
    # Force exact size (the bang ignores aspect ratio) — linuxdeploy rejects
    # non-square icons, so any drift in the source dimensions must not leak in.
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
MimeType=text/x-sonic-pi-set;
EOF
    ln -sf "usr/share/applications/sonic-pi.desktop" "$APPDIR/sonic-pi.desktop"
}

write_mime() {
    # Declares .sonicpi set files so desktop integration (appimaged /
    # AppImageLauncher) can associate them with Sonic Pi.
    local mimedir="$APPDIR/usr/share/mime/packages"
    mkdir -p "$mimedir"
    cat > "$mimedir/sonic-pi.xml" <<'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
  <mime-type type="text/x-sonic-pi-set">
    <comment>Sonic Pi Set</comment>
    <sub-class-of type="text/plain"/>
    <glob pattern="*.sonicpi"/>
    <icon name="sonic-pi"/>
  </mime-type>
</mime-info>
EOF
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
    echo "[3/6] Writing icon, .desktop, mime, AppRun..."
    write_icon
    write_desktop
    write_mime
    write_apprun
}

# ── Bundling + packaging ─────────────────────────────────────────────────────

bundle_deps() {
    echo "[4/6] Bundling Qt + native deps via linuxdeploy..."
    local exclude_args=()
    for lib in "${EXCLUDED_LIBS[@]}"; do
        exclude_args+=(--exclude-library "${lib}*")
    done

    # Tell linuxdeploy about every native binary that runs at user time so its
    # transitive .so deps land in usr/lib.
    local extra_exes=()
    if [ -n "${BUNDLED_RUBY_DIR:-}" ] && [ -x "$APPDIR/usr/ruby/bin/ruby" ]; then
        extra_exes+=(--executable "$APPDIR/usr/ruby/bin/ruby")
    fi

    "$LINUXDEPLOY" \
        --appdir "$APPDIR" \
        --plugin qt \
        --executable "$APPDIR/usr/bin/sonic-pi" \
        --executable "$APPDIR/usr/share/sonic-pi/app/server/native/sonic-pi-supersonic" \
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

# Report (and optionally enforce) the AppImage's glibc floor.
#
# The floor is the highest GLIBC_x.y symbol version referenced by anything we
# bundle — NOT the build machine's glibc, which is only an upper bound. A
# distro whose glibc predates the floor cannot run the AppImage at all: the
# dynamic loader fails before main(). So this number, not the build container's
# version, is what actually decides how many distros we support.
#
# Set SP_MAX_GLIBC=x.y to fail the build if the floor ever rises above a
# budget — that catches a newly-added dependency silently dropping distros.
check_glibc_floor() {
    echo "[5/6] Checking glibc floor..."
    if ! command -v objdump >/dev/null 2>&1; then
        echo "    objdump not found (install binutils) — skipping"
        return
    fi

    local versions
    versions=$(find "$APPDIR" -type f \( -perm -u+x -o -name '*.so*' \) -print0 \
        | xargs -0 -r objdump -T 2>/dev/null \
        | grep -o 'GLIBC_[0-9][0-9.]*' \
        | sed 's/^GLIBC_//; s/\.$//' \
        | sort -Vu)

    if [ -z "$versions" ]; then
        echo "    no GLIBC_ symbol versions found — skipping"
        return
    fi

    local floor
    floor="$(echo "$versions" | tail -n1)"
    echo "    glibc floor: ${floor} (highest symbol version referenced)"

    if [ -n "${SP_MAX_GLIBC:-}" ]; then
        # sort -V orders ascending, so the floor is over budget iff it, rather
        # than the budget, sorts last.
        if [ "$(printf '%s\n%s\n' "$SP_MAX_GLIBC" "$floor" | sort -V | tail -n1)" != "$SP_MAX_GLIBC" ]; then
            echo "ERROR: glibc floor ${floor} exceeds SP_MAX_GLIBC=${SP_MAX_GLIBC}." >&2
            echo "       Something newly bundled needs a newer glibc, which drops" >&2
            echo "       support for distros that were previously able to run this." >&2
            exit 1
        fi
    fi
}

package_appimage() {
    echo "[6/6] Packaging AppImage..."
    # appimagetool reads ARCH from the env to pick the runtime it embeds in the
    # AppImage header. Use `env` so the assignment is scoped to this one call
    # and doesn't clobber the script's own ARCH.
    env ARCH="$APPIMAGETOOL_ARCH" "$APPIMAGETOOL" "$APPDIR" "$OUTPUT"
}

# ── Main ─────────────────────────────────────────────────────────────────────

main() {
    parse_args "$@"
    resolve_version
    # Filename matches the DMG/MSI convention (Sonic-Pi-for-Mac-arm64-v5.0.0-RC-2.dmg),
    # including the "5.0.0-RC2" → "5.0.0-RC-2" pre-release hyphenation.
    local version_dist
    version_dist="$(printf '%s' "$VERSION" | sed -E 's/-([A-Za-z]+)([0-9]+)$/-\1-\2/')"
    OUTPUT="${SCRIPT_DIR}/build/Sonic-Pi-for-Linux-${ARCH_DISPLAY}-v${version_dist}.AppImage"

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
