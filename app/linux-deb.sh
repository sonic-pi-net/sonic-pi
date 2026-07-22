#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

# Build a Sonic Pi .deb from the AppDir that linux-appimage.sh stages.
#
# This is the release package — the apt-installable sibling of the AppImage,
# carrying the same self-contained tree (including the Qt libraries linuxdeploy
# gathered) so that it installs on any Debian-derived system with a matching
# architecture. It is NOT the Debian archive package: that one links system
# libraries and follows Debian Policy, and lives in packaging/debian.
#
# Run linux-appimage.sh first (or pass --stage) so the AppDir exists.
#
# Usage:
#   ./linux-deb.sh                  build .deb from the existing AppDir
#   ./linux-deb.sh --stage          run linux-appimage.sh first
#   VERSION=4.6.0 ./linux-deb.sh    override version (else read from the
#                                    repo-root VERSION file)
#
# Environment:
#   SP_DEB_ARCH  override the detected architecture (amd64/arm64/i386)

APPDIR="${SCRIPT_DIR}/build/Sonic_Pi.AppDir"
STAGE_DIR="${SCRIPT_DIR}/build/deb_stage"
INSTALL_ROOT="/usr/lib/sonic-pi"

usage() {
    cat <<EOF
Usage: ./linux-deb.sh [--stage]

  --stage    run linux-appimage.sh first to produce the AppDir
  VERSION=x.y.z ./linux-deb.sh    override version (else read from VERSION file)
EOF
}

read_version() {
    if [ -n "${VERSION:-}" ]; then
        return
    fi
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

# In dpkg's ordering "5.0.0-RC1" sorts ABOVE a final "5.0.0" (the suffix beats
# the empty revision), so anyone on an RC would never be offered the release.
# Tilde is dpkg's sorts-before-everything marker, and "5.0.0~RC1" < "5.0.0".
deb_version() {
    DEB_VERSION="${VERSION//-/~}"
}

# dpkg spells the architectures differently from uname, and the AppImage
# script's display names differ again — keep the translation in one place.
detect_arch() {
    if [ -n "${SP_DEB_ARCH:-}" ]; then
        DEB_ARCH="${SP_DEB_ARCH}"
        return
    fi
    if command -v dpkg >/dev/null 2>&1; then
        DEB_ARCH="$(dpkg --print-architecture)"
        return
    fi
    case "$(uname -m)" in
        x86_64)      DEB_ARCH="amd64" ;;
        aarch64)     DEB_ARCH="arm64" ;;
        i[3456]86)   DEB_ARCH="i386"  ;;
        *)           echo "ERROR: unmapped architecture $(uname -m) — pass SP_DEB_ARCH" >&2; exit 1 ;;
    esac
}

require_appdir() {
    if [ ! -d "$APPDIR" ]; then
        echo "ERROR: no AppDir at $APPDIR" >&2
        echo "Run ./linux-appimage.sh first, or pass --stage." >&2
        exit 1
    fi
    if [ ! -e "$APPDIR/usr/share/sonic-pi/app/build/gui/sonic-pi" ]; then
        echo "ERROR: AppDir has no GUI binary — staging looks incomplete" >&2
        exit 1
    fi
}

# The AppDir is already a working self-contained tree, so packaging is mostly a
# matter of moving it under $INSTALL_ROOT and keeping the desktop-integration
# files at the paths the freedesktop specs expect.
stage_tree() {
    echo "[1/4] Staging package tree..."
    rm -rf "$STAGE_DIR"
    mkdir -p "$STAGE_DIR${INSTALL_ROOT}" \
             "$STAGE_DIR/usr/bin" \
             "$STAGE_DIR/usr/share/applications" \
             "$STAGE_DIR/usr/share/icons" \
             "$STAGE_DIR/usr/share/mime/packages" \
             "$STAGE_DIR/usr/share/doc/sonic-pi"

    # Everything the AppImage runs from, minus its own bootstrap bits.
    cp -a "$APPDIR/usr/share/sonic-pi/." "$STAGE_DIR${INSTALL_ROOT}/"

    # The bundled Qt/plugin libraries linuxdeploy gathered.
    if [ -d "$APPDIR/usr/lib" ]; then
        cp -a "$APPDIR/usr/lib" "$STAGE_DIR${INSTALL_ROOT}/"
    fi
    if [ -d "$APPDIR/usr/plugins" ]; then
        cp -a "$APPDIR/usr/plugins" "$STAGE_DIR${INSTALL_ROOT}/"
    fi
    if [ -d "$APPDIR/usr/translations" ]; then
        cp -a "$APPDIR/usr/translations" "$STAGE_DIR${INSTALL_ROOT}/"
    fi
    if [ -d "$APPDIR/usr/qml" ]; then
        cp -a "$APPDIR/usr/qml" "$STAGE_DIR${INSTALL_ROOT}/"
    fi
    if [ -d "$APPDIR/usr/ruby" ]; then
        cp -a "$APPDIR/usr/ruby" "$STAGE_DIR${INSTALL_ROOT}/"
    fi

    cp -a "$APPDIR/usr/share/applications/sonic-pi.desktop" "$STAGE_DIR/usr/share/applications/"
    cp -a "$APPDIR/usr/share/icons/." "$STAGE_DIR/usr/share/icons/"
    if [ -f "$APPDIR/usr/share/mime/packages/sonic-pi.xml" ]; then
        cp -a "$APPDIR/usr/share/mime/packages/sonic-pi.xml" "$STAGE_DIR/usr/share/mime/packages/"
    fi
    cp -a "${SCRIPT_DIR}/../LICENSE.md" "$STAGE_DIR/usr/share/doc/sonic-pi/copyright"
}

# A launcher rather than a symlink: the bundled Qt needs to win over any system
# copy, Qt has to be told where its own plugins went, and a bundled Ruby has to
# be put on PATH. This mirrors the AppImage's AppRun — the two must agree, since
# they carry the same payload.
write_launcher() {
    echo "[2/4] Writing launcher..."
    cat > "$STAGE_DIR/usr/bin/sonic-pi" <<EOF
#!/bin/bash
ROOT="${INSTALL_ROOT}"

# Bundled Ruby (configure --enable-load-relative) finds its stdlib relative to
# the bin/ruby that's invoked. Falling back to the system ruby is the path for
# packages built without BUNDLED_RUBY_DIR set.
if [ -x "\$ROOT/ruby/bin/ruby" ]; then
    export PATH="\$ROOT/ruby/bin:\$PATH"
    export LD_LIBRARY_PATH="\$ROOT/lib:\$ROOT/ruby/lib:\${LD_LIBRARY_PATH:-}"
elif command -v ruby >/dev/null 2>&1; then
    export LD_LIBRARY_PATH="\$ROOT/lib:\${LD_LIBRARY_PATH:-}"
else
    msg="Sonic Pi requires Ruby 3.x to run.

Please install it with:  sudo apt install ruby"
    if command -v zenity >/dev/null 2>&1; then
        zenity --error --text="\$msg" --width=400
    else
        echo "\$msg" >&2
    fi
    exit 1
fi

export QT_PLUGIN_PATH="\$ROOT/plugins:\${QT_PLUGIN_PATH:-}"
export QML2_IMPORT_PATH="\$ROOT/qml:\${QML2_IMPORT_PATH:-}"
exec "\$ROOT/app/build/gui/sonic-pi" "\$@"
EOF
    chmod 755 "$STAGE_DIR/usr/bin/sonic-pi"
}

# The bundle carries Qt and friends, but linuxdeploy's excludelist deliberately
# leaves the base system stack (glibc, X11/xcb, OpenGL, ALSA, fontconfig, ...)
# to the host, so the package must depend on it or a minimal system gets a
# binary that won't start. Rather than curating that list by hand, resolve
# every library the shipped binaries still take from the system and map each
# back to the dpkg package that owns it — the same idea as dpkg-shlibdeps.
compute_depends() {
    DEPENDS="libc6, libstdc++6"
    if ! command -v ldd >/dev/null 2>&1 || ! command -v dpkg-query >/dev/null 2>&1; then
        echo "    WARNING: need ldd + dpkg-query to compute Depends — using minimal fallback" >&2
        return
    fi

    local root="$STAGE_DIR${INSTALL_ROOT}"

    # ldd on an executable walks the whole closure, including the system
    # dependencies of the bundled Qt libraries it pulls in. Plugins and Ruby
    # C extensions are dlopened rather than linked, so every shipped .so is
    # scanned as a root of its own.
    local libs
    libs=$( { find "$root" -name '*.so*' -type f; \
              echo "$root/app/build/gui/sonic-pi"; \
              echo "$root/app/server/native/supersonic"; \
              echo "$root/app/server/native/aubio_onset"; \
              if [ -x "$root/ruby/bin/ruby" ]; then echo "$root/ruby/bin/ruby"; fi; } \
            | LD_LIBRARY_PATH="$root/lib:$root/ruby/lib" xargs -r ldd 2>/dev/null \
            | awk '$2 == "=>" && $3 ~ /^\// {print $3}' \
            | sort -u | grep -v "^$STAGE_DIR" ) || true
    if [ -z "$libs" ]; then
        echo "    WARNING: no system libraries resolved — using minimal Depends" >&2
        return
    fi

    # realpath first: ldd reports paths through the /lib merged-usr symlink,
    # while the dpkg database records the /usr/lib file that owns them.
    local pkgs
    pkgs=$(echo "$libs" | xargs -r realpath 2>/dev/null | sort -u \
        | xargs -r dpkg-query -S 2>/dev/null \
        | cut -d: -f1 | sort -u | paste -sd, - | sed 's/,/, /g') || true
    if [ -n "$pkgs" ]; then
        DEPENDS="$pkgs"
    fi
    echo "    computed Depends: $DEPENDS"
}

write_control() {
    echo "[3/4] Writing control metadata..."
    mkdir -p "$STAGE_DIR/DEBIAN"

    # Installed-Size is in KiB and is advisory, but apt shows it before install.
    local size
    size="$(du -sk "$STAGE_DIR" | cut -f1)"

    compute_depends
    local depends="$DEPENDS"

    # A package carrying its own Ruby must not also demand the system one.
    if [ -x "$STAGE_DIR${INSTALL_ROOT}/ruby/bin/ruby" ]; then
        echo "    bundled Ruby present — not depending on system ruby"
    else
        echo "    no bundled Ruby — depending on system ruby"
        depends="$depends, ruby | ruby3.0 | ruby3.1 | ruby3.2 | ruby3.3"
    fi

    cat > "$STAGE_DIR/DEBIAN/control" <<EOF
Package: sonic-pi
Version: ${DEB_VERSION}
Section: sound
Priority: optional
Architecture: ${DEB_ARCH}
Installed-Size: ${size}
Maintainer: Sam Aaron <sam@sonic-pi.net>
Homepage: https://sonic-pi.net
Depends: ${depends}
Recommends: pulseaudio | pipewire-pulse | jackd2
Description: Live Coding Music Synth for Everyone
 Sonic Pi is a code-based music creation and performance tool. It is
 simple enough for computing and music lessons in schools, yet powerful
 enough for professional musicians performing live.
 .
 This package carries its own copy of the libraries it needs and installs
 into ${INSTALL_ROOT}, so it does not track the distribution's own Sonic Pi
 package. Use the distribution package if you would rather have one that
 links the system libraries.
EOF

    # Refresh the desktop and MIME caches; failures here must not fail the
    # install, since a headless system may have neither tool.
    cat > "$STAGE_DIR/DEBIAN/postinst" <<'EOF'
#!/bin/sh
set -e
if [ "$1" = "configure" ]; then
    if command -v update-desktop-database >/dev/null 2>&1; then
        update-desktop-database -q /usr/share/applications || true
    fi
    if command -v update-mime-database >/dev/null 2>&1; then
        update-mime-database /usr/share/mime || true
    fi
    if command -v gtk-update-icon-cache >/dev/null 2>&1; then
        gtk-update-icon-cache -q -f /usr/share/icons/hicolor || true
    fi
fi
exit 0
EOF
    chmod 755 "$STAGE_DIR/DEBIAN/postinst"

    cat > "$STAGE_DIR/DEBIAN/postrm" <<'EOF'
#!/bin/sh
set -e
if [ "$1" = "remove" ] || [ "$1" = "purge" ]; then
    if command -v update-desktop-database >/dev/null 2>&1; then
        update-desktop-database -q /usr/share/applications || true
    fi
    if command -v update-mime-database >/dev/null 2>&1; then
        update-mime-database /usr/share/mime || true
    fi
fi
exit 0
EOF
    chmod 755 "$STAGE_DIR/DEBIAN/postrm"
}

build_deb() {
    echo "[4/4] Building package..."
    # dpkg-deb does not generate md5sums, but dpkg -V and package verification
    # tools expect it.
    ( cd "$STAGE_DIR" && find . -type f ! -path './DEBIAN/*' -printf '%P\0' \
        | xargs -0 md5sum > DEBIAN/md5sums )
    # Root-owned files without needing root: --root-owner-group is in dpkg-deb
    # 1.19+ (Debian 10, Ubuntu 20.04 and later).
    dpkg-deb --build --root-owner-group "$STAGE_DIR" "$OUTPUT" >/dev/null
}

main() {
    if [ "${1:-}" = "--help" ] || [ "${1:-}" = "-h" ]; then
        usage
        exit 0
    fi

    if ! command -v dpkg-deb >/dev/null 2>&1; then
        echo "ERROR: dpkg-deb not found — it ships in the dpkg package" >&2
        exit 1
    fi

    read_version
    deb_version
    detect_arch

    if [ "${1:-}" = "--stage" ]; then
        "${SCRIPT_DIR}/linux-appimage.sh"
    fi

    require_appdir

    OUTPUT="${SCRIPT_DIR}/build/sonic-pi_${DEB_VERSION}_${DEB_ARCH}.deb"

    cat <<EOF
Building Sonic Pi .deb
  version: ${DEB_VERSION}
  arch:    ${DEB_ARCH}
  appdir:  ${APPDIR}
  output:  ${OUTPUT}
EOF

    stage_tree
    write_launcher
    write_control
    build_deb

    ls -lh "$OUTPUT"
    echo
    echo "Install with:  sudo apt install ${OUTPUT}"
}

main "$@"
