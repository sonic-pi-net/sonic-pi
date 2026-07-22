#!/bin/bash

# Drive the Debian-Policy package pipeline in CI, one phase per invocation so
# a failure points at the stage that broke. Mirrors the supersonic repo's
# scripts/ci-debian-package.sh — this is the package that should let the
# Debian maintainers drop their patch stack, so it is built, linted and
# tested the way the archive would.
#
# Usage:
#   app/linux-debian-ci.sh <phase>
#
# Phases (in workflow order):
#   deps        packaging toolchain (devscripts, lintian, autopkgtest, …)
#   builddeps   install Build-Depends from packaging/debian/control
#   source      assemble .dsc + orig/component tarballs (linux-debian-source.sh)
#   build       extract the .dsc fresh and dpkg-buildpackage it — OFFLINE;
#               runs the server test suite via debian/rules
#   lintian     lintian on source + binaries, failing on errors AND warnings
#   autopkgtest run debian/tests (layout gate + flaky engine boot) against
#               the built debs
#   smoke       install the debs in a FRESH container, check the installed
#               layout and that the GUI binary resolves against system libs
#               (run this phase in a separate pristine container)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
WORK="$SCRIPT_DIR/build/debian"

export DEBIAN_FRONTEND=noninteractive

setup_env() {
    # CI runs the container as root against a workspace owned by the runner's
    # uid, which git otherwise refuses to touch.
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0='*'
}

phase_deps() {
    echo "=== deps: packaging toolchain ==="
    apt-get update
    apt-get install -y --no-install-recommends \
        build-essential devscripts dpkg-dev debhelper equivs \
        lintian autopkgtest \
        git ca-certificates xz-utils
}

phase_builddeps() {
    echo "=== builddeps: install Build-Depends ==="
    mk-build-deps --install --remove \
        --tool 'apt-get -y -o Debug::pkgProblemResolver=yes --no-install-recommends' \
        "$PROJECT_ROOT/packaging/debian/control"
}

phase_source() {
    echo "=== source package ==="
    setup_env
    "$SCRIPT_DIR/linux-debian-source.sh" "$WORK"
}

phase_build() {
    echo "=== build (offline) ==="
    # Loudly record whether the offline claim is actually being tested.
    if timeout 5 bash -c 'exec 3<>/dev/tcp/deb.debian.org/80' 2>/dev/null; then
        echo "WARNING: network is reachable — this build does NOT prove offline buildability" >&2
    else
        echo "network unreachable — offline build proof is live"
    fi
    rm -rf "$WORK/build-area"
    mkdir -p "$WORK/build-area"
    dsc=("$WORK"/*.dsc)
    # Extract the freshly built source package rather than reusing the
    # assembly tree: this is what proves the .dsc is complete.
    (cd "$WORK/build-area" && dpkg-source -x "${dsc[0]}" src)
    (cd "$WORK/build-area/src" && dpkg-buildpackage -us -uc -b)
}

phase_lintian() {
    echo "=== lintian (fail on errors + warnings) ==="
    lintian --fail-on error,warning --info "$WORK"/*.dsc
    lintian --fail-on error,warning --info "$WORK"/build-area/*.changes
}

phase_autopkgtest() {
    echo "=== autopkgtest ==="
    # debs + .dsc (a binary-only .changes does not reference the source, and
    # autopkgtest needs the source tree for debian/tests).
    autopkgtest "$WORK"/build-area/*.deb "$WORK"/*.dsc -- null
}

phase_smoke() {
    echo "=== smoke: install in a pristine container ==="
    apt-get update
    # apt resolves the inter-package Depends (server, samples, doc) and the
    # system libraries in one go — a missing runtime Depends fails here in a
    # way the build container, with its Build-Depends installed, cannot show.
    apt-get install -y "$WORK"/build-area/*.deb
    test -x /usr/bin/sonic-pi
    test -x /usr/lib/sonic-pi/app/server/native/supersonic
    # A Policy package links the system Qt and friends — every library must
    # resolve with no bundled paths and no display.
    missing=$(ldd /usr/bin/sonic-pi | grep 'not found' || true)
    if [ -n "$missing" ]; then
        echo "FAIL: unresolved libraries in /usr/bin/sonic-pi:" >&2
        echo "$missing" >&2
        exit 1
    fi
    # The deterministic installed-layout gate, same script autopkgtest runs.
    sh "$PROJECT_ROOT/packaging/debian/tests/layout"
    echo "smoke OK"
}

case "${1:-}" in
    deps)        phase_deps ;;
    builddeps)   phase_builddeps ;;
    source)      phase_source ;;
    build)       phase_build ;;
    lintian)     phase_lintian ;;
    autopkgtest) phase_autopkgtest ;;
    smoke)       phase_smoke ;;
    *)
        echo "Usage: $0 {deps|builddeps|source|build|lintian|autopkgtest|smoke}" >&2
        exit 1
        ;;
esac
