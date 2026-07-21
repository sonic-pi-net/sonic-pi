#!/bin/bash
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

# Build, test and package 32-bit x86 Sonic Pi. This script expects to already
# be running *inside* a 32-bit Debian userland — use linux-appimage-i386.sh to
# launch that container, or run it directly on a native i386 Debian box.
#
# Why a container rather than a cross-compile: Qt ships no 32-bit Linux
# binaries (aqtinstall carries linux_x64 and linux_arm64 only), so Qt has to
# come from the distro. Debian bookworm is the oldest suite with a complete
# Qt 6 i386 stack, and its glibc (2.36) caps the AppImage's runtime floor.
#
# Usage:
#   ./linux-appimage-i386-container.sh [phase]
#
# Phases run in order when given `all` (the default). CI runs them one at a
# time against a long-lived container so each gets its own log group and
# failures point at a specific stage:
#   deps     apt dependencies
#   ruby     source-build the bundled Ruby
#   rust     rustup toolchain (supersonic's MIDI/gamepad/OSC subsystems)
#   build    prebuild + configure + compile
#   test     Spider (Ruby), API and GUI test suites
#   package  stage the AppDir and produce the AppImage
#
# Environment:
#   RUBY_VERSION       Ruby to bundle (default 3.4.4)
#   BUNDLED_RUBY_DIR   where the bundled Ruby lives (default <repo>/_bundled_ruby);
#                      reused as-is if it already holds a working bin/ruby, so CI
#                      can restore it from cache
#   SP_MAX_GLIBC       fail packaging if the glibc floor exceeds this version

REPO_DIR="$( cd "${SCRIPT_DIR}/.." && pwd )"
RUBY_VERSION="${RUBY_VERSION:-3.4.4}"
BUNDLED_RUBY_DIR="${BUNDLED_RUBY_DIR:-${REPO_DIR}/_bundled_ruby}"
export BUNDLED_RUBY_DIR

check_arch() {
    if [ "$(getconf LONG_BIT)" != "32" ]; then
        echo "ERROR: this script must run inside a 32-bit userland (getconf LONG_BIT = $(getconf LONG_BIT))." >&2
        echo "       Use ./linux-appimage-i386.sh to launch the i386 container." >&2
        exit 1
    fi
}

# Every phase after `ruby`/`rust` needs the bundled Ruby ahead of any system
# ruby, and cargo on PATH. Each phase is its own process (CI invokes them as
# separate `docker exec` calls), so this is re-applied rather than inherited.
setup_paths() {
    export PATH="${BUNDLED_RUBY_DIR}/bin:${PATH}"
    if [ -f "${CARGO_HOME:-$HOME/.cargo}/env" ]; then
        # shellcheck disable=SC1091
        . "${CARGO_HOME:-$HOME/.cargo}/env"
    fi

    # In CI the container runs as root against a workspace owned by the
    # runner's uid, so every git call (linux-prebuild.sh's submodule update
    # first among them) would abort with "detected dubious ownership".
    # Set it via the environment rather than `git config --global` so a dev
    # running this on a native i386 box doesn't get their gitconfig rewritten.
    export GIT_CONFIG_COUNT=1
    export GIT_CONFIG_KEY_0=safe.directory
    export GIT_CONFIG_VALUE_0='*'
}

phase_deps() {
    echo "=== deps: installing build dependencies ==="
    export DEBIAN_FRONTEND=noninteractive
    apt-get update
    # Mirrors the aqtinstall-based AppImage jobs' apt list, with Qt 6 added
    # (no 32-bit Qt prebuilds exist, so it comes from Debian) plus the tools a
    # bare container needs, the AppImage tooling needs, ./configure needs for
    # the source build of Ruby, and the test suites need (jackd2 for the API
    # tests' dummy audio backend).
    apt-get install -y \
        pulseaudio dbus-x11 libssl-dev \
        alsa-utils libasound2-dev libjack-jackd2-dev jackd2 \
        libudev-dev autoconf automake libtool-bin gettext \
        pkg-config m4 libaubio-dev libpng-dev libboost-dev \
        libxrandr-dev libxinerama-dev libxcursor-dev libxcomposite-dev libxext-dev libxrender-dev \
        libfreetype6-dev libfontconfig1-dev \
        libgl1-mesa-dev libglu1-mesa-dev \
        qt6-base-dev qt6-base-dev-tools qt6-svg-dev \
        qt6-tools-dev qt6-tools-dev-tools qt6-l10n-tools \
        build-essential cmake ca-certificates curl git sudo \
        unzip xz-utils p7zip-full \
        file imagemagick fuse libfuse2 \
        libyaml-dev libffi-dev libreadline-dev zlib1g-dev libgdbm-dev libgmp-dev
}

phase_ruby() {
    if [ -x "${BUNDLED_RUBY_DIR}/bin/ruby" ]; then
        echo "=== ruby: reusing bundled Ruby at ${BUNDLED_RUBY_DIR} ==="
    else
        echo "=== ruby: building Ruby ${RUBY_VERSION} (--enable-load-relative) ==="
        local src="/tmp/ruby-${RUBY_VERSION}"
        rm -rf "$src"
        cd /tmp
        curl -fsSLO "https://cache.ruby-lang.org/pub/ruby/${RUBY_VERSION%.*}/ruby-${RUBY_VERSION}.tar.gz"
        tar xzf "ruby-${RUBY_VERSION}.tar.gz"
        cd "$src"
        ./configure \
            --prefix="${BUNDLED_RUBY_DIR}" \
            --enable-load-relative \
            --disable-install-doc \
            --disable-install-rdoc
        make -j"$(nproc)"
        make install
    fi

    # rake + test-unit drive the Spider test suite. rugged is deliberately
    # absent: it's referenced only by gitsave.rb, which falls back cleanly on
    # LoadError, and the AppImage strips it anyway (see linux-appimage.sh).
    setup_paths
    gem install --no-document test-unit
    # Ruby ships rake as a bundled gem, so the source build has already put a
    # rake binary in the bundled tree — `gem install rake` then refuses with
    # "rake conflicts with .../bin/rake" and takes the whole job down. Only
    # install it where it is genuinely absent.
    command -v rake >/dev/null 2>&1 || gem install --no-document rake
}

phase_rust() {
    # supersonic's MIDI/gamepad/OSC subsystems are cargo-built staticlibs, so
    # its CMake configure needs cargo on PATH. Debian's rustc is too old to
    # track supersonic's toolchain, so use rustup.
    #
    # i686-unknown-linux-gnu is a tier-1 Rust target, but rustup-init has to be
    # told: it infers the host triple from the KERNEL, which is x86_64 even in
    # a 32-bit userland, and installs a 64-bit toolchain whose binaries cannot
    # run here. That install reports success, and the failure only lands later
    # as the shim saying "command failed: 'cargo'".
    #
    # Note this sets the CPU floor at SSE2 (Pentium 4 / Athlon 64 and later):
    # rustc enables SSE2 by default for i686, above Debian's own i686 baseline.
    local triple=i686-unknown-linux-gnu
    setup_paths   # pick up a toolchain a previous phase already installed

    if cargo --version >/dev/null 2>&1; then
        echo "=== rust: reusing existing toolchain ==="
    else
        echo "=== rust: installing toolchain ==="
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
            | sh -s -- -y --profile minimal \
                --default-host "$triple" --default-toolchain stable
        setup_paths
    fi

    # State the host and toolchain rather than trusting whatever the installer
    # settled on: an image carrying a pre-existing ~/.rustup keeps the default
    # from its own settings.toml and ignores the inferred one.
    rustup set default-host "$triple"
    rustup toolchain install "stable-$triple" --profile minimal
    rustup default "stable-$triple"

    # Fail here, with the toolchain in view, rather than deep into a build.
    cargo --version
    rustc --version
}

phase_build() {
    echo "=== build: prebuild + configure + compile ==="
    setup_paths

    # Skip the native Ruby C extension compile: the AppImage deliberately ships
    # none (see linux-appimage.sh), because user Rubies vary in ABI.
    export SP_SKIP_RUBY_EXTS=1

    # Leave QT_INSTALL_LOCATION unset so find_package(Qt6) resolves Debian's
    # system Qt from the default CMake prefix path.
    unset QT_INSTALL_LOCATION

    "${SCRIPT_DIR}/linux-prebuild.sh"
    "${SCRIPT_DIR}/linux-config.sh" --config Release
    "${SCRIPT_DIR}/linux-build-gui.sh"
}

phase_test() {
    echo "=== test: Spider, API and GUI suites ==="
    setup_paths

    echo "--- Spider (Ruby) tests"
    ( cd "${SCRIPT_DIR}/server/ruby" && rake test )

    echo "--- API tests"
    # The API tests need a JACK server. The dummy driver gives them one with no
    # audio hardware, and -r skips the realtime scheduling a container can't grant.
    jackd -rd dummy &
    local jackd_pid=$!
    ( cd "${SCRIPT_DIR}/build/api-tests" && SONIC_PI_ENV=test ctest --verbose )
    kill "$jackd_pid" 2>/dev/null || true

    echo "--- GUI tests"
    # Catch2 + Qt Test, headless via Qt's offscreen platform plugin.
    ( cd "${SCRIPT_DIR}/build/gui-tests" && QT_QPA_PLATFORM=offscreen ctest --verbose )
}

phase_package() {
    echo "=== package: building AppImage ==="
    setup_paths

    # linuxdeploy-plugin-qt locates Qt by running qmake; Debian's is suffixed
    # and lives in the multiarch qt6 dir rather than on PATH.
    local qmake="/usr/lib/i386-linux-gnu/qt6/bin/qmake6"
    if [ ! -x "$qmake" ]; then
        # Fall back to whatever the distro provides rather than failing on a
        # path that may move between Debian suites.
        qmake="$(command -v qmake6 || command -v qmake || true)"
    fi
    if [ -z "$qmake" ]; then
        echo "ERROR: no qmake6 found — is qt6-base-dev-tools installed?" >&2
        exit 1
    fi
    export QMAKE="$qmake"

    # `uname -m` inside the container follows the process personality, which is
    # not guaranteed to be set to 32-bit by every container runtime. Pin it so
    # the wrong (64-bit) tools are never fetched.
    export SP_APPIMAGE_ARCH=i686

    "${SCRIPT_DIR}/linux-appimage.sh"
}

main() {
    local phase="${1:-all}"
    check_arch

    case "$phase" in
        deps)    phase_deps ;;
        ruby)    phase_ruby ;;
        rust)    phase_rust ;;
        build)   phase_build ;;
        test)    phase_test ;;
        package) phase_package ;;
        all)
            cat <<EOF
=== Sonic Pi 32-bit x86 (i686) ===
  ruby:   ${RUBY_VERSION} -> ${BUNDLED_RUBY_DIR}
  repo:   ${REPO_DIR}

EOF
            phase_deps
            phase_ruby
            phase_rust
            phase_build
            phase_test
            phase_package
            ;;
        -h|--help)
            sed -n '12,40p' "${BASH_SOURCE[0]}"
            ;;
        *)
            echo "Unknown phase: $phase" >&2
            echo "Valid: all deps ruby rust build test package" >&2
            exit 1
            ;;
    esac
}

main "$@"
