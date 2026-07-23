#!/bin/bash
set -euo pipefail
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

# Build, test and package 32-bit x86 Sonic Pi inside a 32-bit Debian
# container. This is what CI runs, so it's also the way to reproduce a CI
# failure locally.
#
# No emulation is involved: x86_64 CPUs execute 32-bit code natively, so this
# runs at full speed on any amd64 host with Docker.
#
# Usage:
#   ./linux-appimage-i386.sh              build + test + package
#   ./linux-appimage-i386.sh test         run a single phase (see the
#                                         container script for the full list)
#   SP_I386_IMAGE=... ./linux-appimage-i386.sh
#
# Running a single phase starts a fresh container, so phases that depend on
# earlier ones (anything past `deps`) only work if the workspace already
# carries their output. CI keeps one container alive across phases instead.
#
# Output lands in app/build/Sonic-Pi-for-Linux-x86-v<version>.AppImage.

REPO_DIR="$( cd "${SCRIPT_DIR}/.." && pwd )"
IMAGE="${SP_I386_IMAGE:-i386/debian:bookworm}"
PHASE="${1:-all}"

if ! command -v docker >/dev/null 2>&1; then
    echo "ERROR: docker not found — needed to run the 32-bit build environment." >&2
    exit 1
fi

echo "=== Sonic Pi 32-bit x86 (${PHASE}) via ${IMAGE} ==="

# --platform is explicit because the tag also resolves on arm64 hosts, where
# it would silently pull an emulated image and take hours.
docker run --rm \
    --platform linux/386 \
    -v "${REPO_DIR}:/src" \
    -w /src/app \
    -e RUBY_VERSION \
    -e SP_MAX_GLIBC \
    "${IMAGE}" \
    /src/app/linux-appimage-i386-container.sh "${PHASE}"
