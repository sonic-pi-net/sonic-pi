#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

# Pin the macOS deployment target for everything spawned by this build.
# CMake, autoconf, mix release, and kerl all honour this env var, so every
# Mach-O the build produces ends up with a consistent LC_BUILD_VERSION
# minos. This must match LSMinimumSystemVersion baked into the .app's
# Info.plist by mac-release-03-info-plist.sh — mac-release-08-compat-audit.sh
# fails the release if it doesn't.
export MACOSX_DEPLOYMENT_TARGET="${MACOSX_DEPLOYMENT_TARGET:-14.0}"

"${SCRIPT_DIR}"/mac-prebuild.sh "$@"
"${SCRIPT_DIR}"/mac-config.sh "$@"
"${SCRIPT_DIR}"/mac-build-gui.sh "$@"
"${SCRIPT_DIR}"/mac-post-tau-prod-release.sh "$@"
