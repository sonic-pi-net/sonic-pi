#!/bin/bash
set -e

# Full pipeline: build Sonic Pi, stage the linux_dist tree, package as AppImage.
# Pass through args to linux-build-all.sh (e.g. --config Debug).

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"
trap 'cd "${WORKING_DIR}"' EXIT

"${SCRIPT_DIR}/linux-build-all.sh" "$@"
"${SCRIPT_DIR}/linux-release.sh"
"${SCRIPT_DIR}/linux-appimage.sh"
