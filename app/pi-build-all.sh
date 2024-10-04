#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

"${SCRIPT_DIR}"/pi-prebuild.sh -n
"${SCRIPT_DIR}"/pi-config.sh -n
"${SCRIPT_DIR}"/pi-build-gui.sh
"${SCRIPT_DIR}"/pi-post-tau-prod-release.sh

