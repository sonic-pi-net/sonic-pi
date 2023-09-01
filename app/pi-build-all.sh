#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

"${SCRIPT_DIR}"/pi-prebuild.sh -n
"${SCRIPT_DIR}"/pi-config.sh -n
"${SCRIPT_DIR}"/pi-build-gui.sh
"${SCRIPT_DIR}"/pi-post-tau-prod-release.sh

cd "${WORKING_DIR}"
