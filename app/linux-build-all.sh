#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

"${SCRIPT_DIR}"/linux-prebuild.sh "$@"
"${SCRIPT_DIR}"/linux-config.sh "$@"
"${SCRIPT_DIR}"/linux-build-gui.sh "$@"
"${SCRIPT_DIR}"/linux-post-tau-prod-release.sh "$@"

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
