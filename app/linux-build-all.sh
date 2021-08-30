#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${SCRIPT_DIR}

"${SCRIPT_DIR}/linux-prebuild.sh"
"${SCRIPT_DIR}/linux-config.sh"

cd build
cmake --build . --config Release

cd ${SCRIPT_DIR}
