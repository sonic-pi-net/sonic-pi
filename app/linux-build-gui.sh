#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"/build
cmake --build . --config Release

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
