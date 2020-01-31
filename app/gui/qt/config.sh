#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Creating build directory..."
mkdir -p "${SCRIPT_DIR}/build"

echo "Generating makefiles..."
cd "${SCRIPT_DIR}/build"
cmake -G "Unix Makefiles" ..

cd "${SCRIPT_DIR}"
