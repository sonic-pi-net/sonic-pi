#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd ${SCRIPT_DIR}

echo "Cleaning out build dir...."
rm -rf build

echo "Cleaning out external/build dir...."
rm -rf external/build

echo "Cleaning completed"
