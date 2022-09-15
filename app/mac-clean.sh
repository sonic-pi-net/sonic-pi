#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"

echo "Cleaning out native dir...."
rm -rf "${SCRIPT_DIR}"/server/native/supercollider
rm -rf "${SCRIPT_DIR}"/server/native/erlang
rm -rf "${SCRIPT_DIR}"/server/native/scsynth

# Use generic linux clean script for everything else
"${SCRIPT_DIR}"/linux-clean.sh "$@"

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
