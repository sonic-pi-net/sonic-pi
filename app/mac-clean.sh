#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

cd "${SCRIPT_DIR}"

echo "Cleaning out vcpkg...."
rm -rf vcpkg

# Use generic linux clean script for everything else
"${SCRIPT_DIR}"/linux-clean.sh "$@"
