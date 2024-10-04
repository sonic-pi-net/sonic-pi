#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

VCPKG_FORCE_SYSTEM_BINARIES=1 CC="$(type -p clang)" CXX="$(type -p clang++)" "${SCRIPT_DIR}"/linux-prebuild.sh "$@"

