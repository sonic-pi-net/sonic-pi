#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Use generic linux pre vcpkg script and add environmental variable
VCPKG_FORCE_SYSTEM_BINARIES=1 "${SCRIPT_DIR}"/linux-pre-vcpkg.sh "$@"
