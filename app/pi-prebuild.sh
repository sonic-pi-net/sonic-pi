#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Use generic linux prebuild script
VCPKG_FORCE_SYSTEM_BINARIES=1 "${SCRIPT_DIR}/linux-prebuild.sh"
