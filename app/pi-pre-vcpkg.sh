#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Use generic linux pre vcpkg script
"${SCRIPT_DIR}"/linux-pre-vcpkg.sh "$@"
