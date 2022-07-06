#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

# Use generic linux tau dev init script
"${SCRIPT_DIR}"/linux-pre-tau-dev-init.sh "$@"

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
