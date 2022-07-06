#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

# Use generic linux pre translations script
"${SCRIPT_DIR}"/linux-pre-translations.sh "$@"

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
