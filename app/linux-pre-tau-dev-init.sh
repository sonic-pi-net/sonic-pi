#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"/server/beam/tau

MIX_ENV=dev mix setup.dev

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
