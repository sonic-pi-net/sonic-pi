#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"
cd "${SCRIPT_DIR}"

echo "Cleaning out build dir...."
rm -rf build

echo "Cleaning out external/build dir...."
rm -rf external/build

echo "Cleaning out BEAM distribution..."
rm -rf server/beam/tau/_build
rm -rf server/beam/tau/priv/static/assets
rm -rf server/beam/tau/priv/*.{so,dylib,dll}
rm -rf server/beam/tau/priv/static/cache_manifest.json
find . -path './server/beam/tau/priv/static/*' -name '*.gz' -delete
find . -path './server/beam/tau/priv/static/*' -name '*-????????????????????????????????.*' -delete

echo "Cleaning completed"

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
