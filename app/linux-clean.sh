#!/bin/bash

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


echo "Cleaning out build dir...."
rm -rf build

echo "Cleaning out any CMakeCache.txt files...."
rm -rf "${SCRIPT_DIR}"/**/CMakeCache.txt

echo "Cleaning out BEAM distribution..."
rm -rf server/beam/tau/_build
rm -f server/beam/tau/priv/*.{so,dylib,dll}
if [ "$MIX_ENV" == dev ]; then
    rm -rf server/beam/tau/priv/static/assets
    rm -rf server/beam/tau/priv/static/cache_manifest.json
    find . -path './server/beam/tau/priv/static/*' -name '*.gz' -delete
    find . -path './server/beam/tau/priv/static/*' -name '*-????????????????????????????????.*' -delete
fi

echo "Cleaning completed"



