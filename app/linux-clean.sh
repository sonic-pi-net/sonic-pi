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

echo "Cleaning out build dir...."
rm -rf build

echo "Cleaning out any CMakeCache.txt files...."
rm -rf "${SCRIPT_DIR}"/**/CMakeCache.txt

# Deployed build outputs in server/native (engine under its per-platform
# name, aubio_onset, piano wavetable) plus legacy leftovers - the build
# redeploys these, and leaving them behind is how stale binaries end up
# shipping. server/native/ruby and .gitkeep are NOT build outputs (ruby is
# the separately deployed bundle) and must survive the clean.
echo "Cleaning deployed build outputs out of server/native...."
rm -rf "${SCRIPT_DIR}"/server/native/"Sonic Pi - SuperSonic" \
       "${SCRIPT_DIR}"/server/native/"Sonic Pi - Plugins" \
       "${SCRIPT_DIR}"/server/native/"Sonic Pi - Plugins.app" \
       "${SCRIPT_DIR}"/server/native/sonic-pi-supersonic \
       "${SCRIPT_DIR}"/server/native/supersonic \
       "${SCRIPT_DIR}"/server/native/aubio_onset \
       "${SCRIPT_DIR}"/server/native/piano_wavetable.dat \
       "${SCRIPT_DIR}"/server/native/scsynth \
       "${SCRIPT_DIR}"/server/native/plugins \
       "${SCRIPT_DIR}"/server/native/sox \
       "${SCRIPT_DIR}"/server/native/*.bak \
       "${SCRIPT_DIR}"/server/native/*.orig-backup \
       "${SCRIPT_DIR}"/server/native/*.log

echo "Cleaning completed"



