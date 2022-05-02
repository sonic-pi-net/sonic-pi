#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"

cp "${SCRIPT_DIR}"/external/build/aubio-prefix/src/aubio-build/aubio_onset "${SCRIPT_DIR}"/server/native/

mkdir -p "${SCRIPT_DIR}"/server/beam/tau/priv/
for f in "${SCRIPT_DIR}"/external/build/sp_midi-prefix/src/sp_midi-build/*.dylib; do
    cp "$f" "${SCRIPT_DIR}"/server/beam/tau/priv/$(basename "$f" .dylib).so
done

for f in "${SCRIPT_DIR}"/external/build/sp_link-prefix/src/sp_link-build/*.dylib; do
    cp "$f" "${SCRIPT_DIR}"/server/beam/tau/priv/$(basename "$f" .dylib).so
done


# Copy prebuilt native files to server
echo "Copying prebuilt binaries to the server..."
mkdir -p "${SCRIPT_DIR}"/server/native/

cp -R "${SCRIPT_DIR}"/../prebuilt/macos/x64/* "${SCRIPT_DIR}"/server/native/

cd "${SCRIPT_DIR}"/server/native/
ln -s supercollider/scsynth scsynth
mv supercollider/extra-plugins/* supercollider/plugins/
rm -rf supercollider/extra-plugins

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
