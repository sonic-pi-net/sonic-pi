#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"


cp "${SCRIPT_DIR}"/external/build/aubio-prefix/src/aubio-build/aubio_onset "${SCRIPT_DIR}"/server/native/

mkdir -p "${SCRIPT_DIR}"/server/beam/tau/priv/
cp "${SCRIPT_DIR}"/external/build/sp_midi-prefix/src/sp_midi-build/*.so "${SCRIPT_DIR}"/server/beam/tau/priv/
cp "${SCRIPT_DIR}"/external/build/sp_link-prefix/src/sp_link-build/*.so "${SCRIPT_DIR}"/server/beam/tau/priv/


# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
