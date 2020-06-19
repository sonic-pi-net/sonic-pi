#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Building server external dependencies..."
mkdir -p "${SCRIPT_DIR}/build"
cd "${SCRIPT_DIR}/build"

echo "Running cmake with: ERLANG_INCLUDE_PATH=\"${SCRIPT_DIR}/../../../../prebuilt/macos/headers/erlang/\""
cmake -G "Unix Makefiles" -D ERLANG_INCLUDE_PATH="${SCRIPT_DIR}/../../../../prebuilt/macos/headers/erlang/" ..

echo "Building sp_midi..."
cmake  --build . --target sp_midi


if [ "$1" = "--build-aubio" ]; then
  echo "Building aubio..."
  cmake --build . --target aubio
fi

cd "${SCRIPT_DIR}"
