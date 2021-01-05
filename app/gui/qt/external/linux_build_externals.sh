#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo "Building server external dependencies..."
mkdir -p "${SCRIPT_DIR}/build"
cd "${SCRIPT_DIR}/build"

ERLANG_INCLUDE_PATH=`erl -noinput -eval 'io:format("~s~n", [filename:join([lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]), "include"])]), init:stop(0).'`

cmake -DERLANG_INCLUDE_PATH=${ERLANG_INCLUDE_PATH} -G "Unix Makefiles" ..

echo "Building sp_midi..."
cmake --build . --target sp_midi

if [ "$1" = "--build-aubio" ]; then
  echo "Building aubio..."
  cmake --build . --target aubio
fi

cd "${SCRIPT_DIR}"
