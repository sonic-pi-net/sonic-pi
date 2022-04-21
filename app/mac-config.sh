#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
config=""

if [ "$1" = "--config" ]; then
  case $2 in
    Release|Debug|RelWithDebInfo|MinSizeRel)
      config="$2"
      ;;
    *)
      echo "`basename $0`: Error: invalid configuration: '${2}'" 1>&2
      echo "Valid configurations: Release, Debug, RelWithDebInfo, MinSizeRel" 1>&2
      exit 1
      ;;
  esac
elif [ ! -z $1 ]; then
  echo "`basename $0`: Error: invalid parameter: '${1}'" 1>&2
  echo "Usage: `basename $0` [--config <Release|Debug|RelWithDebInfo|MinSizeRel>]" 1>&2
  exit 1
fi

echo "Creating build directory..."
mkdir -p "${SCRIPT_DIR}/build"

echo "Generating makefiles..."
cd "${SCRIPT_DIR}/build"
cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=\"$config\" ..

cd "${SCRIPT_DIR}"
