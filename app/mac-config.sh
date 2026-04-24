#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"
VCPKG_TOOLCHAIN="${SCRIPT_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

args=("$@")
config="Release"

# extract options and their arguments into variables.
while [ -n "$1" ]; do
    case "$1" in
        -c|--config)
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
            shift 2
            ;;
        -s|--system-libs|-o|--offline-build)
            shift
            ;;
        --) shift ; break ;;
        *) echo "Invalid argument: $1" ; exit 1 ;;
    esac
done

echo "Creating build directory..."
mkdir -p "${SCRIPT_DIR}/build"

echo "Generating makefiles..."
cd "${SCRIPT_DIR}/build"

option() {
  if [ "$1" == "true" ] || [ "$1" == "!" ] || [ "$1" == "!false" ]; then
    echo ON
  else
    echo OFF
  fi
}

if [[ $(uname -m) == 'arm64' ]] || [ "$SONIC_PI_BUILD_TARGET" == 'arm64' ]
then
  cmake -G "Unix Makefiles" -DCMAKE_OSX_ARCHITECTURES="arm64" -DCMAKE_TOOLCHAIN_FILE="$VCPKG_TOOLCHAIN" -DVCPKG_TARGET_TRIPLET=arm64-osx -DCMAKE_BUILD_TYPE="$config"  ..
else
  cmake -G "Unix Makefiles" -DCMAKE_OSX_ARCHITECTURES="x86_64" -DCMAKE_TOOLCHAIN_FILE="$VCPKG_TOOLCHAIN" -DVCPKG_TARGET_TRIPLET=x64-osx -DCMAKE_BUILD_TYPE="$config"  ..
fi