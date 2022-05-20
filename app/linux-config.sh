#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

args=("$@")
config=""
no_imgui=false
system_libs=false

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
        -n|--no-imgui)
            no_imgui=true
            shift
            ;;
        -s|--system-libs|-o|--offline-build)
            system_libs=true
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

cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE="$config" -DBUILD_IMGUI_INTERFACE="$(option "!$no_imgui")" -DUSE_SYSTEM_LIBS="$(option "$system_libs")" ..

cd "${SCRIPT_DIR}"


# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
