#!/bin/bash
set -e # Quit script on error

args=("$@")
no_imgui=false

# extract options and their arguments into variables.
while [ -n "$1" ]; do
    case "$1" in
        -c|--config)
            shift 2
            ;;
        -n|--no-imgui)
            no_imgui=true
            shift
            ;;
        -s|--system-libs|-o|--offline-build)
            shift
            ;;
        --) shift ; break ;;
        *) echo "Invalid argument: $1" ; exit 1 ;;
    esac
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cd "${SCRIPT_DIR}"

# Build vcpkg
if [ ! -d "vcpkg" ]; then
    echo "Cloning vcpkg"
    git clone --depth 1 --branch "${VCPKG_BRANCH:-2022.06.16.1}" https://github.com/microsoft/vcpkg.git vcpkg
fi

if [ ! -f "vcpkg/vcpkg" ]; then
    echo "Building vcpkg"
    cd vcpkg
    ./bootstrap-vcpkg.sh -disableMetrics
    cd "${SCRIPT_DIR}"
fi

cd vcpkg

if [ "$no_imgui" == true ]; then
    ./vcpkg install aubio[core] kissfft crossguid platform-folders reproc catch2 --recurse
else
    ./vcpkg install aubio[core] kissfft fmt crossguid sdl2[x11] gl3w reproc gsl-lite concurrentqueue platform-folders catch2 --recurse
fi


# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
