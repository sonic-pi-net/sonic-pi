#!/bin/bash
set -e # Quit script on error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

cleanup_function() {
    # Restore working directory as it was prior to this script running on exit
    cd "${WORKING_DIR}"
}
trap cleanup_function EXIT

args=("$@")

# extract options and their arguments into variables.
while [ -n "$1" ]; do
    case "$1" in
        -c|--config)
            shift 2
            ;;
        -s|--system-libs|-o|--offline-build)
            shift
            ;;
        --) shift ; break ;;
        *) echo "Invalid argument: $1" ; exit 1 ;;
    esac
done

cd "${SCRIPT_DIR}"

# Build vcpkg
if [ ! -d "vcpkg" ]; then
    echo "Cloning vcpkg"
    git clone --depth 1 --branch 2026.04.27 https://github.com/microsoft/vcpkg.git vcpkg
fi

export VCPKG_ROOT="$SCRIPT_DIR/vcpkg"

if [ ! -f "vcpkg/vcpkg" ]; then
    echo "Building vcpkg"
    cd vcpkg
    ./bootstrap-vcpkg.sh -disableMetrics
    cd "${SCRIPT_DIR}"
fi

cd vcpkg

if [[ $(uname -m) == 'arm64' ]] || [ "$SONIC_PI_BUILD_TARGET" == 'arm64' ]
then
  TRIPLET=arm64-osx
else
  TRIPLET=x64-osx
fi

./vcpkg install libsndfile[core,external-libs] --triplet ${TRIPLET} --recurse

