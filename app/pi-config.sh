#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"
config=""
no_imgui=false

# read the options

TEMP=`getopt -o c:n --long config:,no-imgui -- "$@"`
eval set -- "$TEMP"

# extract options and their arguments into variables.
while true ; do
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
        --) shift ; break ;;
        *) echo "Internal error!" ; exit 1 ;;
    esac
done

echo "Creating build directory..."
mkdir -p "${SCRIPT_DIR}/build"

echo "Generating makefiles..."
cd "${SCRIPT_DIR}/build"

if [ "$no_imgui" == true ]
then
    cmake -G "Unix Makefiles" -DBUILD_IMGUI_INTERFACE=OFF -DRASPBERRY_PI=1 -DCMAKE_BUILD_TYPE="$config" ..
else
    cmake -G "Unix Makefiles" -DBUILD_IMGUI_INTERFACE=ON -DRASPBERRY_PI=1 -DCMAKE_BUILD_TYPE="$config" ..
fi

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
