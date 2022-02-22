#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKING_DIR="$(pwd)"

while getopts ":n" opt; do
  case $opt in
    n)
      no_imgui=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

if [ "$no_imgui" == true ]
then
    VCPKG_FORCE_SYSTEM_BINARIES=1 "${SCRIPT_DIR}"/linux-prebuild.sh -n
else
    VCPKG_FORCE_SYSTEM_BINARIES=1 "${SCRIPT_DIR}"/linux-prebuild.sh
fi

# Restore working directory as it was prior to this script running...
cd "${WORKING_DIR}"
