#!/usr/bin/env bash
#
# Environment variables:
#
# SOURCE_DIR: Set to the directory of the libgit2 source (optional)
#     If not set, it will be derived relative to this script.

set -e

SOURCE_DIR=${SOURCE_DIR:-$( cd "$( dirname "${BASH_SOURCE[0]}" )" && dirname $( pwd ) )}
BUILD_DIR=$(pwd)
BUILD_PATH=${BUILD_PATH:=$PATH}
CMAKE=$(which cmake)
CMAKE_GENERATOR=${CMAKE_GENERATOR:-Unix Makefiles}

indent() { sed "s/^/    /"; }

echo "Source directory: ${SOURCE_DIR}"
echo "Build directory:  ${BUILD_DIR}"
echo ""

if [ "$(uname -s)" = "Darwin" ]; then
	echo "macOS version:"
	sw_vers | indent
fi

if [ -f "/etc/debian_version" ]; then
	echo "Debian version:"
	(source /etc/lsb-release && echo "${DISTRIB_DESCRIPTION}") | indent
fi

echo "Kernel version:"
uname -a 2>&1 | indent

echo "CMake version:"
env PATH="$BUILD_PATH" "$CMAKE" --version 2>&1 | indent

if test -n "$CC"; then
	echo "Compiler version:"
	"$CC" --version 2>&1 | indent
fi
echo ""

echo "##############################################################################"
echo "## Configuring build environment"
echo "##############################################################################"

echo cmake ${SOURCE_DIR} -DENABLE_WERROR=ON -DBUILD_EXAMPLES=ON -DBUILD_FUZZERS=ON -DUSE_STANDALONE_FUZZERS=ON -G \"${CMAKE_GENERATOR}\" ${CMAKE_OPTIONS}
env PATH="$BUILD_PATH" "$CMAKE" ${SOURCE_DIR} -DENABLE_WERROR=ON -DBUILD_EXAMPLES=ON -DBUILD_FUZZERS=ON -DUSE_STANDALONE_FUZZERS=ON -G "${CMAKE_GENERATOR}" ${CMAKE_OPTIONS}

echo ""
echo "##############################################################################"
echo "## Building libgit2"
echo "##############################################################################"

env PATH="$BUILD_PATH" "$CMAKE" --build .
