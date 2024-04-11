#!/bin/bash -e

if test -z "$COVERITY_TOKEN"
then
    echo "Need to set a coverity token"
    exit 1
fi

case $(uname -m) in
    i?86)
        BITS=32;;
    amd64|x86_64)
        BITS=64;;
    *)
        echo "Unsupported arch '$(uname -m)'"
        exit 1;;
esac

SCAN_TOOL=https://scan.coverity.com/download/cxx/linux${BITS}
SOURCE_DIR=$(realpath "$(dirname "${BASH_SOURCE[0]}")"/..)
BUILD_DIR=${SOURCE_DIR}/coverity-build
TOOL_DIR=${BUILD_DIR}/coverity-tools

# Install coverity tools
if ! test -d "$TOOL_DIR"
then
    mkdir -p "$TOOL_DIR"
    curl --silent --show-error --location --data "project=libgit2&token=$COVERITY_TOKEN" "$SCAN_TOOL" |
        tar -xzC "$TOOL_DIR"
    ln -s "$(find "$TOOL_DIR" -type d -name 'cov-analysis*')" "$TOOL_DIR"/cov-analysis
fi

cp "${SOURCE_DIR}/script/user_nodefs.h" "$TOOL_DIR"/cov-analysis/config/

# Build libgit2 with Coverity
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"
cmake "$SOURCE_DIR"
COVERITY_UNSUPPORTED=1 \
    "$TOOL_DIR/cov-analysis/bin/cov-build" --dir cov-int \
    cmake --build .

# Upload results
tar -czf libgit2.tgz cov-int
REVISION=$(cd ${SOURCE_DIR} && git rev-parse --short HEAD)
HTML="$(curl \
    --silent --show-error \
    --write-out "\n%{http_code}" \
    --form token="$COVERITY_TOKEN" \
    --form email=libgit2@gmail.com \
    --form file=@libgit2.tgz \
    --form version="$REVISION" \
    --form description="libgit2 build" \
    https://scan.coverity.com/builds?project=libgit2)"

# Status code is the last line
STATUS_CODE="$(echo "$HTML" | tail -n1)"
if test "${STATUS_CODE}" != 200 && test "${STATUS_CODE}" != 201
then
    echo "Received error code ${STATUS_CODE} from Coverity"
    exit 1
fi
