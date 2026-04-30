#!/bin/bash
# Build OpenSSL from source with our chosen MACOSX_DEPLOYMENT_TARGET.
#
# We need to do this ourselves rather than reuse Homebrew's openssl@3 bottle
# because:
#
#   1. Pre-built bottles are stamped with the host SDK's macOS version in
#      LC_BUILD_VERSION minos. dyld then refuses to load them on older macOS
#      regardless of whether the symbols would actually work.
#
#   2. `brew install --build-from-source openssl@3` runs in a `superenv`
#      that scrubs MACOSX_DEPLOYMENT_TARGET. The locally rebuilt bottle ends
#      up at the host's minos too — i.e. the same problem.
#
# Building OpenSSL ourselves gives us a libssl/libcrypto whose
# LC_BUILD_VERSION matches the SDK availability annotations the compiler
# saw — no metadata-vs-reality skew, no vtool patches.
#
# Output:
#   app/external/openssl-build/install/lib/libssl.3.dylib
#   app/external/openssl-build/install/lib/libcrypto.3.dylib
#   app/external/openssl-build/install/include/openssl/...
#
# `mac-release-02-bundle-dylibs.sh` prefers these over whatever a binary's
# original install_name pointed at when copying into Contents/Frameworks/.
#
# This script is idempotent: it skips rebuild when libssl is already
# present at the requested OpenSSL version AND has the requested minos.
# Edit OPENSSL_VERSION below or set $OPENSSL_VERSION to bump.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
OPENSSL_VERSION="${OPENSSL_VERSION:-3.5.2}"
TARGET_MINOS="${MACOSX_DEPLOYMENT_TARGET:-14.0}"

BUILD_ROOT="${SCRIPT_DIR}/external/openssl-build"
PREFIX="${BUILD_ROOT}/install"
SRC_DIR="${BUILD_ROOT}/src/openssl-${OPENSSL_VERSION}"
TARBALL="${BUILD_ROOT}/cache/openssl-${OPENSSL_VERSION}.tar.gz"

# ---------------------------------------------------------------------------
# Skip if already built at the right version + minos
# ---------------------------------------------------------------------------
LIBSSL="${PREFIX}/lib/libssl.3.dylib"
if [ -f "${LIBSSL}" ]; then
    actual_minos="$(vtool -show "${LIBSSL}" 2>/dev/null \
        | awk '/^[[:space:]]*minos[[:space:]]/{print $2; exit}')"
    actual_version="$("${PREFIX}/bin/openssl" version 2>/dev/null \
        | awk '{print $2; exit}')" || actual_version=""

    # Match either "14" or "14.0" against requested target.
    target_short="${TARGET_MINOS%%.*}"
    minos_short="${actual_minos%%.*}"

    if [ "${actual_version}" = "${OPENSSL_VERSION}" ] \
       && [ "${minos_short}" = "${target_short}" ]; then
        echo "openssl ${OPENSSL_VERSION} already built at minos ${actual_minos} in ${PREFIX}"
        exit 0
    fi
    echo "openssl in ${PREFIX} is ${actual_version:-?} minos ${actual_minos:-?};" \
         "rebuilding at ${OPENSSL_VERSION} minos ${TARGET_MINOS}"
fi

# ---------------------------------------------------------------------------
# Fetch source (cached)
# ---------------------------------------------------------------------------
mkdir -p "$(dirname "${TARBALL}")"
if [ ! -f "${TARBALL}" ]; then
    echo "Fetching openssl-${OPENSSL_VERSION}.tar.gz..."
    curl -fL --retry 3 --retry-delay 2 \
        -o "${TARBALL}.tmp" \
        "https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz"
    mv "${TARBALL}.tmp" "${TARBALL}"
fi

# ---------------------------------------------------------------------------
# Unpack into a fresh src dir
# ---------------------------------------------------------------------------
mkdir -p "$(dirname "${SRC_DIR}")"
rm -rf "${SRC_DIR}"
tar xzf "${TARBALL}" -C "$(dirname "${SRC_DIR}")"

# ---------------------------------------------------------------------------
# Configure + build + install
# ---------------------------------------------------------------------------
# `darwin64-arm64-cc` is OpenSSL's canonical Apple Silicon target.
# `--openssldir` controls the runtime config search path; we keep it inside
# our private prefix so we don't accidentally pick up system /etc/ssl/ certs
# at runtime — Ruby supplies its own cert store anyway.
cd "${SRC_DIR}"
echo "Configuring openssl-${OPENSSL_VERSION} for minos ${TARGET_MINOS}..."
./Configure darwin64-arm64-cc \
    --prefix="${PREFIX}" \
    --openssldir="${PREFIX}/etc/ssl" \
    no-tests no-docs >/dev/null

echo "Building openssl-${OPENSSL_VERSION}..."
MACOSX_DEPLOYMENT_TARGET="${TARGET_MINOS}" make -j"$(sysctl -n hw.ncpu)"

echo "Installing to ${PREFIX}..."
rm -rf "${PREFIX}"
mkdir -p "${PREFIX}"
make install_sw >/dev/null

# ---------------------------------------------------------------------------
# Confirm
# ---------------------------------------------------------------------------
echo
echo "openssl-${OPENSSL_VERSION} built. LC_BUILD_VERSION:"
vtool -show "${LIBSSL}" 2>&1 | grep -E "minos|sdk|platform" | sed 's/^/    /'

# Drop the source tree to save disk; keep the cached tarball for next time.
rm -rf "${SRC_DIR}"
