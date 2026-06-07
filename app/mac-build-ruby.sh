#!/bin/bash
# Build the Ruby we bundle in app/server/native/ruby from source, with our
# chosen MACOSX_DEPLOYMENT_TARGET, with zero Homebrew dependencies.
#
# Why build it ourselves rather than grab a prebuilt Ruby:
#
#   1. LC_BUILD_VERSION minos. A Ruby compiled on the host without a pinned
#      deployment target inherits the host SDK's macOS version, which dyld
#      then refuses to load on older macOS. Honouring
#      MACOSX_DEPLOYMENT_TARGET is the only way to get a binary whose minos
#      matches the rest of the bundle (see mac-build-openssl.sh).
#
#   2. No Homebrew. A stock build links psych against /opt/homebrew/.../
#      libyaml and openssl against /opt/homebrew/opt/openssl@3 — paths that
#      don't exist on a user's machine. We instead:
#        - link the openssl extension against our own relocatable libssl
#          (--with-openssl-dir, built by mac-build-openssl.sh), and
#        - statically embed libyaml into psych (--with-libyaml-source-dir),
#          so psych has no external libyaml dependency at all.
#      A portability gate at the end fails the build if any /opt/homebrew
#      or /usr/local reference survives.
#
#   3. Relocatability. --enable-load-relative makes the binary find its
#      stdlib relative to its own path, so the tree works after we copy it
#      into app/server/native/ruby/ regardless of the build --prefix.
#
# Output:
#   app/server/native/ruby/{bin,include,lib,share}
#
# This is NOT part of mac-prebuild.sh — it's a deliberate step run when
# bumping the bundled Ruby version. It is idempotent: it skips when
# app/server/native/ruby already holds the requested version at the
# requested minos.

# Usage: mac-build-ruby.sh [RUBY_SRC_DIR]
#   RUBY_SRC_DIR  extracted Ruby source tree (must contain ./configure).
#                 Defaults to tmp/ruby-${RUBY_VERSION}. The version is taken
#                 from the directory name (ruby-X.Y.Z) unless $RUBY_VERSION
#                 is set.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
LIBYAML_VERSION="${LIBYAML_VERSION:-0.2.5}"
TARGET_MINOS="${MACOSX_DEPLOYMENT_TARGET:-14.0}"

# Source tree: positional arg, else $RUBY_SRC, else tmp/ruby-${RUBY_VERSION}.
RUBY_SRC="${1:-${RUBY_SRC:-${SCRIPT_DIR}/../tmp/ruby-${RUBY_VERSION:-4.0.5}}}"
# Version comes from the source dir name (ruby-X.Y.Z) unless set explicitly.
if [ -z "${RUBY_VERSION:-}" ]; then
    RUBY_VERSION="$(basename "${RUBY_SRC}")"
    RUBY_VERSION="${RUBY_VERSION#ruby-}"
fi

HOST_ARCH="$(uname -m)"

BUILD_ROOT="${SCRIPT_DIR}/external/ruby-build"
PREFIX="${BUILD_ROOT}/install/${RUBY_VERSION}"
OPENSSL_DIR="${SCRIPT_DIR}/external/openssl-build/install"
BUNDLE_DIR="${SCRIPT_DIR}/server/native/ruby"

LIBYAML_ROOT="${SCRIPT_DIR}/external/libyaml-build"
LIBYAML_SRC="${LIBYAML_ROOT}/src/yaml-${LIBYAML_VERSION}"
LIBYAML_TARBALL="${LIBYAML_ROOT}/cache/yaml-${LIBYAML_VERSION}.tar.gz"

# ---------------------------------------------------------------------------
# Skip if the bundle already holds the right version + minos + arch
# ---------------------------------------------------------------------------
BUNDLED_RUBY="${BUNDLE_DIR}/bin/ruby"
if [ -x "${BUNDLED_RUBY}" ]; then
    actual_version="$("${BUNDLED_RUBY}" -e 'print RUBY_VERSION' 2>/dev/null)" || actual_version=""
    actual_minos="$(vtool -show "${BUNDLED_RUBY}" 2>/dev/null \
        | awk '/^[[:space:]]*minos[[:space:]]/{print $2; exit}')"
    actual_arch="$(lipo -archs "${BUNDLED_RUBY}" 2>/dev/null)"

    target_short="${TARGET_MINOS%%.*}"
    minos_short="${actual_minos%%.*}"

    if [ "${actual_version}" = "${RUBY_VERSION}" ] \
       && [ "${minos_short}" = "${target_short}" ] \
       && [ "${actual_arch}" = "${HOST_ARCH}" ]; then
        echo "ruby ${RUBY_VERSION} already bundled at minos ${actual_minos} arch ${actual_arch} in ${BUNDLE_DIR}"
        exit 0
    fi
    echo "bundled ruby is ${actual_version:-?} minos ${actual_minos:-?} arch ${actual_arch:-?};" \
         "rebuilding at ${RUBY_VERSION} minos ${TARGET_MINOS} arch ${HOST_ARCH}"
fi

if [ ! -x "${RUBY_SRC}/configure" ]; then
    echo "No ./configure in ${RUBY_SRC} — point RUBY_SRC at an extracted Ruby ${RUBY_VERSION} source tree." >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Our relocatable OpenSSL must exist first so the openssl extension links
# against it rather than Homebrew. mac-build-openssl.sh is idempotent.
# ---------------------------------------------------------------------------
if [ ! -f "${OPENSSL_DIR}/lib/libssl.3.dylib" ]; then
    echo "Building OpenSSL first..."
    "${SCRIPT_DIR}"/mac-build-openssl.sh
fi

# ---------------------------------------------------------------------------
# Fetch libyaml source (cached). psych's extconf builds + statically links
# this when handed --with-libyaml-source-dir, so the result needs no
# external libyaml dylib.
# ---------------------------------------------------------------------------
mkdir -p "$(dirname "${LIBYAML_TARBALL}")"
if [ ! -f "${LIBYAML_TARBALL}" ]; then
    echo "Fetching yaml-${LIBYAML_VERSION}.tar.gz..."
    curl -fL --retry 3 --retry-delay 2 -o "${LIBYAML_TARBALL}.tmp" \
        "https://github.com/yaml/libyaml/releases/download/${LIBYAML_VERSION}/yaml-${LIBYAML_VERSION}.tar.gz"
    mv "${LIBYAML_TARBALL}.tmp" "${LIBYAML_TARBALL}"
fi
mkdir -p "$(dirname "${LIBYAML_SRC}")"
rm -rf "${LIBYAML_SRC}"
tar xzf "${LIBYAML_TARBALL}" -C "$(dirname "${LIBYAML_SRC}")"

# ---------------------------------------------------------------------------
# Configure + build + install into a private prefix
# ---------------------------------------------------------------------------
# --enable-load-relative      : stdlib found relative to the binary (relocatable)
# --enable-yjit               : YJIT compiler
# --disable-install-doc       : skip rdoc/ri generation
# --without-valgrind          : no valgrind hooks
# --without-gmp               : keep Bignum on the built-in impl, never Homebrew gmp
# --with-openssl-dir          : link our libssl/libcrypto, not Homebrew's
# --with-libyaml-source-dir   : statically embed libyaml into psych
BUILD_DIR="${BUILD_ROOT}/build/${RUBY_VERSION}"
rm -rf "${BUILD_DIR}" "${PREFIX}"
mkdir -p "${BUILD_DIR}"
cd "${BUILD_DIR}"

echo "Configuring ruby-${RUBY_VERSION} for ${HOST_ARCH} minos ${TARGET_MINOS}..."
MACOSX_DEPLOYMENT_TARGET="${TARGET_MINOS}" "${RUBY_SRC}/configure" \
    --prefix="${PREFIX}" \
    --disable-install-doc \
    --enable-load-relative \
    --without-valgrind \
    --without-gmp \
    --enable-yjit \
    --with-openssl-dir="${OPENSSL_DIR}" \
    --with-libyaml-source-dir="${LIBYAML_SRC}" >/dev/null

echo "Building ruby-${RUBY_VERSION}..."
MACOSX_DEPLOYMENT_TARGET="${TARGET_MINOS}" make -j"$(sysctl -n hw.ncpu)" >/dev/null

echo "Installing to ${PREFIX}..."
MACOSX_DEPLOYMENT_TARGET="${TARGET_MINOS}" make install >/dev/null

# ---------------------------------------------------------------------------
# Sync into the bundle (old bundle moved aside to .prev as a fallback)
# ---------------------------------------------------------------------------
echo "Syncing into ${BUNDLE_DIR}..."
if [ -d "${BUNDLE_DIR}" ]; then
    rm -rf "${BUNDLE_DIR}.prev"
    mv "${BUNDLE_DIR}" "${BUNDLE_DIR}.prev"
fi
mkdir -p "${BUNDLE_DIR}"
for d in bin include lib share; do
    [ -d "${PREFIX}/${d}" ] && cp -R "${PREFIX}/${d}" "${BUNDLE_DIR}/"
done

# ---------------------------------------------------------------------------
# Compile the native extensions (rugged, concurrent-ruby) with this Ruby so
# they carry the same minos.
# ---------------------------------------------------------------------------
echo "Compiling native ruby extensions..."
MACOSX_DEPLOYMENT_TARGET="${TARGET_MINOS}" \
    "${BUNDLE_DIR}/bin/ruby" "${SCRIPT_DIR}/server/ruby/bin/compile-extensions.rb"

# Drop runtime-unnecessary build artifacts, matching mac-release-01-prune.sh:
# the static archive (binary is fully static; nothing links it) and the
# debug-symbol bundles.
rm -f "${BUNDLE_DIR}/lib/libruby."*"-static.a"
find "${BUNDLE_DIR}" -name '*.dSYM' -type d -prune -exec rm -rf {} +

# ---------------------------------------------------------------------------
# Portability gate: nothing in the bundle may link Homebrew or /usr/local.
# Our own relocatable openssl (an absolute app/external path) is rewritten
# to @rpath by mac-release-02-bundle-dylibs.sh, so it's allowed here.
# ---------------------------------------------------------------------------
echo "Auditing bundle for non-portable references..."
offenders=""
while IFS= read -r f; do
    if otool -L "$f" 2>/dev/null | grep -qE '/opt/homebrew|/usr/local|/opt/local'; then
        offenders="${offenders}\n--- ${f}\n$(otool -L "$f" 2>/dev/null | grep -E '/opt/homebrew|/usr/local|/opt/local')"
    fi
done < <(find "${BUNDLE_DIR}" -type f \( -name '*.bundle' -o -name '*.dylib' -o -perm +111 \) 2>/dev/null)

if [ -n "${offenders}" ]; then
    echo "PORTABILITY FAILURE — bundle links Homebrew / non-system paths:" >&2
    printf '%b\n' "${offenders}" >&2
    exit 1
fi

# ---------------------------------------------------------------------------
# Confirm
# ---------------------------------------------------------------------------
echo
echo "$("${BUNDLE_DIR}/bin/ruby" -e 'print RUBY_DESCRIPTION') bundled — no Homebrew references."
echo "LC_BUILD_VERSION:"
vtool -show "${BUNDLE_DIR}/bin/ruby" 2>&1 | grep -E "minos|sdk|platform" | sed 's/^/    /'
echo "openssl extension links:"
otool -L "$(find "${BUNDLE_DIR}/lib" -name openssl.bundle | head -1)" 2>/dev/null \
    | grep -iE "ssl|crypto" | sed 's/^/    /'
echo "psych extension links (should be self-contained — no libyaml):"
otool -L "$(find "${BUNDLE_DIR}/lib" -name psych.bundle | head -1)" 2>/dev/null \
    | grep -iE "yaml" | sed 's/^/    /' || echo "    (none)"

# Drop the build tree; keep the install prefix for reference.
rm -rf "${BUILD_DIR}"
