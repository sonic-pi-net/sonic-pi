#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Builds the one native piece the oracle needs: Sonic Pi's aubio_onset, the
# program its runtime runs to find a sample's onsets. Built from the aubio
# source and the tiny front-end Sonic Pi carries in its own tree, so the
# oracle's onsets are Sonic Pi's, not a distribution's aubio.
#
#   scripts/build-oracle.sh
#
# Needs cmake, a C compiler and libsndfile (with its CMake config):
#   macOS:  brew install cmake libsndfile
#   Debian: apt install cmake build-essential libsndfile1-dev
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SP="$(cd "$ROOT/../.." && pwd)"
BUILD="$ROOT/build/aubio"
OUT="$ROOT/oracle/bin"
[ -f "$SP/app/external/aubio/aubioonset.c" ] || { echo "no Sonic Pi sources: git submodule update --init --depth 1" >&2; exit 1; }

cmake -S "$SP/app/external/aubio-0.4.9" -B "$BUILD" -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX="$BUILD/package" -DCMAKE_POLICY_VERSION_MINIMUM=3.5 > "$BUILD.configure.log" 2>&1 \
  || { cat "$BUILD.configure.log"; exit 1; }
cmake --build "$BUILD" --config Release -j > "$BUILD.build.log" 2>&1 || { tail -40 "$BUILD.build.log"; exit 1; }
cmake --install "$BUILD" --config Release > /dev/null

mkdir -p "$OUT"
SNDFILE_LIBS="$(pkg-config --libs sndfile 2>/dev/null || echo -lsndfile)"
SNDFILE_CFLAGS="$(pkg-config --cflags sndfile 2>/dev/null || true)"
cc -O2 -o "$OUT/aubio_onset" "$SP/app/external/aubio/aubioonset.c" "$SP/app/external/aubio/utils.c" \
   -I"$BUILD/package/include" -I"$SP/app/external/aubio" $SNDFILE_CFLAGS -L"$BUILD/package/lib" -laubio $SNDFILE_LIBS -lm
echo "built $OUT/aubio_onset"
"$OUT/aubio_onset" "$SP/etc/samples/loop_amen.flac" | head -3
