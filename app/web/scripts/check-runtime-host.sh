#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Builds and runs runtime/host/runtime_host_check.cpp: the runtime in a native host (runtime/host/runtime_host.h),
# as a plugin or an app runs it, with no engine. Samples by name, by path and from a folder, and several runtimes at
# once, each on its own thread. Then runtime/host/sp_instances_check.c: several bare runtimes at once, below any
# host, each tracing and playing a live session on its own thread exactly as one alone does.
#
#   scripts/build-runtime.sh --native && scripts/check-runtime-host.sh
#
# Needs a C++20 compiler, and oscpack from the SuperSonic submodule (app/external/supersonic).
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/build/runtime"
HOST="$ROOT/runtime/host"
MRUBY_BUILD_DIR="$ROOT/build/mruby"
OSC="$ROOT/../external/supersonic/clockwork/src/vendor/oscpack"
[ -f "$OUT/runtime_native_irep.c" ] || { echo "no native runtime: scripts/build-runtime.sh --native first" >&2; exit 1; }
[ -f "$OUT/runtime_irep.c" ] || { echo "no runtime: scripts/build-runtime.sh first" >&2; exit 1; }
[ -f "$OSC/osc/OscTypes.cpp" ] || { echo "no oscpack at $OSC: git submodule update --init --recursive app/external/supersonic" >&2; exit 1; }

# mruby's flags, as build-runtime.sh reads them
FLAGS="$MRUBY_BUILD_DIR/host/lib/libmruby.flags.mak"
mruby_flag() { sed -n "s/^MRUBY_$1 = *//p" "$FLAGS" | sed -e "s|\$(MRUBY_PACKAGE_DIR)|$MRUBY_BUILD_DIR/host|g" -e 's/"//g'; }
EXE=""
case "$(uname -s 2>/dev/null || echo)" in MINGW*|MSYS*|CYGWIN*) EXE=".exe";; esac

OBJ="$OUT/runtime-host-check.objects"
mkdir -p "$OBJ"
"${CC:-cc}" $(mruby_flag CFLAGS) -O1 -c "$HOST/sp_host.c" -o "$OBJ/sp_host.o"
"${CC:-cc}" $(mruby_flag CFLAGS) -O1 -c "$OUT/runtime_native_irep.c" -o "$OBJ/runtime_native_irep.o"
"${CXX:-c++}" -std=c++20 -pthread $(mruby_flag CXXFLAGS) -O1 -I"$OSC" \
  "$HOST/runtime_host_check.cpp" "$HOST/runtime_host.cpp" "$HOST/files.cpp" "$HOST/osc_codec.cpp" \
  "$OSC/osc/OscOutboundPacketStream.cpp" "$OSC/osc/OscReceivedElements.cpp" "$OSC/osc/OscTypes.cpp" \
  "$OBJ/sp_host.o" "$OBJ/runtime_native_irep.o" \
  $(mruby_flag LDFLAGS) $(mruby_flag LIBS) -o "$OUT/runtime-host-check$EXE"
"$OUT/runtime-host-check$EXE" "$ROOT"

"${CC:-cc}" $(mruby_flag CFLAGS) -O1 -c "$OUT/runtime_irep.c" -o "$OBJ/runtime_irep.o"
"${CC:-cc}" -pthread $(mruby_flag CFLAGS) -O1 "$HOST/sp_instances_check.c" "$OBJ/sp_host.o" "$OBJ/runtime_irep.o" \
  $(mruby_flag LDFLAGS) $(mruby_flag LIBS) -o "$OUT/sp-instances-check$EXE"
"$OUT/sp-instances-check$EXE" "$ROOT/../../etc/buffers"
