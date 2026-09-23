#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Builds the runtime: mruby for the host and for wasm, the runtime's Ruby
# compiled to bytecode, the wasm module the page and Node load, and sp-trace,
# the same runtime as a native command.
#
#   scripts/build-runtime.sh            # everything
#   scripts/build-runtime.sh --wasm     # relink the wasm
#   scripts/build-runtime.sh --native   # relink sp-trace, building only the host mruby if it is missing
#
# Needs rake, a C compiler and Emscripten (emcc on PATH). --native wants no
# Emscripten: it links the runtime against the host's mruby, so the specs can
# be run through the interpreter Sonic Pi ships rather than through MRI
# (ADAPTER="$PWD/build/runtime/sp-trace" ruby scripts/check.rb), where a core
# method mruby lacks would otherwise only show up in the browser.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
MR="$ROOT/runtime/mruby"
export MRUBY_CONFIG="$ROOT/runtime/build_config.rb"
export MRUBY_BUILD_DIR="$ROOT/build/mruby"
OUT="$ROOT/build/runtime"
mkdir -p "$OUT"
[ -f "$MR/Rakefile" ] || { echo "no mruby: git submodule update --init --depth 1 runtime/mruby" >&2; exit 1; }

MODE="${1:-}"
case "$MODE" in ""|--wasm|--native) ;; *) echo "usage: $(basename "$0") [--wasm|--native]" >&2; exit 1;; esac

# mruby itself. The whole build always rebuilds it; --wasm and --native build whatever is missing, so a fresh
# checkout can go straight to either, and a checkout that has one can ask for the other. --native needs the host
# interpreter alone and asks for no Emscripten; --wasm needs the emscripten target too, which a --native build
# leaves out.
HOST_MRBC="$MRUBY_BUILD_DIR/host/bin/mrbc"
EM_CONFIG="$MRUBY_BUILD_DIR/emscripten/host-bin/mruby-config"
NEED_MRUBY=""
if [ -z "$MODE" ]; then
  NEED_MRUBY="host + emscripten"
elif [ "$MODE" = "--native" ] && [ ! -x "$HOST_MRBC" ]; then
  NEED_MRUBY="host"
elif [ "$MODE" = "--wasm" ] && { [ ! -x "$HOST_MRBC" ] || [ ! -x "$EM_CONFIG" ]; }; then
  NEED_MRUBY="host + emscripten"
fi
if [ -n "$NEED_MRUBY" ]; then
  echo "mruby ($NEED_MRUBY)…"
  [ "$NEED_MRUBY" = "host" ] && export SP_HOST_ONLY=1
  (cd "$MR" && rake -j"$(sysctl -n hw.ncpu 2>/dev/null || nproc)" > "$OUT/mruby-build.log" 2>&1) || { tail -40 "$OUT/mruby-build.log"; exit 1; }
fi

# The runtime's Ruby, in load order, as one bytecode blob.
LIB="$ROOT/runtime/lib/sonic_pi"
# the rules are Sonic Pi's own file, compiled in as it is: the server reads it, the runtime is built from it
SERVER_LIB="$(cd "$ROOT/../../app/server/ruby/lib/sonicpi" && pwd)"
cat "$LIB/errors.rb" "$LIB/defaults.rb" "$SERVER_LIB/validation.rb" "$LIB/float_format.rb" "$LIB/ring.rb" "$LIB/rand.rb" "$LIB/rand_verbs.rb" "$LIB/note.rb" "$LIB/time_state.rb" "$LIB/preparser.rb" "$ROOT/runtime/data/synths.rb" "$ROOT/runtime/data/theory.rb" "$ROOT/runtime/data/lang.rb" "$ROOT/runtime/data/samples.rb" "$LIB/theory.rb" "$LIB/samples.rb" "$LIB/cue_history.rb" "$LIB/scheduler.rb" "$LIB/lang.rb" "$LIB/lang_more.rb" "$LIB/synth_meta.rb" "$LIB/adapter.rb" > "$OUT/runtime.rb"
"$MRUBY_BUILD_DIR/host/bin/mrbc" -B sp_runtime_irep -o "$OUT/runtime_irep.c" "$OUT/runtime.rb"

if [ "$MODE" != "--native" ]; then
  CONFIG="$MRUBY_BUILD_DIR/emscripten/host-bin/mruby-config"
  emcc $("$CONFIG" --cflags) $("$CONFIG" --ldflags) \
    "$ROOT/runtime/host/sp_host.c" "$OUT/runtime_irep.c" $("$CONFIG" --libs) \
    -O2 -g0 -o "$OUT/sp_runtime.mjs" \
    -sMODULARIZE=1 -sEXPORT_ES6=1 -sENVIRONMENT=node,web,worker -sALLOW_MEMORY_GROWTH=1 \
    -sEXPORTED_FUNCTIONS=_sp_init,_sp_install_table,_sp_install_synth,_sp_set_samples_dir,_sp_install_sample,_sp_trace,_sp_version,_sp_live_boot,_sp_run,_sp_tick,_sp_stop_all,_sp_stop_job,_sp_run_group,_sp_stop_group,_sp_group_under,_sp_stop_subtree,_sp_process_table,_sp_process_table_len,_sp_live_stop_after,_sp_set_link_bpm,_sp_set_time_warp,_sp_hold,_sp_sched_ahead,_sp_cue,_sp_out_ptr,_sp_out_len,_sp_buffer_for,_sp_synthdef_for,_malloc,_free \
    -sEXPORTED_RUNTIME_METHODS=ccall,cwrap,UTF8ToString,HEAPU8,HEAPF64
  ls -la "$OUT/sp_runtime.mjs" "$OUT/sp_runtime.wasm"
fi

# The native command: the same host seam and the same bytecode, linked
# against the host's mruby. SP_ASSET_ROOT is where it looks for the random
# tables and the built-in sounds when it is not told; this checkout, since
# that is what the specs are run from.
if [ "$MODE" != "--wasm" ]; then
  # mruby's flags, read from the file its mruby-config is made from: a shell script elsewhere, but a .bat on Windows,
  # which bash cannot run, so the file itself (its $(MRUBY_PACKAGE_DIR) the host build, its quotes dropped: the
  # words go to the compiler as they are)
  FLAGS="$MRUBY_BUILD_DIR/host/lib/libmruby.flags.mak"
  mruby_flag() { sed -n "s/^MRUBY_$1 = *//p" "$FLAGS" | sed -e "s|\$(MRUBY_PACKAGE_DIR)|$MRUBY_BUILD_DIR/host|g" -e 's/"//g'; }
  EXE=""
  case "$(uname -s 2>/dev/null || echo)" in MINGW*|MSYS*|CYGWIN*) EXE=".exe";; esac
  "${CC:-cc}" $(mruby_flag CFLAGS) -DSP_ASSET_ROOT="\"$ROOT\"" -O2 \
    "$ROOT/runtime/host/sp_host.c" "$ROOT/runtime/host/sp_trace_main.c" "$OUT/runtime_irep.c" \
    $(mruby_flag LDFLAGS) $(mruby_flag LIBS) -o "$OUT/sp-trace$EXE"
  ls -la "$OUT/sp-trace$EXE"
fi
