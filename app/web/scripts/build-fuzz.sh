#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Builds the runtime's fuzz target (runtime/fuzz/fuzz_program.c): mruby under ASan and UBSan with libFuzzer's
# coverage (runtime/build_config.rb, "fuzz"), the runtime's bytecode, and the harness over them. It also gathers the
# seeds: every program in specs, as build/fuzz/seeds (specs holds each program's expected trace beside it, and a
# trace is not a program).
#
#   scripts/build-runtime.sh --native && scripts/build-fuzz.sh
#   build/fuzz/fuzz_program <a scratch corpus> build/fuzz/seeds runtime/fuzz/corpus -max_total_time=60
#
# --coverage builds build/fuzz/fuzz_program_coverage instead, which counts the lines a corpus reaches
# (scripts/fuzz-coverage.sh).
#
# Needs a clang with libFuzzer: CC names it (Apple's has none; Homebrew's llvm does). A libFuzzer binary given a
# directory writes new inputs into the first one, so give it a scratch directory first and the corpus after.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="$ROOT/build/mruby-src"
OUT="$ROOT/build/fuzz"
export MRUBY_CONFIG="$ROOT/runtime/build_config.rb"
export MRUBY_BUILD_DIR="$ROOT/build/mruby"
: "${CC:=clang}"
export CC LD="$CC"
[ -f "$SRC/Rakefile" ] && [ -f "$ROOT/build/runtime/runtime_irep.c" ] || { echo "no runtime: scripts/build-runtime.sh --native first" >&2; exit 1; }
mkdir -p "$OUT"
if [ "${1:-}" = "--coverage" ]; then
  BUILD=fuzz-coverage; ASK=SP_FUZZ_COVERAGE; BIN=fuzz_program_coverage; HARNESS="-fsanitize=fuzzer -fprofile-instr-generate -fcoverage-mapping"
else
  BUILD=fuzz; ASK=SP_FUZZ; BIN=fuzz_program; HARNESS="-fsanitize=fuzzer,address,undefined"
fi
JOBS="${JOBS:-$(sysctl -n hw.ncpu 2>/dev/null || nproc)}"
(cd "$SRC" && env SP_HOST_ONLY=1 "$ASK=1" rake -j"$JOBS" > "$OUT/mruby-build.log" 2>&1) || { tail -40 "$OUT/mruby-build.log"; exit 1; }

# the mutator's words are the dictionary's
sed -n 's/^\(".*"\)$/\1,/p' "$ROOT/runtime/fuzz/sonic-pi.dict" > "$OUT/words.inc"
rm -rf "$OUT/seeds" && mkdir -p "$OUT/seeds"
(cd "$ROOT/specs" && find . -name '*.rb' | while read -r f; do f="${f#./}"; cp "$f" "$OUT/seeds/${f//\//__}"; done)

FLAGS="$MRUBY_BUILD_DIR/$BUILD/lib/libmruby.flags.mak"
mruby_flag() { sed -n "s/^MRUBY_$1 = *//p" "$FLAGS" | sed -e "s|\$(MRUBY_PACKAGE_DIR)|$MRUBY_BUILD_DIR/$BUILD|g" -e 's/"//g'; }
# shellcheck disable=SC2046,SC2086
"$CC" $(mruby_flag CFLAGS) $HARNESS -I"$OUT" \
  "$ROOT/runtime/fuzz/fuzz_program.c" "$ROOT/runtime/host/sp_host.c" "$ROOT/build/runtime/runtime_irep.c" \
  $(mruby_flag LDFLAGS) $(mruby_flag LIBS) -o "$OUT/$BIN"
ls -la "$OUT/$BIN"
echo "$(find "$OUT/seeds" -type f | wc -l | tr -d ' ') seeds in $OUT/seeds"
