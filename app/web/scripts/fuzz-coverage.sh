#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Says how much of mruby and the runtime's C a fuzz corpus reaches, file by file: the check that the fuzzer
# (runtime/fuzz/fuzz_program.c) is trying the code it is there to try.
#
#   scripts/fuzz-coverage.sh build/fuzz/seeds <a corpus> ...
#
# Needs llvm-profdata and llvm-cov beside the clang that CC names.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OUT="$ROOT/build/fuzz"
: "${CC:=clang}"
export CC
[ $# -gt 0 ] || { echo "usage: $0 <corpus directory> ..." >&2; exit 1; }
"$ROOT/scripts/build-fuzz.sh" --coverage > /dev/null
TOOLS="$(dirname "$(command -v "$CC")")"
rm -f "$OUT"/coverage.profraw
LLVM_PROFILE_FILE="$OUT/coverage.profraw" "$OUT/fuzz_program_coverage" -runs=0 "$@" 2>&1 | grep -E '^Done|^INFO: seed corpus' >&2
"$TOOLS/llvm-profdata" merge -o "$OUT/coverage.profdata" "$OUT/coverage.profraw"
"$TOOLS/llvm-cov" report "$OUT/fuzz_program_coverage" -instr-profile="$OUT/coverage.profdata" \
  -ignore-filename-regex='fuzz_program\.c|runtime_irep\.c'
