#!/bin/bash
# Compiles the harness-only test synthdefs. Needs SuperCollider's sclang,
# same as the shipping synthdefs in etc/synthdefs/designs.
set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SCLANG="${SCLANG:-/Applications/SuperCollider.app/Contents/MacOS/sclang}"
if [ ! -x "$SCLANG" ]; then
  echo "sclang not found at $SCLANG — set SCLANG to your SuperCollider sclang" >&2
  exit 1
fi
mkdir -p "$DIR/compiled"
# sclang ignores a file passed as an argument when stdin is not a tty, so the
# file is loaded from a piped statement instead.
printf '"%s/mixtest.scd".load;\n0.exit;\n' "$DIR" | "$SCLANG" > /dev/null 2>&1
ls "$DIR/compiled"
