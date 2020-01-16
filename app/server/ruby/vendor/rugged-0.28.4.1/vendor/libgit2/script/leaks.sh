#!/bin/sh
export MallocStackLogging=1
export MallocScribble=1
export MallocLogFile=/dev/null
export CLAR_AT_EXIT="leaks -quiet \$PPID"
exec "$@"
