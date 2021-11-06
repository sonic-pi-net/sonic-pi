#!/bin/bash
set -e # Quit script on error
SCRIPT_DIR="$(dirname "$0")"
cd "${SCRIPT_DIR}"

# Close standard output file descriptor
exec 1<&-
# Close standard error file descriptor
exec 2<&-

# Close standard in file descriptor
exec <&-

# Open standard output file for read and write.
exec 1>>"$9"

# Redirect standard error to standard output
exec 2>&1

echo "Booting Sonic Pi on Linux..."

TAU_ENABLED=$1 TAU_INTERNAL=$2 TAU_MIDI_ON=$3 TAU_LINK_ON=$4 TAU_IN_PORT=$5 TAU_API_PORT=$6 TAU_SPIDER_PORT=$7 TAU_DAEMON_PORT=$8 TAU_LOG_PATH=$9 _build/"${MIX_ENV:-prod}"/rel/tau/bin/tau start
