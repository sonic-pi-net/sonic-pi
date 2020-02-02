#! /usr/bin/env bash

# This script sets the LD_LIBRARY_PATH environment variable to ./build/src to
# execute aubio binaries without installing libaubio.
#
# Usage: $ source scripts/setenv_local.sh
#
# Note: on macOs, the variable is DYLD_LIBRARY_PATH

AUBIODIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

AUBIOLIB=$AUBIODIR/build/src

if [ "$(uname)" == "Darwin" ]; then
  export DYLD_LIBRARY_PATH=$AUBIOLIB
  echo export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH
else
  export LD_LIBRARY_PATH=$AUBIOLIB
  echo export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
fi
