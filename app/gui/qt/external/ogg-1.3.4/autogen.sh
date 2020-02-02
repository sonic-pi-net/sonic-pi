#!/bin/sh
# Run this to set up the build system: configure, makefiles, etc.
set -e

package="libogg"

srcdir=`dirname $0`
test -n "$srcdir" && cd "$srcdir"

echo "Updating build configuration files for $package, please wait...."

mkdir -p m4
autoreconf -if
