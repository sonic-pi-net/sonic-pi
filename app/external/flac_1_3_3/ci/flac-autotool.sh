# Continuous integration build script for FLAC.
# This script is run by automated frameworks to verify commits
# see https://mf4.xiph.org/jenkins/job/flac/

# This is intended to be run from the top-level source directory.

set -x

./autogen.sh

./configure

# Should do 'distcheck' here instead of 'check', but 'distcheck' is currently busted.
V=1 make clean distcheck

# Since we're doing 'make distcheck' we remove the generated source tarball.
rm -f flac-*.tar.xz
