#!/bin/bash
set -e

# Environment check
[ -z "$COVERITY_TOKEN" ] && echo "Need to set a coverity token" && exit 1

# Only run this on our branches
echo "Pull request: $TRAVIS_PULL_REQUEST  |  Slug: $TRAVIS_REPO_SLUG"
if [ "$TRAVIS_PULL_REQUEST" != "false" -o "$TRAVIS_REPO_SLUG" != "libgit2/libgit2" ];
then
	echo "Only analyzing 'development' on the main repo."
	exit 0
fi

COV_VERSION=6.6.1
case `uname -m` in
	i?86)				BITS=32 ;;
	amd64|x86_64)	BITS=64 ;;
esac
SCAN_TOOL=https://scan.coverity.com/download/linux-${BITS}
TOOL_BASE=`pwd`/_coverity-scan

# Install coverity tools
if [ ! -d $TOOL_BASE ]; then
	echo "Downloading coverity..."
	mkdir -p $TOOL_BASE
	cd $TOOL_BASE
	wget -O coverity_tool.tgz $SCAN_TOOL \
		--post-data "project=libgit2&token=$COVERITY_TOKEN"
	tar xzf coverity_tool.tgz
	cd ..
	TOOL_DIR=`find $TOOL_BASE -type d -name 'cov-analysis*'`
	ln -s $TOOL_DIR $TOOL_BASE/cov-analysis
fi

COV_BUILD="$TOOL_BASE/cov-analysis/bin/cov-build"

# Configure and build
rm -rf _build
mkdir _build
cd _build
cmake .. -DTHREADSAFE=ON
COVERITY_UNSUPPORTED=1 \
	$COV_BUILD --dir cov-int \
	cmake --build .

# Upload results
tar czf libgit2.tgz cov-int
SHA=`git rev-parse --short HEAD`
curl \
	--form project=libgit2 \
	--form token=$COVERITY_TOKEN \
	--form email=bs@github.com \
	--form file=@libgit2.tgz \
	--form version=$SHA \
	--form description="Travis build" \
	http://scan5.coverity.com/cgi-bin/upload.py
