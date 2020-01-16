#!/bin/bash

set -e

# Results check
[ ! -d "cov-int" ] && echo "Coverity directory not found" && exit 1

# Upload results
tar czf libgit2.tgz cov-int

SOURCE_DIR=${SOURCE_DIR:-$( cd "$( dirname "${BASH_SOURCE[0]}" )" && dirname $( pwd ) )}
SHA=$(cd ${SOURCE_DIR} && git rev-parse --short HEAD)

HTML="$(curl \
	--silent \
	--write-out "\n%{http_code}" \
	--form token="$COVERITY_TOKEN" \
	--form email=libgit2@gmail.com \
	--form file=@libgit2.tgz \
	--form version="$SHA" \
	--form description="libgit2 build" \
	https://scan.coverity.com/builds?project=libgit2)"

# Body is everything up to the last line
BODY="$(echo "$HTML" | head -n-1)"

# Status code is the last line
STATUS_CODE="$(echo "$HTML" | tail -n1)"

if [ "${STATUS_CODE}" != "200" -a "${STATUS_CODE}" != "201" ]; then
	echo "Received error code ${STATUS_CODE} from Coverity"
	exit 1
fi
