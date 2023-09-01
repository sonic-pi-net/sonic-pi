#!/bin/sh

set -ex

choco install hyperfine zip

CHOCO_PATH=$(mktemp -d)
curl -L https://github.com/ethomson/PurgeStandbyList/releases/download/v1.0/purgestandbylist.1.0.0.nupkg -o "${CHOCO_PATH}/purgestandbylist.1.0.0.nupkg"
choco install purgestandbylist -s $(cygpath -w "${CHOCO_PATH}")
