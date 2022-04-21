#! /bin/bash

set -e
#set -x

WAFVERSION=2.0.17
WAFTARBALL=waf-$WAFVERSION.tar.bz2
WAFURL=https://waf.io/$WAFTARBALL
WAFUPSTREAMKEY=https://gitlab.com/ita1024/waf/raw/master/utils/pubkey.asc

WAFBUILDDIR=`mktemp -d`

function cleanup () {
  rm -rf $WAFBUILDDIR
}

trap cleanup SIGINT SIGTERM

function download () {
  ( [[ -n `which wget` ]] && wget -qO $1 $2 ) || ( [[ -n `which curl` ]] && curl -so $1 $2 )
}

function checkwaf () {
  download $WAFTARBALL.asc $WAFURL.asc
  if [[ -z `which gpg` ]]
  then
    echo "Warning: gpg not found, not verifying signature for $WAFTARBALL"
  else
    download - $WAFUPSTREAMKEY | gpg --import
    gpg --verify $WAFTARBALL.asc || exit 1
  fi
}

function fetchwaf () {
  download $WAFTARBALL $WAFURL
  checkwaf
}

function buildwaf () {
  tar xf $WAFTARBALL
  pushd waf-$WAFVERSION
  NOCLIMB=1 python waf-light --tools=c_emscripten $*
  popd
}

pushd $WAFBUILDDIR
fetchwaf
buildwaf
popd

cp -prv $WAFBUILDDIR/waf-$WAFVERSION/waf "$PWD"
chmod +x waf

cleanup
