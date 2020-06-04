#!/bin/sh
if [ $# -eq 0 ]
  then
    echo "Need to specify the version"
    exit 1
fi

VERSION=$1
OSMIDDIR=`pwd`
TARGETDIR=~/tmp/osmid-$VERSION

mkdir $TARGETDIR
cp CMakeLists.txt $TARGETDIR
cp -r external_libs $TARGETDIR
cp -r src $TARGETDIR

cd $TARGETDIR/..
tar czvf osmid_$VERSION.orig.tar.gz osmid-$VERSION
rm -rf $TARGETDIR

tar xvf osmid_$VERSION.orig.tar.gz

cp -r $OSMIDDIR/installer/debian/debian $TARGETDIR/debian

echo "Now, go into $TARGETDIR/debian and amend the debian files (mostly changelog) to reflect the current status"
echo "then cd .."
echo "and execute: debuild -us -uc"
