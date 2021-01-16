#!/bin/bash -eu

pushd $SRC
mv people.xiph.org/*.ogg decode_corpus/
zip -r "$OUT/decode_fuzzer_seed_corpus.zip" decode_corpus/
popd

pushd $SRC/ogg
./autogen.sh
./configure --prefix="$WORK" --enable-static --disable-shared --disable-crc
make clean
make -j$(nproc)
make install
popd


./autogen.sh
./configure --prefix="$WORK" --enable-static --disable-shared
make clean
make -j$(nproc)
make install

$CXX $CXXFLAGS $SRC/vorbis/contrib/oss-fuzz/decode_fuzzer.cc -o $OUT/decode_fuzzer -L"$WORK/lib" -I"$WORK/include" -lFuzzingEngine -lvorbisfile -lvorbis -logg
