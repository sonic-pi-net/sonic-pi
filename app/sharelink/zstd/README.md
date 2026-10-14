# Zstandard (zstd)

Meta's zstd, v1.5.7, as one source file: `zstd.c` is `build/single_file_libs/zstd-in.c` from
https://github.com/facebook/zstd/tree/v1.5.7 amalgamated with its own `combine.py`, with the dictionary builder
(`dictBuilder/*`, which trains a dictionary: `zstd --train` does that, once, for a format) and multithreading
(`ZSTD_MULTITHREAD`, `zstdmt_compress.c`) left out, since the codec compresses one small program on the calling thread:

    sed -e '/dictBuilder/d' -e '/^#ifndef __EMSCRIPTEN__$/,/^#endif$/d' -e '/^#ifdef ZSTD_MULTITHREAD$/,/^#endif$/d' zstd-in.c > sp-zstd-in.c
    python3 combine.py -r ../../lib -x legacy/zstd_legacy.h -k zstd.h -o zstd.c sp-zstd-in.c
    sed -i '' 's|#include "\.\./zstd\.h"|#include "zstd.h"|' zstd.c     # the kept header, beside the file rather than above it

`zstd.h` and `zstd_errors.h` are `lib/`'s, unmodified. BSD 3-Clause (`LICENSE`; zstd is also offered under the GPLv2,
which is not taken up here).

The share link's codec (`../sharelink.cpp`) compresses format 2 links with it, against a preset dictionary, for native
Sonic Pi and, compiled to wasm, for the web's page. The codec defines `ZSTD_STATIC_LINKING_ONLY` for the magicless
frame format (`ZSTD_f_zstd1_magicless`), an experimental API that is stable for this vendored version.
