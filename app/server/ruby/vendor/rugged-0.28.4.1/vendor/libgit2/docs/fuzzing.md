# Fuzzing

libgit2 is currently using [libFuzzer](https://libfuzzer.info) to perform
automated fuzz testing. libFuzzer only works with clang.

## Prerequisites** for building fuzz targets:

1. All the prerequisites for [building libgit2](https://github.com/libgit2/libgit2).
2. A recent version of clang. 6.0 is preferred. [pre-build Debian/Ubuntu
   packages](https://github.com/libgit2/libgit2)

## Build

1. Create a build directory beneath the libgit2 source directory, and change
   into it: `mkdir build && cd build`
2. Choose one sanitizers to add. The currently supported sanitizers are
   [`address`](https://clang.llvm.org/docs/AddressSanitizer.html),
   [`undefined`](https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html),
   and [`leak`/`address,leak`](https://clang.llvm.org/docs/LeakSanitizer.html).
3. Create the cmake build environment and configure the build with the
   sanitizer chosen: `CC=/usr/bin/clang-6.0 CFLAGS="-fsanitize=address" cmake
   -DBUILD_CLAR=OFF -DBUILD_FUZZERS=ON -DCMAKE_BUILD_TYPE=RelWithDebInfo ..`.
   Note that building the fuzzer targets is incompatible with the
   tests and examples.
4. Build libgit2: `cmake --build .`
5. Exit the cmake build environment: `cd ..`

## Run the fuzz targets

1. `ASAN_SYMBOLIZER_PATH=/usr/bin/llvm-symbolize-6.0
   LSAN_OPTIONS=allocator_may_return_null=1
   ASAN_OPTIONS=allocator_may_return_null=1 ./build/fuzz/fuzz_packfile_raw
   fuzz/corpora/fuzz_packfile_raw/`

The `LSAN_OPTIONS` and `ASAN_OPTIONS` are there to allow `malloc(3)` to return
`NULL`. The `LLVM_PROFILE_FILE` is there to override the path where libFuzzer
will write the coverage report.

## Get coverage

In order to get coverage information, you need to add the "-fcoverage-mapping"
and "-fprofile-instr-generate CFLAGS, and then run the fuzz target with
`-runs=0`. That will produce a file called `default.profraw` (this behavior can
be overridden by setting the `LLVM_PROFILE_FILE="yourfile.profraw"` environment
variable).

1. `llvm-profdata-6.0 merge -sparse default.profraw -o
   fuzz_packfile_raw.profdata` transforms the data from a sparse representation
   into a format that can be used by the other tools.
2. `llvm-cov-6.0 report ./build/fuzz/fuzz_packfile_raw
   -instr-profile=fuzz_packfile_raw.profdata` shows a high-level per-file
   coverage report.
3. `llvm-cov-6.0 show ./build/fuzz/fuzz_packfile_raw
   -instr-profile=fuzz_packfile_raw.profdata [source file]` shows a line-by-line
   coverage analysis of all the codebase (or a single source file).

## Standalone mode

In order to ensure that there are no regresions, each fuzzer target can be run
in a standalone mode. This can be done by passing `-DUSE_STANDALONE_FUZZERS=ON`.
This makes it compatible with gcc. This does not use the fuzzing engine, but
just invokes every file in the chosen corpus.

In order to get full coverage, though, you might want to also enable one of the
sanitizers. You might need a recent version of clang to get full support.

## References

* [libFuzzer](https://llvm.org/docs/LibFuzzer.html) documentation.
* [Source-based Code
  Coverage](https://clang.llvm.org/docs/SourceBasedCodeCoverage.html).
