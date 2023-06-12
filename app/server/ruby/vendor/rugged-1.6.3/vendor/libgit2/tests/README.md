# libgit2 tests

These are the unit and integration tests for the libgit2 projects.

* `benchmarks`
  These are benchmark tests that excercise the CLI.
* `clar`  
  This is [clar](https://github.com/clar-test/clar) the common test framework.
* `headertest`  
  This is a simple project that ensures that our public headers are
  compatible with extremely strict compilation options.
* `libgit2`  
  These tests exercise the core git functionality in libgit2 itself.
* `resources`  
  These are the resources for the tests, including files and git
  repositories.
* `util`  
  These are tests of the common utility library.

## Writing tests for libgit2

libgit2 uses the [clar test framework](http://github.com/clar-test/clar), a
C testing framework.

The best resources for learning clar are [clar itself](https://github.com/clar-test/clar)
and the existing tests within libgit2.  In general:

* If you place a `.c` file into a test directory, it is eligible to contain
test cases.
* The function name for your test is important; test function names begin
  with `test_`, followed by the folder path (underscore separated), two
  underscores as a delimiter, then the test name.  For example, a file
  `merge/analysis.c` may contain a test `uptodate`:

  ```
  void test_merge_analysis__uptodate(void)
  {
    ...
  }
  ```

* You can run an individual test by passing `-s` to the test runner.  Tests
  are referred to by their function names; for example, the function
  `test_merge_analysis__uptodate` is referred to as `merge::analysis::uptodate`.
  To run only that function you can use the `-s` option on the test runner:

  ```
  libgit2_tests -smerge::analysis::uptodate
  ```

## Memory leak checking

These are automatically run as part of CI, but if you want to check locally:

### Linux

Uses [`valgrind`](http://www.valgrind.org/):

```console
$ cmake -DBUILD_TESTS=ON -DVALGRIND=ON ..
$ cmake --build .
$ valgrind --leak-check=full --show-reachable=yes --num-callers=50 --suppressions=../libgit2_tests.supp \
  ./libgit2_tests
```

### macOS

Uses [`leaks`](https://developer.apple.com/library/archive/documentation/Performance/Conceptual/ManagingMemory/Articles/FindingLeaks.html), which requires XCode installed:

```console
$ MallocStackLogging=1 MallocScribble=1 MallocLogFile=/dev/null CLAR_AT_EXIT="leaks -quiet \$PPID" \
  ./libgit2_tests
```

### Windows

Build with the `WIN32_LEAKCHECK` option:

```console
$ cmake -DBUILD_TESTS=ON -DWIN32_LEAKCHECK=ON ..
$ cmake --build .
$ ./libgit2_tests
```
