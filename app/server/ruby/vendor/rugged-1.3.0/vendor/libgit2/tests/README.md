Writing Clar tests for libgit2
==============================

For information on the Clar testing framework and a detailed introduction
please visit:

https://github.com/vmg/clar


* Write your modules and tests. Use good, meaningful names.

* Make sure you actually build the tests by setting:

        cmake -DBUILD_CLAR=ON build/

* Test:

        ./build/libgit2_clar

* Make sure everything is fine.

* Send your pull request. That's it.


Memory leak checks
------------------

These are automatically run as part of CI, but if you want to check locally:

#### Linux

Uses [`valgrind`](http://www.valgrind.org/):

```console
$ cmake -DBUILD_CLAR=ON -DVALGRIND=ON ..
$ cmake --build .
$ valgrind --leak-check=full --show-reachable=yes --num-callers=50 --suppressions=../libgit2_clar.supp \
  ./libgit2_clar
```

#### macOS

Uses [`leaks`](https://developer.apple.com/library/archive/documentation/Performance/Conceptual/ManagingMemory/Articles/FindingLeaks.html), which requires XCode installed:

```console
$ MallocStackLogging=1 MallocScribble=1 MallocLogFile=/dev/null CLAR_AT_EXIT="leaks -quiet \$PPID" \
  ./libgit2_clar
```
