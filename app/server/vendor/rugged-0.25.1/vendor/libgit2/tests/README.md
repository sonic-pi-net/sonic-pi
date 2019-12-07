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
