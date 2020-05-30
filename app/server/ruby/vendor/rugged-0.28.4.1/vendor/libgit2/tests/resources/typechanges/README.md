This is a test repo for libgit2 where tree entries have type changes

Types
-----

The key types that could be found in tree entries are:

1. GIT_FILEMODE_NEW             = 0000000 (i.e. file does not exist)
2. GIT_FILEMODE_TREE            = 0040000
3. GIT_FILEMODE_BLOB            = 0100644
4. GIT_FILEMODE_BLOB_EXECUTABLE = 0100755
5. GIT_FILEMODE_LINK            = 0120000
6. GIT_FILEMODE_COMMIT          = 0160000

I will try to have every type of transition somewhere in the history
of this repo.

Commits
-------

* `a(1--1) b(1--1) c(1--1) d(1--1) e(1--1)`
  **Initial commit**<br>
  `79b9f23e85f55ea36a472a902e875bc1121a94cb`
* `a(1->2) b(1->3) c(1->4) d(1->5) e(1->6)`
  **Create content**<br>
  `9bdb75b73836a99e3dbeea640a81de81031fdc29`
* `a(2->3) b(3->4) c(4->5) d(5->6) e(6->2)`
  **Changes #1**<br>
  `0e7ed140b514b8cae23254cb8656fe1674403aff`
* `a(3->5) b(4->6) c(5->2) d(6->3) e(2->4)`
  **Changes #2**<br>
  `9d0235c7a7edc0889a18f97a42ee6db9fe688447`
* `a(5->3) b(6->4) c(2->5) d(3->6) e(4->2)`
  **Changes #3**<br>
  `9b19edf33a03a0c59cdfc113bfa5c06179bf9b1a`
* `a(3->2) b(4->3) c(5->4) d(6->5) e(2->6)`
  **Changes #4**<br>
  `1b63caae4a5ca96f78e8dfefc376c6a39a142475`<br>
  Matches **Changes #1** except README.md
* `a(2->1) b(3->1) c(4->1) d(5->1) e(6->1)`
  **Changes #5**<br>
  `6eae26c90e8ccc4d16208972119c40635489c6f0`<br>
  Matches **Initial commit** except README.md and .gitmodules
