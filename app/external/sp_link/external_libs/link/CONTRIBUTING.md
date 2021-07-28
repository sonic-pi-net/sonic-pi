Bug Reports
===========

If you've found a bug in Link itself, then please file a new issue here at GitHub. If you
have found a bug in a Link-enabled app, it might be wiser to reach out to the developer of
the app before filing an issue here.

Any and all information that you can provide regarding the bug will help in our being able
to find it. Specifically, that could include:

 - Stacktraces, in the event of a crash
 - Versions of the software used, and the underlying operating system
 - Steps to reproduce
 - Screenshots, in the case of a bug which results in a visual error


Pull Requests
=============

We are happy to accept pull requests from the GitHub community, assuming that they meet
the following criteria:

 - You have signed and returned Ableton's [CLA][cla]
 - The [tests pass](#testing)
 - The PR passes all CI service checks
 - The code is [well-formatted](#code-formatting)
 - The git commit messages comply to [the commonly accepted standards][git-commit-msgs]

Testing
-------

Link ships with unit tests that are run on [Travis CI][travis] and [AppVeyor][appveyor] for
all PRs. There are two test suites: `LinkCoreTest`, which tests the core Link
functionality, and `LinkDiscoverTest`, which tests the network discovery feature of Link.
A third virtual target, `LinkAllTest` is provided by the CMake project as a convenience
to run all tests at once.

The unit tests are run on every platform which Link is officially supported on, and also
are run through [Valgrind][valgrind] on Linux to check for memory corruption and leaks. If
valgrind detects any memory errors when running the tests, it will fail the build.

If you are submitting a PR which fixes a bug or introduces new functionality, please add a
test which helps to verify the correctness of the code in the PR.

Code Formatting
---------------

Link uses [clang-format][clang-format] to enforce our preferred code style. At the moment,
we use **clang-format version 6.0**. Note that other versions may format code differently.

Any PRs submitted to Link are also checked with clang-format by the Travis CI service. If
you get a build failure, then you can format your code by running the following command:

```
clang-format -style=file -i (filename)
```

[cla]: http://ableton.github.io/cla/
[git-commit-msgs]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[clang-format]: http://llvm.org/builds
[valgrind]: http://valgrind.org
[travis]: http://travis-ci.com
[appveyor]: http://appveyor.com
