Projects For LibGit2
====================

So, you want to start helping out with `libgit2`? That's fantastic! We
welcome contributions and we promise we'll try to be nice.

This is a list of libgit2 related projects that new contributors can take
on.  It includes a number of good starter projects and well as some larger
ideas that no one is actively working on.

## Before You Start

Please start by reading the [README.md](README.md),
[CONTRIBUTING.md](CONTRIBUTING.md), and [CONVENTIONS.md](CONVENTIONS.md)
files before diving into one of these projects.  Those explain our work
flow and coding conventions to help ensure that your work will be easily
integrated into libgit2.

Next, work through the build instructions and make sure you can clone the
repository, compile it, and run the tests successfully.  That will make
sure that your development environment is set up correctly and you are
ready to start on libgit2 development.

## Starter Projects

These are good small projects to get started with libgit2.

* Look at the `examples/` programs, find an existing one that mirrors a
  core Git command and add a missing command-line option.  There are many
  gaps right now and this helps demonstrate how to use the library.  Here
  are some specific ideas (though there are many more):
    * Fix the `examples/diff.c` implementation of the `-B`
      (a.k.a. `--break-rewrites`) command line option to actually look for
      the optional `[<n>][/<m>]` configuration values. There is an
      existing comment that reads `/* TODO: parse thresholds */`. The
      trick to this one will be doing it in a manner that is clean and
      simple, but still handles the various cases correctly (e.g. `-B/70%`
      is apparently a legal setting).
    * Implement the `--log-size` option for `examples/log.c`. I think all
      the data is available, you would just need to add the code into the
      `print_commit()` routine (along with a way of passing the option
      into that function).
    * As an extension to the matching idea for `examples/log.c`, add the
      `-i` option to use `strcasestr()` for matches.
    * For `examples/log.c`, implement the `--first-parent` option now that
      libgit2 supports it in the revwalk API.
* Pick a Git command that is not already emulated in `examples/` and write
  a new example that mirrors the behavior.  Examples don't have to be
  perfect emulations, but should demonstrate how to use the libgit2 APIs
  to get results that are similar to Git commands.  This lets you (and us)
  easily exercise a particular facet of the API and measure compatability
  and feature parity with core git.
* Submit a PR to clarify documentation! While we do try to document all of
  the APIs, your fresh eyes on the documentation will find areas that are
  confusing much more easily.

If none of these appeal to you, take a look at our issues list to see if
there are any unresolved issues you'd like to jump in on.

## Larger Projects

These are ideas for larger projects mostly taken from our backlog of
[Issues](https://github.com/libgit2/libgit2/issues).  Please don't dive
into one of these as a first project for libgit2 - we'd rather get to
know you first by successfully shipping your work on one of the smaller
projects above.

Some of these projects are broken down into subprojects and/or have
some incremental steps listed towards the larger goal.  Those steps
might make good smaller projects by themselves.

* Port part of the Git test suite to run against the command line emulation
  in examples/
    * Pick a Git command that is emulated in our examples/ area
    * Extract the Git tests that exercise that command
    * Convert the tests to call our emulation
    * These tests could go in examples/tests/...
* Fix symlink support for files in the .git directory (i.e. don't overwrite
  the symlinks when writing the file contents back out)
* Add hooks API to enumerate and manage hooks (not run them at this point)
    * Enumeration of available hooks
    * Lookup API to see which hooks have a script and get the script
    * Read/write API to load a hook script and write a hook script
    * Eventually, callback API to invoke a hook callback when libgit2
      executes the action in question
* Isolate logic of ignore evaluation into a standalone API
* Upgrade internal libxdiff code to latest from core Git
* Improve index internals with hashtable lookup for files instead of
  using binary search every time
* Tree builder improvements:
    * Extend to allow building a tree hierarchy
* Apply-patch API
* Add a patch editing API to enable "git add -p" type operations
* Textconv API to filter binary data before generating diffs (something
  like the current Filter API, probably).
* Performance profiling and improvement
* Support "git replace" ref replacements
* Include conflicts in diff results and in status
    * GIT_DELTA_CONFLICT for items in conflict (with multiple files)
    * Appropriate flags for status
* Support sparse checkout (i.e. "core.sparsecheckout" and ".git/info/sparse-checkout")
