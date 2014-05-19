# Welcome to libgit2!

We're making it easy to do interesting things with git, and we'd love to have
your help.

## Licensing

By contributing to libgit2, you agree to release your contribution under
the terms of the license.  Except for the `examples` directory, all code
is released under the [GPL v2 with linking exception](COPYING).

The `examples` code is governed by the
[CC0 Public Domain Dedication](examples/COPYING), so that you may copy
from them into your own application.

## Discussion & Chat

We hang out in the #libgit2 channel on irc.freenode.net.

Also, feel free to open an
[Issue](https://github.com/libgit2/libgit2/issues/new) to start a discussion
about any concerns you have.  We like to use Issues for that so there is an
easily accessible permanent record of the conversation.

## Reporting Bugs

First, know which version of libgit2 your problem is in and include it in
your bug report.  This can either be a tag (e.g.
[v0.17.0](https://github.com/libgit2/libgit2/tree/v0.17.0) ) or a commit
SHA (e.g.
[01be7863](https://github.com/libgit2/libgit2/commit/01be786319238fd6507a08316d1c265c1a89407f)
).  Using [`git describe`](http://git-scm.com/docs/git-describe) is a great
way to tell us what version you're working with.

If you're not running against the latest `development` branch version,
please compile and test against that to avoid re-reporting an issue that's
already been fixed.

It's *incredibly* helpful to be able to reproduce the problem.  Please
include a list of steps, a bit of code, and/or a zipped repository (if
possible).  Note that some of the libgit2 developers are employees of
GitHub, so if your repository is private, find us on IRC and we'll figure
out a way to help you.

## Pull Requests

Our work flow is a typical GitHub flow, where contributors fork the
[libgit2 repository](https://github.com/libgit2/libgit2), make their changes
on branch, and submit a
[Pull Request](https://help.github.com/articles/using-pull-requests)
(a.k.a. "PR").

Life will be a lot easier for you (and us) if you follow this pattern
(i.e. fork, named branch, submit PR).  If you use your fork's `development`
branch, things can get messy.

Please include a nice description of your changes with your PR; if we have
to read the whole diff to figure out why you're contributing in the first
place, you're less likely to get feedback and have your change merged in.

If you are working on a particular area then feel free to submit a PR that
highlights your work in progress (and flag in the PR title that it's not
ready to merge). This will help in getting visibility for your fix, allow
others to comment early on the changes and also let others know that you
are currently working on something.

## Porting Code From Other Open-Source Projects

`libgit2` is licensed under the terms of the GPL v2 with a linking
exception.  Any code brought in must be compatible with those terms.

The most common case is porting code from core Git.  Git is a pure GPL
project, which means that in order to port code to this project, we need the
explicit permission of the author.  Check the
[`git.git-authors`](https://github.com/libgit2/libgit2/blob/development/git.git-authors)
file for authors who have already consented.

Other licenses have other requirements; check the license of the library
you're porting code *from* to see what you need to do.  As a general rule,
MIT and BSD (3-clause) licenses are typically no problem.  Apache 2.0
license typically doesn't work due to GPL incompatibility.

If you are pulling in code from core Git, another project or code you've
pulled from a forum / Stack Overflow then please flag this in your PR and
also make sure you've given proper credit to the original author in the
code snippet.

## Style Guide

The public API of `libgit2` is [ANSI C](http://en.wikipedia.org/wiki/ANSI_C)
(a.k.a. C89) compatible.  Internally, `libgit2` is written using a portable
subset of C99 - in order to compile with GCC, Clang, MSVC, etc., we keep
local variable declarations at the tops of blocks only and avoid `//` style
comments.  Additionally, `libgit2` follows some extra conventions for
function and type naming, code formatting, and testing.

We like to keep the source code consistent and easy to read.  Maintaining
this takes some discipline, but it's been more than worth it.  Take a look
at the
[conventions file](https://github.com/libgit2/libgit2/blob/development/CONVENTIONS.md).

## Starter Projects

See our [projects list](https://github.com/libgit2/libgit2/blob/development/PROJECTS.md).
