The maintainers of the libgit2 project believe that having a stable API
to program against is important for our users and the ecosystem - whether
you're building against the libgit2 C APIs directly, creating a wrapper to
a managed language, or programming against one of those managed wrappers
like LibGit2Sharp or Rugged.

Our API stability considerations are:

* Our standard API is considered stable through a major release.

  * We define our "standard API" to be anything included in the "git2.h"
    header - in other words, anything defined in a header in the `git2`
    directory.

  * APIs will maintain their signature and will not be removed within a
    major release, but new APIs may be added.

  * Any APIs may be marked as deprecated within a major release, but will
    not be removed until the next major release (at the earliest).  You
    may define `GIT_DEPRECATE_HARD` to produce compiler warnings if you
    target these deprecated APIs.

  * We consider API compatibility to be against the C APIs.  That means
    that we may use macros to keep API compatibility - for example, if we
    rename a structure from `git_widget_options` to `git_foobar_options`
    then we would `#define git_widget_options git_foobar_options` to retain
    API compatibility.  Note that this does _not_ provide ABI compatibility.

* Our systems API is only considered stable through a _minor_ release.

  * We define our "systems API" to be anything included in the `git2/sys`
    directory.  These are not "standard" APIs but are mechanisms to extend
    libgit2 by adding new extensions - for example, a custom HTTPS transport,
    TLS engine, or merge strategy.

  * Additionally, the cmake options and the resulting constants that it
    produces to be "systems API".

  * Generally these mechanism are well defined and will not need significant
    changes, but are considered a part of the library itself and may need

  * Systems API changes will be noted specially within a release's changelog.

* Our ABI is only considered stable through a _minor_ release.

  * Our ABI consists of actual symbol names in the library, the function
    signatures, and the actual layout of structures.  These are only
    stable within minor releases, they are not stable within major releases
    (yet).

  * Since many FFIs use ABIs directly (for example, .NET P/Invoke or Rust),
    this instability is unfortunate.

  * In a future major release, we will begin providing ABI stability
    throughout the major release cycle.

  * ABI changes will be noted specially within a release's changelog.

* Point releases are _generally_ only for bugfixes, and generally do _not_
  include new features.  This means that point releases generally do _not_
  include new APIs.  Point releases will never break API, systems API or
  ABI compatibility.

