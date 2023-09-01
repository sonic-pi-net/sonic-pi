core.longpaths support
======================

Historically, Windows has limited absolute path lengths to `MAX_PATH`
(260) characters.

Unfortunately, 260 characters is a punishing small maximum.  This is
especially true for developers where dependencies may have dependencies
in a folder, each dependency themselves having dependencies in a
sub-folder, ad (seemingly) infinitum.

So although the Windows APIs _by default_ honor this 260 character
maximum, you can get around this by using separate APIs.  Git honors a
`core.longpaths` configuration option that allows some paths on Windows
to exceed these 260 character limits.

And because they've gone and done it, that means that libgit2 has to
honor this value, too.

Since `core.longpaths` is a _configuration option_ that means that we
need to be able to resolve a configuration - including in _the repository
itself_ in order to know whether long paths should be supported.

Therefore, in libgit2, `core.longpaths` affects paths in working
directories _only_.  Paths to the repository, and to items inside the
`.git` folder, must be no longer than 260 characters.

This definition is required to avoid a paradoxical setting: if you
had a repository in a folder that was 280 characters long, how would
you know whether `core.longpaths` support should be enabled?  Even if
`core.longpaths` was set to true in a system configuration file, the
repository itself may set `core.longpaths` to false in _its_ configuration
file, which you could only read if `core.longpaths` were set to true.

Thus, `core.longpaths` must _only_ apply to working directory items,
and cannot apply to the `.git` folder or its contents.
