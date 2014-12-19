
*   Replace `Remote#rename!` with `RemoteCollection#rename`.

    This brings the `RemoteCollection` more in line with the API of
    `ReferenceCollection` and `BranchCollection`.

    *Arthur Schreiber*

*   Remove URL validation from `Remote#url=`, `Remote#push_url=`,
    `RemoteCollection#create_anonymous` and `RemoteCollection#create`,
    as the underlying function `git_remote_supported_url()` was removed
    from libgit2.

    *Arthur Schreiber*

*   Update to the latest libgit2 version (ce8822cb409917d0201f359cabe3ae55d25895da).

    *Arthur Schreiber*

*   Add `Repository#merge_bases`.

    This returns an array containing all merge bases between one or
    multiple commits.

    *Arthur Schreiber*

*   Add submodule support.

    Expose git submodules functionality through `Rugged::Submodule` and
    `Rugged::SubmoduleCollection`.

    *Nikolai Vladimirov*

*   Add `Rugged::Walker#push_range`.

    *Evgeniy Sokovikov*

*   Implement `Rugged::Blob::HashSignature` and `Rugged::Blob#hashsig`.

    Allows similarity detection of `Rugged::Blob` instances against other blobs or
    arbitrary strings.

    *Vicent Martí*

*   Add `Rugged::Repository#attributes`.

    This method allows accessing the attributes for different path names as
    specified by `.gitattributes` files.

    *Vicent Martí*

*   Add `Rugged::TagCollection#create_annotation`.

    This method allows the creation of a tag object, but without creating
    a tag reference.

    *Charlie Somerville*

*   Add `Rugged::Repository#cherrypick`.

    *Arthur Schreiber*

*   Add `Rugged::Repository#descendant_of?`

    *Jake Douglas*

*   `Rugged::Index#read_tree` now actually checks that the given object is a
    `Rugged::Tree` instance.

    Fixes #401.

    *Andy Delcambre*

*   Add `Rugged::Repository#expand_oids`.

    This allows expanding a list of shortened SHA1 strings, optionally restricting
    the expansion to a specific object type.

    *Vicent Martí*

*   Add `Rugged::Remote#check_connection`.

    This is useful if one needs to check if it is possible to fetch/push
    from/to the remote.

    Basically, it is analogue to `git fetch --dry-run` and `git push --dry-run`.

    *Dmitry Medvinsky*

*   Remove defunct `Rugged::Diff::Line#hunk` and `Rugged::Diff::Line#owner`.

    Fixes #390.

    *Arthur Schreiber*

*   Remove `Rugged::Diff#tree` and change `Rugged::Diff#owner` to return the
    repository that the `Rugged::Diff` object belongs to.

    We need to keep a reference from the `Rugged::Diff` to the repository to
    ensure that the underlying libgit2 data does not get freed accidentally.

    Fixes #389.

    *Arthur Schreiber*

*   Add `#additions` and `#deletions` to `Rugged::Patch`.

    *Mindaugas Mozūras*


## Rugged 0.21.3 (December 18, 2014) ##

*   Update bundled libgit2 to 0.21.3.

    See https://github.com/libgit2/libgit2/releases/tag/v0.21.3 for a list
    of fixed issues.

    *Arthur Schreiber*


## Rugged 0.21.2 (November 16, 2014) ##

*   Update bundled libgit2 to 0.21.2 (from 0.21.0).

    See https://github.com/libgit2/libgit2/releases/tag/v0.21.1 and
    https://github.com/libgit2/libgit2/releases/tag/v0.21.2 for a list
    of fixed issues.

    *Arthur Schreiber*
