*   Add `Rugged::Repository#checkout_index`.

    This allows to perform checkout from a given GIT index.
    It might be handy in case of manual merge conflicts resolution with user intervention.

    *Dmytro Milinevskyy*

*   Add accessors for the Repository ident.

    Added `Repository#ident` and `Repository#ident=` to read and set the
    identity that is used when writing reflog entries.

    *Arthur Schreiber*

*   `Rugged::Remote` instances are now immutable.

    * `Remote#clear_refspecs` and `Remote#save` were removed without
      replacement.

    * `Remote#url=` and `Remote#push_url=` were removed and replaced by
      `RemoteCollection#set_url` and `RemoteCollection#set_push_url`.

    * `Remote#add_push` and `Remote#add_fetch` were removed and replaced by
      `RemoteCollection#add_push_refspec` and
      `RemoteCollection#add_fetch_refspec`.

    *Arthur Schreiber*

*   Update bundled libgit2 to 9042693e283f65d9afb4906ed693a862a250664b.

    *Arthur Schreiber*

*   Updated the API of reflog modifying methods.

    This removes both the optional `:message` as well as `:signature` options from
    the following methods:

    * `BranchCollection#create`, `BranchCollection#move`, `BranchCollection#rename`
    * `ReferenceCollection#create`, `ReferenceCollection#rename`
    * `Remote#push`
    * `Repository#reset`

    Additionally, the `:signature` option from `Remote#fetch` was removed as well.

    The reflog message is now automatically generated and committed with the
    the identity that is set in the Repository's configuration.

    *Arthur Schreiber*

*   The `:safe_create` flag was removed from `Repository#checkout_tree`.

    You can use `:create` in combination with `:recreate_missing` instead.

    *Arthur Schreiber*


## Rugged 0.22.2 (May 17, 2015) ##

*   Update bundled libgit2 to 0.22.2.

    See https://github.com/libgit2/libgit2/releases/tag/v0.22.0,
    https://github.com/libgit2/libgit2/releases/tag/v0.22.1 and
    https://github.com/libgit2/libgit2/releases/tag/v0.22.2 for a list
    of fixed issues and new features.

    *Arthur Schreiber*

*   Add `Rugged::Tree#count_recursive`.

    This counts all blobs in a tree, recursively, with an optional limit
    to bail early. This allows asking things like: "Are there more
    than 1 million files in this repo?" in a very performant way.

    Fixes #464.

    *Andy Delcambre*

*   Add missing handling of libgit2 errors in `Rugged::BranchCollection#each`
    and `Rugged::BranchCollection#each_name`.

    Fixes #457.

    *aiionx*

*   The `Rugged::Tree::Builder` API was changed to account for libgit2 changes.

    When creating a new `Rugged::Tree::Builder` instance through
    `Rugged::Tree::Builder.new` you have to pass a repository instance,
    while `Rugged::Tree::Builder#write` does not take any arguments anymore.

    *Vicent Martí*

*   Add alternative backend support (experimental).

    Bare repositories can now be stored using an alternative backend.

    Fixes #410.

    *Viktor Charypar*

*   Replace `Remote#rename!` with `RemoteCollection#rename`.

    This brings the `RemoteCollection` more in line with the API of
    `ReferenceCollection` and `BranchCollection`.

    *Arthur Schreiber*

*   Remove URL validation from `Remote#url=`, `Remote#push_url=`,
    `RemoteCollection#create_anonymous` and `RemoteCollection#create`,
    as the underlying function `git_remote_supported_url()` was removed
    from libgit2.

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


## Rugged 0.21.4 (January 18, 2015) ##

*   Update bundled libgit2 to 0.21.4.

    See https://github.com/libgit2/libgit2/releases/tag/v0.21.4 for a list
    of fixed issues.

    *Arthur Schreiber*


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
